(ns bites.idz.hashids
  (:require [bites.util :as util]
            [clojure.string :as str]))


(def ^:const DEFAULT_ALPHABET "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
(def ^:const DEFAULT_SEPS "cfhistuCFHISTU")
(def ^:const DEFAULT_SALT "")
(def ^:const DEFAULT_MIN_LENGTH 0)
(def ^:const MIN_ALPHABET_LENGTH 16)
(def ^:const SEP_DIV (double (/ 7 2)))
(def ^:const GUARD_DIV 12)

(defn consistent-shuffle
  [alphabet salt]
  (if (empty? salt)
    alphabet
    (let [ab-length   (count alphabet)
          salt-length (count salt)]
      (loop [alph (transient (vec alphabet))
             idx  (int 0)
             p    (int 0)]
        (let [i (- ab-length idx 1)
              v (long (mod idx salt-length))
              n (int (nth salt v))
              p (unchecked-add-int p n)]
          (if (zero? i) ;; done
            (->> alph persistent! (apply str))
            (recur
              (util/vec-swap! alph i (mod (+ n v p) i))
              (unchecked-inc-int idx)
              p)))))))

(defn enhash
  ;; Known as 'hash' in other implementations
  ([^long input alphabet]
   (enhash input alphabet (num input) ""))
  ([^long input ^String alphabet n res]
   (let [ab-length (count alphabet)]
     (loop [n (long n)
            res res]
       (cond
         (zero? input) (subs alphabet 0 1)
         (zero? n) res
         :else
         (recur (long (/ n ab-length))
                (-> alphabet
                    (.charAt (mod n ab-length))
                    (str res))))))))

(defn dehash
  ;; Known as 'unhash' in other implementations
  [input ^String alphabet]
  (let [ab-length    (count alphabet)
        input-length (count input)]
    (transduce
      (keep-indexed
        (fn [^long idx c]
          (when-some [^long pos (->> (range ab-length)
                                     (util/find-first #(= ^char c (.charAt alphabet ^long %))))]
            (* pos (long (util/expt ab-length (- input-length idx 1)))))))

      +
      input)))

(defn encode-numbers
  [{:keys [seps alphabet salt hash-int numbers]
    :as args}]
  (let [ab-length (count alphabet)
        numbers-length (count numbers)
        lottery  (->> ab-length (mod hash-int) (nth alphabet) str)
        sepsc (cycle seps)
        [ab h] (reduce
                 (fn [[alph ret] [^long idx n]]
                   (let [buf     (concat lottery salt alph)
                         alph    (consistent-shuffle alph (into [] (take ab-length) buf))
                         encchar (enhash n alph)
                         addsep  (nth sepsc (mod n (unchecked-add (long (first encchar)) idx)))]
                     (if (< (unchecked-inc idx) numbers-length)
                       [alph (str ret encchar addsep)]
                       [alph (str ret encchar)])))
                 [alphabet lottery] ;; reduce by passing along alphabet, which is transformed in each iteration
                 (map-indexed vector numbers))]
    (assoc args
      :alphabet ab
      :hash-str h)))


(defn add-guards
  [{:keys [^long min-length guards ^long hash-int hash-str] :as all-args}]
  (let [prepend-guard #(if (< (count %) min-length)
                         (str (nth (cycle guards)
                                   (unchecked-add hash-int (long (first %))))
                              %)
                         %)
        append-guard #(if (< (count %) min-length)
                        (str %
                             (nth (cycle guards)
                                  (unchecked-add hash-int (long (nth % 2)))))
                        %)]
    (->> hash-str
         prepend-guard
         append-guard
         (assoc all-args :hash-str))))

(defn ensure-min-length
  [{:keys [^long min-length alphabet hash-str] :as all-args}]
  (let [half-length (long (/ (count alphabet) 2))
        upsize (fn [[alph ret]]
                 (let [alph (consistent-shuffle alph alph)
                       rplusalph (str (subs alph half-length) ret (subs alph 0 half-length))
                       ;_ (println (count rplusalph))
                       excess (unchecked-subtract (count rplusalph) min-length)
                       ret-start (long (/ excess 2))
                       ret-end (+ ret-start min-length)]
                   (if (pos? excess)
                     [alph (subs rplusalph ret-start ret-end)]
                     [alph rplusalph])))
        [ab h] (->> [alphabet hash-str]
                    (iterate upsize)
                    (util/find-first #(>= (count (second %)) min-length)))]
    (assoc all-args :alphabet ab :hash-str h)))

(defn balance-seps
  "Balance alphabet and seps, the ratio of sizes of which should SEP_DIV"
  [seps alph]
  (let [ab-length (count alph)
        seps-length-ceil (util/ceil (/ ab-length SEP_DIV))
        seps-length (if (= 1 seps-length-ceil) 2 seps-length-ceil)
        seps-diff (unchecked-subtract seps-length (count seps))
        [ab-left ab-right] (map str/join (split-at seps-diff alph))]
    (if (or (empty? seps)
            (> (double (/ ab-length (count seps)))
               SEP_DIV))
      (if (pos? seps-diff)
        [(str seps ab-left) ab-right]
        [(subs seps 0 seps-length) (str/join alph)])
      [(str/join seps) (str/join alph)])))

(defn extract-guards
  "Take portions of seps or alphabet to make guards"
  [alph seps]
  (let [guard-length (util/ceil (/ (count alph) GUARD_DIV))]
    (if (< (count alph) 3)
      {:guards (subs seps 0 guard-length)
       :seps (subs seps guard-length)
       :alphabet alph}
      {:guards (subs alph 0 guard-length)
       :seps seps
       :alphabet (subs alph guard-length)})))


(defn setup
  ([] (setup {}))
  ([{:keys [seps alphabet salt min-length]
     :or {seps       DEFAULT_SEPS
          alphabet   DEFAULT_ALPHABET
          salt       DEFAULT_SALT
          min-length DEFAULT_MIN_LENGTH}}]
   {:pre  [(>= (count alphabet) MIN_ALPHABET_LENGTH)]}
   (let [alph-unbal (->> (util/chars-subtraction alphabet seps) ;; Alphabet should not contains seps
                         distinct
                         util/strip-whitespace)
         seps-unbal (->> (util/chars-intersection alphabet seps) ;; Seps should only contain characters present in alphabet
                         distinct
                         util/strip-whitespace)
         [seps-bal alph-bal] (-> (consistent-shuffle seps-unbal salt)
                                 (balance-seps alph-unbal))]
     (-> (consistent-shuffle alph-bal salt)
         (extract-guards seps-bal)
         (assoc :min-length min-length
                :salt salt)))))

(defn- encode-intern
  [opts numbers]
  ;{:pre [(coll? numbers)]}
  (let [settings (setup opts)
        hash-int (transduce
                   (map-indexed
                     (fn [^long idx num]
                       (mod num (unchecked-add idx 100))))
                   +
                   numbers)]
    (->> (assoc settings :hash-int hash-int
                         :numbers numbers)
         encode-numbers
         add-guards
         ensure-min-length)))

;; PUBLIC API
;;-----------

(defn encode
  "Encodes a seq of positive integers <nums> (presumably ids)
   into a `hashids` String (per <opts>). Avoid passing in empty <nums>."
  [opts nums]
  {:pre [(every? nat-int? nums)]}
  (-> (encode-intern opts nums)
      :hash-str))

(defn decode
  "Decodes a hashids String into a vector of positive integer(s)."
  [opts ^String encoded]
  {:pre [(seq encoded)]}
  (let [{:keys [seps alphabet salt guards]} (setup opts)
        breakdown (util/split-on-chars encoded guards)
        breakdown-length (count breakdown)
        breakdown-idx (if (or (== breakdown-length 2)
                              (== breakdown-length 3))
                        1
                        0)
        bdn       (str/join (nth breakdown breakdown-idx))
        lottery   (first bdn)
        ab-length (count alphabet)
        arr       (util/split-on-chars (subs bdn 1) seps)
        decoded-result (loop [ret (transient [])
                              ab alphabet
                              arr arr]
                         (if-some [sub-hash (first arr)]
                           (let [buf  (str lottery salt ab)
                                 alph (consistent-shuffle ab (into [] (take ab-length) buf))]
                             (recur
                               (conj! ret (dehash sub-hash alph))
                               alph
                               (next arr)))
                           (persistent! ret)))]

    (if (:confirm? opts true) ;; let consumers bypass this
      (if (= encoded (encode opts decoded-result))
        decoded-result
        [])
      decoded-result)))

(comment
  (require '[criterium.core :refer :all])

  (let [ids (range 10)]
    (bench ;; Execution time mean : 374.496413 µs
      (encode
        {:salt "this is my salt"
         :min-length 8}
        ids)))

  (bench
    (decode ;; Execution time mean : 354.487376 µs
      {:salt "this is my salt"
       :confirm? false}
      "N4UzHgunhEtycqIZTNCM"))
  )
