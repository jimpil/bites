(ns bites.util
  (:require [clojure.string :as str]
            [bites.constants :as const]
            [clojure.set :as set])
  (:import [java.util Base64 Arrays Collections]
           (java.nio ByteBuffer)
           (java.nio.charset Charset CharsetEncoder)
           (clojure.lang IReduceInit)
           (java.nio.channels ReadableByteChannel WritableByteChannel)
           (java.lang.reflect Field Modifier Constructor)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn str!
  ([]                    (StringBuilder.)) ;; init
  ([^StringBuilder sb]   (.toString sb))   ;; complete
  ([^StringBuilder sb x] (.append sb x)))  ;; accumulate

(defn ab-divide
  "Calculates a sequence of tokens (from alphabet) for a byte array."
  [^String alphabet ^bytes bs]
  (let [base (count alphabet)]
    (loop [n (bigint (BigInteger. 1 bs))
           tokens (list)]
      (if (< n base)
        (conj tokens (.charAt alphabet (int n)))
        (let [digit (mod n base)]
          (recur
            (/ (- n digit) base)
            (conj tokens (.charAt alphabet (int digit)))))))))

(defn ab-multiply
  "Uses optimized big-integer multiplication to decode a sequence of byte
  values from a string of tokens. Only works in Clojure!"
  [^String alphabet tokens]
  ;; Bigint math: ~74 µs
  (let [data (->> (reverse tokens)
                  (map vector (iterate (partial * (count alphabet)) 1N))
                  ^clojure.lang.BigInt
                  (reduce
                    (fn read-token
                      [n [base token]]
                      (let [digit (.indexOf alphabet (str token))]
                        (if (neg? digit)
                          (throw (ex-info
                                   (str "Invalid token: " (pr-str token)
                                        " is not in alphabet " (pr-str alphabet))
                                   {:alphabet alphabet
                                    :token token}))
                          (+ n (* (bigint digit) base)))))
                    0N)
                  .toBigInteger
                  .toByteArray)]
    (if (and (> (alength data) 1)
             (zero? (aget data 0))
             (neg? (aget data 1)))
      (vec (next data))
      (vec data))))

(defn find-first
  [pred coll]
  (some #(when (pred %) %) coll))

(defn octal-bytes
  ^bytes [^String s]
  {:pre [(false? (.isEmpty s))]}
  (-> s
      (BigInteger. 8)
      .toByteArray))

(defn octal-str
  ^String [^bytes bs]
  {:pre [(pos? (alength bs))]}
  (-> (BigInteger. bs)
      (.toString  8)))

(defn b58-str
  [^bytes bs]
  (let [zeroes (count (take-while zero? bs))]
    (->>
      (when (< zeroes (alength bs))
        (ab-divide const/B58-alphabet bs))
      (concat (repeat zeroes (first const/B58-alphabet)))
      (apply str))))

(defn b58-bytes
  [^String s]
  (let [zeroes (count (take-while #{(first const/B58-alphabet)} s))]
    (if (== zeroes (count s))
      (byte-array zeroes)
      (let [byte-seq (ab-multiply const/B58-alphabet s)
            data (byte-array (+ zeroes (count byte-seq)))]
        (dotimes [i (count byte-seq)]
          (aset data (+ zeroes i) (byte (nth byte-seq i))))
        data))))

(defn b64-str
  "Encodes the provided byte-array <bs> in Base64.
   Returns String."
  (^String [bs]
   (b64-str bs nil))
  (^String [^bytes bs input-type]
   (case input-type
     :url (-> (Base64/getUrlEncoder)
              (.encodeToString bs))
     :mime (-> (Base64/getMimeEncoder)
               (.encodeToString bs))
     (-> (Base64/getEncoder)
         (.encodeToString bs)))))

(defn b64-bytes
  "Decodes the provided String <s> from Base64.
   Returns byte-array."
  (^bytes [s]
   (b64-bytes s nil))
  (^bytes [^String s input-type]
   (case input-type
     :url (-> (Base64/getUrlDecoder)
              (.decode s))
     :mime (-> (Base64/getMimeDecoder)
               (.decode s))
     (-> (Base64/getDecoder)
         (.decode s)))))
;;======================================
(defn byte->hex
  "Convert a single byte value to a two-character hex string."
  [b]
  (let [hex (->> (byte b) Byte/toUnsignedInt Integer/toHexString)]
    (cond->> hex
             (== 1 (count hex))
             (str \0))))

(defn hex->byte
  "Convert a two-character hex string to a byte value."
  [octet]
  (let [b (Integer/parseInt octet 16)]
    (cond-> b
            (> b 127)
            (- const/MAX_UNSIGNED_BYTE))))

(defn b16-str
  "Encodes the provided byte-array <bs> in Base16 (i.e. hex).
   Returns String."
  (^String [bs]
   (b16-str bs :lower))
  (^String [^bytes bs char-case]
   (let [ret (transduce
               (map byte->hex)
               str!
               (StringBuilder. (int (* 2 (alength bs))))
               bs)]
     (cond-> ret (= :upper char-case) str/upper-case))))

(defn b16-bytes
  "Decodes the provided String <bs> from Base16 (i.e. hex).
   Returns byte-array."
  ^bytes [^String s]
  (let [length (int (/ (count s) 2))
        data (byte-array length)]
    (dotimes [i length]
      (let [octet (subs s (* 2 i) (* 2 (unchecked-inc i)))]
        (aset-byte data i (byte (hex->byte octet)))))
    data))
;;=======================================

(defn binary-str
  "Encodes the provided byte-array <bs> in Base2 (i.e. binary).
   Returns String."
  ^String [^bytes bs]
  (let [ret (-> (BigInteger. 1 bs) (.toString 2))
        ret-length (* (alength bs) 8)
        padding (- ret-length (.length ret))]
    (if (zero? padding)
      ret
      (str (.repeat "0" padding) ret))))

(defn binary-bytes
  "Decodes the provided String <bs> from Base2 (i.e. binary).
   If the length of <s> is not cleanly divisible by 8,
   any excess bits will be ignored. Returns byte-array."
  ^bytes [^String bit-str]
  ;{:pre [(-> bit-str count (rem 8) zero?)]}
  (->> bit-str
       (partition 8)
       (map (fn [bits]
              (let [i (-> bits str/join (Integer/parseInt 2))]
                (cond-> i
                        (> i Byte/MAX_VALUE)
                        (- const/MAX_UNSIGNED_BYTE)))))
       byte-array))

(defmacro current-thread-interrupted? []
  `(.isInterrupted (Thread/currentThread)))

(defn pad-bits
  [^long target-length ^String bits]
  (let [bits-length (count bits)]
    (cond-> bits
            (> target-length bits-length)
            (->> ;; pad it at the front
              (concat (repeat (- target-length bits-length) \0))
              (apply str)))))

(defn bytes->bits
  ^String [^bytes bs]
  (let [size (long (* 8 (alength bs)))
        bi   (BigInteger. 1 bs)
        bits (.toString bi 2)]
    (cond->> bits
             (> size (.length bits))
             (pad-bits size))))

(defn  copy-of-byte-array
  (^bytes [^bytes bs]
   (aclone bs))
  (^bytes [^bytes bs from]
   (copy-of-byte-array bs from (alength bs)))
  (^bytes [^bytes bs from to]
   (Arrays/copyOfRange bs (int from) (int to))))

(defn copy-bytes!
  [^bytes source ^bytes dest]
  {:pre [(== (alength source)
             (alength dest))]}
  (areduce source i ret dest (doto ret (aset i (aget source i)))))

(defn concat-byte-arrays
  ^bytes [& arrays]
  (let [total-length (transduce (map #(alength ^bytes %)) + arrays)
        ^ByteBuffer buffer (reduce
                             (fn [^ByteBuffer buff ^bytes arr]
                               (.put buff arr))
                             (ByteBuffer/allocate total-length)
                             arrays)]
    (.array buffer)))

(defn charset-encoder
  ^CharsetEncoder [opts]
  (-> opts
      (:encoding const/UTF-8)
      (Charset/forName)
      .newEncoder))

(def not-neg? (complement neg?))

(defn update!
  ([m k f]
   (assoc! m k (f (get m k))))
  ([m k f arg1]
   (assoc! m k (f (get m k) arg1)))
  ([m k f arg1 arg2]
   (assoc! m k (f (get m k) arg1 arg2)))
  ([m k f arg1 arg2 arg3]
   (assoc! m k (f (get m k) arg1 arg2 arg3)))
  ([m k f arg1 arg2 arg3 & args]
   (assoc! m k (apply f (get m k) arg1 arg2 arg3 args))))

(defn map-vals
  "Transforms the values of map <m>,
   passing each one through the provided function."
  [f m]
  (persistent!
    (reduce-kv
      #(assoc! %1 %2 (f %3))
      (transient {}) m)))

(defn reverse-bytes
  (^bytes [array]
   (reverse-bytes array 0))
  ([^bytes array from]
   (reverse-bytes array from (alength array)))
  (^bytes [^bytes array ^long from ^long to] ;; inclusive/exclusive
   ;{:pre [(< from to)]}
   (if (zero? (alength array))
     array
     (let [target-length (- to from)
           bs (byte-array target-length)]
       (dotimes [idx target-length]
         (->> (- to idx)
              unchecked-dec
              (aget array)
              (aset bs idx)))
       bs))))

(defn string-bytes
  ^bytes [^String encoding ^String s]
  (.getBytes s encoding))

(defn reducible-range
  ([]
   (reify IReduceInit
     (reduce [_ f init]
       (loop [result init
              i 0]
         (if (reduced? result)
           @result
           (recur (f result i) (unchecked-inc i)))))))
  ([end]
   (reducible-range 0 end))
  ([start end]
   (reducible-range start end 1))
  ([^long start ^long end ^long step]
   (reify IReduceInit
     (reduce [_ f init]
       (loop [result init,
              i start]
         (if (== i end)
           @(ensure-reduced result)
           (recur (f result i) (unchecked-add i step))))))))

(defn strict-map
  [m lenient?]
  (fn [k]
    (if-some [value (m k)]
      value
      (if lenient?
        k
        (throw
          (ex-info (str "Unknown enum key: " k)
                   {:enum m :key k}))))))

(defn transfer!
  ([in out]
   (transfer! in out nil))
  ([^ReadableByteChannel in ^WritableByteChannel out opts]
   (let [buff (ByteBuffer/allocate (:buffer-size opts const/DEFAULT_BUFFER_SIZE))]
     ;; per the Java docs
     (while (not-neg? (.read in buff))
       (.flip buff)
       (.write out buff)
       (.compact buff)))))

(defn rand-long
  (^long [^long minimum maximum]
   (+ minimum (rand-long maximum)))
  (^long [maximum]
   (long (rand maximum))))

(defn name++
  "Like `clojure.core/name`, but takes into account the namespace."
  [x]
  (if (string? x)
    x
    (if-some [ns-x (namespace x)]
      (str ns-x \/ (name x))
      (name x))))

(defn get-class-fields
  [^Class klass]
  (->> (.getDeclaredFields klass)
       (remove (fn [^Field x]
                 (Modifier/isStatic (.getModifiers x))))))

(def ^:private keym
  {:boolean Boolean/TYPE
   :byte    Byte/TYPE
   :double  Double/TYPE
   :float   Float/TYPE
   :int     Integer/TYPE
   :long    Long/TYPE
   :short   Short/TYPE})

(def ^:private prims
  (map keym [:boolean :byte :double :float :int :long :short]))

(def ^:private boxed
  [Boolean Byte Double Float Integer Long Short])

(def ^:private convm
  (zipmap (concat prims boxed)
          (concat boxed prims)))

(defn find-best-ctors
  [^Class klass args]
  (let [args (->> args
                  (map #(if (class? %) % (keyword %)))
                  (map #(keym % %)))
        ctors (->> (.getConstructors klass)
                   (filter #(== (count args) (count (.getParameterTypes ^Constructor %))))
                   (filter #(every? (fn [[^Class pt a]]
                                      (or (.isAssignableFrom pt a)
                                          (if-let [^Class pt* (convm pt)]
                                            (.isAssignableFrom pt* a))))
                                    (zipmap (.getParameterTypes ^Constructor %) args))))]
    (when (seq ctors)
      (let [count-steps (fn count-steps [pt a]
                          (loop [ks #{a} cnt 0]
                            (if (or (ks pt) (ks (convm pt)))
                              cnt
                              (recur (set (mapcat parents ks)) (inc cnt)))))
            steps (map (fn [^Constructor ctor]
                         (map count-steps (.getParameterTypes ctor) args))
                       ctors)
            m (zipmap steps ctors)
            min-steps (->> steps
                           (apply min-key (partial apply max))
                           (apply max))]
        (->> m
             (filter (comp #{min-steps} (partial apply max) key))
             vals)))))

(defn vec-swap! [v i1 i2]
  (assoc! v
          i2 (v i1)
          i1 (v i2)))

(defn strip-whitespace
  [characters]
  (remove #(Character/isWhitespace ^char %) characters))

(defn chars-intersection
  [str1 str2]
  (keep #(some #{%} str2) (distinct str1)))

(defn chars-subtraction
  [str1 str2]
  (remove #(some #{%} str2) str1))

(defn ceil
  ^long [v]
  (long (Math/ceil v)))

(defn expt
  [b e]
  (Math/pow b e))

(defmacro xor
  ([] nil)
  ([a] a)
  ([a b]
   `(let [a# ~a
          b# ~b]
      (if a#
        (if b# false a#)
        (if b# b# false)))))

(defn split-on-chars
  [^String instr splitstr]
  (let [instr-length (.length instr)
        sep-set (set splitstr)]
    (loop [idx 0
           chg false
           letters (transient [])]
      (if (< idx instr-length)
        (let [letter (.charAt instr idx)
              sep?   (contains? sep-set letter) ;(boolean (some #{letter} splitstr))
              this-chg (xor chg sep?)]
          (recur (unchecked-inc idx)
                 this-chg
                 (cond-> letters
                         (not sep?)
                         (conj! [this-chg letter]))))
        (->> letters
             persistent!
             (partition-by first)
             (mapv (partial map second)))))))


(comment

  (seq (octal-bytes "31646541")) ;; => (103, 77, 97)
  )
