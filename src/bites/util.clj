(ns bites.util
  (:require [clojure.string :as str])
  (:import  [java.util Base64]))

(defn octal-bytes
  ^bytes [^String s]
  (let [bi (BigInteger. s 8)]
    (.toByteArray bi)))

(defn octal-str
  ^String [^bytes bs]
  (let [bi (BigInteger. bs)]
    (.toString bi 8)))

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
  (^bytes [bs]
   (b64-bytes bs nil))
  (^bytes [^String s input-type]
   (case input-type
     :url (-> (Base64/getUrlDecoder)
              (.decode s))
     :mime (-> (Base64/getMimeDecoder)
               (.decode s))
     (-> (Base64/getDecoder)
         (.decode s)))))
;;======================================
(defn b16-str
  "Encodes the provided byte-array <bs> in Base16 (i.e. hex).
   Returns String."
  (^String [bs]
   (b16-str bs :lower))
  (^String [^bytes bs char-case]
   (let [fmt (str "%0"
                  (bit-shift-left (alength bs) 1)
                  (case char-case
                    :upper \X
                    :lower \x))]
     (format fmt (BigInteger. 1 bs)))))

(defn b16-bytes
  "Decodes the provided String <bs> from Base16 (i.e. hex).
   Returns byte-array."
  ^bytes [^String s]
  (let [bs (.toByteArray (BigInteger. s 16))
        bs-len (alength bs)
        proper-len (bit-shift-right (.length s) 1)]
    (cond
      (= bs-len proper-len)
      bs

      (> proper-len bs-len)
      (let [padding (- proper-len bs-len)
            ret (byte-array (+ padding bs-len)
                            (repeat padding (byte 0)))]
        (System/arraycopy bs 0 ret padding bs-len)
        ret)

      :else
      (byte-array (next bs)))))
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
      (str/join (concat (repeat padding \0) ret)))))

(defn binary-bytes
  "Decodes the provided String <bs> from Base2 (i.e. binary).
   If the length of <s> is not cleanly divisible by 8,
   any excess bits will be ignored. Returns byte-array."
  ^bytes [^String s]
  (->> s
       (partition 8)
       (map #(Integer/parseInt (str/join %) 2))
       byte-array))

(defn unsigned
  [x]
  (assert (<= x 255)
          "Integer value cannot be greater than 255")
  (assert (>= x 0)
          "Integer value cannot be less than 0")
  (if (> x 127)
    (byte (bit-or -128 (- x 128)))
    (byte x)))

(defmacro current-thread-interrupted? []
  `(.isInterrupted (Thread/currentThread)))

