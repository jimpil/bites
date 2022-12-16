(ns bites.buffer
  (:require [bites.util :as util])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.util Arrays)))

(defn set-byte-order!
  [^ByteBuffer bb bo]
  (case bo
    :be (.order bb ByteOrder/BIG_ENDIAN)
    :le (.order bb ByteOrder/LITTLE_ENDIAN)
    bb))

(defn ^ByteBuffer byte-buffer
  ([n] (byte-buffer n nil))
  ([^long n endianess]
   (-> (ByteBuffer/allocate n)
       (set-byte-order! endianess))))

(defn ^ByteBuffer wrap
  ([^bytes bs] (wrap bs nil))
  ([^bytes bs endianess]
   (-> (-> (ByteBuffer/wrap bs)
           (set-byte-order! endianess)))))

(defn read-byte   [^ByteBuffer b] (.get b))
(defn read-short  [^ByteBuffer b] (.getShort b))
(defn read-int    [^ByteBuffer b] (.getInt b))
(defn read-float  [^ByteBuffer b] (.getFloat b))
(defn read-long   ^long [^ByteBuffer b] (.getLong b))
(defn read-double ^double [^ByteBuffer b] (.getDouble b))
(defn read-ulong  [^ByteBuffer b]
  (BigInteger. 1 ^bytes
               (cond-> (.array b)
                       (= ByteOrder/LITTLE_ENDIAN (.order b))
                       util/reverse-bytes)))

(defn write-byte   [^ByteBuffer b x] (.put b (byte x)))
(defn write-short  [^ByteBuffer b x] (.putShort b x))
(defn write-int    [^ByteBuffer b x] (.putInt b x))
(defn write-float  [^ByteBuffer b x] (.putFloat b x))
(defn write-long   [^ByteBuffer b x] (.putLong b x))
(defn write-double [^ByteBuffer b x] (.putDouble b x))
(defn write-ulong  [^ByteBuffer b ^BigInteger x]
  (let [^bytes bigint-bs (.toByteArray x)
        len (alength bigint-bs)
        diff (- 8 len)
        le? (= ByteOrder/LITTLE_ENDIAN (.order b))]
    (cond
      (neg? diff)
      (let [^bytes ret (if le? ;; ignore sign
                         (util/reverse-bytes bigint-bs 1)
                         (Arrays/copyOfRange bigint-bs 1 len))]
        (.put b ret))
      (pos? diff)
      (.putLong b (.longValue x)) ;; long fits
      :else
      (let [^bytes ret (cond-> bigint-bs le? util/reverse-bytes)]
        (.put b ret)))))