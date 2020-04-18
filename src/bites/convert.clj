(ns bites.convert
  (:require [bites.util :as ut]
            [clojure.java.io :as io])
  (:import (java.nio.charset Charset)
           (java.util UUID)
           (java.nio ByteBuffer)
           (java.io InputStream ByteArrayOutputStream ByteArrayInputStream File Serializable ObjectOutputStream ObjectInputStream FileInputStream)
           (java.net URL URI)))
;;==============<ABSTRACTIONS>================
(defprotocol ToBytes (toBytes ^bytes [x opts]))
(defmulti fromBytes (fn [klass x opts] klass))
;;==============<PRIVATE HELPERS>================
(defn- base-encoded
  [^String s enc b64-flavor]
  (if (.isEmpty s)
    (byte-array 0)
    (case enc
    :uuid (-> (UUID/fromString s) (toBytes s))
    :b2  (ut/binary-bytes s)
    :b64 (ut/b64-bytes s b64-flavor)
    :b16 (ut/b16-bytes s)
    nil)))

(defn- base-decoded
  ^String [^bytes bs enc b64-flavor]
  (if (empty? bs)
    ""
    (case enc
    :uuid (str (fromBytes UUID bs nil))
    :b2  (ut/binary-str bs)
    :b64 (ut/b64-str bs b64-flavor)
    :b16 (ut/b16-str bs)
    nil)))
;;==============<CONCRETIONS>================
(defmethod fromBytes UUID
  [_ ^bytes bs _]
  (let [bb   (ByteBuffer/wrap bs)
        high (.getLong bb)
        low  (.getLong bb)]
    (UUID. high low)))

(defmethod fromBytes Integer
  [_ ^bytes bs _]
  (-> bs ByteBuffer/wrap .getInt))

(defmethod fromBytes Long
  [_ ^bytes bs _]
  (-> bs ByteBuffer/wrap .getLong))

(defmethod fromBytes Double
  [_ ^bytes bs _]
  (-> bs ByteBuffer/wrap .getDouble))

(defmethod fromBytes BigInteger
  [_ ^bytes bs _]
  (BigInteger. bs))

(defmethod fromBytes BigDecimal
  [_ ^bytes bs _]
  (let [scale-bs   (byte-array (take 4 bs))
        ^int scale (fromBytes Integer scale-bs nil)
        unscaled-value-bs (byte-array (drop 4 bs))
        unscaled-value (BigInteger. unscaled-value-bs)]
    (BigDecimal. unscaled-value scale)))

(defmethod fromBytes InputStream
  [_ ^bytes bs _]
  (ByteArrayInputStream. bs))

(defmethod fromBytes String
  [_ ^bytes bs opts]
  (if-let [enc (:encoding opts)]
    (condp instance? enc
      Charset (String. bs ^Charset enc)
      String  (String. bs ^String enc)
      (base-decoded bs enc (:b64-flavor opts)))
    (String. bs)))

(defmethod fromBytes Serializable
  [_ ^bytes bs _]
  (let [in (ByteArrayInputStream. bs)]
    (with-open [oin (ObjectInputStream. in)]
      (.readObject oin))))

;;----------------------------------------------
(extend-protocol ToBytes
  Integer
  (toBytes [this _]
    (let [bb (ByteBuffer/allocate Integer/BYTES)]
      (.putInt bb this)
      (.array bb)))

  Long
  (toBytes [this _]
    (let [bb (ByteBuffer/allocate Long/BYTES)]
      (.putLong bb this)
      (.array bb)))

  Double
  (toBytes [this _]
    (let [bb (ByteBuffer/allocate Double/BYTES)]
      (.putDouble bb this)
      (.array bb)))

  BigInteger
  (toBytes [this _]
    (.toByteArray this))

  BigDecimal
  (toBytes [this _]
    (let [bi        (.unscaledValue this)
          bi-bytes  (.toByteArray bi)
          bi-length (alength bi-bytes)
          scale     (.scale this)
          scale-bytes (toBytes scale nil)
          ret (byte-array (+ 4 bi-length) scale-bytes)]
      (System/arraycopy bi-bytes 0 ret 4 bi-length)
      ret))

  String
  (toBytes [this opts]
    (if-let [enc (:encoding opts)]
      (condp instance? enc
        Charset (.getBytes this ^Charset enc)
        String  (.getBytes this ^String  enc)
        (base-encoded this enc (:b64-flavor opts)))
      (.getBytes this)))

  UUID
  (toBytes [this _]
    (let [bb (ByteBuffer/allocate 16)]
      (.putLong bb (.getMostSignificantBits  this))
      (.putLong bb (.getLeastSignificantBits this))
      (.array bb)))

  InputStream
  (toBytes [this opts]
    (let [buffer (:buffer-size opts 1024)
          out (ByteArrayOutputStream. buffer)]
      ;; does NOT .close() this
      (io/copy this out :buffer-size buffer)
      (.toByteArray out)))

  File
  (toBytes [this opts]
    (with-open [in (FileInputStream. this)]
      ;; delegates to `InputStream` impl
      (toBytes in opts)))

  URL
  (toBytes [this opts]
    (with-open [in (.openStream this)]
      ;; delegates to `InputStream` impl
      (toBytes in opts)))

  URI
  (toBytes [this opts]
    ;; delegates to `URL` impl
    (-> this .toURL (toBytes opts)))

  Serializable
  (toBytes [this opts]
    (let [buffer (:buffer-size opts 1024)
          out (ByteArrayOutputStream. buffer)]
      (with-open [oout (ObjectOutputStream. out)]
        (.writeObject oout this)
        (.flush oout)
        (.toByteArray out))))

  )