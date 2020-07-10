(ns bites.array
  (:require [bites
             [io]
             [protocols :as proto]
             [constants :as constants]
             [util :as ut]]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer ReadOnlyBufferException ByteOrder CharBuffer)
           (java.nio.charset Charset CharsetEncoder)
           (java.nio.channels FileChannel ReadableByteChannel Channels)
           (java.net URI URL)
           (java.io File InputStream ByteArrayOutputStream ObjectOutputStream Serializable ByteArrayInputStream ObjectInputStream FileInputStream)
           (java.util UUID Arrays)
           (javax.imageio ImageIO)
           (java.awt.image BufferedImage)))



;;==============<PRIVATE HELPERS>================
(defn- base-encoded
  [^String s enc b64-flavor]
  (if (empty? s)
    (byte-array 0)
    (case enc
      :uuid (-> (UUID/fromString s) (proto/toBytes s))
      :b2  (ut/binary-bytes s)
      :b8  (ut/octal-bytes s)
      :b64 (ut/b64-bytes s b64-flavor)
      :b16 (ut/b16-bytes s)
      nil)))

(defn- base-decoded
  ^String [^bytes bs enc b64-flavor]
  (if (empty? bs)
    constants/EMPTY_STRING
    (case enc
      :uuid (str (proto/fromBytes UUID bs nil))
      :b2  (ut/binary-str bs)
      :b8  (ut/octal-str bs)
      :b64 (ut/b64-str bs b64-flavor)
      :b16 (ut/b16-str bs)
      nil)))

;;==============<CONCRETIONS>================
(defmethod proto/fromBytes UUID
  [_ ^bytes bs _]
  (let [bb   (ByteBuffer/wrap bs)
        high (.getLong bb)
        low  (.getLong bb)]
    (UUID. high low)))

(defmethod proto/fromBytes Integer
  [_ ^bytes bs _]
  (-> bs ByteBuffer/wrap .getInt))

(defmethod proto/fromBytes Long
  [_ ^bytes bs _]
  (-> bs ByteBuffer/wrap .getLong))

(defmethod proto/fromBytes Double
  [_ ^bytes bs _]
  (-> bs ByteBuffer/wrap .getDouble))

(defmethod proto/fromBytes BigInteger
  [_ ^bytes bs _]
  (BigInteger. bs))

(defmethod proto/fromBytes BigDecimal
  [_ ^bytes bs _]
  (let [scale-bs   (byte-array (take 4 bs))
        ^int scale (proto/fromBytes Integer scale-bs nil)
        unscaled-value-bs (byte-array (drop 4 bs))
        unscaled-value (BigInteger. unscaled-value-bs)]
    (BigDecimal. unscaled-value scale)))

(defmethod proto/fromBytes InputStream
  [_ ^bytes bs _]
  (ByteArrayInputStream. bs))

(defmethod proto/fromBytes String
  [_ ^bytes bs opts]
  (if-let [enc (:encoding opts)]
    (condp instance? enc
      Charset (String. bs ^Charset enc)
      String  (String. bs ^String enc)
      (base-decoded bs enc (:b64-flavor opts)))
    (String. bs)))

(defmethod proto/fromBytes BufferedImage
  [_ ^bytes bs _]
  (let [in (ByteArrayInputStream. bs)]
    (ImageIO/read in)))

(defmethod proto/fromBytes ByteBuffer
  [_ ^bytes bs _]
  (ByteBuffer/wrap bs))

(defmethod proto/fromBytes ReadableByteChannel
  [_ ^bytes bs _]
  (let [in (ByteArrayInputStream. bs)]
    (Channels/newChannel in)))

(defmethod proto/fromBytes Serializable
  [_ ^bytes bs _]
  (let [in (ByteArrayInputStream. bs)]
    (with-open [oin (ObjectInputStream. in)]
      (.readObject oin))))

;;----------------------------------------------
(defn- set-byte-order!
  [^ByteBuffer bb bo]
  (case bo
    :be (.order bb ByteOrder/BIG_ENDIAN)
    :le (.order bb ByteOrder/LITTLE_ENDIAN)
    nil))

(extend-protocol proto/ToByteArray
  Short
  (toBytes [this opts]
    (let [bb (ByteBuffer/allocate Short/BYTES)]
      (some->> opts :byte-order (set-byte-order! bb))
      (.putShort bb this)
      (.array bb)))

  Integer
  (toBytes [this opts]
    (let [bb (ByteBuffer/allocate Integer/BYTES)]
      (some->> opts :byte-order (set-byte-order! bb))
      (.putInt bb this)
      (.array bb)))

  Long
  (toBytes [this opts]
    (let [bb (ByteBuffer/allocate Long/BYTES)]
      (some->> opts :byte-order (set-byte-order! bb))
      (.putLong bb this)
      (.array bb)))

  Double
  (toBytes [this opts]
    (let [bb (ByteBuffer/allocate Double/BYTES)]
      (some->> opts :byte-order (set-byte-order! bb))
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
          scale-bytes (proto/toBytes scale nil)
          ret (byte-array (+ 4 bi-length) scale-bytes)]
      (System/arraycopy bi-bytes 0 ret 4 bi-length)
      ret))

  ByteArrayOutputStream
  (toBytes [this _]
    (.toByteArray this))

  String
  (toBytes [this {:keys [length-prefix encoding b64-flavor]}]
    (if encoding
      (condp instance? encoding
        Charset (.getBytes this ^Charset encoding)
        String  (.getBytes this ^String  encoding)
        (base-encoded this encoding b64-flavor))
      (.getBytes this)))

  UUID
  (toBytes [this _]
    (let [bb (ByteBuffer/allocate 16)]
      (.putLong bb (.getMostSignificantBits  this))
      (.putLong bb (.getLeastSignificantBits this))
      (.array bb)))

  InputStream
  (toBytes [this opts]
    (let [buffer (:buffer-size opts constants/DEFAULT_BUFFER_SIZE)
          out (ByteArrayOutputStream. buffer)]
      (io/copy this out)
      (proto/toBytes out nil)))

  File
  (toBytes [this opts]
    (with-open [in (FileInputStream. this)]
      (-> in .getChannel (proto/toBytes opts))))

  FileChannel
  (toBytes [this opts]
    (let [length (.size this)
          ^ByteBuffer buf (if (> length constants/MAX_ARRAY_SIZE)
                            (throw (OutOfMemoryError. "Required array size too large!"))
                            (ByteBuffer/allocate (int length)))]
      (.read this buf)
      (.array buf)))

  URL
  (toBytes [this opts]
    (with-open [in (.openStream this)]
      ;; delegates to `InputStream` impl
      (proto/toBytes in opts)))

  URI
  (toBytes [this opts]
    ;; delegates to `URL` impl
    (-> this .toURL (proto/toBytes opts)))

  BufferedImage
  (toBytes [this opts]
    (let [buffer (:buffer-size opts constants/DEFAULT_BUFFER_SIZE)
          ^String img-type (:image-type opts "png")
          out (ByteArrayOutputStream. buffer)]
      (ImageIO/write this img-type out)
      (proto/toBytes out nil)))

  ByteBuffer
  (toBytes [this _]
    (try
      ;; one copy, but the ByteBuffer remains usable
      (aclone (.array this))
      (catch UnsupportedOperationException _
        (byte-array 0))
      (catch ReadOnlyBufferException _
        (-> this
            (io/make-output-stream nil)
            (proto/toBytes nil)))))

  CharBuffer
  (toBytes [this opts]
    (-> opts
        ut/charset-encoder
        (.encode this)
        .array))

  ReadableByteChannel
  (toBytes [this opts]
    (let [buffer-size (:buffer-size opts constants/DEFAULT_BUFFER_SIZE)
          read!  #(.read this %)]
      (loop [accum  (transient [])
             buffer (ByteBuffer/allocate buffer-size)
             nread  (read! buffer)]
        (cond
          (> (count accum) constants/MAX_ARRAY_SIZE)
          ;; mimic `Files/readAllBytes` method
          (throw (OutOfMemoryError. "Required array size too large!"))
          ;; EOS - return result
          (neg? nread)  (-> accum persistent! byte-array)
          ;; nothing was read - recur without touching anything
          (zero? nread) (recur accum buffer (read! buffer))
          ;; something was read - grow the result, clear the buffer and recur
          :else
          (let [relevant (cond->> (.array buffer)
                                  (< nread buffer-size) (take nread))
                accum    (reduce conj! accum relevant)
                buffer   (.clear buffer)]
            (recur accum buffer (read! buffer)))))))

  Serializable ;; catch-all extension clause
  (toBytes [this opts]
    (let [buffer (:buffer-size opts constants/DEFAULT_BUFFER_SIZE)
          out (ByteArrayOutputStream. buffer)]
      (with-open [oout (ObjectOutputStream. out)]
        (.writeObject oout this)
        (.flush oout)
        (proto/toBytes out nil))))

  )

(extend-protocol proto/ToByteArray
  (Class/forName "[C")

  (toBytes [this opts]
    (-> (CharBuffer/wrap this)
        (proto/toBytes opts))))

(extend-protocol proto/ToByteArray
  (Class/forName "[B")
  (toBytes [this _] this))
