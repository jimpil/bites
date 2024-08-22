(ns bites.array
  (:require [bites
             [buffer :as buffer]
             [constants :as constants]
             [util :as ut]
             [bin-string :as bin-string]]
            [clojure.java.io :as io])
  (:import (com.fasterxml.jackson.databind.util ByteBufferBackedOutputStream)
           (java.nio ByteBuffer ReadOnlyBufferException CharBuffer)
           (java.nio.charset Charset)
           (java.nio.channels FileChannel ReadableByteChannel Channels)
           (java.net URI URL)
           (java.io File InputStream ByteArrayOutputStream ObjectOutputStream Serializable ByteArrayInputStream ObjectInputStream FileInputStream)
           (java.util UUID)
           (javax.imageio ImageIO)
           (java.awt.image BufferedImage)))

(defprotocol ToByteArray (toBytes ^bytes [x opts]))
(defmulti fromBytes (fn [klass x opts] klass))

(defmethod bin-string/to-bytes   :uuid [_ ^String s _] (-> (UUID/fromString s) (toBytes nil)))
(defmethod bin-string/from-bytes :uuid [_ ^bytes bs _] (str (fromBytes UUID bs nil)))

;;==============<CONCRETIONS>================
(defmethod fromBytes UUID
  [_ ^bytes bs opts]
  (let [^ByteBuffer bb (buffer/wrap bs (:byte-order opts))
        high (.getLong bb)
        low  (.getLong bb)]
    (UUID. high low)))

(defmethod fromBytes Integer
  [_ ^bytes bs _]
  (-> bs ByteBuffer/wrap .getInt))

(defmethod fromBytes Long
  [_ ^bytes bs opts]
  (-> bs (buffer/wrap (:byte-order opts)) .getLong))

(defmethod fromBytes Double
  [_ ^bytes bs opts]
  (-> bs (buffer/wrap (:byte-order opts)) .getDouble))

(defmethod fromBytes BigInteger
  [_ ^bytes bs _]
  (BigInteger. bs))

(defmethod fromBytes BigDecimal
  [_ ^bytes bs _]
  (let [[scale-bs* unscaled-value-bs*] (split-at 4 bs)
        scale-bs   (byte-array scale-bs*)
        ^int scale (fromBytes Integer scale-bs nil)
        unscaled-value-bs (byte-array unscaled-value-bs*)
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
      (bin-string/from-bytes enc bs (:b64-flavor opts)))
    (String. bs)))

(defmethod fromBytes BufferedImage
  [_ ^bytes bs _]
  (let [in (ByteArrayInputStream. bs)]
    (ImageIO/read in)))

(defmethod fromBytes ByteBuffer
  [_ ^bytes bs _]
  (ByteBuffer/wrap bs))

(defmethod fromBytes ReadableByteChannel
  [_ ^bytes bs _]
  (let [in (ByteArrayInputStream. bs)]
    (Channels/newChannel in)))

(defmethod fromBytes Serializable
  [_ ^bytes bs _]
  (let [in (ByteArrayInputStream. bs)]
    (with-open [oin (ObjectInputStream. in)]
      (.readObject oin))))

;;----------------------------------------------
(extend-protocol ToByteArray
  Short
  (toBytes [this opts]
    (let [^ByteBuffer bb (buffer/byte-buffer Short/BYTES (:byte-order opts))]
      (.putShort bb this)
      (.array bb)))

  Integer
  (toBytes [this opts]
    (let [^ByteBuffer bb (buffer/byte-buffer Integer/BYTES (:byte-order opts))]
      (.putInt bb this)
      (.array bb)))

  Long
  (toBytes [this opts]
    (let [^ByteBuffer bb (buffer/byte-buffer Long/BYTES (:byte-order opts))]
      (.putLong bb this)
      (.array bb)))

  Double
  (toBytes [this opts]
    (let [^ByteBuffer bb (buffer/byte-buffer Double/BYTES (:byte-order opts))]
      (.putDouble bb this)
      (.array bb)))

  BigInteger
  (toBytes [this opts]
    (let [be-bs (.toByteArray this)]
      (if (= :le (:byte-order opts))
        (byte-array (reverse be-bs))
        be-bs))

    )

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

  ByteArrayOutputStream
  (toBytes [this _]
    (.toByteArray this))

  String
  (toBytes [this {:keys [encoding b64-flavor]}]
    (if (nil? encoding)
      (.getBytes this)
      (condp instance? encoding
        Charset (.getBytes this ^Charset encoding)
        String (.getBytes this ^String  encoding)
        (bin-string/to-bytes encoding this b64-flavor))))

  UUID
  (toBytes [this opts]
    (let [^ByteBuffer bb (buffer/byte-buffer 16 (:byte-order opts))]
      (.putLong bb (.getMostSignificantBits  this))
      (.putLong bb (.getLeastSignificantBits this))
      (.array bb)))

  InputStream
  (toBytes [this opts]
    (.readAllBytes this))

  File  ;; delegates to `InputStream` impl
  (toBytes [this opts]
    (with-open [in (FileInputStream. this)]
      (toBytes in opts)))

  FileChannel
  (toBytes [this opts]
    (let [length (.size this)
          ^ByteBuffer buf (if (> length constants/MAX_ARRAY_SIZE)
                            (throw (OutOfMemoryError. "Required array size too large!"))
                            (buffer/byte-buffer length (:byte-order opts)))]
      (.read this buf)
      (.array buf)))

  URL ;; delegates to `InputStream` impl
  (toBytes [this opts]
    (with-open [in (.openStream this)]
      (toBytes in opts)))

  URI ;; delegates to `URL` impl
  (toBytes [this opts]
    (-> (.toURL this)
        (toBytes opts)))

  BufferedImage
  (toBytes [this opts]
    (let [buffer (:buffer-size opts constants/DEFAULT_BUFFER_SIZE)
          ^String img-type (:image-type opts "png")
          out (ByteArrayOutputStream. buffer)]
      (ImageIO/write this img-type out)
      (toBytes out nil)))

  ByteBuffer
  (toBytes [this _]
    ;; a snapshot of the buffer's contents
    ;; clears the mark (via `.rewind`)
    (let [ret (byte-array (.position this))]
      (.rewind this)
      (.get this ret)
      ret))

  CharBuffer
  (toBytes [this opts]
    (-> opts
        ut/charset-encoder
        (.encode this)
        .array))

  Character ;; delegates to `CharBuffer` impl
  (toBytes [this opts]
    (-> [this]
        char-array
        CharBuffer/wrap
        (toBytes opts)))

  ReadableByteChannel
  (toBytes [this opts]
    (let [^long buffer-size (:buffer-size opts constants/DEFAULT_BUFFER_SIZE)
          read!  #(.read this %)]
      (loop [accum  (transient [])
             buffer (ByteBuffer/allocate buffer-size)
             ^long nread  (read! buffer)]
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
        (toBytes out nil))))

  )

(extend-protocol ToByteArray
  (Class/forName "[C")

  (toBytes [this opts]
    (-> (CharBuffer/wrap ^chars this)
        (toBytes opts))))

(extend-protocol ToByteArray
  (Class/forName "[B")
  (toBytes [this _] this))
