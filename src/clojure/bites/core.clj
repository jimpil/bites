(ns bites.core
  (:require [bites.array :as array]
            [bites.cbor :as cbor]
            [bites.length-prefix :as prefix]))

;(set! *warn-on-reflection* true)

(defn to-bytes
  "Wrapper fn around `toBytes` protocol. Turns <x> into a byte-array (per applicable <opts>).
   Does not close any input-streams other than the one(s) internally created
   (i.e. if <x> is one it will NOT be closed)."
  (^bytes [x]
   (to-bytes x nil))
  (^bytes [x opts]
   (array/toBytes x opts)))

(defn to-cbor-bytes
  "Wrapper fn around `array/toBytes` protocol.
   Turns object <x> into a CBOR-encoded byte-array (per applicable <opts>).
   Does not close any input-streams other than the one(s) internally
   created (i.e. if <x> is one it will NOT be closed)."
  (^bytes [x]
   (to-cbor-bytes x nil))
  (^bytes [x opts]
   (cbor/to-bytes x opts)))

(defn from-bytes
  "Wrapper around `fromBytes` multi-method.
   Turns byte-array <x> into an object.
   Requires type-hinting at the call-site.
   See `def-from` for defining type-hinted variants."
  ([klass x]
   (from-bytes klass x nil))
  ([klass x opts]
   (array/fromBytes klass x opts)))

(defn from-cbor-bytes
  "Wrapper around `cbor/fromCBOR` protocol.
   Turns <x> (byte-array/ByteBuffer/InputStream/File)
   into an object."
  ([x]
   (from-cbor-bytes x nil))
  ([x opts]
   (cbor/read-cbor x opts)))

(defn with-length-prefix
  "Returns a new byte-array whose N (depends on <encoding>) first bytes
   denote the length of the provided bytes <bs>. `:int16` (2-byte),
   `:int32` (4-byte) & `:int64` (8-byte) encodings are supported."
  ^bytes [encoding byte-order bs]
  (let [bytes-in? (bytes? bs)
        length (if bytes-in? (alength ^bytes bs) (count bs))
        length-bs (prefix/encode-length length [encoding byte-order])]
    (cond-> (concat length-bs bs)
            bytes-in? byte-array)))

(defmacro def-from
  "Defines a type-hinted (per <klass>) function named <sym> taking 1 or 2 args,
   which delegates to `from-bytes` (hard-coding <klass> as the first argument to it).
   This (obviously) won't work for `java.io.Serializable`."
  [sym doc-string klass]
  `(def
     ~(with-meta sym {:tag klass})
     ~(or doc-string (format "Type-hinted (%s) variant of `from-bytes`." klass))
     (partial from-bytes ~klass)))

(comment
  (def-from bytes->string nil String)

  (-> (bytes->string (.getBytes "hi") nil)
      (.substring 0 2)) ;; no reflection!


  ;; image->b64-str
  (-> (ImageIO/read (io/file "..."))    ;; the image in question
      (to-bytes {:image-type "png"})    ;; its bytes
      (bytes->string {:encoding :b64})) ;; as a (potentially large) string ready to be used inside a <img> html tag
  )
