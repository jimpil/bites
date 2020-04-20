(ns bites.core
  (:require [bites.array :as ba]))

;(set! *warn-on-reflection* true)

(defn to-bytes
  "Wrapper fn around `toBytes` protocol. Turns <x> into a byte-array (per applicable <opts>).
   Does not close any input-streams other than the one(s) internally created
   (i.e. if <x> is one it will NOT be closed)."
  (^bytes [x]
   (to-bytes x nil))
  (^bytes [x opts]
   (ba/toBytes x opts)))

(defn from-bytes
  "Wrapper around `fromBytes` multi-method.
   Requires type-hinting at the call-site.
   See `def-from` for defining type-hinted variants."
  ([klass x]
   (from-bytes klass x nil))
  ([klass x opts]
   (ba/fromBytes klass x opts)))

(defmacro def-from
  "Defines a type-hinted (per <klass>) function named <sym> taking 1 or 2 args,
   which delegates to `from-bytes` (hard-coding <klass> as the first argument to it).
   This (obviously) won't work for `java.io.Serializable`."
  [sym doc-string klass]
  `(def
     ~(with-meta sym {:tag klass})
     ~(or doc-string (format "Type-hinted (%s) variant of `from-bytes` taking 1 or 2 args." klass))
     (partial from-bytes ~klass)))

(comment
  (def-from bytes->string nil String)

  (-> (bytes->string (.getBytes "hi") nil)
      (.substring 0 2)) ;; no reflection!


  ;; image->b64-str
  (-> (ImageIO/read (io/file "...")) ;; the image in question
      (to-bytes {:image-type "png"})
      (bytes->string {:encoding :b64}))
  ;; => a (potentially large) String encoding the image bytes (in Base64)
  ;; ready to be used inside a <img> html tag


  )
