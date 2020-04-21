(ns bites.core
  (:require [bites.protocols :as proto]
            [bites.util :as ut])
  (:import (java.util.concurrent Exchanger TimeUnit)))

;(set! *warn-on-reflection* true)

(defn to-bytes
  "Wrapper fn around `toBytes` protocol. Turns <x> into a byte-array (per applicable <opts>).
   Does not close any input-streams other than the one(s) internally created
   (i.e. if <x> is one it will NOT be closed)."
  (^bytes [x]
   (to-bytes x nil))
  (^bytes [x opts]
   (proto/toBytes x opts)))

(defn from-bytes
  "Wrapper around `fromBytes` multi-method.
   Requires type-hinting at the call-site.
   See `def-from` for defining type-hinted variants."
  ([klass x]
   (from-bytes klass x nil))
  ([klass x opts]
   (proto/fromBytes klass x opts)))

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

(defn exchange!
  ([^Exchanger e x]
   (.exchange e x))
  ([^Exchanger e x timeout]
   (.exchange e x timeout TimeUnit/MILLISECONDS)))


(defn- start-consuming-with
  [consume! ^long buffer ^Exchanger exch]
  (loop [idx 0
         buf (object-array buffer)]
    (when-not (ut/current-thread-interrupted?)
      (if (zero? idx)
        (recur buffer (exchange! exch buf))
        (do (consume! (aget buf (- buffer idx)))
            (recur (unchecked-dec idx) buf))))))

(defn- start-producing-with
  [produce! ^long buffer ^Exchanger exch]
  (loop [idx 0
         buf (object-array buffer)]
    (when-not (ut/current-thread-interrupted?)
      (if (= buffer idx)
        (recur 0 (exchange! exch buf))
        (do (aset buf idx (produce!))
            (recur (unchecked-inc idx) buf))))))

(defn with-sync-exchange!
  "Given a producing-fn (no-args), and a consuming-fn (1-arg),
   assign each its own thread and object-array of size <buffer>,
   and has them exchanging arrays when they're BOTH ready
   (i.e. their respective operation exhausted the array it was working with).
   Returns a vector of two futures (representing the producing/consuming loops).
   Cancelling either stops its internal loop. A slow consumer will (eventually)
   block a faster producer, so a non-blocking consuming-fn would be ideal.
   This type of data-exchange between two threads requires no GC.
   See `java.util.concurrent.Exchanger` for the core idea,
   and the underlying construct this is implemented on top of."
  [buffer produce! consume!]
  (assert (pos-int? buffer) "buffer must be a positive integer")
  (let [exchanger (Exchanger.)]
    [(future (start-producing-with produce! buffer exchanger))
     (future (start-consuming-with consume! buffer exchanger))]))
