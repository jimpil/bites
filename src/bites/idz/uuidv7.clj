(ns bites.idz.uuidv7
  (:require [bites.random :as random]
            [bites.util :as util]
            [clojure.string :as str]
            [bites.array :as array]
            [bites.bin-string :as bin-string]
            [bites.buffer :as buffer])
  (:import [java.time Instant Duration]
           (java.util Arrays)
           (java.io Externalizable Writer)
           (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const UNIX_TS_MS_BIT_COUNT 48)
(def ^:const VERSION_BITS "0111")
(def ^:const VARIANT_BITS "10")
(defonce FORMAT-844412
  (let [r (range 32)]
    (->> r
         (map
           (fn [i]
             (condp > i
               8  \1
               12 \2
               16 \3
               20 \4
               32 \5)))
         (zipmap r))))

(defn- as-text
  ^String [^bytes raw]
  (transduce
    (comp
      (map-indexed vector)
      (partition-by (comp FORMAT-844412 first))
      (map (partial map second))
      (interpose "-")
      cat)
    util/str!
    (StringBuilder. 36)
    (eduction (mapcat util/byte->hex) raw)))

(deftype UUIDv7 [raw] ;; do NOT mutate `raw`
  array/ToByteArray
  (toBytes [this opts]
    (if (= :le (:byte-order opts))
      (let [^ByteBuffer bb (buffer/byte-buffer 16 :le)
            most-significant  (util/copy-of-byte-array raw 0 8)
            least-significant (util/copy-of-byte-array raw 8 16)]
        (->> most-significant  (BigInteger.) long (.putLong bb))
        (->> least-significant (BigInteger.) long (.putLong bb))
        (.array bb))
      ;; avoid all the above for :be
      (aclone ^bytes raw)))
  Comparable
  (compareTo [_ other]
    (if (instance? UUIDv7 other)
      ;; compare timestamps (first 6 bytes),
      ;; and if that fails (comes back zero) compare counters
      (let [this-ts  (BigInteger. 1 (Arrays/copyOfRange ^bytes raw 0 6))
            other-ts (BigInteger. 1 (Arrays/copyOfRange ^bytes (.raw ^UUIDv7 other) 0 6))
            ret      (.compareTo this-ts other-ts)]
        (if (zero? ret) ;; we should have counter-bits
          (let [this-counter  (BigInteger. 1 (Arrays/copyOfRange ^bytes raw 6 8))
                other-counter (BigInteger. 1 (Arrays/copyOfRange ^bytes (.raw ^UUIDv7 other) 6 8)) ]
            (.compareTo this-counter other-counter))
          ret))
      -1))
  Externalizable
  (readExternal [_ in]
    (when (every? zero? raw) ;; don't proceed unless raw is empty
      (let [restored (byte-array 16)]
        (when (== 16 (.read in restored)) ;; don't proceed unless 16 bytes were read
          (util/copy-bytes! restored raw)))))
  (writeExternal [_ out] (.write out ^bytes raw))
  Object
  (equals [_ other]
    (if (instance? UUIDv7 other)
      (Arrays/equals ^bytes raw ^bytes (.raw ^UUIDv7 other))
      false))
  (hashCode [_]     (Arrays/hashCode ^bytes raw))
  (toString [_]     (as-text raw)))

(defn placeholder
  "Returns an empty UUIDv7.
   Useful for restoring UUIDv7 objects from raw bytes."
  ^UUIDv7 []
  (->UUIDv7 (byte-array 16)))

(defn from-string
  ^UUIDv7 [^String s]
  (-> s
      (str/replace "-" "")
      util/b16-bytes
      ->UUIDv7))

(defmethod bin-string/from-bytes :uuidv7 [_ ^bytes bs _] (str (->UUIDv7 bs)))
(defmethod bin-string/to-bytes   :uuidv7 [_ s _]  (-> (from-string s) (array/toBytes nil)))
(defmethod array/fromBytes UUIDv7 [_ ^bytes bs _] (->UUIDv7 bs))
(defmethod print-method UUIDv7 [u ^Writer wrt] (->> (str "#uuidv7" \" (str u) \") (.write wrt)))
(def data-readers {'uuidv7 from-string})
;;---------------------------------------
(defn unix-ts-ms
  "Returns a String of 48 bits (6 bytes)
   for the epoch-milli of the Instant provided.
   If the epoch-milli bit-pattern is longer than 48 digits
   keep the 48 least significant bits (right most).
   If it is shorter than 48, pad the most significant bits."
  ([]
   (-> (Instant/now) .toEpochMilli unix-ts-ms))
  ([^long epoch-millis]
   ;{:post [(= 48 (count %))]}
   (let [epoch-millis-bits (Long/toBinaryString epoch-millis)
         length (count epoch-millis-bits)] ;; this can't be more than 64
     (cond
       ;; https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.14
       (> length UNIX_TS_MS_BIT_COUNT)
       ;; keep the 48 least significant bits
       (subs epoch-millis-bits (- length UNIX_TS_MS_BIT_COUNT))
       ;; https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.12
       (< length UNIX_TS_MS_BIT_COUNT)
       ;; pad the most significant bits
       (util/pad-bits UNIX_TS_MS_BIT_COUNT epoch-millis-bits)

       :else ;; exactly 48
       epoch-millis-bits))))


(defn- gen-bytes
  "-----------------------------------------------------------------
   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                           unix_ts_ms                          |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |          unix_ts_ms           |  ver  |       rand_a          |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |var|                        rand_b                             |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                            rand_b                             |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
  ^bytes [counter-bits ts]
  ;(println ts)
  (util/bits->bytes
    (str
      (unix-ts-ms ts)       ;; 48 -- first 6 bytes (48 bits)
      VERSION_BITS          ;; 4
      (or (some->> counter-bits (util/pad-bits 12))
          (random/bits 12)) ;; 12 -- middle 2 bytes (4 + 12 bits)
      VARIANT_BITS          ;; 2
      (random/bits 62))))   ;; 62  -- last 8 bytes (2 + 62 bits)

(defn new-id
  "Generates a new uuid-v7 in non-batch mode.
   See https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#name-uuid-version-7"
  ^UUIDv7 []
  (->> (Instant/now)
       .toEpochMilli
       (gen-bytes nil)
       ->UUIDv7))

(defn batch-generator
  "Returns a no-arg fn to be used as a uuid-v7 generator in batching mode.
   Replaces the 12 `rand_a` bits with a counter which can go up to and including 4095,
   in an attempt to maintain correctness (in terms of ordering/sorting), even in cases of
   collision (i.e. generating more than one within the same millisecond)."
  ([]
   (->> {:ts 0
         :counter 0}
        atom
        (partial batch-generator)))
  ([state]
   (let [now (Instant/now)
         now-epoch (.toEpochMilli now)
         {:keys [^long ts]} @state
         {:keys [^long counter]}
         (cond
           ;; The clock sequence MUST start at zero and increment monotonically
           ;; for each new UUID created on by the application on the same timestamp.
           (== now-epoch ts)
           (swap! state update :counter inc)
           ;; When the timestamp increments the clock sequence MUST be reset to zero.
           (> now-epoch ts)
           (swap! state assoc :ts now-epoch :counter 0)
           :else
           ;; seems like the clock has moved back
           ;; https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.10
           (let [diff-ms (-> (Duration/between ts now) .toMillis)]
             (cond
               (zero? diff-ms)
               ;; clock is fine we're just some nanos behind
               ;; this can happen in distributed situations
               (swap! state update :counter inc)

               (> diff-ms 10000)
               ;; clock moved back by more than 10 seconds!
               ;; this is bad - something must have happened to the machine
               ;; reset everything
               (swap! state assoc :ts now-epoch :counter 0))
             :else
             ;; clock moved back by less than 10 seconds
             ;; not sure what to do here
             (swap! state update :counter inc)))]
     (if (< counter 4096) ;; we have 12 bits available (i.e. 2^12)
       (-> counter
           Long/toBinaryString
           (gen-bytes now-epoch)
           ->UUIDv7)
       ;; counter overflow (highly unlikely)
       ;; pretend we're on the next clock tick
       (->> (inc now-epoch)
            (gen-bytes "0")
            ->UUIDv7)
       )
     )
   )
  )


(comment

  (def gen-id! (batch-generator))
  ;; collision stress test
  (->> (repeatedly 1000 gen-id!)
       (into #{} (map str))
       count
       time)

  ;; sorting test
  (let [[u1 u2 u3 :as ids] (repeatedly 10 gen-id!)]
    (run! println ids)
    (sort ids))

  (def u1 (new-id))
  (def u2 (new-id))
  (def u3 (new-id))


  )


