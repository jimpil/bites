(ns bites.idz.uuidv7
  (:require [bites.random :as random]
            [bites.util :as util]
            [bites.array :as array]
            [bites.bin-string :as bin-string])
  (:import [java.time Instant Duration]
           (java.io Writer)
           (bites.idz UUIDV7)))

(def ^:const UNIX_TS_MS_BIT_COUNT 48)
(def ^:const VERSION_BITS "0111")
(def ^:const VARIANT_BITS "10")

(defn ->UUIDV7 [^bytes bs] (UUIDV7. bs))
(defn from-string ^UUIDV7 [^String s] (UUIDV7/fromString s))
(defn created-at ^Instant [^UUIDV7 u] (.createdAt u))

(defn seq-counter
  "If <u> was created via `batch-generator`,
   returns the 12-bit seq-counter - otherwise
   returns some meaningless (random) number."
  ^long [^UUIDV7 u]
  (let [bs   (.toByteArray u)
        bits (util/bytes->bits bs)
        counter-bits (subs bits 4)] ;; drop the 4 VERSION bits
    (Long/parseLong counter-bits 2)))

(extend-type UUIDV7
  array/ToByteArray
  (toBytes [this _]
    (.toByteArray this)))

(defmethod bin-string/from-bytes :uuidv7 [_ ^bytes bs _] (-> bs ->UUIDV7 str))
(defmethod bin-string/to-bytes   :uuidv7 [_ s _]  (-> s from-string (array/toBytes nil)))
(defmethod array/fromBytes UUIDV7 [_ ^bytes bs _] (->UUIDV7 bs))
(defmethod print-method UUIDV7 [u ^Writer wrt] (->> (str "#uuidv7" \" u \") (.write wrt)))
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
  ^UUIDV7 []
  (->> (Instant/now)
       .toEpochMilli
       (gen-bytes nil)
       ->UUIDV7))

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
           ->UUIDV7)
       ;; counter overflow (highly unlikely)
       ;; pretend we're on the next clock tick
       (->> (inc now-epoch)
            (gen-bytes "0")
            ->UUIDV7)
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
  (let [[u1 u2 u3 :as ids] (repeatedly 100 gen-id!)]
    (run! println ids)
    (time (sort ids)))

  (def u1 (new-id))
  (def u2 (new-id))
  (def u3 (new-id))


  )


