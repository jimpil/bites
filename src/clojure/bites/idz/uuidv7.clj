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
(def edn-reader {'uuidv7 from-string})
;;---------------------------------------
#_(defn unix-ts-ms
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


#_(defn- gen-bytes
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

(defn generate
  "Generates a new uuid-v7 in non-batch mode.
   See https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#name-uuid-version-7"
  ^UUIDV7 []
  (UUIDV7/supply1))

(defn generator
  "Returns a no-arg fn to be used as a uuid-v7 generator in batching mode.
   Replaces the 12 `rand_a` bits with a counter which can go up to and including 4095,
   in an attempt to maintain correctness (in terms of ordering/sorting), even in cases of
   collision (i.e. generating more than one within the same millisecond)."
  []
  (let [supplier (UUIDV7/supplier)]
    (fn ^UUIDV7 []
      (.get supplier))))


(comment

  (def gen-id! (generator))
  ;; collision stress test
  (->> (repeatedly 1000 gen-id!)
       (into #{} (map str))
       count
       time)

  (def u1 (generate))
  (def u2 (generate))
  (def u3 (generate))


  )


