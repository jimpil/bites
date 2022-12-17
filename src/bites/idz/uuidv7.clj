(ns bites.idz.uuidv7
  (:require [bites.random :as random]
            [bites.util :as util]
            [clojure.string :as str]
            [bites.array :as array]
            [bites.bin-string :as bin-string]
            [bites.buffer :as buffer])
  (:import [java.time Instant Duration]
           (java.util Arrays)
           (java.io Writer ObjectInput ObjectOutput)
           (java.nio ByteBuffer)))

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

(gen-class
  :name bites.idz.UUIDv7
  :implements [bites.array.ToByteArray java.io.Externalizable Comparable]
  :prefix "impl-"
  :init init
  :constructors {["[B"] []
                 [] []} ;; nullary ctor required for Externalizable
  :methods [[createdAt [Object] java.time.Instant]
            [seqCounter [Object] Long]
            ^:static [fromString [String] Object]] ;; wtf - https://stackoverflow.com/questions/29329798/clojure-gen-class-returning-own-class
  :factory fromBytes
  :state state)

(defn ->UUIDv7 [bs] (new bites.idz.UUIDv7 bs))
(defn- state* [^bites.idz.UUIDv7 this] @(.state this))
(defn from-string
  ^bites.idz.UUIDv7 [^String s]
  (-> s
      (str/replace "-" "")
      util/b16-bytes
      ->UUIDv7))

(defn created-at
  ^Instant [^bites.idz.UUIDv7 u]
  (let [{:keys [^bytes ts-bs]} (state* u)]
    (->> ts-bs
         (BigInteger. 1)
         long
         Instant/ofEpochMilli)))

(defn seq-counter
  "If <u> was created via `batch-generator`,
   returns the 12-bit seq-counter - otherwise
   returns some meaningless (random) number."
  ^long [^bites.idz.UUIDv7 u]
  (let [{:keys [^bytes seq-bs]} (state* u)
        bits (util/bytes->bits seq-bs)
        counter-bits (subs bits 4)] ;; drop the 4 VERSION bits
    (Long/parseLong counter-bits 2)))

(def ^:private impl-fromString from-string)
(def ^:private impl-createdAt  created-at)
(def ^:private impl-seqCounter seq-counter)

(defn- init* [raw]
  {:raw     raw
   :hex-str (as-text raw)
   :ts-bs   (util/copy-of-byte-array raw 0 6)
   :seq-bs  (util/copy-of-byte-array raw 6 8)})

(defn- impl-init
  ([]
   [[] (volatile! {})])
  ([raw]
   [[] (volatile! (init* raw))]))

(defn- impl-toBytes [this opts]
  (let [{:keys [^bytes raw]} (state* this)]
    (if (= :le (:byte-order opts))
      (let [^ByteBuffer bb     (buffer/byte-buffer 16 :le)
            most-significant   (util/copy-of-byte-array raw 0 8)
            least-significant  (util/copy-of-byte-array raw 8 16)]
        (->> most-significant  (BigInteger.) long (.putLong bb))
        (->> least-significant (BigInteger.) long (.putLong bb))
        (.array bb))
      ;; avoid all the above for :be - just clone raw
      (util/copy-of-byte-array raw))))

(defn- impl-compareTo
  [this other]
  (if (instance? bites.idz.UUIDv7 other)
    ;; compare timestamps (first 6 bytes),
    ;; and if that fails (comes back zero) compare counters
    (let [{:keys [^bytes ts-bs ^bytes seq-bs]} (state* this)
          other-state (state* other)
          this-ts  (BigInteger. 1 ts-bs)
          other-ts (BigInteger. 1 ^bytes (:ts-bs other-state))
          ret      (.compareTo this-ts other-ts)]
      (if (zero? ret) ;; we should have counter-bits
        (let [this-counter  (BigInteger. 1 seq-bs)
              other-counter (BigInteger. 1 ^bytes (:seq-bs other-state)) ]
          (.compareTo this-counter other-counter))
        ret))
    (throw
      (IllegalArgumentException.
        (str "Can NOT compare " (class this) " against " (class other))))))

(defn- impl-readExternal
  [^bites.idz.UUIDv7 this ^ObjectInput in]
  (let [state (.state this)
        buf (byte-array 16)]
        (when (== 16 (.read in buf)) ;; don't proceed unless 16 bytes were read
          (->> (init* buf)
               (vreset! state)))))

(defn- impl-writeExternal
  [this ^ObjectOutput out]
  (let [{:keys [^bytes raw]} (state* this)]
    (.write out raw)
    (.flush out)))

(defn impl-equals
  [this other]
  (and (instance? bites.idz.UUIDv7 other)
       (let [^bytes this-raw  (-> (state* this)  :raw)
             ^bytes other-raw (-> (state* other) :raw)]
         (Arrays/equals this-raw other-raw))))

(defn impl-hashCode
  [this]
  (let [{:keys [^bytes raw]} (state* this)]
    (Arrays/hashCode raw)))

(defn impl-toString
  [this]
  (-> (state* this) :hex-str))

(defmethod bin-string/from-bytes :uuidv7 [_ ^bytes bs _] (-> bs ->UUIDv7 str))
(defmethod bin-string/to-bytes   :uuidv7 [_ s _]  (-> s from-string (array/toBytes nil)))
(defmethod array/fromBytes bites.idz.UUIDv7 [_ ^bytes bs _] (->UUIDv7 bs))
(defmethod print-method bites.idz.UUIDv7 [u ^Writer wrt] (->> (str "#uuidv7" \" (str u) \") (.write wrt)))
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
  ^bites.idz.UUIDv7 []
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
  (let [[u1 u2 u3 :as ids] (repeatedly 100 gen-id!)]
    (run! println ids)
    (time (sort ids)))

  (def u1 (new-id))
  (def u2 (new-id))
  (def u3 (new-id))


  )


