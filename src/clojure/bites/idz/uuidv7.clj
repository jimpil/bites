(ns bites.idz.uuidv7
  (:require [bites.util :as util]
            [bites.array :as array]
            [bites.bin-string :as bin-string])
  (:import (java.time Instant)
           (java.io Writer ByteArrayInputStream ObjectInputStream ByteArrayOutputStream ObjectOutputStream)
           (bites.idz UUIDV7)))

(defn from-string ^UUIDV7 [^String s] (UUIDV7/fromString s))
(defn created-at ^Instant [^UUIDV7 u] (.createdAt u))

(extend-type UUIDV7
  array/ToByteArray
  (toBytes [this _]
    (.toByteArray this)))

(defn- ->UUIDV7
  "Returns a UUIDV7 instance given the provided (presumed 16) bytes.
   Reuses the `.readExternal()` method and that's why there is a fair
   bit of copying taking place. This is the price to pay for not giving
   access to the raw bytes of the object, nor the 1-arg ctor (they are
   both declared private)."
  ^UUIDV7 [^bytes bs]
  (let [bos (ByteArrayOutputStream. 16)
        oos (ObjectOutputStream. bos)]
    (.write oos bs)
    (.flush oos)
    (doto (UUIDV7.)
      (.readExternal
        (ObjectInputStream.
          (ByteArrayInputStream.
            (.toByteArray bos)))))))

(defmethod array/fromBytes UUIDV7 [_ ^bytes bs _] (->UUIDV7 bs))
(defmethod bin-string/from-bytes :uuidv7 [_ ^bytes bs _] (str (array/fromBytes UUIDV7 bs nil)))
(defmethod bin-string/to-bytes   :uuidv7 [_ s _]  (-> s from-string (array/toBytes nil)))
(defmethod print-method UUIDV7 [u ^Writer wrt] (->> (str "#uuidv7" \" u \") (.write wrt)))
(def edn-reader {'uuidv7 from-string})
;;---------------------------------------

(defn generate
  "Generates a new uuid-v7 in non-batch mode.
   See https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#name-uuid-version-7"
  ^UUIDV7 []
  (UUIDV7/supply1))

(defn generator
  "Returns a no-arg fn to be used as a uuid-v7 generator in batch mode.
   Replaces the 12 `rand_a` bits with a counter which can go up to 4095 (inclusive),
   in an attempt to maintain correctness (in terms of equality/ordering/sorting),
   even in cases of collision (i.e. generating more than one within the same millisecond).
   The <cutoff> param (nat-int) is only relevant in the face of clock drifts into the past.
   In such cases any drift greater than the cutoff is considered machine-wide,
   and therefore valid. The internal state is reset and a new UUID is produced with a timestamp
   into the past. On the other hand, any drifts less than (or equal to) the cutoff,
   are considered invalid, and an exception is thrown. Passing zero essentially means
   'never-throw' - i.e. any drift into the past is valid."
  ([]
   (generator nil))
  ([cutoff]
   (let [supplier (if (some-> cutoff nat-int?)
                    (UUIDV7/supplier cutoff)
                    (UUIDV7/supplier))]
     (fn ^UUIDV7 [] (.get supplier)))))

(defn seq-counter
  "If <u> was created via `generator`,
   returns the 12-bit seq-counter - otherwise
   returns some meaningless (random) number."
  ^long [^UUIDV7 u]
  (let [bs   (.toByteArray u)
        bits (util/bytes->bits bs)
        counter-bits (subs bits 4)] ;; drop the 4 VERSION bits
    (Long/parseLong counter-bits 2)))


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


