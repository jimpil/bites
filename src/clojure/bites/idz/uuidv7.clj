(ns bites.idz.uuidv7
  (:require [bites.util :as util]
            [bites.array :as array]
            [bites.bin-string :as bin-string])
  (:import (java.time Instant)
           (java.io Writer)
           (bites.idz UUIDV7)))

(defn ->UUIDV7 [^bytes bs] (UUIDV7. bs))
(defn from-string ^UUIDV7 [^String s] (UUIDV7/fromString s))
(defn created-at ^Instant [^UUIDV7 u] (.createdAt u))

(defn seq-counter
  "If <u> was created via `generator`,
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
    (fn ^UUIDV7 [] (.get supplier))))


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


