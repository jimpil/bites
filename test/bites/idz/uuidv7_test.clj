(ns bites.idz.uuidv7-test
  (:require [clojure.test :refer :all]
            [bites.idz.uuidv7 :as uuidv7])
  (:import (java.io ObjectInputStream ByteArrayInputStream ByteArrayOutputStream ObjectOutputStream)
           (bites.idz UUIDV7)))

(deftest externalisable
  (let [u1  (uuidv7/generate)
        bos (ByteArrayOutputStream.)
        _   (-> (ObjectOutputStream. bos)
                (.writeObject u1))
        written-bytes (.toByteArray bos)
        u2 (->> written-bytes
                (ByteArrayInputStream. )
                (ObjectInputStream.)
                .readObject)]
    (is (= u1 u2))
    (is (not (identical? u1 u2)))
    )
  )

(deftest comparable
  (let [gen-id! (uuidv7/generator)
        uuids   (repeatedly 100 gen-id!)
        unique (set uuids)
        sorted  (sort unique)]
    (is (== 100 (count unique)))
    (is (= uuids sorted))
    (is (= (map str uuids) (map str sorted)))))

(deftest static-from-string
  (let [u1 (uuidv7/generate)
        u2 (UUIDV7/fromString (str u1))]
    (is (= u1 u2))))
