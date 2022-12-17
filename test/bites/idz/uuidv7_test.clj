(ns bites.idz.uuidv7-test
  (:require [clojure.test :refer :all]
            [bites.idz.uuidv7 :as uuidv7])
  (:import (java.io ObjectInputStream ByteArrayInputStream ByteArrayOutputStream ObjectOutputStream)))

(deftest externalisable
  (let [u1  (uuidv7/new-id)
        bos (ByteArrayOutputStream.)
        _   (-> (ObjectOutputStream. bos)
                (.writeObject  u1))
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
  (let [gen-id! (uuidv7/batch-generator)
        uuids   (doall (repeatedly 100 gen-id!))]
    (is (= uuids (sort uuids)))))

(deftest static-from-string
  (let [u1 (uuidv7/new-id)
        u2 (bites.idz.UUIDv7/fromString (str u1))]
    (is (= u1 u2))))
