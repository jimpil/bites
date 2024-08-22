(ns bites.idz.uuidv7-test
  (:require [clojure.test :refer :all]
            [bites.idz.uuidv7 :as uuidv7])
  (:import (java.io ObjectInputStream ByteArrayInputStream ByteArrayOutputStream ObjectOutputStream)
           (bites.idz UUIDV7)
           [java.util UUID]))

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
        uuids   (repeatedly 200 gen-id!)
        unique  (set uuids)
        sorted  (sort unique)]
    (is (== 200 (count unique)))
    (is (= uuids sorted))
    (is (= (map str uuids) (map str sorted)))))

(deftest static-from-string
  (doseq[^UUIDV7 u (repeatedly 100 uuidv7/generate)]
    (is (instance? UUID (.asUUID u)))
    (is (= u (UUIDV7/fromString (str u))))))

(deftest seq-counter-tests
  (testing "counter always increases when timestamps collide"
    (let [gen-id! (uuidv7/generator)
          collision-groups (->> (repeatedly 1250 gen-id!)
                                (map (juxt uuidv7/created-at uuidv7/seq-counter))
                                (partition-by first))]
      (doseq [group collision-groups]
        (is (apply < (map second group)))))
    )
  )

(deftest examples-found-online
  (testing "https://uuid.ramsey.dev/en/stable/rfc4122/version7.html"
    (let [u1 (uuidv7/from-string "01833ce0-3486-7bfd-84a1-ad157cf64005")
          u2 (uuidv7/from-string "ffffffff-ffff-7964-a8f6-001336ac20cb")
          u3 (uuidv7/from-string "00000000-0000-7964-a8f6-001336ac20cb")]
      (is (= "2022-09-14T16:41:10.022Z"   (str (uuidv7/created-at u1)))) ;; Wed, 14 Sep 2022 16:41:10
      (is (= "+10889-08-02T05:31:50.655Z" (str (uuidv7/created-at u2)))) ;; Tue, 02 Aug 10889 05:31:50
      (is (= "1970-01-01T00:00:00Z"       (str (uuidv7/created-at u3))))))

  (testing "https://pypi.org/project/uuid7/"
    (let [u (uuidv7/from-string "061cb26a-54b8-7a52-8000-2124e7041024")]
      (is (= "2182-12-16T12:09:23.896Z" (str (uuidv7/created-at u))))))

  (testing "https://stackoverflow.com/questions/534839/how-to-create-a-guid-uuid-in-python"
    (let [u (uuidv7/from-string "01818aa2-ec45-74e8-1f85-9d74e4846897")]
      (is (= "2022-06-22T08:59:02.085Z" (str (uuidv7/created-at u))))))
  )
