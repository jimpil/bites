(ns bites.padding-test
  (:require [clojure.test :refer :all]
            [bites.padding :refer :all]))


(deftest padding-bytes-test
  (testing "left-padding"
    (testing "fixed"
      (is (= [97 1 2 3 4]
             (-> [1 2 3 4]
                 byte-array
                 (left-pad \a)
                 seq)))
      (is (= [97 98 99 1 2 3 4]
             (-> [1 2 3 4]
                 byte-array
                 (left-pad "abc") ;; (seq (.getBytes "abc")) => (97 98 99)
                 seq)))
      )
    (testing "up to length"
      (is (= [97 97 1 2 3 4]
             (-> [1 2 3 4]
                 byte-array
                 (left-pad \a 6)
                 seq)))
      )
    )
  (testing "right-padding"
    (testing "fixed"
      (is (= [1 2 3 4 97]
             (-> [1 2 3 4]
                 byte-array
                 (right-pad \a)
                 seq)))
      (is (= [1 2 3 4 97 98 99]
             (-> [1 2 3 4]
                 byte-array
                 (right-pad "abc")
                 seq)))
      )
    (testing "up to length"
      (is (= [1 2 3 4 97 97]
             (-> [1 2 3 4]
                 byte-array
                 (right-pad \a 6)
                 seq)))
      )
    )
  )

(deftest unpadding-bytes-test
  (testing "left-unpadding"
    (is (= [1 2 3 4]
           (-> [97 1 2 3 4]
               byte-array
               (left-unpad \a) ;; (byte \a) => 97
               seq)))
    (is (= [1 2 3 4]
           (-> [97 98 99  1 2 3 4]
               byte-array
               (left-unpad "abc") ;; (seq (.getBytes "abc")) => (97 98 99)
               seq)))
    )
  (testing "right-padding"
    (is (= [1 2 3 4]
           (-> [1 2 3 4 97]
               byte-array
               (right-unpad \a)
               seq)))
    (is (= [1 2 3 4]
           (-> [1 2 3 4 97 98 99]
               byte-array
               (right-unpad "abc")
               seq)))
    )
  )
