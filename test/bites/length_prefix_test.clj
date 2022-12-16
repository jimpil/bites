(ns bites.length-prefix-test
  (:require [clojure.test :refer :all]
            [bites.length-prefix :as lp]))

(deftest encode-length-test
  (testing "length = 260"
    ;; https://stackoverflow.com/questions/36978879/python-string-prefix-by-4-byte-length
    (testing "int16"
      (is (= [1 4] (lp/encode-length 260 [:int16 :be])))
      (is (= [4 1] (lp/encode-length 260 [:int16 :le]))))
    (testing "int32"
      (is (= [0 0 1 4] (lp/encode-length 260 [:int32 :be])))
      (is (= [4 1 0 0] (lp/encode-length 260 [:int32 :le]))))
    (testing "int64"
      (is (= [0 0 0 0 0 0 1 4] (lp/encode-length 260 [:int64 :be])))
      (is (= [4 1 0 0 0 0 0 0] (lp/encode-length 260 [:int64 :le]))))
    )

  (testing "length = 1234"
    (testing "int16"
      (is (= [4 -46] (lp/encode-length 1234 [:int16 :be])))
      (is (= [-46 4] (lp/encode-length 1234 [:int16 :le]))))
    (testing "int32"
      (is (= [0 0 4 -46] (lp/encode-length 1234 [:int32 :be])))
      (is (= [-46 4 0 0] (lp/encode-length 1234 [:int32 :le]))))
    (testing "int64"
      (is (= [0 0 0 0 0 0 4 -46] (lp/encode-length 1234 [:int64 :be])))
      (is (= [-46 4 0 0 0 0 0 0] (lp/encode-length 1234 [:int64 :le]))))
    )

  (testing "length = 1695609641"
    ;; https://stackoverflow.com/questions/2183240/java-integer-to-byte-array
    (testing "int16"
      (is (thrown? IllegalArgumentException
                   (lp/encode-length 1695609641 [:int16 :be])))
      (is (thrown? IllegalArgumentException
                   (lp/encode-length 1695609641 [:int16 :le]))))

    (testing "int32"
      ;; -13 is the same as 0xF3 because a long can represent 0xF3 (243)
      ;; without overflowing and becoming negative. However, Java uses
      ;; signed bytes so 243 doesn't fit in a Java byte (127 is the max).
      ;; (Byte/toUnsignedLong (byte -13)) => 243
      (is (= [101 16 -13 41]
             (lp/encode-length 1695609641 [:int32 :be])))
      (is (= [41 -13 16 101]
             (lp/encode-length 1695609641 [:int32 :le]))))

    (testing "int64"
      (is (= [0 0 0 0 101 16 -13 41]
             (lp/encode-length 1695609641 [:int64 :be])))
      (is (= [41 -13 16 101 0 0 0 0]
             (lp/encode-length 1695609641 [:int64 :le]))))
    )

  (testing "length = 18446744073709551615N (max unsigned long)"
    (testing "int32"
      (is (thrown? AssertionError
                   (lp/encode-length 18446744073709551615N [:int32 :be])))
      (is (thrown? AssertionError
                   (lp/encode-length 18446744073709551615N [:int32 :le]))))

    (testing "int64"
      ;; (Byte/toUnsignedLong (byte -1)) => 255
      (is (= [0 0 0 0 0 0 0 -1]
             (lp/encode-length 255 [:int64 :be])))
      (is (= [-1 0 0 0]
             (lp/encode-length 255 [:int32 :le]))))
    )

  )
