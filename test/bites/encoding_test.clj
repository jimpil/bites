(ns bites.encoding-test
  (:require [clojure.test :refer :all]
            [bites.encoding :as enc]
            [bites.constants :as const]
            [bites.util :as util])
  (:import (java.util BitSet)))

(defn- assert-bitsets!
  [mbs  ^BitSet jbs]
  (is (= (.cardinality jbs)
         (count (remove zero? mbs))))
  (let [length (count mbs)]
    (doseq [^long i (range length)
            :let [^long my-bit (mbs i)]]
      (is (= (if (zero? my-bit) false true)
             ;; it's little-endian - start from the end!
             (.get jbs (Math/abs (int (- i (dec length))))))))))

(deftest bitset-tests
  (testing "byte->bitset"
    (doseq [b (range -127 128)]
      (let [my-bitset (enc/byte->bitset b)
            java-bitset (BitSet/valueOf (byte-array [(int b)]))]
        (assert-bitsets! my-bitset java-bitset))))

  (testing "bytes->bitset"
    (let [data [2r00000001 2r01101101]
          my-bitset (enc/bytes->bitset data)
          ;; must be little-endian, so reverse the data
          ;; https://docs.oracle.com/javase/7/docs/api/java/util/BitSet.html#valueOf(byte[])
          java-bitset (BitSet/valueOf (byte-array (reverse data)))]
      (assert-bitsets! my-bitset java-bitset)))

  (testing "bitset->bytes"
    (let [bitset [0 0 0 0 0 0 0 1 0 1 1 0 1 1 0 1]]
      (is (= [1 109] (enc/bitset->bytes bitset)))))

  )

(deftest unumber-tests
  (testing "(u)byte"
    (doseq [i (range Byte/MIN_VALUE (inc Byte/MAX_VALUE))]
      (is (enc/ubyte? (enc/byte->ubyte i))))
    (doseq [i (range 0 const/MAX_UNSIGNED_BYTE)]
      (is (enc/ubyte->byte i)))
    )
  (testing "(u)short"
    (doseq [i (->> (partial util/rand-long Short/MIN_VALUE (inc Short/MAX_VALUE))
                   (repeatedly 100))]
      (is (enc/ushort? (enc/short->ushort i))))
    (doseq [i (->> const/MAX_UNSIGNED_SHORT
                   (partial util/rand-long)
                   (repeatedly 100))]
      (is (enc/ushort->short i)))
    )
  (testing "(u)int"
    (doseq [i (->> (partial util/rand-long Integer/MIN_VALUE (inc Integer/MAX_VALUE))
                   (repeatedly 100))]
      (is (enc/uint? (enc/int->uint i))))
    (doseq [i (->> const/MAX_UNSIGNED_INT
                   (partial util/rand-long)
                   (repeatedly 1000))]
      (is (enc/uint->int i)))
    )


  )
