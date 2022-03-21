(ns bites.codec.primitives-test
  (:require [clojure.test :refer :all]
            [bites.codec :refer [encode-with decode-with]]
            [bites.constants :as const])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.util.concurrent ThreadLocalRandom)))

(defn roundtrip* [k v]
  (let [codec k
        out   (ByteArrayOutputStream.)
        _     (encode-with codec out v)
        encoded (.toByteArray out)
        decoded (decode-with codec (ByteArrayInputStream. encoded))]
    [decoded encoded]))

(defn- random-shorts [n]
  (let [rnd (ThreadLocalRandom/current)]
    (repeatedly n #(.nextInt rnd Short/MIN_VALUE (inc Short/MAX_VALUE)))))

(defn- random-ints [n]
  (let [rnd (ThreadLocalRandom/current)]
    (repeatedly n #(.nextInt rnd))))

(defn- random-ushorts [n] (repeatedly n (partial rand-int const/MAX_UNSIGNED_SHORT)))
(defn- random-long [n] (long (rand n)))
(defn- random-bigint [^bytes bs] (BigInteger. 1 bs))
(defn- random-uints [n] (repeatedly n (partial random-long  const/MAX_UNSIGNED_INT)))
(defn- random-ulongs [n]
  (let [rnd (ThreadLocalRandom/current)
        bs (byte-array 8)]
    (repeatedly n #(random-bigint (do (.nextBytes rnd bs) bs)))))
(defn- random-longs [n]
  (let [rnd (ThreadLocalRandom/current)]
    (repeatedly n #(.nextLong rnd))))
(defn- random-floats [n]
  (let [rnd (ThreadLocalRandom/current)]
    (repeatedly n #(.nextFloat rnd))))
(defn- random-doubles [n]
  (let [rnd (ThreadLocalRandom/current)]
    (repeatedly n #(.nextDouble rnd))))

(deftest primitive-codecs

  (testing "SIGNED"
    (testing ":byte"
      (let [raw (range Byte/MIN_VALUE (inc Byte/MAX_VALUE))] ;; test the entire valid range
        (doseq [expected raw]
          (let [[read written] (roundtrip* :byte expected)]
            (is (== expected read (first written)))))))
    (testing ":short"
      (let [raw (random-shorts 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :short-be expected)
                [read-le written-le] (roundtrip* :short-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    (testing ":int"
      (let [raw (random-ints 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :int-be expected)
                [read-le written-le] (roundtrip* :int-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    (testing ":float"
      (let [raw (random-floats 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :float-be expected)
                [read-le written-le] (roundtrip* :float-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    (testing ":double"
      (let [raw (random-doubles 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :double-be expected)
                [read-le written-le] (roundtrip* :double-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    (testing ":long"
      (let [raw (random-longs 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :long-be expected)
                [read-le written-le] (roundtrip* :long-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    )

  (testing "UNSIGNED"
    (testing ":ubyte"
      (let [raw (range 0 const/MAX_UNSIGNED_BYTE)] ;; test the entire valid range
        (doseq [expected raw]
          (let [[read written] (roundtrip* :ubyte expected)]
            (is (== expected read))))))
    (testing ":ushort"
      (let [raw (random-ushorts 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :ushort-be expected)
                [read-le written-le] (roundtrip* :ushort-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    (testing ":uint"
      (let [raw (random-uints 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :uint-be expected)
                [read-le written-le] (roundtrip* :uint-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    (testing ":ulong"
      (let [raw (random-ulongs 1000)]
        (doseq [expected raw]
          (let [[read-be written-be] (roundtrip* :ulong-be expected)
                [read-le written-le] (roundtrip* :ulong-le expected)]
            (testing "-be"
              (is (== expected read-be)))
            (testing "-le"
              (is (== expected read-le))
              (is (= (seq written-be)
                     (reverse written-le))))))))
    )
  )



