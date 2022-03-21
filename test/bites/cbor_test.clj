(ns bites.cbor-test
  (:require [clojure.test :refer :all]
            [bites.cbor :as cbor]
            [bites.util :as ut])
  (:import (java.time LocalDate Instant)
           (java.net URI)
           (java.util UUID)
           (java.io ByteArrayInputStream)))

(defn- roundtrip*
  [x w-opts r-opts]
  (let [written (cbor/to-bytes x w-opts)
        read    (cbor/read-cbor written r-opts)]
    [read (ut/b16-str written :upper)]))

(deftest cbor-roundtrip
  (testing "Instant (epoch seconds)"
    (testing "w/o nano adjustment"
      (let [x (Instant/ofEpochMilli 1363896240000)
            expected "C11A514B67B0"
            [read written] (roundtrip* x nil nil)]
        (is (= x read))
        (is (= expected written))))

    (testing "with nano adjustment"
      (let [x (Instant/ofEpochMilli 1363896240500)
            expected "C1FB41D452D9EC200000"
            [read written] (roundtrip* x nil nil)]
        (is (= x read))
        (is (= expected written)))))

  (testing "LocalDate"
    (testing "first day"
      (let [x (LocalDate/ofEpochDay 0)
            expected "D86400"
            [read written] (roundtrip* x nil nil)]
        (is (= x read))
        (is (= expected written))))
    (testing "some random day"
      (let [x (LocalDate/parse "2020-06-14")
            expected "D8641947FB"
            [read written] (roundtrip* x nil nil)]
        (is (= x read))
        (is (= expected written)))))

  (testing "String (:date)"
    (testing "first day"
      (let [x "1970-01-01"
            expected "D903EC6A313937302D30312D3031"
            [read written] (roundtrip* x {:as :date} nil)]
        (is (= x read))
        (is (= expected written))))
    (testing "some random day"
      (let [x "2020-06-14"
            expected "D903EC6A323032302D30362D3134"
            [read written] (roundtrip* x {:as :date} nil)]
        (is (= x read))
        (is (= expected written)))))

  (testing "UUID"
    (testing "object in - object out"
      (let [x (UUID/fromString "dbd559ef-333b-4f11-96b1-b0654babe844")
            expected "D82550DBD559EF333B4F1196B1B0654BABE844"
            [read written] (roundtrip* x nil nil)]
        (is (= x read))
        (is (= expected written))))
    (testing "string in - object out"
      (let [x "dbd559ef-333b-4f11-96b1-b0654babe844"
            expected "D82550DBD559EF333B4F1196B1B0654BABE844"
            [read written] (roundtrip* x {:as :uuid} {:as :string})]
        (is (= x read)) ;;
        (is (= expected written)))))

  (testing "Pattern"
    (let [x #"abc123"
          expected "D82366616263313233"
          [read written] (roundtrip* x nil nil)]
      (is (= (str x) (str read)))
      (is (= expected written))))

  (testing "URI"
    (let [x (URI. "http://www.example.com")
          expected "D82076687474703A2F2F7777772E6578616D706C652E636F6D"
          [read written] (roundtrip* x nil nil)]
      (is (= x read))
      (is (= expected written))))

  (testing "Ratio"
    (let [x1 1/3
          x2 11/37
          expected1 "D81E820103"
          expected2 "D81E820B1825"
          [read1 written1] (roundtrip* x1 nil nil)
          [read2 written2] (roundtrip* x2 nil nil)]
      (is (= x1 read1))
      (is (= x2 read2))
      (is (= expected1 written1))
      (is (= expected2 written2))))

  )

(deftest cbor->json-tests
  (let [bs-in (cbor/to-bytes {:foo 1 :bar 2} nil)
        in    (ByteArrayInputStream. bs-in)
        json-out (cbor/cbor->json in)]
    (is (= json-out "{\"foo\":1,\"bar\":2}")))
  )

(deftest cbor->edn-tests
  (let [bs-in (cbor/to-bytes {:foo 1 :bar 2} nil)
        in    (ByteArrayInputStream. bs-in)
        edn-out (cbor/cbor->edn in {:key-fn keyword})]
    (is (= edn-out "{:foo 1, :bar 2}")))
  )
