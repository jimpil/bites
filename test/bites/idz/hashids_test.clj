(ns bites.idz.hashids-test
  (:require [bites.idz.hashids :as hid]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(def gen-alphabet (gen/return hid/DEFAULT_ALPHABET))
(def gen-salt     gen/string-alphanumeric)
(def gen-nums     (gen/not-empty (gen/vector gen/nat)))

(def RUNS 1000)

(defspec consistent-shuffle-empty-salt
  ;;"consistent-shuffle returns the alphabet if given an empty salt"
  RUNS
  (prop/for-all [alphabet gen-alphabet]
    (is (= alphabet (hid/consistent-shuffle alphabet "")))))

(defspec consistent-shuffle-non-empty
  ;;"consistent-shuffle returns something other than the alphabet for a non-empty salt"
  RUNS
  (prop/for-all [alphabet gen-alphabet
                 salt (gen/not-empty gen-salt)]
    (is (not= alphabet (hid/consistent-shuffle alphabet salt)))))

(defspec test-respects-min-length
  ;;"encode a bunch of numbers, and make sure that they return an empty collection when you attempt to decrypt with a different salt"
  RUNS
  (prop/for-all [salt gen-salt
                 nums gen-nums
                 min-length gen/nat]
    (is (<= min-length (count (hid/encode {:salt salt :min-length min-length} nums))))))

(defspec encode-decode-roundtrip
  ;;"encode a bunch of numbers, and make sure that they return an empty collection when you attempt to decrypt with a different salt"
  RUNS
  (prop/for-all [salt gen-salt
                 nums gen-nums
                 min-length gen/nat]
    (is (->> nums
             (hid/encode {:salt salt :min-length min-length})
             (hid/decode {:salt salt :min-length min-length :confirm? false})
             (= nums)))))

(deftest setup-creates-proper-guards
  "Setup returns default values"
  (let [{:keys [alphabet salt seps guards min-length]} (hid/setup {:salt "this is my salt"})]
    (is (= "5N6y2rljDQak4xgzn8ZR1oKYLmJpEbVq3OBv9WwXPMe7" alphabet))
    (is (= "this is my salt" salt))
    (is (= "UHuhtcITCsFifS" seps))
    (is (= "AdG0" guards))
    (is (zero? min-length))))

(deftest setup-returns-defaults
  (let [{:keys [alphabet salt seps guards min-length]} (hid/setup)]
    (is (= "gjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" alphabet))
    (is (= hid/DEFAULT_SEPS seps))
    (is (= "abde" guards))
    (is (= "" salt))
    (is (zero? min-length))))

(deftest consistent-shuffle-known-shuffle
  (is (= "dceba"
         (hid/consistent-shuffle "abcde" "this is my salt")))
  (is (= "fcaodykrgqvblxjwmtupzeisnh"
         (hid/consistent-shuffle "abcdefghijklmnopqrstuvwxyz" "this is my salt")))
  (is (= "f17a8zvCwo0iuqYDXlJ4RmAS2end5ghTcpjbOWLK9GFyE6xUI3ZBMQtPsNHrkV"
         (hid/consistent-shuffle hid/DEFAULT_ALPHABET "salt"))))

(deftest enhash-known-cases
  (is (= "a" (hid/enhash 0 "abcdefg")))
  (is (= "bf" (hid/enhash 12 "abcdefg")))
  (is (= "ga" (hid/enhash 42 "abcdefg")))
  (is (= "cde" (hid/enhash 123 "abcdefg")))
  (is (= "cggc" (hid/enhash 1024 "abcdefg")))
  (is (= "bbadeefc" (hid/enhash 950000 "abcdefg")))
  (is (= "ääå-ÅÅÄö" (hid/enhash 950000 "åäö-ÅÄÖ")))
  (is (= "ebfbfaea" (hid/enhash 3500000 "abcdefg")))
  (is (= "1y-y-X1X" (hid/enhash 3500000 "Xyz01-å"))))

(deftest dehash-known-cases
  (is (= 59 (hid/dehash "abbd" "abcdefg")))
  (is (= 66 (hid/dehash "abcd" "abcdefg")))
  (is (= 100 (hid/dehash "acac" "abcdefg")))
  (is (= 139 (hid/dehash "acfg" "abcdefg")))
  (is (= 218 (hid/dehash "x21y" "xyz1234")))
  (is (= 440 (hid/dehash "yy44" "xyz1234")))
  (is (= 1045 (hid/dehash "1xzz" "xyz1234"))))

(deftest known-encoding-tests
  "Test known encodings of integers from other hashids libraries, for a given salt"
  (let [known-encodings ['("this is my salt" [12345] "NkK9")
                         '("this is my salt" [12346] "69PV")
                         '("this was my salt" [12345] "dRn3")
                         '("this was my salt" [12345] "dRn3")
                         '("" [0] "gY")
                         '("" [0 1 1000000] "pwcnfVMX3")
                         '("this is my salt" [547 31 241271 311 31397 1129 71129] "3RoSDhelEyhxRsyWpCx5t1ZK")]]
    (testing "encode/decode"
      (doseq [[salt nums encoding] known-encodings]
        (is (= encoding (hid/encode {:salt salt} nums)))
        (is (= nums (hid/decode {:salt salt} encoding)))))))

(deftest min-length-known-values
  "Test known encodings of integers from other hashids libraries, for a given salt"
  (is (= "B0NkK9A5" (hid/encode {:salt "this is my salt" :min-length 8} [12345]))))

(deftest failed-decodings-return-empty-collection
  "encode a set of numbers, and ensure that they return an empty collection when decrypted with a different salt"
  (is (empty? (hid/decode {:salt "xyzzy"} (hid/encode {:salt "abcde"} [0 1 2]))))
  (is (empty? (hid/decode {:salt "xyzzy"} (hid/encode {:salt "z"} [9000])))))

(deftest ensure-min-length-sanity-check
  (is (-> (hid/ensure-min-length {:min-length 8
                                  :alphabet "4VNWO5kPrnZ1Y3LgKoBmXyzwb9aMj7l2RDQ6EJexqv8p"
                                  :hash-str "0NkK9A"})
          :hash-str
          (= "B0NkK9A5"))))

(deftest decoding-a-string-an-alphabet-mismatch-returns-empty-list
  (is (empty? (hid/decode {:alphabet "aeiou09123456789"} "aBMswoO2UB3Sj"))))

(deftest different-alphabet
  (is (= "02949" (hid/encode {:alphabet "aeiouy0123456789"} [12345]))))
