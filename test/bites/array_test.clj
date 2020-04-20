(ns bites.array-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [bites.array :as ba]
            [bites.util :as ut])
  (:import (java.nio.charset StandardCharsets)
           (java.util UUID Arrays)
           (org.apache.commons.codec.binary Hex BinaryCodec)
           (java.nio.channels ReadableByteChannel)))

(defn- round-trip*
  "Helper that does the actual round-tripping
   and checks that classes match - returns nil if they don't."
  [obj opts]
  (let [obj-class (class obj)
        obj-bytes (ba/toBytes obj opts)
        round-tripped (ba/fromBytes obj-class obj-bytes opts)
        round-tripped-class (class round-tripped)]
    (if (= obj-class round-tripped-class)
      round-tripped
      (println obj obj-class \newline
               round-tripped round-tripped-class))))

(def examples
  {(Integer/valueOf 2020)               nil
   (Long/valueOf 12345678987654)        nil
   (Double/valueOf 123.45678)           nil
   (BigInteger/valueOf 123456789876544) nil
   (BigDecimal/valueOf 123.456788)      nil

   "whatever1"                          nil
   "whatever2"                          {:encoding "UTF-8"}
   "whatever3"                          {:encoding StandardCharsets/UTF_8}
   "0000ffffa1"                         {:encoding :b16} ;; 5 bytes
   "00010101"                           {:encoding :b2}  ;; 1 byte
   (str (UUID/randomUUID))              {:encoding :uuid}
   (UUID/randomUUID)                    nil

   })

(deftest round-tripping

  (testing "round-tripping (by example) from object to bytes and back"
    (doseq [[x opts] examples]
      ;; null means classes didn't match after round-trip
      (is (= x (round-trip* x opts))
          (format "Class %s doesn't round-trip correctly!" (class x))))))
;;================<GENERATIVE TESTING>==================
(def default-runs 10000)

(defspec round-tripping-gen-small-int default-runs
  (prop/for-all [v (gen/vector gen/small-integer)]
    (= v (round-trip* v nil))))

(defspec round-tripping-gen-large-int default-runs
  (prop/for-all [v (gen/vector gen/large-integer)]
    (= v (round-trip* v nil))))

(defspec round-tripping-gen-double default-runs
  (prop/for-all [v (gen/vector (gen/double* {:NaN? false}))]
    (= v (round-trip* v nil))))

(defspec round-tripping-gen-ascii-string default-runs
  (prop/for-all [v (gen/vector gen/string-ascii)]
    (= v (round-trip* v {:encoding StandardCharsets/US_ASCII}))))

(defspec round-tripping-gen-utf8-string default-runs
  (prop/for-all [v (gen/vector gen/string)]
    (= v (round-trip* v {:encoding "UTF-8"}))))

(defspec round-tripping-gen-hex-string default-runs
  (prop/for-all [v (gen/fmap ;; use external lib during testing
                     #(Hex/encodeHexString ^bytes %)
                     gen/bytes)]
    (= v (round-trip* v {:encoding :b16}))))

(defspec round-tripping-gen-octal-string default-runs
  (prop/for-all [v (gen/fmap
                     #(if (empty? %) "" (ut/octal-str %))
                     gen/bytes)]
    (= v (round-trip* v {:encoding :b8}))))

(defspec round-tripping-gen-binary-string default-runs
  (prop/for-all [v (gen/fmap ;; use external lib during testing
                     #(String. (BinaryCodec/toAsciiBytes ^bytes %))
                     gen/bytes)]
    (= v (round-trip* v {:encoding :b2}))))

(defspec round-tripping-gen-uuid default-runs
  (prop/for-all [v (gen/vector gen/uuid)]
    (= v (round-trip* v nil))))

(defspec round-tripping-gen-uuid-str default-runs
  (prop/for-all [v (gen/vector (gen/fmap str gen/uuid))]
    (= v (round-trip* v {:encoding :uuid}))))

(defspec round-tripping-gen-rbs default-runs
  (prop/for-all [^bytes v gen/bytes]
    (let [rbc       (ba/fromBytes ReadableByteChannel v nil)
          rbc-bytes (ba/toBytes rbc {:buffer-size 10})]
      (Arrays/equals v rbc-bytes))))
