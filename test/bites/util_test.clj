(ns bites.util-test
  (:require
    [bites.array :as array]
    [clojure.test :refer :all]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.generators :as gen]
    [bites.util :as util]))

(defspec can-reverse-all-bytes 100
  (prop/for-all [^bytes array gen/bytes]
    (= (seq (reverse array))
       (seq (util/reverse-bytes array)))))

(defspec can-reverse-bytes-from-idx 100
  (prop/for-all [[^bytes array idx]
                 (gen/bind gen/bytes
                           (fn [^bytes bs]
                             (->> bs
                                  alength
                                  unchecked-dec
                                  (max 0)        ;; for empty arrays return 0
                                  (gen/choose 0) ;; choose a valid index
                                  (gen/tuple (gen/return bs)))))]
    (= (seq (reverse (drop idx array)))
       (seq (util/reverse-bytes array idx)))))

(deftest uuid-in-b58-examples
  (testing "https://hexdocs.pm/short_uuid/ShortUUID.html"
    (is (= "DTEETeS5R2XxjrVTZxXoJS"
           (-> "64d7280f-736a-4ffa-b9c0-383f43486d0b"
               (array/toBytes {:encoding :uuid})
               util/b58-str))))
  (testing "https://github.com/greglook/alphabase/blob/main/test/alphabase/base58_test.cljc"
    (is (= "SZAGsv33aLcoudQhUGqRiy"
           (util/b58-str (byte-array [206 241 251 119 171 175 222 43 229 46 42 96 211 113 239 178]))))
    (is (= "NjKBom1a4wfq24FUN9DhFR"
           (util/b58-str (byte-array [175 248 91 160 59 158 190 226 43 83 14 172 225 89 130 36]))))

    )
  (testing "https://digitalbazaar.github.io/base58-spec/"
    (is (= "2NEpo7TZRRrLZSi2U" (util/b58-str (.getBytes "Hello World!"))))
    (is (= "USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z"
           (util/b58-str (.getBytes "The quick brown fox jumps over the lazy dog."))))
    (is (= "1111233QC4" (util/b58-str (array/toBytes 0x0000287fb4cd {}))))
    )
  )
