(ns bites.util-test
  (:require [clojure.test :refer :all]
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
