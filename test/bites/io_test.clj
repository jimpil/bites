(ns bites.io-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [bites.io :as bio])
  (:import (java.util Arrays)
           (java.nio.channels Channels)
           (java.nio ByteBuffer)
           (java.io ByteArrayOutputStream StringReader ByteArrayInputStream)))

(def default-runs 10000)

(defspec copy-byte-buffer-to-byte-stream default-runs
  (prop/for-all [^bytes v gen/bytes]
    (let [in   (ByteBuffer/wrap v)
          bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy in out nil)
      (Arrays/equals v (.toByteArray bout)))))

(defspec copy-string-to-byte-stream default-runs
  (prop/for-all [v gen/string-ascii]
    (let [bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy v out {:encoding "ASCII"})
      (= v (String. (.toByteArray bout))))))

(defspec copy-CHARS-to-byte-stream default-runs
  (prop/for-all [^chars v (gen/fmap char-array (gen/vector gen/char-ascii))]
    (let [bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy v out nil)
      (->> bout
           .toByteArray
           String.
           .toCharArray
           (Arrays/equals v)))))

(defspec copy-reader-to-byte-stream default-runs
  (prop/for-all [v gen/string-ascii]
    (let [in   (StringReader. v)
          bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy v out {:encoding "ASCII"
                       :buffer-size 8})
      (= v (String. (.toByteArray bout))))))

(defspec copy-byte-stream-to-byte-stream default-runs
  (prop/for-all [^bytes v gen/bytes]
    (let [in   (-> (ByteArrayInputStream. v)
                   (Channels/newChannel))
          bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy in out {:buffer-size 13})
      (Arrays/equals v (.toByteArray bout)))))
