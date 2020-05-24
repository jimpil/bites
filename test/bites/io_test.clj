(ns bites.io-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [bites.core :as core]
            [bites.io :as bio]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util Arrays)
           (java.nio.channels Channels Pipe)
           (java.nio ByteBuffer)
           (java.io ByteArrayOutputStream StringReader ByteArrayInputStream)
           (java.nio.file Files)))

(def default-runs 10000)

(defspec copy-byte-buffer-to-byte-stream default-runs
  (prop/for-all [^bytes v gen/bytes]
    (let [in   (ByteBuffer/wrap v)
          bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy in out nil)
      (Arrays/equals v (core/to-bytes bout)))))

(defspec copy-string-to-byte-stream default-runs
  (prop/for-all [v gen/string-ascii]
    (let [bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy v out {:encoding "ASCII"})
      (= v (String. (core/to-bytes bout))))))

(defspec copy-CHARS-to-byte-stream default-runs
  (prop/for-all [^chars v (gen/fmap char-array (gen/vector gen/char-ascii))]
    (let [bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy v out nil)
      (->> bout
           core/to-bytes
           String.
           .toCharArray
           (Arrays/equals v)))))

(defspec copy-reader-to-byte-stream default-runs
  (prop/for-all [v gen/string-ascii]
    (let [in   (StringReader. v)
          bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy in out {:encoding "ASCII"
                        :buffer-size 8})
      (= v (String. (core/to-bytes bout))))))

(defspec copy-byte-stream-to-byte-stream default-runs
  (prop/for-all [^bytes v gen/bytes]
    (let [in   (-> (ByteArrayInputStream. v)
                   (Channels/newChannel))
          bout (ByteArrayOutputStream.)
          out  (Channels/newChannel bout)]
      (bio/copy in out {:buffer-size 13})
      (Arrays/equals v (core/to-bytes bout)))))

(deftest copy-to-pipe
  (let [v (byte-array (gen/generate gen/bytes (rand-int 2000)))
        pipe (Pipe/open)]
    (bio/into-pipe! pipe v)
    (Arrays/equals v (bio/from-pipe! pipe (alength v)))))

(deftest copy-file
  (let [temp-path "/tmp/bite.txt"
        temp-file (io/file temp-path)
        data (str/join \newline (gen/generate gen/string-ascii 200))
        data-bytes (.getBytes data)]

    ;; this test creates the file
    (testing "readable-byte-channel->file"
      (let [bin (ByteArrayInputStream. data-bytes)
            rch (Channels/newChannel bin)]
        (bio/copy rch temp-file nil)
        (is (Arrays/equals data-bytes (Files/readAllBytes (.toPath temp-file))))))

    (testing "file->writeable-byte-channel"
      (let [bout (ByteArrayOutputStream.)
            wch  (Channels/newChannel bout)]
        (bio/copy temp-file wch nil)
        (is (Arrays/equals data-bytes (core/to-bytes bout)))))

    (io/delete-file temp-path))


  )

