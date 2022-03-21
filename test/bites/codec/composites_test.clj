(ns bites.codec.composites-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [bites.codec :refer :all]
            [bites.codec.primitives-test :refer [roundtrip*]]
            [bites.array :as array])
  (:import (java.util Date)
           (java.io ByteArrayOutputStream)))

(deftest pre-post-processing
  (let [c1 (wrap :int dec inc)
        c2 (wrap :long #(.getTime %) #(Date. ^long %))
        [read1 written1] (roundtrip* c1 (int 1))
        [read2 written2] (roundtrip* c2 #inst "1999-12-31T23:59:59")]
    (is (= 1 read1))
    (is (= [0 0 0 0] (seq written1)))

    (is (= #inst "1999-12-31T23:59:59" read2))
    (is (= [0 0 0 -36 106 -49 -88 24] (seq written2)))
    )
  )

(deftest string-encodings
  (let [c1 (string "UTF-8" :length-prefix :byte)
        c2 (string "UTF-8" :length-prefix :int-be)
        c3 (string "UTF-8" :length-prefix :short-le)
        c4 (string "UTF-8" :length 2)
        c5 (string "US-ASCII")
        v "ABC"
        [read1 written1] (roundtrip* c1 v)
        [read2 written2] (roundtrip* c2 v)
        [read3 written3] (roundtrip* c3 v)
        [read4 written4] (roundtrip* c4 "AA")
        [read5 written5] (roundtrip* c5 v)]

    (is (= v read1))
    (is (= [3 65 66 67] (seq written1)))

    (is (= v read2))
    (is (= [0 0 0 3 65 66 67] (seq written2)))

    (is (= v read3))
    (is (= [3 0 65 66 67] (seq written3)))

    (is (= "AA" read4))
    (is (= [65 65] (seq written4)))

    (is (= v read5))
    (is (= [65 66 67] (seq written5)))
    )
  )

(deftest cstring-encodings
  (let [c1 (cstring "UTF-8")
        c2 (repeated (cstring "UTF-8") :length 2)
        v1 "ABC"
        v2 ["AAA" "BBB"]
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)]

    (is (= v1 read1))
    (is (= [65 66 67 0] (seq written1)))

    (is (= v2 read2))
    (is (= [65 65 65 0  ;; first element
            66 66 66 0] ;; second element
           (seq written2)))
    )
  )

(deftest map-encodings
  (let [c (ordered-map :foo :int-be
                       :bar :short-le
                       :baz :ubyte)
        v {:foo 1
           :bar 2
           :baz 255}
        [read written] (roundtrip* c v)]
    (is (= v read))
    (is (= [0 0 0 1 ;; int-be
            2 0     ;; short-le
            0 -1]   ;; ubyte (written as short-be)
           (seq written)))))

(deftest repeated-encodings
  (let [c1 (repeated :byte :length-prefix :byte)
        c2 (repeated :byte :length 5)
        c3 (repeated (string "UTF8" :length-prefix :byte)
                     :length-prefix :int-be)
        c4 (repeated :byte)
        c5 (repeated :short-le)
        c6 (repeated :short-le :separator 123)
        v (vec (range 5))
        [read1 written1] (roundtrip* c1 v)
        [read2 written2] (roundtrip* c2 v)
        [read3 written3] (roundtrip* c3 ["AAA" "BB" "C"])
        [read4 written4] (roundtrip* c4 v)
        [read5 written5] (roundtrip* c5 v)
        [read6 written6] (roundtrip* c6 v)]

    (is (= v read1))
    (is (= [5 0 1 2 3 4] (seq written1)))

    (is (= v read2))
    (is (= [0 1 2 3 4] (seq written2)))

    (is (= ["AAA" "BB" "C"] read3))
    (is (= [0 0 0 3   ;; length of outer sequence - int-be
            3         ;; length of first element - byte
            65 65 65  ;; first element
            2         ;; length of second element - byte
            66 66     ;; second element
            1         ;; length of third element - byte
            67]       ;; third element
           (seq written3)))

    (is (= v read4))
    (is (= v (seq written4)))

    (is (= v read5))
    (is (= [0 0 1 0 2 0 3 0 4 0] (seq written5)))

    (is (= v read6))
    (is (= [0 0 1 0 2 0 3 0 4 0 123] (seq written6)))
    ))

(deftest blob-encodings
  (let [c1 (blob)
        c2 (blob :length 7)
        c3 (blob :length-prefix :byte)
        v1 (byte-array (repeat 1025 42))
        v2 (byte-array 7)
        v3 (byte-array 7)
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)
        [read3 written3] (roundtrip* c3 v3)]

    (is (= (seq v1) (seq read1)))
    (is (= (repeat 1025 42) (seq written1)))

    (is (= (seq v2) (seq read2)))
    (is (= (repeat 7 0) (seq written2)))

    (is (= (seq v3) (seq read3)))
    (is (= (cons 7 (repeat 7 0))  (seq written3)))))

(deftest sequence-encodings
  (let [c1 [:byte :byte]
        c2 [:short-be :int-be :short-le]
        v1 [1 2]
        v2 [1 2 99]
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)]

    (is (= v1 (seq read1)))
    (is (= v1 (seq written1)))

    (is (= v2 (seq read2)))
    (is (= [0 1 0 0 0 2 99 0] (seq written2)))))

(deftest bitmasks
  (let [c1 (bits [:a :b :c nil nil nil nil :last])
        c2 (bits [:0 :1 nil nil nil nil :6 :7 :8 nil :10])
        c3 (bits [:flag1 :flag2])
        c4 (bits [:flag1 :flag2])
        v1 #{:c :last}
        v2 #{:1 :7 :10}
        v3 #{:flag2}
        v4 #{}
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)
        [read3 written3] (roundtrip* c3 v3)
        [read4 written4] (roundtrip* c4 v4)]

    (is (= v1 read1))
    (is (= [-124] (seq written1)))

    (is (= v2 read2))
    (is (= [4 -126] (seq written2)))

    (is (= v3 read3))
    (is (= [2r00000010] (seq written3)))

    (is (= v4 read4))
    (is (= [0] (seq written4)))))

(deftest mixed-encodings
  (let [c (ordered-map :foo [:byte :byte]
                       :baz (blob :length 4)
                       :bar (string "UTF-8" :length-prefix :int-be))
        v {:foo [1 2]
           :bar "test"
           :baz (byte-array 4 (byte 55))}
        [read written] (roundtrip* c v)]
    (is (= (update v :baz seq)
           (update read :baz seq)))
    (is (= [1 2              ;; :foo
            55 55 55 55      ;; :baz
            0 0 0 4          ;; :bar prefix-length (as :int-be)
            116 101 115 116] ;; :bar bytes (4 as per prefix-length)
           (seq written)))))

(deftest wrong-length
  (are [codec values]
    (is (thrown? RuntimeException (encode-with codec (ByteArrayOutputStream.) values)))
    (string "UTF-8" :length 5) "X"
    (repeated :int :length 3) [1 2]
    (blob :length 3) (byte-array 2)
    (padded :int-le :length 1) (int 1234)
    (padded :int-le :length 3) (int 1234)
    (padded (repeated (cstring "UTF-8")) :length 1) ["abc" "def" "ghi"]))

(deftest paddings
  (let [c1 (padded :int-be :length 6 :padding-byte (int \x))
        c2 (padded (string "UTF-8" :length 6) :length 6)
        c3 (padded (repeated :int-le :length-prefix :byte) :length 10 :padding-byte 0x99)
        c4 (padded (cstring "US-ASCII") :length 12 :padding-byte (byte \x) :truncate? true)
        c5 (padded (cstring "US-ASCII") :length 3 :padding-byte (byte \x) :truncate? true)
        c6 (padded (repeated (cstring "UTF-8")) :length 11 :truncate? true)
        v1 (int 55)
        v2 "abcdef"
        v3 [1 2]
        v4 "version"
        v5 "ABC"
        v6 ["abc" "def" "ghi"]
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)
        [read3 written3] (roundtrip* c3 v3)
        [read4 written4] (roundtrip* c4 v4)
        [read5 written5] (roundtrip* c5 v5)
        [read6 written6] (roundtrip* c6 v6)
        ]

    (is (= v1 read1))
    (is (= [0 0 0 55 ;; int-be
            120 120] ;; padding
           (seq written1)))

    (is (= v2 read2))
    (is (= [97 98 99 100 101 102] (seq written2)))

    (is (= v3 read3))
    (is (= [2        ;; sequence length - byte
            1 0 0 0  ;; int-le
            2 0 0 0  ;; int-le
            -103]    ;; padding
           (seq written3)))

    (is (= v4 read4))
    (is (= [0x76 0x65 0x72 0x73 0x69 0x6F 0x6E 00 ;; cstring
            120 120 120 120] ;; padded with \x
           (seq written4)))

    (is (= v5 read5))
    (is (= [65 66 67] (seq written5)))

    (is (= v6 read6))
    (is (= (seq (array/toBytes "abc\u0000def\u0000ghi" {}))
           (seq written6)))
    ))


(deftest alignment
  (let [c1 (aligned :int-be :modulo 8 :padding-byte 1)
        c2 (aligned (repeated :short-be :length 3) :modulo 9 :padding-byte 55)
        c3 (aligned [:short-le :short-be] :modulo 6)
        v1 5
        v2 [1 2 3]
        v3 [1 5]
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)
        [read3 written3] (roundtrip* c3 v3)]

    (is (= v1 read1))
    (is (= [0 0 0 5 1 1 1 1]
           (seq written1)))

    (is (= v2 read2))
    (is (= [0 1 ;; first short
            0 2 ;; second short
            0 3 ;; third short
            55 55 55] ;; padding to reach 9
           (seq written2)))

    (is (= v3 read3))
    (is (= [1 0 ;; short-le
            0 5 ;; short-be
            0 0] ;; default padding to reach 6
           (seq written3)))))

(deftest constants
  (let [c1 (constant :int-le 7)
        c2 (constant (string "UTF-8" :length 2) "AB")
        v1 7
        v2 "AB"
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)]

    (is (= v1 read1))
    (is (= [7 0 0 0] (seq written1)))

    (is (= v2 read2))
    (is (= [65 66] (seq written2)))))

(deftest headers
  (let [c1 (header :byte #(string "UTF-8" :length %) count) ;; like prefix-length
        c2 (header :byte #(padded (repeated :int-le :length-prefix :byte)
                                  :length % :padding-byte 0x99) (constantly 11))
        c3 (header :byte #(repeated :short-be :length %) nil :with-header? true)
        v1 "ABC"
        v2 [5 9]
        v3 {:header 2 :body [1 5]}
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)
        [read3 written3] (roundtrip* c3 v3)]

    (is (= v1 read1))
    (is (= [3 ;; 1 byte for length
            65 66 67]
           (seq written1)))

    (is (= v2 read2))
    (is (= [11 ;; 1 byte for outer byte-length
            2  ;; 1 byte for inner sequence-length
            5 0 0 0 ;; int-le
            9 0 0 0 ;;int-le
            -103 -103] ;; padding
           (seq written2)))

    (is (= v3 read3))
    (is (= [2 0 1 0 5] (seq written3)))))

(deftest enums
  (let [c1 (enum :byte {:apple 1 :banana 2 :durian 3})
        c2 (enum (string "UTF8" :length 2) {:alabama "AL" :alaska "AK" :arizona "AZ"})
        c3 (enum (ordered-map :red :ubyte
                              :green :ubyte
                              :blue :ubyte)
                 {:yellow {:red 255
                           :green 255
                           :blue 0}})
        c4 (enum :byte {true 1 false 0})
        c5 (enum :byte {true 1 false 0})
        v1 :durian
        v2 :alaska
        v3 :yellow
        v4 false
        v5 true
        [read1 written1] (roundtrip* c1 v1)
        [read2 written2] (roundtrip* c2 v2)
        [read3 written3] (roundtrip* c3 v3)
        [read4 written4] (roundtrip* c4 v4)
        [read5 written5] (roundtrip* c5 v5)]

    (is (= v1 read1))
    (is (= [3] (seq written1)))

    (is (= v2 read2))
    (is (= [65 75] (seq written2)))

    (is (= v3 read3))
    (is (= [0 -1 ;; 255
            0 -1 ;; 255
            0 0] ;; 0
           (seq written3)))

    (is (= v4 read4))
    (is (= [0] (seq written4)))

    (is (= v5 read5))
    (is (= [1] (seq written5)))
    )
  (is (thrown? RuntimeException
               (->> (byte-array 1 (byte 2))
                    io/input-stream
                    (decode-with (enum :byte {:val 1}))))))

(deftest unions
  (let [codec (union 4
                     {:integer :int-be
                      :shorts [:short-be :short-be]
                      :bytes  [:byte :byte :byte :byte]
                      :prefixed (repeated :byte :length-prefix :byte)
                      :str (string "UTF-8" :length-prefix :byte)})
        result {:integer 0x03345678
                :shorts [0x0334 0x5678]
                :bytes [0x03 0x34 0x56 0x78]
                :prefixed [0x34 0x56 0x78]
                :str "4Vx"}]
    (are [value]
      (= result (first (roundtrip* codec value)))
      {:integer 0x03345678}
      {:shorts [0x0334 0x5678]}
      {:bytes  [0x03 0x34 0x56 0x78]}
      {:prefixed [0x34 0x56 0x78]}
      {:str "4Vx"})))