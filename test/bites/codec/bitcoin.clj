(ns bites.codec.bitcoin
  (:require [clojure.test :refer :all]
            [bites.codec.primitives-test :refer [roundtrip*]]
            [bites.codec :refer :all]
            [bites.encoding :as u]
            [bites.channel :as channel]
            [clojure.set :as set])
  (:import (java.security MessageDigest)
           (java.util Date)))

;;;;;;;;;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def var-int-le
  (reify BinaryCodec
    (bin-read  [_ in]
      (let [[b] (bin-read :byte in)]
        (condp = b
          ;; 0xfd
          -3 (-> (bin-read :short-le in)
                 (update 1 inc))
          ;; 0xfe
          -2 (-> (bin-read :int-le in)
                 (update 1 inc))
          ;; 0xff
          -1 (-> (bin-read :long-le in)
                 (update 1 inc))
          [b 1])))
    (bin-write [_ out v]
      (cond
        (< v 0xfd)
        (channel/write-byte out v)

        (< v 0xffff)
        (do (channel/write-byte out 0xfd)
            (inc (bin-write :short-le out v)))
        (< v 0xffffffff)
        (do (channel/write-byte out 0xfe)
            (inc (bin-write :int-le out v)))
        :else
        (do (channel/write-byte out 0xff)
            (inc (bin-write :long-le out v)))))))

;;;;;;;;;;; messages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- sha256 [^bytes bs]
  (let [hash (MessageDigest/getInstance "SHA-256")]
    (.digest hash bs)))

(defn- message-checksum [bs]
  (let [hash (-> bs byte-array sha256)
        res (byte-array 4)]
    (System/arraycopy hash 0 res 0 4)
    (map u/byte->ubyte res)))

;; see https://en.bitcoin.it/wiki/Protocol_documentation#Message_structure
(def message-codec
  (ordered-map
    :magic (enum :uint-le {:main     0xD9B4BEF9
                           :testnet  0xDAB5BFFA
                           :testnet3 0x0709110B
                           :namecoin 0xFEB4BEF9})
    :command (-> (cstring "US-ASCII")
                 (padded :length 12 :padding-byte 0 :truncate? true))
    :payload (header
               (ordered-map :length :uint-le
                            :checksum (repeated :ubyte :length 4))
               ;; how should we parse the body for this header?
               (fn [{:keys [length checksum]}]
                 (wrap
                   (repeated :ubyte :length length)
                   identity
                   (fn [payload]
                     (assert (= checksum (message-checksum payload)))
                     payload)))
               ;; create a new header for this body
               (fn [payload]
                 {:length (count payload)
                  :checksum (message-checksum payload)}))))

;;;;;;;;;;; transaction scripts ;;;;;;;;;;;;;;;;;;;;;;;;

(defonce opcodes
  {:OP_0	0
   :OP_PUSHDATA1	76
   :OP_PUSHDATA2	77
   :OP_PUSHDATA4	78
   :OP_1NEGATE	79
   :OP_1 81
   :OP_2 82
   :OP_3 83
   :OP_4 84
   :OP_5 85
   :OP_6 86
   :OP_7 87
   :OP_8 88
   :OP_9 89
   :OP_10	90
   :OP_11	91
   :OP_12	92
   :OP_13	93
   :OP_14	94
   :OP_15	95
   :OP_16	96
   :OP_NOP	97
   :OP_IF	99
   :OP_NOTIF	100
   :OP_ELSE	103
   :OP_ENDIF	104
   :OP_VERIFY	105
   :OP_RETURN	106
   :OP_TOALTSTACK	107
   :OP_FROMALTSTACK	108
   :OP_IFDUP	115
   :OP_DEPTH	116
   :OP_DROP	117
   :OP_DUP	118
   :OP_NIP	119
   :OP_OVER	120
   :OP_PICK	121
   :OP_ROLL	122
   :OP_ROT	123
   :OP_SWAP	124
   :OP_TUCK	125
   :OP_2DROP	109
   :OP_2DUP	110
   :OP_3DUP	111
   :OP_2OVER	112
   :OP_2ROT	113
   :OP_2SWAP	114
   :OP_CAT	126
   :OP_SUBSTR	127
   :OP_LEFT	128
   :OP_RIGHT	129
   :OP_SIZE	130
   :OP_INVERT	131
   :OP_AND	132
   :OP_OR	133
   :OP_XOR	134
   :OP_EQUAL	135
   :OP_EQUALVERIFY	136
   :OP_1ADD	139
   :OP_1SUB	140
   :OP_2MUL	141
   :OP_2DIV	142
   :OP_NEGATE	143
   :OP_ABS	144
   :OP_NOT	145
   :OP_0NOTEQUAL	146
   :OP_ADD	147
   :OP_SUB	148
   :OP_MUL	149
   :OP_DIV	150
   :OP_MOD	151
   :OP_LSHIFT	152
   :OP_RSHIFT	153
   :OP_BOOLAND	154
   :OP_BOOLOR	155
   :OP_NUMEQUAL	156
   :OP_NUMEQUALVERIFY	157
   :OP_NUMNOTEQUAL	158
   :OP_LESSTHAN	159
   :OP_GREATERTHAN	160
   :OP_LESSTHANOREQUAL	161
   :OP_GREATERTHANOREQUAL	162
   :OP_MIN	163
   :OP_MAX	164
   :OP_WITHIN	165
   :OP_RIPEMD160	166
   :OP_SHA1	167
   :OP_SHA256	168
   :OP_HASH160	169
   :OP_HASH256	170
   :OP_CODESEPARATOR	171
   :OP_CHECKSIG	172
   :OP_CHECKSIGVERIFY	173
   :OP_CHECKMULTISIG	174
   :OP_CHECKMULTISIGVERIFY	175
   :OP_PUBKEYHASH	253
   :OP_PUBKEY	254
   :OP_INVALIDOPCODE	255
   :OP_RESERVED	80
   :OP_VER	98
   :OP_VERIF	101
   :OP_VERNOTIF	102
   :OP_RESERVED1	137
   :OP_RESERVED2	138})

(defonce opcodes-rev (set/map-invert opcodes))
(defonce push-codec-opcodes (set (range 1 76)))
(defonce push-codecs (->> push-codec-opcodes
                          (map (partial repeated :ubyte :length))
                          (zipmap push-codec-opcodes)))

(def script-codec
  (let [byte-codec (primitive :byte)]
    (reify BinaryCodec
      (bin-read  [_ in]
        (let [[overall] (bin-read var-int-le in)]
          (loop [n 0
                 res (transient [])]
            (if
              (= n overall)
              [(persistent! res) overall]
              (let [b (-> (bin-read byte-codec in)
                          first
                          u/byte->ubyte)]
                (if (contains? push-codec-opcodes b)
                  (recur (+ n b 1) (conj! res (first (bin-read (push-codecs b) in))))
                  (recur (inc n) (conj! res (opcodes-rev b)))))))))
      (bin-write [_ out script]
        (let [len (reduce #(if (keyword? %2)
                             (inc %1)
                             (+ %1 1 (count %2)))
                          0
                          script)]
          (bin-write var-int-le out len)
          (transduce
            (map (fn [token]
                   (if (keyword? token)
                     (channel/write-byte out (opcodes token))
                     (let [len (count token)]
                       (bin-write byte-codec out (byte len))
                       (-> (push-codecs len)
                           (bin-write out token)
                           inc)))))
            + 1 script))))))


;;;;;;;;;; blocks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def block-magic
  (constant (repeated :ubyte :length 4) [0xf9 0xbe 0xb4 0xd9]))

(def block-hash (repeated :ubyte :length 32))

(defn var-len [codec]
  (repeated codec :length-prefix var-int-le))

(def transaction-input
  (ordered-map
    :hash block-hash
    :index :int-le
    :script script-codec
    :sequence-number :int-le))

(def transaction-output
  (ordered-map
    :amount :long-le
    :script script-codec))

(def transaction
  (ordered-map
    :transaction-version :int-le
    :inputs  (var-len transaction-input)
    :outputs (var-len transaction-output)
    :lock-time :int-le))

(def block-codec
  (ordered-map
    :separator block-magic
    :length :int-le
    :header (ordered-map
              :block-version :int-le
              :previous-hash block-hash
              :merkle-root block-hash
              :timestamp (wrap :int-le
                           #(int (/ (.getTime ^Date %) 1000))
                           #(Date. (long (* % 1000))))
              :target :int-le
              :nonce :int-le)
    :transactions (var-len transaction)))


(deftest bitcoin
  (let [payload [0x62 0xEA 00 00 01 00 00 00 00 00 00 00 0x11 0xB2 0xD0 0x50 00 00 00 00
                 0x01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0xFF 0xFF 00 00 00 00 00 00
                 0x01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0xFF 0xFF 00 00 00 00 00 00
                 0x3B 0x2E 0xB3 0x5D 0x8C 0xE6 0x17 0x65
                 0x0F 0x2F 0x53 0x61 0x74 0x6F 0x73 0x68 0x69 0x3A 0x30 0x2E 0x37 0x2E 0x32 0x2F
                 0xC0 0x3E 0x03 00]]
    (let [c1 message-codec
          c2 block-codec
          v1 {:magic   :main
              :command "version"
              :payload payload}
          v2 {:transactions
               [{:lock-time  0,
                 :outputs [{:script [[4 103 138 253 176 254 85 72 39 25 103 241 166 113 48 183 16 92 214 168 40 224 57 9 166 121 98 224 234 31 97 222 182 73 246 188 63 76 239 56 196 243 85 4 229 30 193 18 222 92 56 77 247 186 11 141 87 138 76 112 43 107 241 29 95] :OP_CHECKSIG],
                            :amount 5000000000}],
                 :inputs [{:sequence-number -1,
                           :script [[255 255 0 29] [4] [84 104 101 32 84 105 109 101 115 32 48 51 47 74 97 110 47 50 48 48 57 32 67 104 97 110 99 101 108 108 111 114 32 111 110 32 98 114 105 110 107 32 111 102 32 115 101 99 111 110 100 32 98 97 105 108 111 117 116 32 102 111 114 32 98 97 110 107 115]],
                           :index  -1,
                           :hash  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]}],
                 :transaction-version 1}],
              :header    {:nonce         2083236893,
                          :target        486604799,
                          :timestamp     #inst "2009-01-03T18:15:05.000-00:00",
                          :merkle-root   [59 163 237 253 122 123 18 178 122 199 44 62 103 118 143 97 127 200 27 195 136 138 81 50 58 159 184 170 75 30 94 74],
                          :previous-hash [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0],
                          :block-version 1},
              :length    285,
              :separator [249 190 180 217]}
          [read1 written1] (roundtrip* c1 v1)
          [read2 written2] (roundtrip* c2 v2)
          ]

      (is (= v1 read1))
      (is (= (seq written1)
             [-7 -66 -76 -39 0 0 0 0 ;; Main network magic bytes (LE)
              118 101 114 115 105 111 110 0 0 0 0 0 ;; "version" command with padding (LE)
              100 0 0 0 ;; 100 bytes payload (LE)
              0 0 0 0 0 -108 0 2 0 -111 0 51 0 ;; payload checksum
              ;; payload
              98 0              ;; 0x62
              -22 0 0 0 0 0 1 0 ;; 0xEA = 234
               0 0 0 0 0 0 0 0  ;; 4 ubytes
               0 0 0 0 0 0 17 0 ;; 0x11
              -78 0 -48 0
               80 0 0 0 0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  -1  0  -1  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  -1  0  -1  0  0  0  0  0  0  0  0  0  0  0  0  0  59  0  46  0  -77  0  93  0  -116  0  -26  0  23  0  101  0  15  0  47  0  83  0  97  0  116  0  111  0  115  0  104  0  105  0  58  0  48  0  46  0  55  0  46  0  50  0  47  0  -64  0  62  0  3  0  0]))

      (is (= v2 read2))
      (is (= (seq written2)
             [0 -7  ;; 249
              0 -66 ;; 190
              0 -76 ;; 180
              0 -39 ;; 217 etc
              29 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 59 0 -93 0 -19 0 -3 0 122 0 123 0 18 0 -78 0 122 0 -57 0 44 0 62 0 103 0 118 0 -113 0 97 0 127 0 -56 0 27 0 -61 0 -120 0 -118 0 81 0 50 0 58 0 -97 0 -72 0 -86 0 75 0 30 0 94 0 74 41 -85 95 73 -1 -1 0 29 29 -84 43 124 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 -1 -1 -1 77 4 0 -1 0 -1 0 0 0 29 1 0 4 69 0 84 0 104 0 101 0 32 0 84 0 105 0 109 0 101 0 115 0 32 0 48 0 51 0 47 0 74 0 97 0 110 0 47 0 50 0 48 0 48 0 57 0 32 0 67 0 104 0 97 0 110 0 99 0 101 0 108 0 108 0 111 0 114 0 32 0 111 0 110 0 32 0 98 0 114 0 105 0 110 0 107 0 32 0 111 0 102 0 32 0 115 0 101 0 99 0 111 0 110 0 100 0 32 0 98 0 97 0 105 0 108 0 111 0 117 0 116 0 32 0 102 0 111 0 114 0 32 0 98 0 97 0 110 0 107 0 115 -1 -1 -1 -1 1 0 -14 5 42 1 0 0 0 67 65 0 4 0 103 0 -118 0 -3 0 -80 0 -2 0 85 0 72 0 39 0 25 0 103 0 -15 0 -90 0 113 0 48 0 -73 0 16 0 92 0 -42 0 -88 0 40 0 -32 0 57 0 9 0 -90 0 121 0 98 0 -32 0 -22 0 31 0 97 0 -34 0 -74 0 73 0 -10 0 -68 0 63 0 76 0 -17 0 56 0 -60 0 -13 0 85 0 4 0 -27 0 30 0 -63 0 18 0 -34 0 92 0 56 0 77 0 -9 0 -70 0 11 0 -115 0 87 0 -118 0 76 0 112 0 43 0 107 0 -15 0 29 0 95 -84 0 0 0 0]))

      )
    )
  )