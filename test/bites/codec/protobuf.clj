(ns bites.codec.protobuf
  (:require [clojure.test :refer :all]
            [bites.codec :as codec]
            [bites.channel :as channel]
            [bites.encoding :as enc]
            [clojure.set :as set]))

(defn- reconstruct-varint [bytes]
  (->> bytes
       (map-indexed #(bit-shift-left (bit-and 2r1111111 %2) (* 7 %)))
       (reduce bit-or 0)))

(defn- construct-varint [number]
  (loop [left number
         bs (transient [])]
    (if (<= left 127)
      (persistent! (conj! bs left))
      (recur (bit-shift-right left 7)
             (conj! bs (bit-or 128 (bit-and 127 left)))))))

(def var-int
  "Refer to https://developers.google.com/protocol-buffers/docs/encoding#varints"
  (reify codec/BinaryCodec
    (bin-read  [_ in]
      (loop [bytes (transient [])]
        (if-let [b (channel/read-byte in)]
          (if (bit-test b 8)
            (recur (conj! bytes b))
            (let [ret (persistent! (conj! bytes b))]
              [(reconstruct-varint ret) (count ret)]))
          (let [ret (persistent! bytes)]
            [(reconstruct-varint ret) (count ret)]))))

    (bin-write [_ out v]
      (codec/sum-writes :byte out (construct-varint v)))))

(def proto-key
  (let [types {:varint 0
               :64bit 1
               :delimited 2
               :start-group 3
               :end-group 4
               :32bit 5}
        rev-types (set/map-invert types)]
    (-> var-int
        (codec/wrap
          (fn [[number type]] (bit-or (bit-shift-left number 3) (get types type)))
          #(vector (bit-shift-right % 3) (get rev-types (bit-and 2r111 %)))))))


(comment
  (reconstruct-varint [2r10101100 2r00000010])  ;; => 300
  (map enc/byte->bitset (construct-varint 300)) ;; => ([1 0 1 0 1 1 0 0] [0 0 0 0 0 0 1 0])
  (reconstruct-varint (construct-varint 300))   ;; => 300

  ;(decode protobuf (input-stream "dev-resources/google_message1.dat"))

  (defn t [codec value]
    (let [baos (java.io.ByteArrayOutputStream.)
          _ (codec/encode-with codec baos value)
          arr (.toByteArray baos)
          encoded-bytes (mapv enc/byte->ubyte (seq arr))
          decoded (codec/decode-with codec (java.io.ByteArrayInputStream. arr))]
      (println value \newline
               encoded-bytes \newline
               decoded)))
  (t proto-key [1 :varint])
  )

