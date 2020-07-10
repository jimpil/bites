(ns bites.length-prefix
  (:require [bites.protocols :as proto]
            [bites.constants :as const]
            [bites.array]
            [bites.padding]))

(defn encode-length
  [length [prefix endianess]]
  (assert (pos? length))
  (assert (or (nil? endianess)
              (#{:be :le} endianess)))
  (case prefix
    :int16
    (if (neg? (Integer/compareUnsigned const/MAX_UNSIGNED_SHORT length))
      (throw (IllegalStateException. (format "Value '%s' too big for 2 bytes!" length)))
      (let [bs (proto/toBytes (int length) {:byte-order endianess})]
        (if (= :le endianess)
          (take 2 bs)
          (take-last 2 bs))))
    :int32
    (if (neg? (Long/compareUnsigned const/MAX_UNSIGNED_INT length))
      (throw (IllegalStateException. (format "Value '%s' too big for 4 bytes!" length)))
      (let [bs (proto/toBytes (long length) {:byte-order endianess})]
        (if (= :le endianess)
          (take 4 bs)
          (take-last 4 bs))))
    :int64
    (let [ub (biginteger length)]
      (if (neg? (.compareTo const/MAX_UNSIGNED_LONG ub))
        (throw (IllegalStateException. (format "Value '%s' too big for 8 bytes!" length)))
        (let [bs (proto/toBytes ub nil)]
          (if (= :le endianess)
            (take 8 (reverse bs))
            (take-last 8 bs)))))
    )
  )

(defn with-length-prefix
  "Returns a new byte-array whose N (depends on <encoding>) first bytes
   denote the length of the provided array <bs>. `:int16` (2-byte),
   `:int32` (4-byte) & `:int64` (8-byte) encodings are supported."
  ^bytes [encoding byte-order bs]
  (let [length (if (bytes? bs) (alength ^bytes bs) (count bs))
        length-bs (encode-length length [encoding byte-order])]
    (byte-array (concat length-bs bs))))

(comment

  (as-> 4294967295 $n
        (encode-length $n [:int32])   ;; => (-1 -1 -1 -1)
        (proto/fromBytes Integer (byte-array $n) nil) ;;=> -1
        (Integer/toUnsignedString $n) ;; => "4294967295"
        )

  )
