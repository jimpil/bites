(ns bites.length-prefix
  (:require [bites.constants :as const]
            [bites.array :as array]
            [bites.padding :as padding]))

(defn encode-length
  [length [prefix endianess]]
  (assert (pos? length))
  (assert (#{:be :le nil} endianess))
  (case prefix
    :int16
    (if (neg? (Integer/compareUnsigned const/MAX_UNSIGNED_SHORT length))
      (throw (IllegalArgumentException. (format "Value '%s' too big for 2 bytes!" length)))
      (let [bs (array/toBytes (int length) {:byte-order endianess})]
        (if (= :le endianess)
          (take 2 bs)
          (take-last 2 bs))))
    :int32
    (if (neg? (Long/compareUnsigned const/MAX_UNSIGNED_INT length))
      (throw (IllegalArgumentException. (format "Value '%s' too big for 4 bytes!" length)))
      (let [bs (array/toBytes (long length) {:byte-order endianess})]
        (if (= :le endianess)
          (take 4 bs)
          (take-last 4 bs))))
    :int64
    (let [ub (biginteger length)]
      (if (neg? (.compareTo const/MAX_UNSIGNED_LONG ub))
        (throw (IllegalArgumentException. (format "Value '%s' too big for 8 bytes!" length)))
        (let [^bytes bs (array/toBytes ub {:byte-order endianess})]
          (if (= :le endianess)
            (take 8 (cond-> bs
                            (> 8 (alength bs))
                            (padding/right-pad 0x00 8)))
            (take-last 8 (cond-> bs
                                 (> 8 (alength bs))
                                 (padding/left-pad 0x00 8)))))))
    )
  )

(comment

  (as-> 4294967295 $n
        (encode-length $n [:int32])   ;; => (-1 -1 -1 -1)
        (array/fromBytes Integer (byte-array $n) nil) ;; => -1
        (Integer/toUnsignedString $n) ;; => "4294967295"
        )

  )
