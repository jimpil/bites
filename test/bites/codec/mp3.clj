(ns bites.codec.mp3
  (:require [clojure.test :refer :all]
            [bites.codec :refer :all]))

(defn- int->synchsafe [x]
  {:pre [(< x (bit-shift-left 1 29))]}
  (let [m0 (bit-and x 127)
        m1 (bit-and (bit-shift-right x 7) 127)
        m2 (bit-and (bit-shift-right x 14) 127)
        m3 (bit-and (bit-shift-right x 21) 127)]
    (->> [m0
          (bit-shift-left m1 8)
          (bit-shift-left m2 16)
          (bit-shift-left m3 24)]
         (reduce bit-or (int 0)))))

(defn- synchsafe->int [x]
  (let [m0 (bit-and x 255)
        m1 (bit-and (bit-shift-right x 8) 255)
        m2 (bit-and (bit-shift-right x 16) 255)
        m3 (bit-and (bit-shift-right x 24) 255)]
    (->> [m0
          (bit-shift-left m1 7)
          (bit-shift-left m2 14)
          (bit-shift-left m3 21)]
         (reduce bit-or (int 0)))))

(defn synchsafe-int []
  (wrap :int-be int->synchsafe synchsafe->int))

(def primary-header
  (ordered-map
    :magic-number (string "ISO-8859-1" :length 3) ;; "ID3"
    :version (ordered-map :major :byte
                          :minor :byte)
    :flags (bits [nil nil nil nil
                  :footer? :experimental?
                  :extended-header? :unsynchronized?])
    :tag-size (synchsafe-int)))

(def extended-header
  (ordered-map
    :header-size (synchsafe-int)
    :flags-num :byte
    :extended-flags (bits [nil nil nil nil
                           :tag-restrictions? :crc? :update?])))

(def idv2-frame
  (ordered-map
    :id (string "ISO-8859-1" :length 4)
    :size (synchsafe-int)
    ;;  section 4.1.
    :flags (ordered-map
             :status (bits [nil nil nil nil :read-only? :frame-discarded-file-alteration? :frame-discarded-tag-alteration?])
             :format (bits [:data-length-added? :unsynchronized? :encrypted? :compressed? nil nil :group-information?]))
    ))

(def mp3-id3v2-codec
  [primary-header extended-header])

(comment

  (->> "https://www.learningcontainer.com/wp-content/uploads/2020/02/Kalimba.mp3"
       clojure.java.io/input-stream
       (decode-with mp3-id3v2-codec)
       clojure.pprint/pprint)

  (->> "https://www.learningcontainer.com/wp-content/uploads/2020/02/Kalimba.mp3"
       clojure.java.io/input-stream
       (decode-with idv2-frame)
       clojure.pprint/pprint)

  )
