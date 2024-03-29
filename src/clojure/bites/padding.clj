(ns bites.padding
  (:require [bites.util :as ut]
            [bites.array :as array]))

(defprotocol Paddable
  (left-pad [this pad]
    [this pad length])
  (left-unpad [this pad])
  (right-pad [this pad]
    [this pad length])
  (right-unpad [this pad]))

(defn- with-padding-values
  [side ^bytes this pad]
  (let [pad-val (array/toBytes pad nil)]
    (case side
      :left  (ut/concat-byte-arrays pad-val this)
      :right (ut/concat-byte-arrays this pad-val))))

(defn- with-target-length
  [side ^bytes this pad ^long target]
  (let [have (alength this)
        need (- target have)]
    (if (pos? need)
      (->> (array/toBytes pad nil)
           first
           (repeat need)
           byte-array
           (with-padding-values side this))
      this)))

(extend-protocol Paddable
  (Class/forName "[B")

  (left-pad
    ([this pad]
     (with-padding-values :left this pad))
    ([this pad target]
     (with-target-length :left this pad target)))

  (right-pad
    ([this pad]
     (with-padding-values :right this pad))
    ([this pad target]
     (with-target-length :right this pad target)))

  (left-unpad [^bytes this pad]
    (let [^bytes pad-bs (array/toBytes pad nil)]
      (byte-array (drop (alength pad-bs) this))))

  (right-unpad [^bytes this pad]
    (let [^bytes pad-bs (array/toBytes pad nil)
          n (- (alength ^bytes this)
               (alength pad-bs))]
      (byte-array (take n this))))

  String
  (left-pad
    ([this pad]       ;; Characters/String
     (apply str pad this))
    ([this pad ^long target]
     (let [need (- target (count this))]
       (apply str  (concat (repeat need pad) this)))))

  (right-pad
    ([this pad]
     (apply str this pad)) ;; Characters/String
    ([this pad ^long target]
     (let [need (- target (count this))]
       (apply str this (repeat need pad)))))

  (left-unpad [this pad]
    (subs this (count pad)))

  (right-unpad [this pad]
    (subs this 0 (- (count this) (count pad))))

  )
