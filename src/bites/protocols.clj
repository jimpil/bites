(ns bites.protocols)

(defprotocol ToByteArray (toBytes ^bytes [x opts]))
(defmulti fromBytes (fn [klass x opts] klass))
(defprotocol Paddable
  (left-pad [this pad]
            [this pad length])
  (left-unpad [this pad])
  (right-pad [this pad]
             [this pad length])
  (right-unpad [this pad])
  )
