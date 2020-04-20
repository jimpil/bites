(ns bites.protocols)

(defprotocol ToByteArray (toBytes ^bytes [x opts]))
(defmulti fromBytes (fn [klass x opts] klass))
