(ns bites.bin-string
  (:require [bites.util :as ut]))

(defmulti to-bytes (fn [base s opts] base))
(defmethod to-bytes :b2  [_ s _] (ut/binary-bytes s))
(defmethod to-bytes :b8  [_ s _] (ut/octal-bytes s))
(defmethod to-bytes :b16 [_ s _] (ut/b16-bytes s))
(defmethod to-bytes :b64 [_ s b64-flavor] (ut/b64-bytes s b64-flavor))

(defmulti from-bytes (fn [base bs opts] base))
(defmethod from-bytes :b2  [_ bs _] (ut/binary-str bs))
(defmethod from-bytes :b8  [_ bs _] (ut/octal-str bs))
(defmethod from-bytes :b16 [_ bs _] (ut/b16-str bs))
(defmethod from-bytes :b64 [_ bs b64-flavor] (ut/b64-str bs b64-flavor))