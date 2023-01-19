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

(defmacro with-base*
  [k encoder decoder & body]
  (case k
    :encode `(~encoder (do ~@body))
    :decode `(~decoder (do ~@body))))

(defmacro with-base
  [b k & body]
  (case b
    (2, "2")   `(with-base* ~k ut/binary-str ut/binary-bytes ~@body)
    (8, "8")   `(with-base* ~k ut/octal-str  ut/octal-bytes  ~@body)
    (16, "16") `(with-base* ~k ut/b16-str    ut/b16-bytes    ~@body)
    (64, "64") `(with-base* ~k ut/b64-str    ut/b64-bytes    ~@body)
    )
  )

(comment

  (macroexpand '(with-base 2 :decode "11111111"))

  )