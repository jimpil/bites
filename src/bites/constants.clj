(ns bites.constants)

(def ^:const DEFAULT_BUFFER_SIZE (int 1024))
(def ^:const DEFAULT_CHARSET     "UTF-8")
(def ^:const MAX_ARRAY_SIZE      (int 2147483639))
(def ^:const EMPTY_STRING "")

(defonce BYTE-ARRAY-TYPE (class (make-array Byte/TYPE 0)))
(defonce CHAR-ARRAY-TYPE (class (make-array Character/TYPE 0)))
