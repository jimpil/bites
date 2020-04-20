(ns bites.constants)

(def ^:const DEFAULT_BUFFER_SIZE (int 1024))
(def ^:const MAX_ARRAY_SIZE      (int 2147483639))
(def ^:const DEFAULT_CHARSET     "UTF-8")
(def ^:const EMPTY_STRING        "")
(def ^:const BYTE-ARRAY-TYPE     (class (byte-array 0)))
(def ^:const CHAR-ARRAY-TYPE     (class (char-array 0)))
