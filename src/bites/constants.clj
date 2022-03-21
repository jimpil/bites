(ns bites.constants)

(def ^:const DEFAULT_BUFFER_SIZE (int 1024))
(def ^:const MAX_ARRAY_SIZE      (int 2147483639))
(def ^:const ^String UTF-8       "UTF-8")
(def ^:const ^String ASCII       "ASCII")
(def ^:const ^String ISO-8859-1  "ISO-8859-1")
(def ^:const ^String EBCDIC      "Cp1047")
(def ^:const ^String EMPTY_STRING "")
(def ^:const ^bytes BYTE-ARRAY-TYPE    (class (byte-array 0)))
(def ^:const ^chars CHAR-ARRAY-TYPE    (class (char-array 0)))
(def ^:const ^int  MAX_UNSIGNED_BYTE   (int (Math/pow 2 8)))
(def ^:const ^int  MAX_UNSIGNED_SHORT  (int (Math/pow 2 16)))
(def ^:const ^long MAX_UNSIGNED_INT    (long (Math/pow 2 32)))
(def ^:const ^BigInteger MAX_UNSIGNED_LONG (biginteger (BigDecimal. (Math/pow 2 64))))
(def ^:const fs (byte 0x1C))
(def ^:const gs (byte 0x1D))
(def ^:const rs (byte 0x1E))
(def ^:const us (byte 0x1F))
(def ^:const al (byte 0x3C))         ;left angle <
(def ^:const ar (byte 0x3E))         ;right angle >
(def ^:const ss-track2 (byte 0x3B))  ;semicolon ;
(def ^:const fs-track2 (byte 0x3D))  ;equal =
(def ^:const fs-ebcdic-equals (byte 0x7E)) ; ebcdic equal =
(def ^:const es-track2 (byte 0x3F))  ;question ?
(def ^:const sc (byte 0x3B))         ; semicolon
(def ^:const bs (byte 0x5C))         ; backslash
(def ^:const ^chars CC (char-array [\C \C]))  ; [\C \C]  (Characters)
(def ^:const ^chars FF (char-array [\F \F]))  ; [\F \F]  (Characters)


