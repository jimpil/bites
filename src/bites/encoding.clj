(ns bites.encoding
  (:require [clojure.string :as str]
            [bites.constants :as const]
            [bites.padding :as padding]))
;; (U)BYTE
(def ubyte? #(< -1 % const/MAX_UNSIGNED_BYTE))
(defn byte->ubyte [x] (Byte/toUnsignedInt (byte x)))
(defn ubyte->byte [b] {:pre [(ubyte? b)]} (byte b)) ;; this will work given the pre-condition

;; (U)SHORT
(def ushort? #(< -1 % const/MAX_UNSIGNED_SHORT))
(defn short->ushort [x] (Short/toUnsignedInt x))

(defn ushort->short
  [x]
  {:pre [(ushort? x)]}
  (let [x (int x)]
    (short
      (cond-> x
              (> x Short/MAX_VALUE)
              (- const/MAX_UNSIGNED_SHORT)))))

;; (U)INT
(def uint? #(< -1 % const/MAX_UNSIGNED_INT))
(defn parse-uint [^String x] (Integer/parseUnsignedInt x))
(defn int->uint ^long [x] (Integer/toUnsignedLong x))

(defn uint->int
  [^long x]
  {:pre [(uint? x)]}
  (int
    (cond-> x
            (> x Integer/MAX_VALUE)
            (- const/MAX_UNSIGNED_INT))))

;; (U)LONG
(def ulong? #(< -1 % const/MAX_UNSIGNED_LONG))
(defn parse-ulong ^long [^String x] (Long/parseUnsignedLong x))

(defn long->ulong
  [^long x]
  (cond-> x
          (neg? x)
          (-> Long/toUnsignedString BigInteger.)))

(defn ulong->long
  ^long [^BigInteger x]
  {:pre [(ulong? x)]}
  (long
    (cond-> x
            (pos? (compare x (BigInteger/valueOf Long/MAX_VALUE)))
            (.subtract (BigInteger/valueOf const/MAX_UNSIGNED_LONG)))))
;;----------------------------------------

(defn nibble->char
  [nibble radix]
  "Convert a nibble to a char using supplied radix. e.g.
   0 -> \\0
   9 -> \\9
   10 -> \\A"
  (let [c (Character/forDigit nibble radix)]
    (if (zero? (int c))
      (throw (IllegalArgumentException. (str "nibble " nibble " is greater than or equal to the radix: " radix)))
      c)))

(defn char->digit
  "Given a <radix>, convert character <c> to a digit."
  [c radix]
  (let [nibble (Character/digit (char c) (int radix))]
    (if (= -1 nibble)
      (throw (IllegalArgumentException. (str "Unable to convert " c " to a digit with radix: " radix)))
      nibble)))

(defn nibbles->chars
  "Returns a seq of chars from a sequence (or array) of <nibbles> with <radix> base.
   If radix is not supplied, 10 is assumed.
  --------------------------------------------------
  nibbles       -> chars
  --------------------------------------------------
  [1 0]         -> (1 0)     ... <radix> 16 (hex)
  [10 11 1 2 3] -> (A B 1 2 3)  ... <radix> 16 (hex)
  [1 0]         -> (1 0)     ... <radix> 2 (binary)
  [1 0 0 1]     -> (1 0 0 1)   ... <radix> 2 (binary)
  [1 0]         -> (1 0)     ... <radix> 10 (decimal)
  [0 1 2 3 4 5] -> (0 1 2 3 4 5) ... <radix> 10 (decimal)
  --------------------------------------------------"
  ([nibbles]
   (nibbles->chars nibbles 10))
  ([nibbles radix]
   (some->> (seq nibbles)
            (mapv #(nibble->char % radix)))))


(defn nibbles->str
  "Returns a string from a sequence (or array) of <nibbles> with <radix> base.
   If radix is not supplied, 10 is assumed
  --------------------------------------------------
  nibbles       -> str
  --------------------------------------------------
  [1 0]         -> '10'     ... <radix> 16 (hex)
  [10 11 1 2 3] -> 'AB123'  ... <radix> 16 (hex)
  [1 0]         -> '10'     ... <radix> 2 (binary)
  [1 0 0 1]     -> '1001'   ... <radix> 2 (binary)
  [1 0]         -> '10'     ... <radix> 10 (decimal)
  [0 1 2 3 4 5] -> '012345' ... <radix> 10 (decimal)
  --------------------------------------------------"
  ([nibbles]
   (nibbles->str nibbles 10))
  ([nibbles radix]
   (when (seq nibbles)
     (->> (nibbles->chars nibbles radix)
          (apply str)
          str/upper-case))))

(defn str->nibbles
  "Returns a sequence of <nibbles> with <radix> base from a string
   If radix is not supplied, 10 is assumed.
  --------------------------------------------------
  str      -> nibbles
  --------------------------------------------------
  '10'     -> [1 0]         ... <radix> 16 (hex)
  'AB123'  -> [10 11 1 2 3] ... <radix> 16 (hex)
  '10'     -> [1 0]         ... <radix> 2 (binary)
  '1001'   -> [1 0 0 1]     ... <radix> 2 (binary)
  '10'     -> [1 0]         ... <radix> 10 (decimal)
  '012345' -> [0 1 2 3 4 5] ... <radix> 10 (decimal)
  --------------------------------------------------"
  ([s]
   (str->nibbles s 10))
  ([s radix]
   (some->> (not-empty s)
            (mapv #(char->digit % radix) s))))


(defn byte->binary-str
  "Creates a bitset string from byte <b>
   --------------------------
   byte         -> binary-str
   --------------------------
   [2r00000001] -> '00000001'
   [2r01101101] -> '01101101'
   --------------------------"
  [b]
  (some-> b
          byte->ubyte
          (Integer/toBinaryString)
          (padding/left-pad \0 8)))

(defn byte->bitset*
  "Creates a bitset from byte <b>
   ---------------------------------
   byte        -> bitset
   ---------------------------------
   2r00000001 -> [0 0 0 0 0 0 0 1]
   2r01101101 -> [0 1 1 0 1 1 0 1]
   ---------------------------------"
  [b]
  (some->> b
           byte->binary-str
           (mapv #(Integer/parseInt (str %) 2))))


(def bitsets ;; has to be a vector so it can be used as a function
  (mapv byte->bitset* (range 256)))

(def byte->bitset
  (comp bitsets byte->ubyte))

(def bitset->byte
  (->> 256
       range
       (map (juxt byte->bitset identity)) ;; [(byte->bitset byte) byte]
       (into {})))

(defn bytes->bitset
  "Creates a bitset from a seq of <bytes>
   ---------------------------------
   bytes        -> bitset
   ---------------------------------
   [2r00000001] -> [0 0 0 0 0 0 0 1]
   [2r01101101] -> [0 1 1 0 1 1 0 1]
   ---------------------------------"
  [bytes]
  (some->> (seq bytes)
           (into [] (mapcat byte->bitset))))

(defn bitset->bytes
  "Creates a sequence of bytes to represent the given <bitset> (sequence of ones and zeros)
   Pads with zeros when necessary to complete a byte.
   ---------------------------------
   bitset            -> bytes
   ---------------------------------
   [0 0 0 0 0 0 0 1] -> [2r00000001]
   [0 1 1 0 1 1 0 1] -> [2r01101101]
   ---------------------------------"
  [bitset]
  (some->> (seq bitset)
           (partition 8 8 (repeat 8 0))
           (mapv bitset->byte)))


