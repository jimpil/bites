(ns bites.codec
  (:require [bites.buffer :as buffer]
            [bites.channel :as channel]
            [bites.array :as array]
            [bites.util :as util]
            [bites.encoding :as enc]
            [clojure.set :as set])
  (:import (java.nio ByteBuffer BufferUnderflowException)
           (java.util Arrays)
           (clojure.lang ISeq IPersistentVector Keyword IPersistentMap IPersistentCollection Seqable Counted ILookup)
           (java.io ByteArrayOutputStream)))

(defprotocol BinaryCodec
  (bin-read  [codec in])
  (bin-write [codec out v]))

(defn codec? [codec]
  (satisfies? BinaryCodec codec))

(defn- sum-reads [f]
  (let [reads (f)]
    (-> (reduce
          (fn [ret [x n]]
            (-> ret
                (util/update! 0 conj! x)
                (util/update! 1 + n)))
          (transient [(transient []) 0])
          reads)
        (util/update! 0 persistent!)
        persistent!)))

(defn sum-writes
  ^long [codec out vs]
  (transduce (map #(bin-write codec out %)) + vs))

(defn primitive*
  "Create a reification of `BinaryCodec` that can read/write a primmitive data type."
  [n read-fn write-fn & [endianess]]
  (let [^ByteBuffer rbuff (buffer/byte-buffer n endianess)
        ^ByteBuffer wbuff (buffer/byte-buffer n endianess)]
    (reify BinaryCodec
       (bin-read [_ in]
         (let [nread (channel/read-into-buffer in rbuff)]
           (.position rbuff 0)
           (let [ret (read-fn rbuff)]
             (.clear rbuff)
             [ret nread])))
       (bin-write [_ out v]
         (let [^ByteBuffer wbuff (write-fn wbuff v)
               nwritten (->> (.flip wbuff)
                             (channel/write-from-buffer out))]
           (.clear wbuff)
           nwritten))
       Object
       (toString [_]
         (str "<BinaryCodec-" n ">")))))

(def noop
  (reify BinaryCodec
    (bin-read [_ in] [nil 0])
    (bin-write [_ out v] 0)
    Object
    (toString [_] "<BinaryCodec-noop>")))


(def primitives
  {:noop      (constantly noop)
   :byte       #(primitive* 1 buffer/read-byte buffer/write-byte)
   :ubyte      #(primitive* 2 buffer/read-short buffer/write-short)

   ;:char (primitive-codec .readChar .writeChar char :be)
   ;:char-le (primitive-codec .readChar .writeChar char :le)
   ;:char-be (primitive-codec .readChar .writeChar char :be)

   :short      #(primitive* 2 buffer/read-short buffer/write-short)
   :short-le   #(primitive* 2 buffer/read-short buffer/write-short :le)
   :short-be   #(primitive* 2 buffer/read-short buffer/write-short)

   :ushort     #(primitive* 4 buffer/read-int buffer/write-int)
   :ushort-le  #(primitive* 4 buffer/read-int buffer/write-int :le)
   :ushort-be  #(primitive* 4 buffer/read-int buffer/write-int)

   :int        #(primitive* 4 buffer/read-int buffer/write-int)
   :int-le     #(primitive* 4 buffer/read-int buffer/write-int :le)
   :int-be     #(primitive* 4 buffer/read-int buffer/write-int)

   :uint       #(primitive* 8 buffer/read-long buffer/write-long)
   :uint-le    #(primitive* 8 buffer/read-long buffer/write-long :le)
   :uint-be    #(primitive* 8 buffer/read-long buffer/write-long)

   :long       #(primitive* 8 buffer/read-long buffer/write-long)
   :long-le    #(primitive* 8 buffer/read-long buffer/write-long :le)
   :long-be    #(primitive* 8 buffer/read-long buffer/write-long)

   :ulong      #(primitive* 8 buffer/read-ulong buffer/write-ulong)
   :ulong-le   #(primitive* 8 buffer/read-ulong buffer/write-ulong :le)
   :ulong-be   #(primitive* 8 buffer/read-ulong buffer/write-ulong)

   :float      #(primitive* 4 buffer/read-float buffer/write-float)
   :float-le   #(primitive* 4 buffer/read-float buffer/write-float :le)
   :float-be   #(primitive* 4 buffer/read-float buffer/write-float)

   :double     #(primitive* 8 buffer/read-double buffer/write-double)
   :double-le  #(primitive* 8 buffer/read-double buffer/write-double :le)
   :double-be  #(primitive* 8 buffer/read-double buffer/write-double)
   })

(defn primitive
  "Returns a new primitive codec, per <k>."
  [k]
  (if-let [ctor (primitives k)]
    (ctor)
    (throw
      (IllegalArgumentException.
        (format "Primitive codec %s not recognised" k)))))

(defn instance [k]
  (cond-> k (keyword? k) primitive))

(defn instances [codecs]
  (loop [cs codecs
         instances {}
         ret []]
    (if-let [codec (first cs)]
     (if-let [obj (get instances codec)]
       (recur (next cs) instances (conj ret obj))
       (let [obj (instance codec)]
         (recur (next cs)
                (assoc instances codec obj)
                (conj ret obj))))
      ret)))

(defn wrap
  "Wrap a `codec` with pre/post-processing functions to be applied
   to the value before writing/after reading. Use these to transform values
   according to domain specific rules."
  [codec pre-encode post-decode]
  {:pre [(codec? codec)]}
  (let [codec (instance codec)]
    (reify BinaryCodec
      (bin-read  [_ in]
        (-> codec
            (bin-read in)
            (update 0 post-decode)))
      (bin-write [_ out v]
        (->> (pre-encode v)
             (bin-write codec out)))
      Object
      (toString [_]
        (str "<BinaryCodec wrapped, inner=" codec ">")))))


(extend-protocol BinaryCodec

  (Class/forName "[B")
  (bin-read [this in]
    (let [buff (buffer/byte-buffer (alength ^bytes this))]
      (channel/read-into-buffer in buff)
      (let [bs (.array buff)]
        (assert (Arrays/equals ^bytes bs ^bytes this)
                (format "Expected to read array '%s', found '%s' instead."
                        (str (seq this)) (str (seq bs))))
       [this (alength ^bytes this)])))
  (bin-write [this out _]
    (let [buff (ByteBuffer/wrap this)]
      (channel/write-from-buffer out buff)))

  String
  (bin-read [this in]
    (let [this-bs (.getBytes this) ;; default charset only when reading
          buff (buffer/byte-buffer (alength this-bs))]
      (channel/read-into-buffer in buff)
      (let [bs (.array buff)
            ret (String. bs)]
        (assert (Arrays/equals bs this-bs)
                (format "Expected to read string '%s', found '%s' instead." this ret))
        [this (alength this-bs)])))
  (bin-write [this out encoding]
    (let [^String charset (or encoding "UTF-8")
          buff (ByteBuffer/wrap (.getBytes ^String this charset))]
      (channel/write-from-buffer out buff)))

  ISeq
  (bin-read [this in]
    (sum-reads (fn [] (map #(bin-read % in) this))))
  (bin-write [this out vs]
    (reduce + (map #(bin-write % out %2) this vs)))

  IPersistentVector
  (bin-read [this in]
    (sum-reads (fn [] (map #(bin-read % in) this))))
  (bin-write [this out vs]
    (reduce + (map #(bin-write % out %2) this vs)))

  Keyword
  (bin-read [this in]
    (bin-read (primitive this) in))
  (bin-write [this out v]
    (bin-write (primitive this) out v))

  IPersistentMap
  (bin-read  [this in]
    (util/map-vals #(bin-read % in) this))
  (bin-write [this out v]
    (reduce + (map (fn [[k v]] (bin-write (get this k) out v)) v)))
  )
;; --------<<PUBLIC API>>---------------
(defn encode-with
  "Serialize a value <v> to <out> according to the codec."
  [codec out v]
  (with-open [channel-out (channel/output out)]
    (bin-write codec channel-out v)))

(defn decode-with
  "Deserialize a value from `in` according to the codec."
  [codec in]
  (with-open [channel-in (channel/input in)]
    (first (bin-read codec channel-in))))

(defn ordered-map
  "Parse a binary stream into a map."
  [& kvs]
  {:pre [(even? (count kvs))]}
  (let [ks (take-nth 2 kvs)
        vs (instances (take-nth 2 (next kvs)))
        key-order (into {} (map-indexed #(vector %2 %) ks))
        internal-map (apply sorted-map-by
                            (comparator #(< ^long (key-order % Long/MAX_VALUE)
                                            ^long (key-order %2 Long/MAX_VALUE)))
                            kvs)]
    (reify
      BinaryCodec
      (bin-read  [_ in]
        (let [[ret nread]
              (sum-reads
                #(eduction
                   (map (fn [c] (bin-read c in)))
                   vs))]
          [(zipmap ks ret) nread]))
      (bin-write [_ out v]
        {:pre [(every? (set ks) (keys v))]}
        (->> (map (partial get v) ks)
             (map #(bin-write %1 out %2) vs)
             (reduce +)))

      ILookup
      (valAt [_ key]
        (get internal-map key))
      (valAt [_ key not-found]
        (get internal-map key not-found))

      Counted
      (count [_]
        (count internal-map))

      ;       clojure.lang.Associative
      ;       (containsKey [_ k]
      ;         (contains? internal-map k))
      ;       (entryAt [_ k]
      ;         (get internal-map k))
      ;       (assoc [this k v]
      ;         (apply ordered-map (apply concat (seq (assoc internal-map k v)))))

      IPersistentMap
      (assoc [this k v]
        (apply ordered-map (apply concat (seq (assoc internal-map k v)))))
      (assocEx [this k v]
        (if (internal-map k)
          (throw (ex-info "Key already present" {:key k}))
          (apply ordered-map (apply concat (seq (assoc internal-map k v))))))
      (without [this k]
        (apply ordered-map (apply concat (seq (dissoc internal-map k)))))

      IPersistentCollection
      (cons [this [k v]] (assoc this k v))
      (empty [_] (ordered-map))
      (equiv [_ other] false)

      Seqable
      (seq [_] (seq internal-map))

      ;; Java interfaces
      Iterable
      (iterator [this] (.iterator ^Iterable (seq this)))
      Object
      (toString [this] (str internal-map)))))

(defn- read-n
  "Performance optimization for `(repeatedly n #(read-data codec big-in little-in))`"
  [codec in n]
  (eduction
    (map (fn [_] (bin-read codec in)))
    (util/reducible-range n)))

(defn- read-all-valid
  "Performance optimization for `(take-while (complement nil? )(repeatedly n #(read-data codec big-in little-in)))`"
  [codec in]
  (eduction
    (map (fn [_]
           (try (bin-read codec in)
                (catch BufferUnderflowException _ [nil -1]))))
    (take-while (comp pos? second))
    (util/reducible-range)))

(defn- read-to-separator
  "Read until the read value equals `sep` (excluding it)."
  [codec in sep]
  (eduction
    (map (fn [^long i]
           (try (bin-read codec in)
                (catch BufferUnderflowException e
                  (if (zero? i)
                    ;; nothing read yet - i.e. empty stream
                    (throw e)
                    [sep])))))
    (take-while (fn [[v n]]
                  (and (not= v sep)
                       (util/not-neg? n))))
    (util/reducible-range)))

(defn repeated
  "Read a sequence of values. Options are pairs of keys and values with possible keys:
- `:length` fixed length of the sequence
- `:length-prefix` codec for the length of the sequence to read prior to the sequence itself.
- `:separator` reads until the read value equals the given separator value. EOF of a stream is regarded a separator too.
That means if the last token is the last element in a stream, the final separator may be missing. Caution: When
writing the data there WILL be a final separator. This means, the written data may have more bytes than initially read!
If there is no options, the decoder tries to read continuously until the stream is exhausted.
Example: To read a sequence of integers with a byte prefix for the length use `(repeated :byte :length-prefix :int)`"
  [codec & {:keys [length length-prefix separator]}]
  {:pre [(codec? codec)]}
  (let [codec (instance codec)] ;; optimise multiple reads/writes
    (cond
      length
      (reify BinaryCodec
        (bin-read  [_ in]
          (sum-reads (partial read-n codec in length)))
        (bin-write [_ out vs]
          (let [values-count (count vs)]
            (if (= length values-count)
              (sum-writes codec out vs)
              (throw
                (IllegalStateException.
                  (format "This sequence should have length %s but has really length %s" length values-count))))))
        Object
        (toString [_]
          (str "<BinaryCodec repeated,length=" length ">")))

      separator
      (let [separator (enc/ubyte->byte separator)]
        (reify BinaryCodec
          (bin-read  [_ in]
            (sum-reads (partial read-to-separator codec in separator)))
          (bin-write [_ out vs]
            (let [n (sum-writes codec out vs)]
              (+ n ^long (channel/write-byte out separator))))
          Object
          (toString [_]
            (str "<BinaryCodec repeated,separator=" separator ">"))))

      (codec? length-prefix)
      (let [prefix-codec (instance length-prefix)]
        (reify BinaryCodec
          (bin-read  [_ in]
            (let [[length ^long nread1] (bin-read prefix-codec in)
                  [ret ^long nread2] (if (some-> length pos-int?)
                                       (sum-reads (partial read-n codec in length))
                                       (sum-reads (partial read-all-valid codec in)))]
              [ret (+ nread1 nread2)]))
          (bin-write [_ out vs]
            (let [written (bin-write prefix-codec out (count vs))]
              (+ ^long written (sum-writes codec out vs))))
          Object
          (toString [_]
            (str "<BinaryCodec repeated,length-prefix=" prefix-codec ">"))))
      :else
      (repeated codec :length-prefix :noop))))

(defn- read-nbytes [in len]
  (let [buff  (buffer/byte-buffer len)
        nread (channel/read-into-buffer in buff)
        bs    (.array buff)]
    [bs nread]))

(defn blob
  "Reads a chunk of binary data as a Java byte array.
   Options as in `repeated`, except :separator is not supported."
  [& {:keys [length length-prefix]}]
  (cond
    length
    (reify BinaryCodec
      (bin-read  [_ in] (read-nbytes in length))
      (bin-write [_ out vs]
        (let [vs-length (alength ^bytes vs)]
          (if (= length vs-length)
            (->> (ByteBuffer/wrap vs)
                 (channel/write-from-buffer out))
            (throw
              (IllegalStateException.
                (format  "This sequence should have length %s but has really length %s" length vs-length))))))
      Object
      (toString [_]
        (str "<BinaryCodec blob,length=" length ">")))

    (codec? length-prefix)
    (let [prefix-codec (instance length-prefix)]
      (reify BinaryCodec
        (bin-read  [_ in]
          (let [[length ^long nread1] (bin-read prefix-codec in)
                [ret ^long nread2] (read-nbytes in length)]
            [ret (+ nread1 nread2)]))
        (bin-write [_ out vs]
          (let [length (alength ^bytes vs)]
            (+ ^long (bin-write prefix-codec out length)
               ^long (channel/write-from-buffer out (ByteBuffer/wrap vs)))))
        Object
        (toString [_]
          (str "<BinaryCodec blob,prefix=" prefix-codec ">"))))
    :else
    (reify BinaryCodec
      (bin-read  [_ in]
        (let [os (ByteArrayOutputStream.)
              _  (util/transfer! (channel/bin-input in) (channel/bin-output os))
              bs (.toByteArray os)]
          [bs (alength bs)]))
      (bin-write [_ out vs]
        (->> (ByteBuffer/wrap vs)
             (channel/write-from-buffer out)))
      Object
      (toString [_] "<BinaryCodec blob,unbounded>"))))

(defn- confirm-constant
  [constantv v]
  (if (= constantv v)
    constantv
    (throw
      (ex-info
        (format "value '%s' should have had the constant value '%s'"
                (str v)
                (str constantv))
        {:constant-value constantv
         :value v}))))

(defn constant
  "Reads a constant value, ignores given value on write. Can be used as a version tag for a composite codec.
Example:
    (encode out (constant :int-le 7) 1234)
    => ;will instead write bytes [7 0 0 0]"
  [codec constant-value]
  (-> codec
      (wrap
        (constantly constant-value)
        (partial confirm-constant constant-value))))

(defn string
  [^String encoding & options]
  (-> (apply repeated :byte options)
      (wrap
        #(array/toBytes % {:encoding encoding})
        #(String. (byte-array %) encoding))))

(defn cstring
  "Zero-terminated string (like in C).
   String is a sequence of bytes, terminated by a 0 byte."
  [^String encoding]
  (string encoding :separator (byte 0)))

(defn- bit-set? [bytes-vec ^long idx]
  (not (zero? (bit-and ^byte (bytes-vec (- (count bytes-vec) 1 (long (quot idx 8))))
                       (bit-shift-left 1 (long (mod idx 8)))))))
(defn- set-bit [bytes ^long idx]
  (update-in bytes
             [(- (count bytes) 1 (long (quot idx 8)))]
             #(byte (bit-or ^int % (bit-shift-left 1 (long (mod idx 8)))))))

(defn bits
  "`flags` is a sequence of flag names. Each flag's index corresponds to the bit with that index.
Flag names `null` are ignored. Bit count will be padded up to the next multiple of 8."
  [flags]
  (let [byte-count (int (Math/ceil (/ (count flags) 8)))
        idx->flags (into {} (keep-indexed #(when %2 [%1 %2])) flags)
        flags->idx (zipmap (vals idx->flags) (keys idx->flags))
        bit-indices (sort (keys idx->flags))]
    (-> (repeated :byte :length byte-count)
        (wrap
          (fn [flags]
            (reduce set-bit
                    (vec (repeat byte-count (byte 0)))
                    (vals (select-keys flags->idx flags))))
          (fn [bs]
            (into #{}
              (comp
                (filter (partial bit-set? bs))
                (map idx->flags))
              bit-indices))))))

(defn header
  "Decodes a header using `header-codec`. Passes this datastructure to `header->body-codec` which returns the codec to
use to parse the body. For writing this codec calls `body->header` with the data as parameter and
expects a value to use for writing the header information.
  If the optional flag `:keep-header` is set, read will return a map with the keys`:header` and `body`
else only the `body` will be returned."
  [header-codec header->body-codec body->header & {:keys [with-header?]}]
  {:pre [(codec? header-codec)]}
  (let [header-codec (instance header-codec)]
    (reify BinaryCodec
      (bin-read [_ in]
        (let [[header ^long n1] (bin-read header-codec in)
              body-codec        (header->body-codec header)
              [body ^long n2]   (bin-read body-codec in)
              ret (if with-header?
                    {:body body :header header}
                    body)]
          [ret (+ n1 n2)]))
      (bin-write [_ out v]
        (let [body (cond-> v with-header? :body)
              header (cond (and with-header? body->header)
                           (body->header (:header v) body)
                           with-header? (:header v)
                           body->header (body->header body)
                           :else
                           (throw (IllegalStateException. "No way to process header!")))
              body-codec (header->body-codec header)
              n1 (bin-write header-codec out header)
              n2 (bin-write body-codec out body)]
          (+ ^long n1 ^long n2)))
      Object
      (toString [_]
        (str "<BinaryCodec header,codec=" header-codec ">")))))

(defn padded
  "Make sure there is always a minimum byte `length` when reading/writing values.
Works by reading `length` bytes into a byte array, then reading from that array using `inner-codec`.
Currently there are three options:
- `:length` is the number of bytes that should be present after writing
- `:padding-byte` is the numeric value of the byte used for padding (default is 0)
- `:truncate?` is a boolean flag that determines the behaviour if `inner-codec` writes more bytes than
`padding` can handle: false is the default, meaning throw an exception. True will lead to truncating the
output of `inner-codec`.
Example:
    (encode (padding (repeated (string \"UTF8\" :separator 0)) :length 11 :truncate? true) outstream [\"abc\" \"def\" \"ghi\"])
    => ; writes bytes [97 98 99 0 100 101 102 0 103 104 105]
       ; observe: the last separator byte was truncated!"
  [inner-codec & {:keys [^long length
                         padding-byte
                         truncate?]
                  :or {padding-byte 0}
                  :as opts}]
  {:pre [(every? number? [padding-byte length])
         (codec? inner-codec)]}
  (let [inner-codec (instance inner-codec)]
    (reify BinaryCodec
      (bin-read  [_ in]
        (let [buff (buffer/byte-buffer length)
              _ (channel/read-into-buffer in buff)
              in (channel/input buff)]
          (bin-read inner-codec in)))
      (bin-write [_ out v]
        (let [baos (ByteArrayOutputStream. length)
              inner-out (channel/output baos)
              written (long (bin-write inner-codec inner-out v))
              target-length (cond->> written truncate? (min length))
              padding-bytes-left (max 0 (- length target-length))
              diff (- written length)
              too-big? (pos? diff)]
          (if (and too-big? (not truncate?))
            (throw
              (ex-info
                (str "Data should be max. " length " bytes, but attempting to write "
                     (Math/abs ^long (- target-length length)) " bytes more!")
                {:overflow-bytes (Math/abs ^long (- target-length length))}))
            (let [baos-bs (.toByteArray baos)
                  ^bytes bs (if too-big?
                              (Arrays/copyOfRange baos-bs 0 (int (- (alength baos-bs) diff)))
                              baos-bs)
                  written (->> bs
                               ByteBuffer/wrap
                               (channel/write-from-buffer out))
                  padded (if (pos? padding-bytes-left)
                           (->> (enc/ubyte->byte padding-byte)
                                (byte-array padding-bytes-left)
                                ByteBuffer/wrap
                                (channel/write-from-buffer out))
                           0)]
              (+ written padded)))))
      Object
      (toString [_]
        (str "<BinaryCodec padding, inner codec=" inner-codec ", options=" opts ">")))))

(defn aligned
  "This codec is related to `padding` in that it makes sure that the number of bytes
written/read to/from a stream always is aligned to a specified byte boundary.
For example, if a format requires aligning all data to 8 byte boundaries this codec
will pad the written data with `padding-byte` to make sure that the count of bytes written
is divisable by 8.
Parameters:
- `modulo`: byte boundary modulo, should be positive
- `:padding-byte` is the numeric value of the byte used for padding (default is 0)
Example:
    (encode (align (repeated :short-be :length 3) :modulo 9 :padding-byte 55) [1 2 3] output-stream)
    ;==> writes these bytes: [0 1 0 2 0 3 55 55 55]"
  [inner-codec & {:keys [^long modulo
                         padding-byte]
                  :or {padding-byte 0
                       modulo 1}
                  :as opts}]
  {:pre [(number? modulo)
         (number? padding-byte)
         (pos? modulo)
         (codec? inner-codec)]}
  (let [inner-codec (instance inner-codec)]
    (reify BinaryCodec
      (bin-read  [_ in]
        (let [[v ^long nread] (bin-read inner-codec in)
              padding-bytes-left (long (mod (- modulo ^long (mod nread modulo)) modulo))
              aligned  (if (pos? padding-bytes-left)
                         (->> (buffer/byte-buffer padding-bytes-left)
                              (channel/read-into-buffer in))
                         0)]
          [v (+ nread aligned)]))
      (bin-write [_ out v]
        (let [written (bin-write inner-codec out v)
              padding-bytes-left (long (mod (- modulo ^long (mod written modulo)) modulo))
              aligned (if (pos? padding-bytes-left)
                        (->> (enc/ubyte->byte padding-byte)
                             (byte-array padding-bytes-left)
                             ByteBuffer/wrap
                             (channel/write-from-buffer out))
                        0)]
          (+ ^long written aligned)))
      Object
      (toString [_]
        (str "<BinaryCodec aligned, options=" opts ">")))))

(defn union
  "Union is a C-style union. A fixed number of bytes may represent different values depending on the
interpretation of the bytes. The value returned by `read-data` is a map of all valid interpretations according to
the specified unioned codecs.
Parameter is the number of bytes needed for the longest codec in this union and a map of value names to codecs.
This codec will read the specified number of bytes from the input streams and then successively try to read
from this byte array using each individual codec.
Example: Four bytes may represent an integer, two shorts, four bytes, a list of bytes with prefix or a string.
    (union 4 {:integer :int-be
              :shorts (repeated :short-be :length 2)
              :bytes (repeated :byte :length 4)
              :prefixed (repeated :byte :prefix :byte)
              :str (string \"UTF8\" :prefix :byte)})"
  [bytes-length codecs-map]
  (let [codecs-map (util/map-vals instance codecs-map)]
    (-> (reify BinaryCodec
          (bin-read  [_ in]
            (let [buff (buffer/byte-buffer bytes-length)
                  n    (channel/read-into-buffer in buff)
                  ret  (util/map-vals
                         (fn [codec]
                           (->> (.duplicate buff)
                                channel/input
                                (bin-read codec)
                                first))
                         codecs-map)]
              [ret n]))
          (bin-write [_ out v]
            (let [k (some (fn [[k v]] (when v k)) v)
                  codec (codecs-map k)]
              (or (some-> codec (bin-write out (get v k)))
                  (throw (ex-info (str "No known codec for v with key " k)
                                  {:value v :unknown-key k :codecs codecs-map})))))
          Object
          (toString [_]
            (str "<BinaryCodec union of " codecs-map ">")))
        (padded :length bytes-length))))

(defn enum
  "An enumerated value. `m` must be a 1-to-1 mapping of names (e.g. keywords) to their decoded values.
Only names and values in `m` will be accepted when encoding or decoding."
  [codec m & {:keys [lenient?]}]
  (let [pre-encode  (util/strict-map m lenient?)
        post-decode (util/strict-map (set/map-invert m) lenient?)]
    (wrap codec pre-encode post-decode)))

