(ns bites.cbor
  (:require [bites.array :as array]
            [bites.util :as ut]
            [clojure.string :as str])
  (:import (java.util.regex Pattern)
           (java.util UUID Set List Map)
           (clojure.lang LazySeq Symbol Keyword Ratio)
           (java.time LocalDate LocalDateTime Instant)
           (java.io ByteArrayOutputStream OutputStream InputStream File StringWriter)
           (com.fasterxml.jackson.dataformat.cbor CBORFactory CBORGenerator CBORParser)
           (com.fasterxml.jackson.core JsonFactory JsonToken)
           (java.lang.reflect Field Constructor)
           (java.net URI)
           (java.nio ByteBuffer)))

(def ^CBORFactory CBOR-factory (CBORFactory.))
(def ^JsonFactory JSON-factory (JsonFactory.))

(defn- emitter*
  ^CBORGenerator [out]
  (if (instance? CBORGenerator out)
    out
    (.createGenerator CBOR-factory ^OutputStream out)))

(defprotocol toCBOR
  (write-cbor [this out opts]))

(defprotocol fromCBOR
  (read-cbor [this opts]))

(extend-protocol toCBOR
  Boolean
  (write-cbor [this out _]
    (-> (emitter* out)
        (.writeBoolean (boolean this))))

  Integer
  (write-cbor [this out _]
    (-> (emitter* out)
      (.writeNumber (int this))))

  BigInteger
  (write-cbor [this out _]
    (let [emitter (emitter* out)]
      (.writeNumber emitter this)))

  BigDecimal
  (write-cbor [this out _]
    (-> (emitter* out)
        (.writeNumber this)))

  Long
  (write-cbor [this out _]
    (-> (emitter* out)
        (.writeNumber (long this))))

  Double
  (write-cbor [this out _]
    (-> (emitter* out)
        (.writeNumber (double this))))

  Float
  (write-cbor [this out _]
    (-> (emitter* out)
        (.writeNumber ^float this)))

  Ratio
  (write-cbor [this out _]
    (let [emitter (emitter* out)]
      (.writeTag emitter 30)
      (.writeStartArray emitter this 2)
      (.writeNumber emitter (long (numerator this)))
      (.writeNumber emitter (long (denominator this)))
      (.writeEndArray emitter)))


  String
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (case (:as opts)
        :date
        (do
          (LocalDate/parse this) ;; verify
          (.writeTag emitter 1004)
          (.writeString emitter this))
        :datetime
        (do
          (LocalDateTime/parse this) ;; verify
          (.writeTag emitter 0)
          (.writeString emitter this))
        :uuid
        (write-cbor (UUID/fromString this) emitter opts)

        (.writeString emitter this))))

  Keyword
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (.writeTag emitter 39)
      (write-cbor (str this) emitter opts)))

  Symbol
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (.writeTag emitter 39)
      (write-cbor (str this) emitter opts)))

  Map
  (write-cbor [this out opts]
    (let [emitter (emitter* out)
          kfn (:key-fn opts ut/name++)
          m (into {} (map (fn [[k v]] [(kfn k) v])) this)]
      (.writeStartObject emitter (count m))
      (run!
        (fn [[k v]]
          (if (string? k)
            (.writeFieldName emitter ^String k)
            (.writeFieldId emitter ^long k))
          (write-cbor v emitter opts))
        m)
      (.writeEndObject emitter)))
  List
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (.writeStartArray emitter this (count this))
      (run! #(write-cbor % emitter opts) this)
      (.writeEndArray emitter)))

  Set
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (.writeTag emitter 258)
      (.writeStartArray emitter this (count this))
      (run! #(write-cbor % emitter opts) this)
      (.writeEndArray emitter)))

  LazySeq
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (.writeStartArray emitter this)
      (run! #(write-cbor % emitter opts) this)
      (.writeEndArray emitter)))

  URI
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (.writeTag emitter 32)
      (write-cbor (str this) emitter opts)))

  Pattern
  (write-cbor [this out opts]
    (let [emitter (emitter* out)
          bs (.getBytes (str this))]
      (.writeTag emitter 35)
      (.writeRawUTF8String emitter bs 0 (alength bs))))

  UUID
  (write-cbor [this out _]
    (let [emitter (emitter* out)]
      (.writeTag emitter 37)
      (.writeBinary emitter (array/toBytes this nil))))

  Instant
  (write-cbor [this out opts]
    (let [emitter (emitter* out)
          epoch-millis  (.toEpochMilli this)
          epoch-seconds (/ epoch-millis 1000)]
      (.writeTag emitter 1)
      (cond-> epoch-seconds
              (ratio? epoch-seconds) double
              true (write-cbor emitter opts))))

  LocalDate
  (write-cbor [this out opts]
    (let [emitter (emitter* out)]
      (.writeTag emitter 100)
      (write-cbor (.toEpochDay this) emitter opts)))

  Object ;; for `deftype` objects
  (write-cbor [this out opts]
    (let [emitter (emitter* out)
          klass   (class this)
          params  (mapv #(.get ^Field % this) (ut/get-class-fields klass))] ;; assuming deftype
      (.writeTag emitter 27)
      (.writeStartArray emitter this 2)
      (.writeString emitter (.getName klass))
      (write-cbor params emitter opts)
      (.writeEndArray emitter)))

  nil
  (write-cbor [_ out _]
    (let [emitter (emitter* out)]
      (.writeNull emitter)))

  )

(defn- cbor->clj
  ([^CBORParser parser opts]
   (cbor->clj parser (.nextToken parser) opts))
  ([^CBORParser parser token opts]
   (letfn
     [(parse-array [^CBORParser p container]
        (loop [ret (transient container)]
          (if (identical? JsonToken/END_ARRAY (.nextToken p))
            (persistent! ret)
            (recur (conj! ret (cbor->clj p (.currentToken p) opts))))))

      (parse-map [^CBORParser p]
        (let [kfn (:key-fn opts identity)]
          (loop [ret (transient {})]
            (if (identical? JsonToken/FIELD_NAME (.nextToken p))
              (let [k (.currentName p)
                    v (cbor->clj p (.nextToken p) opts)]
                (recur (assoc! ret (kfn k) v)))
              (persistent! ret)))))

      (parse-simple [^CBORParser p]
        (case (.name (.getCurrentToken p))
          "VALUE_NUMBER_INT" (.getLongValue p)
          "VALUE_NUMBER_FLOAT" (.getDoubleValue p)
          "VALUE_STRING" (.getValueAsString p)
          "VALUE_FALSE" false
          "VALUE_TRUE" true
          "START_OBJECT" (parse-map p)
          "START_ARRAY" (parse-array p [])
          "VALUE_NULL" nil))

      (parse-per-tag [^CBORParser p ^long tag]
        (case tag
          0 (let [s (.getText p)]
              (LocalDateTime/parse s) ;; verify
              s)
          1 (let [seconds (.getNumberValue p)
                  [^long seconds fraction]
                  (if (integer? seconds)
                    [seconds 0]
                    (let [ipart (long seconds)
                          fpart (- seconds ipart)]
                      [ipart fpart]))]
              (Instant/ofEpochSecond seconds (* fraction 1000000000)))

          27 (let [[^String class-name ctor-params] (parse-array p [])]
               (if (empty? ctor-params)
                 (-> (Class/forName class-name)
                     (.getConstructor (into-array Class []))
                     (.newInstance (object-array 0)))
                 (let [ns-end (.lastIndexOf class-name ".")
                       class-simple-name (subs class-name (inc ns-end) (.length class-name))
                       package-str (subs class-name 0 ns-end)]
                   (if-some [ctor (-> package-str
                                      (str "/->" class-simple-name)
                                      clojure.main/demunge
                                      symbol
                                      requiring-resolve)]
                     ;; found deftype
                     (apply ctor ctor-params)
                     (if-some [^Constructor ctor (-> (Class/forName class-name)
                                                     (ut/find-best-ctors (map class ctor-params))
                                                     first)]
                       (.newInstance ctor (object-array ctor-params))
                       (throw
                         (IllegalStateException.
                           (str "Unable to construct generic-object: " class-name)))))))


               )

          100  (LocalDate/ofEpochDay (.getNumberValue p))
          1004 (let [s (.getText p)]
                 (LocalDate/parse s) ;; verify
                 s)

          30 (let [[numerator denominator] (parse-array p [])]
               (Ratio. (biginteger numerator)
                       (biginteger denominator)))
          32 (URI. (.getText p))
          35 (re-pattern (.getText p))
          37 (let [data (ByteBuffer/wrap (.getBinaryValue p))
                   uuid (UUID. (.getLong data) (.getLong data))]
               (cond-> uuid
                       (some-> (:as opts) (= :string))
                       str))

          39 (let [s (.getText p)]
               (if (str/starts-with? s ":")
                 (keyword (subs s 1 (.length s)))
                 (symbol s)))

          258 (parse-array p #{})
          nil))]
     (when (some? token)
       (or (parse-per-tag parser (.getCurrentTag parser))
           (parse-simple parser))))))

(defn- cbor->clj-stream
  "Returns an `eduction` representing the stream
   of available tokens from this <parser>."
  [^CBORParser parser opts]
  (->> (partial cbor->clj parser opts)
       repeatedly
       (eduction (take-while some?))))

(defn- cbor->clj*
  [^CBORParser p opts]
  (if (:stream? opts)
    (cbor->clj-stream p opts)
    (cbor->clj p opts)))


(extend-protocol fromCBOR
  (Class/forName "[B")
  (read-cbor [this opts]
    (with-open [p (.createParser CBOR-factory ^bytes this)]
      (cbor->clj* p opts)))
  InputStream
  (read-cbor [this opts]
    (with-open [p (.createParser CBOR-factory ^InputStream this)]
      (cbor->clj* p opts)))
  ByteBuffer ;; delegate to array impl
  (read-cbor [this opts]
    (read-cbor (.array this) opts))
  File
  (read-cbor [this opts]
    (with-open [p (.createParser CBOR-factory ^File this)]
      (cbor->clj* p opts)))
  )

;;--------------<PUBIC API>----------------------
(defn to-bytes
  "Returns the bytes of <x> encoded in CBOR."
  ^bytes [x opts]
  (let [out (ByteArrayOutputStream.)]
    (with-open [emitter (emitter* out)]
      (write-cbor x emitter opts)
      (.flush emitter)
      (.toByteArray out))))

(defn cbor->json
  ^String [^InputStream in]
  (let [cbor-parser (.createParser CBOR-factory in)
        wrt (StringWriter.)
        json-generator (.createGenerator JSON-factory wrt)]
    (while (some? (.nextToken cbor-parser))
      (.copyCurrentEvent json-generator cbor-parser))
    (.flush json-generator)
    (str wrt)))

(defn cbor->edn
  ^String [^InputStream in opts]
  (let [x (read-cbor in opts)]
    (binding [*print-length* nil
              *print-level* nil]
      (pr-str x))))



