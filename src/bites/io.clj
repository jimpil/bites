(ns
  ^{:author "Dimitrios Piliouras"
    :doc "This file extends the polymorphic I/O utility functions already present in Clojure to ByteBuffer/ReadableByteChannel/WritableByteChannel."}
  bites.io
  (:require [clojure.java.io :as io]
            [bites.protocols :as proto]
            [bites.constants :as constants])
  (:import (java.nio ByteBuffer CharBuffer)
           (java.nio.channels ReadableByteChannel Channels WritableByteChannel FileChannel Pipe)
           (java.io InputStream OutputStream Writer File FileOutputStream FileInputStream Reader ByteArrayOutputStream)
           (java.nio.charset Charset CharsetEncoder)
           (java.util Arrays)))

(set! *warn-on-reflection* true)

(defn- charset-encoder [opts]
  (-> opts
      (:encoding constants/DEFAULT_CHARSET)
      (Charset/forName)
      .newEncoder))

(defmulti do-copy ;; io/do-copy is private :(
  (fn [input output opts]
    [(type input) 
     (type output)]))

(defmethod do-copy :default 
  [in out opts]
  ;; delegate to io/copy for anything that doesn't match
  (apply io/copy in out opts))

(defn copy
  "Copies input to output.  Returns nil or throws IOException.
   Input may be an InputStream, Reader, File, byte[], char[], String, ByteBuffer, or ReadableByteChannel.
   Output may be an OutputStream, Writer, File, or WritableByteChannel.

   Options are key/value pairs and may be one of

     :buffer-size  buffer size to use, default is 1024.
     :encoding     encoding to use if converting between
                   byte and char streams.

   Does not close any streams except those it opens itself
   (on a File)."
  [input output opts]
  (do-copy input output opts))

(extend-protocol io/IOFactory

  ByteBuffer
  (make-reader [this opts]
    (-> (proto/toBytes this nil)
        (io/make-reader opts)))
  (make-writer [this opts]
    (-> (io/make-output-stream this nil)
        (io/make-writer opts)))
  (make-input-stream [this opts]
    (-> (proto/toBytes this nil)
        (io/make-input-stream opts)))
  (make-output-stream [this _]
    (let [bout    (ByteArrayOutputStream.)
          channel (Channels/newChannel bout)]
      (.write channel this)
      bout))

  ReadableByteChannel
  (make-reader [this opts]
    (Channels/newReader this ^String (:encoding opts constants/DEFAULT_CHARSET)))
  (make-input-stream [this opts]
    (Channels/newInputStream this))

  WritableByteChannel
  (make-writer [this opts]
    (Channels/newWriter this ^String (:encoding opts constants/DEFAULT_CHARSET)))
  (make-output-stream [this opts]
    (Channels/newOutputStream this))

  )

;; ByteBuffer => OutputStream, Writer, File, WritableByteChannel
(defmethod do-copy [ByteBuffer OutputStream]
  [^ByteBuffer in out opts]
  (-> (proto/toBytes in nil)
      (do-copy out opts)))

(defmethod do-copy [ByteBuffer Writer]
  [^ByteBuffer in out opts]
  (-> (proto/toBytes in nil)
      (do-copy out opts)))

(defmethod do-copy [ByteBuffer File]
  [^ByteBuffer in out opts]
  (-> (proto/toBytes in nil)
      (do-copy out opts)))

(defmethod do-copy [ByteBuffer WritableByteChannel]
  [^ByteBuffer in ^WritableByteChannel out _]
  (loop [written 0]
    (if (.hasRemaining in)
      (recur (unchecked-add written (.write out in)))
      written)))

(defmethod do-copy [String WritableByteChannel]
  [^String in ^WritableByteChannel out opts]
  (-> (proto/toBytes in opts)
      (ByteBuffer/wrap)
      (do-copy out opts)))

(defmethod do-copy [constants/CHAR-ARRAY-TYPE WritableByteChannel]
  [^chars in ^WritableByteChannel out opts]
  (let [^CharsetEncoder enc (or (:encoder opts)
                                (charset-encoder opts))]
    (-> enc
        (.encode (CharBuffer/wrap in))
        (do-copy out opts))))

(defmethod do-copy [Reader WritableByteChannel]
  [^Reader in ^WritableByteChannel out opts]
  (let [buf-size (:buffer-size opts constants/DEFAULT_BUFFER_SIZE)
        encoder (charset-encoder opts)
        opts (assoc opts :encoder encoder)
        buffer (char-array buf-size)]
    (loop [offset 0]
      (let [nread (.read in buffer offset buf-size)]
        ;(println "Read:" nread)
        (when-not (neg? nread)
          (let [written (cond-> buffer
                                (> buf-size nread)
                                (Arrays/copyOfRange 0 nread)
                                true (do-copy out opts))
                leftover (long (- nread written))]
            ;(println "Written:" written)
            (if (pos? leftover)
              (do ;;compact manually
                (System/arraycopy buffer 0 buffer written nread)
                (recur leftover))
              (recur 0))))))))

(defmethod do-copy [constants/BYTE-ARRAY-TYPE WritableByteChannel]
  [in ^WritableByteChannel out opts]
  (-> (ByteBuffer/wrap in)
      (do-copy out opts)))

;; ReadableByteChannel => OutputStream and the opposite
;; InputStream         => WritableByteChannel

(defmethod do-copy [ReadableByteChannel ByteBuffer]
  [^ReadableByteChannel in ^ByteBuffer out opts]
  (.read in out))

(defmethod do-copy [ReadableByteChannel OutputStream]
  [^ReadableByteChannel in ^OutputStream out opts]
  (do-copy in (Channels/newChannel out) opts))

(defmethod do-copy [InputStream WritableByteChannel]
  [^InputStream in ^WritableByteChannel out opts]
  (do-copy (Channels/newChannel in) out opts))

;; ReadableByteChannel =>  Writer (via InputStream)
(defmethod do-copy [ReadableByteChannel Writer]
  [^ReadableByteChannel in ^Writer out opts]
  (-> (io/make-input-stream in opts)
      (do-copy out opts)))

;; Optimised (depending on the OS) copying to and from file
;; [File File] is already covered and does use NIO
(defmethod do-copy [ReadableByteChannel File]
  [^ReadableByteChannel in ^File out _]
  (with-open [^FileChannel ch (Channels/newChannel
                                (FileOutputStream. out))]
    (.transferFrom ch in 0 Long/MAX_VALUE)
    nil))

(defmethod do-copy [File WritableByteChannel]
  [^File in ^WritableByteChannel out _]
  (with-open [^FileChannel ch (Channels/newChannel
                                (FileInputStream. in))]
    (.transferTo ch 0 Long/MAX_VALUE out)
    nil))

;; ReadableByteChannel =>  WritableByteChannel (base case)
(defmethod do-copy [ReadableByteChannel WritableByteChannel]
  [^ReadableByteChannel in ^WritableByteChannel out opts]
  (let [buffer (ByteBuffer/allocate
                 (:buffer-size opts constants/DEFAULT_BUFFER_SIZE))]
    ;; per the Java docs
    (while (not (neg? (.read in buffer)))
      (.flip buffer)
      (.write out buffer)
      (.compact buffer))))

(defn into-pipe!
  "Writes <x> into this Pipe's sink-channel.
   Returns nil."
  ([pipe x]
   (into-pipe! pipe x nil))
  ([^Pipe pipe x opts]
   (copy x (.sink pipe) opts)))

(defn from-pipe!
  "Reads <n> bytes from this Pipe's source-channel.
   Returns byte-array."
  ^bytes [^Pipe pipe n]
  (let [buf (ByteBuffer/allocate n)]
    (copy (.source pipe) buf nil)
    (.array buf)))
