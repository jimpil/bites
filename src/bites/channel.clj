(ns bites.channel
  (:require [clojure.java.io :as io]
            [bites.buffer :as buffer]
            [bites.encoding :as enc])
  (:import (java.nio.channels Channels WritableByteChannel ReadableByteChannel)
           (java.nio ByteBuffer)
           (java.io OutputStream InputStream ByteArrayOutputStream ByteArrayInputStream)))

(defprotocol BinaryInput  (bin-input  [in]))
(defprotocol BinaryOutput (bin-output [out]))

(extend-protocol BinaryInput
  (Class/forName "[B")
  (bin-input [this] (bin-input (ByteArrayInputStream. this)))
  ByteBuffer
  (bin-input [this] (bin-input (.array this)))
  ReadableByteChannel
  (bin-input [this] this)
  InputStream
  (bin-input [this] (Channels/newChannel this))
  Object
  (bin-input [this] (Channels/newChannel (io/input-stream this))))

(extend-protocol BinaryOutput
  Long
  (bin-output [this] (bin-output (ByteArrayOutputStream. this)))
  WritableByteChannel
  (bin-output [this] this)
  OutputStream
  (bin-output [this] (Channels/newChannel this))
  Object
  (bin-output [this] (Channels/newChannel (io/output-stream this))))

(defn input  ^ReadableByteChannel [in]  (bin-input in))
(defn output ^WritableByteChannel [out] (bin-output out))

(defn read-into-buffer
  ^long [^ReadableByteChannel in ^ByteBuffer buff]
  (.read in buff))

(defn write-from-buffer
  ^long [^WritableByteChannel ch ^ByteBuffer buff]
  (.write ch buff))

(defn read-byte [in]
  (let [buff (buffer/byte-buffer 1)
        _ (read-into-buffer in buff)]
    (aget (.array buff) 0)))

(defn write-byte [out b]
  (->> b
       enc/ubyte->byte
       (byte-array 1)
       ByteBuffer/wrap
       (write-from-buffer out)))
