(ns bites.exchange
  (:require [bites.util :as ut])
  (:import (java.util.concurrent Exchanger TimeUnit)))

(defn exchange!
  ([^Exchanger e x]
   (.exchange e x))
  ([^Exchanger e x timeout-ms]
   (.exchange e x timeout-ms TimeUnit/MILLISECONDS)))


(defn- start-consuming-with
  [consume! ^long buffer ^Exchanger exch]
  (loop [idx 0
         buf (object-array buffer)]
    (when-not (ut/current-thread-interrupted?)
      (if (zero? idx)
        (recur buffer (exchange! exch buf))
        (do (consume! (aget buf (- buffer idx)))
            (recur (unchecked-dec idx) buf))))))

(defn- start-producing-with
  [produce! ^long buffer ^Exchanger exch]
  (loop [idx 0
         buf (object-array buffer)]
    (when-not (ut/current-thread-interrupted?)
      (if (= buffer idx)
        (recur 0 (exchange! exch buf))
        (do (aset buf idx (produce!))
            (recur (unchecked-inc idx) buf))))))

(defn with-sync-exchange!
  "Given a producing-fn (no-args), and a consuming-fn (1-arg),
   assign each its own thread and object-array of size <buffer>,
   and has them exchanging arrays when they're BOTH ready
   (i.e. their respective operation exhausted the array it was working with).
   Returns a vector of two futures (representing the producing/consuming loops).
   Cancelling either stops its internal loop. A slow consumer will (eventually)
   block a faster producer, so a non-blocking consuming-fn would be ideal.
   Unlike using a typical data-structure, this kind of data-exchange between two
   threads is allocation-free (but synchronous). See `java.util.concurrent.Exchanger`
   for the core idea, and the underlying construct this is implemented on top of."
  ([buffer produce! consume!]
   (assert (pos-int? buffer) "buffer must be a positive integer")
   (let [exchanger (Exchanger.)]
     [(future (start-producing-with produce! buffer exchanger))
      (future (start-consuming-with consume! buffer exchanger))])))

