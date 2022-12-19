(ns bites.exchange
  (:require [bites.util :as ut]
            [bites.constants :as constants])
  (:import (java.util.concurrent Exchanger TimeUnit ArrayBlockingQueue BlockingQueue)))
;; ======================<SYNCHRONOUS & BIDIRECTIONAL>========================
(defn exchange!
  ([^Exchanger e x]
   (.exchange e x))
  ([^Exchanger e x timeout-ms]
   (.exchange e x timeout-ms TimeUnit/MILLISECONDS)))


(defn- start-consuming-with
  [consume! ^long buffer ^Exchanger exch]
  (loop [idx 0
         buf  (object-array buffer)]
    (when-not (ut/current-thread-interrupted?)
      (if (zero? idx)
        ;; the very first time round there won't be any data
        ;; so wait for the producing side to become ready to exchange
        (recur buffer (exchange! exch buf))
        (let [idx* (- buffer idx)
              v (aget buf idx*)]
          ;(println "Consuming value:" v)
          (aset buf idx* (consume! v))
          (recur (unchecked-dec idx) buf))))))

(defn- start-producing-with
  ([produce! ^long buffer ^Exchanger exch]
   (start-producing-with produce! buffer ::prod-init exch))
  ([produce! ^long buffer init ^Exchanger exch]
   (loop [idx 0
          buf (->> init (repeat buffer) object-array)]
     (when-not (ut/current-thread-interrupted?)
       (if (== buffer idx)
         (recur 0 (exchange! exch buf))
         ;; `produce!` must support 2 arities:
         ;; 1. no-args for the first time round
         ;; 2. one-arg for all the rest rounds
         ;; Note: it is conceivable that ::init
         ;; will appear again (after the first round)
         ;; as a result of the consumer putting it there
         ;; This is of course extremely unlikely,
         ;; unless of the consumer does it on purpose
         ;; (e.g. wants to ask the producer to 're-seed' itself?)
         (let [v (aget buf idx)]
           ;(println "Producing with value:" v)
           (->> (if (= v init) (produce!) (produce! v))
                (aset buf idx))
           (recur (unchecked-inc idx) buf)))))))

(defn with-sync-exchange!
  "Given a producing-fn (0/1-arg), and a consuming-fn (1-arg),
   assign each its own thread and object-array of size <buffer>,
   and has them exchanging arrays when they're BOTH ready
   (i.e. their respective operation exhausted the array it was working with).
   During the initial producing, the producing-fn must be able to deal with nils.
   Returns a vector of two futures (representing the producing/consuming loops).
   Cancelling either stops its internal loop. A slow consumer will (eventually)
   block a faster producer, so a non-blocking consuming-fn would be ideal.
   Unlike using a typical data-structure, this kind of data-exchange between two
   threads is allocation-free (but synchronous). See `java.util.concurrent.Exchanger`
   for the core idea, and the underlying construct this is implemented on top of."
  ([produce! consume!]
   (with-sync-exchange! 512 produce! consume!))
  ([buffer produce! consume!]
   (assert (pos-int? buffer) "buffer must be a positive integer")
   (let [exchanger (Exchanger.)]
     [(future (start-producing-with produce! buffer exchanger))
      (future (start-consuming-with consume! buffer exchanger))])))


;; ======================<ASYNCHRONOUS & 1-DIRECTIONAL>========================

(defn start-producing-into
  "An endless (unless thread interrupted)
   producing loop for this given queue.
   `produce!` must be a no-arg fn."
  [^BlockingQueue q produce!]
  (while (not (ut/current-thread-interrupted?))
    (.put q (produce!))))

(defn start-consuming-from
  "An endless (unless thread interrupted)
   consuming loop for this given queue.
   `consume!` must be a 1-arg fn."
  [^BlockingQueue q consume!]
  (while (not (ut/current-thread-interrupted?))
    (consume! (.take q))))

(defn with-blocking-queue!
  "The opposite of `with-sync-exchange` in terms of semantics
  (i.e. synchronous/bi-directional VS asynchronous/one-directional).
  The benefit here is that multiple producers/consumers are supported.
  Default queue is a fair `ArrayBlockingQueue` with capacity 1024,
  which should exhibit very similar memory allocation characteristics
  with a `with-sync-exchange` call (with default buffer).
  Returns a vector of two seqs (the producing/consuming futures in
  the same order as the provided producing/consuming fns)."
  ([produce! consume!]
   (let [Q (ArrayBlockingQueue. constants/DEFAULT_BUFFER_SIZE true)]
     (with-blocking-queue! Q produce! consume!)))
  ([^BlockingQueue buffer produce! consume!]
   (let [produce-with (partial start-producing-into buffer)
         consume-with (partial start-consuming-from buffer)]

     [(if (sequential? produce!) ;; n producers
        (let [[produce! n] produce!]
          (->> #(future (produce-with produce!))
               (repeatedly n)
               doall))
        [(future (produce-with produce!))])

      (if (sequential? consume!)
        (let [[consume! n] consume!] ;; n consumers
          (->> #(future (consume-with consume!))
               (repeatedly n)
               doall))
        [(future (consume-with consume!))])])))

