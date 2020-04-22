(ns bites.exchange-test
  (:require [clojure.test :refer :all]
            [bites.exchange :refer :all])
  (:import (java.util.concurrent.atomic AtomicLong)
           (java.util.concurrent ArrayBlockingQueue)))

(defn- do-exchange-test!
  [start-exchange!]
  (let [ret   (agent [])
        done? (promise)
        limit 1000
        consume! (partial send-off ret conj)
        id (AtomicLong. 0)
        produce* (partial str "Message-")
        produce! (fn [] ;; produce at random intervals
                   (Thread/sleep (rand-int 100))
                   (produce* (.incrementAndGet id)))
        [[_ _ :as ploops]
         [_ _ :as cloops]] (start-exchange! produce! consume!)]
    (add-watch ret :abort
               (fn [_ _ _ n]
                 (when (= limit (count n))
                   (map future-cancel ploops)
                   (map future-cancel cloops)
                   (deliver done? true))
                 ;(println (peek n))
                 ))
    (time
      (and @done?
           (is (<= limit (.get id)))
           (is (= @ret (mapv produce* (range 1 (inc (count @ret))))))))))

(deftest sync-exchange-test
  (do-exchange-test!
    (fn [p c]
      (->> (with-sync-exchange! 128 p c)
           (map vector)))))

(deftest async-exchange-test
  (do-exchange-test!
    (partial with-async-exchange!
             (ArrayBlockingQueue. 256 true))))