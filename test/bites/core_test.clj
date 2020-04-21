(ns bites.core-test
  (:require [clojure.test :refer :all]
            [bites.core :refer :all])
  (:import (java.util.concurrent.atomic AtomicLong)))

(deftest blocking-exchange-test
  (let [ret   (agent [])
        done? (promise)
        limit 100
        consume! (partial send-off ret conj)
        id (AtomicLong. 0)
        produce* (partial str "Message-")
        produce! (fn [] ;; produce at random intervals
                   (Thread/sleep (rand-int 500))
                   (produce* (.incrementAndGet id)))
        [prod-fut consu-fut] (with-blocking-exchange! 10 produce! consume!)]
    (add-watch ret :abort
      (fn [_ _ _ n]
        (when (= limit (count n))
          (future-cancel prod-fut)
          (future-cancel consu-fut)
          (deliver done? true))
        ;(println (peek n))
        ))
    (and @done?
         (is (= limit (.get id)))
         (is (= @ret (mapv produce* (range 1 (inc limit))))))
    )
  )
