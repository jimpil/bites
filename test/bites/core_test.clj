(ns bites.core-test
  (:require [clojure.test :refer :all]
            [bites.core :refer :all])
  (:import (java.util.concurrent.atomic AtomicLong)))

(deftest sync-exchange-test
  (let [ret   (agent [])
        done? (promise)
        limit 1000
        consume! (partial send-off ret conj)
        id (AtomicLong. 0)
        produce* (partial str "Message-")
        produce! (fn [] ;; produce at random intervals when printing
                   ;(Thread/sleep (rand-int 200))
                   (produce* (.incrementAndGet id)))
        [prod-fut consu-fut] (with-sync-exchange! 128 produce! consume!)]
    (add-watch ret :abort
      (fn [_ _ _ n]
        (when (= limit (count n))
          (future-cancel prod-fut)
          (future-cancel consu-fut)
          (deliver done? true))
        ;(println (peek n))
        ))
    (and @done?
         (is (<= limit (.get id)))
         (is (= @ret (mapv produce* (range 1 (inc (count @ret))))))
         )
    )
  )
