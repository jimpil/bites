(ns bites.random
  (:require [bites.util :as util])
  (:import (java.security SecureRandom)
           (java.util.concurrent ThreadLocalRandom)))

(defonce SECURE-RANDOM (delay (SecureRandom.)))

(defn secure-bytes
  ^bytes [^long n]
  (let [bs (byte-array n)
        ^SecureRandom sr @SECURE-RANDOM]
    (.nextBytes sr bs)
    bs))

(defn thread-local-bytes
  ^bytes [^long n]
  (let [bs (byte-array n)]
    (-> (ThreadLocalRandom/current)
        (.nextBytes bs))
    bs))

(defn bits
  [^long n]
  (let [^SecureRandom rnd @SECURE-RANDOM
        random-num (BigInteger. n rnd)]
    (cond->> (.toString random-num 2)
             (> n (.bitLength random-num))
             (util/pad-bits n))))
