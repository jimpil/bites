(ns bites.idz.nanoid
  (:require [bites.random :as random]
            [bites.util :as util]))

(defonce DEFAULT-ALPHABET
  (.toCharArray
    "_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def ^:const DEFAULT-MASK 63) ;; 0x3f
(def ^:const DEFAULT-SIZE 21)

(defn- with-mask*
  [^bytes bs ^long mask ^long i]
  (bit-and (aget bs i) mask))

(defn new-id
  "Generates a new nano-id of the specified <size> (21 by default).
   The 4-arg arity allows for custom alphabet, mask, and random-bytes
   (e.g. a seeded generator can be used instead)."
  (^String []
   (new-id DEFAULT-SIZE))
  (^String [size]
   (->> (random/secure-bytes size)
        (new-id DEFAULT-ALPHABET DEFAULT-MASK)))
  (^String
   [^chars alphabet ^long mask ^bytes random-bytes]
   (let [size (alength random-bytes)]
     (transduce
       (comp
         (map (partial with-mask* random-bytes mask))
         (map (partial aget alphabet)))
       util/str!
       (StringBuilder. size)
       (range size)))))

(defn generator*
  "Returns a no-arg fn that will generate nano-ids
   given the provided params."
  [{:keys [alphabet mask size random-bytes!]
    :or {alphabet DEFAULT-ALPHABET
         mask     DEFAULT-MASK
         size     DEFAULT-SIZE}}]
  {:pre [(fn? random-bytes!)
         (>= 256 (alength ^chars alphabet))]} ;; https://github.com/ai/nanoid#custom-alphabet-or-size
  #(->> (random-bytes! size)
        (new-id alphabet mask)))

(defmulti generator (fn [random opts] random))
(defmethod generator :default [_ opts] (generator :secure opts))
(defmethod generator :secure
  [_ opts]
  (->> random/secure-bytes
       (assoc opts :random-bytes!)
       generator*))

(defmethod generator :thread-local
  [_ opts]
  (->> random/thread-local-bytes
       (assoc opts :random-bytes!)
       generator*))

(defmethod generator :custom ;; pass-through
  [_ opts]
  (generator* opts))

(comment

  (def gen-fast!   (generator :thread-local {:size 18}))
  (gen-fast!)
  (def gen-secure! (generator :secure {:size 25}))
  (gen-secure!)

  )

