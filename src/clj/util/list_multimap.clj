(ns util.list_multimap
  (:use [clojure.set :only (union)])
  (:gen-class))

(defn add
  ([mm k v]
   (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs]
   (apply add (add mm k v) kvs)))

(defn del
  ([mm k v]
   (assoc mm k (disj (get mm k) v)))
  ([mm k v & kvs]
   (apply del (del mm k v) kvs)))

(defn mm-merge
  [& mms]
  (apply (partial merge-with union) mms))
