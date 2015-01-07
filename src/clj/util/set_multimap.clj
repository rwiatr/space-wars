(ns util.set_multimap
  (:use [clojure.set :only (union)])
  (:gen-class))

(defn multimap [] {})

(defn add
  ([mm k v] (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs] (apply add (add mm k v) kvs)))

(defn addseq
  ([mm k vseq]
   (let [s (get mm k #{})]
     (assoc mm k (apply conj s vseq)))))

(defn del
  ([mm k] (dissoc mm k))
  ([mm k v] (let [nm (assoc mm k (disj (get mm k) v))]
              (if (empty? (get nm k))
                (del nm k)
                nm)))
  ([mm k v & kvs] (apply del (del mm k v) kvs)))

(defn mm-merge [& mms]
  (apply (partial merge-with union) mms))
