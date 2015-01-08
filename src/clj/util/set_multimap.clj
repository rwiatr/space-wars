(ns util.set_multimap
  (:use [clojure.set :only (union)]
        [clojure.algo.generic.functor :only (fmap)])
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

(defn mm-index
  ([f vls] (mm-index (multimap) f vls))
  ([mm kf vls] (mm-index mm kf identity vls))
  ([mm kf vf vls] (apply add mm (mapcat #(list (kf %) (vf %)) vls))))

(defn- mm-reverse-row [mm row]
  (reduce #(add %1 %2 (first row)) mm (second row)))

(defn mm-reverse [mm]
  (reduce mm-reverse-row (multimap) (seq mm)))

(defn mm-fmap
  ([f mm]
   (mm-fmap f mm map))
  ([f mm mapper] ; use if f returns a collection that has to be merged with mapcat
   (fmap #(into #{} (mapper f %)) mm)))

(defn mm-to-map [f mm]
  (fmap f mm))
