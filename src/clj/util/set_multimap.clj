(ns util.set_multimap
  (:use [clojure.set :only (union)]
        [clojure.algo.generic.functor :only (fmap)])
  (:gen-class))

(defn multimap [] {})

(defmacro apply-rec2 [f x args]
   `(loop [x# ~x, args# ~args]
      (if (empty? args#) x#
        (recur (~f x# (first args#) (second args#)) (rest (rest args#))))))

(defn add
  ([mm k v] (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs]
   (apply-rec2 add (add mm k v) kvs)))

(defn addseq
  ([mm k vseq]
   (if (empty? vseq) mm
     (let [s (get mm k #{})]
       (assoc mm k (apply conj s vseq))))))

(defn del
  ([mm k] (dissoc mm k))
  ([mm k v] (let [nm (assoc mm k (disj (get mm k) v))]
              (if (empty? (get nm k))
                (del nm k)
                nm)))
  ([mm k v & kvs] (apply-rec2 del (del mm k v) kvs)))

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

(defn mm-kv-fmap
  ([f mm] (mm-kv-fmap f mm map))
  ([f mm mapper] ; use if f returns a collection that has to be merged with mapcat
   (into {} (for [[k vs] mm
                  :let [nvs (into #{} (mapper #(f k %) vs))]
                  :when (not-empty nvs)] [k nvs]))))
(defn mm-fmap
  ([f mm]
   (mm-fmap f mm map))
  ([f mm mapper] ; use if f returns a collection that has to be merged with mapcat or theres need to use filtering
   (let [nm (fmap #(into #{} (mapper f %)) mm)]
     (select-keys nm (for [[k vs] nm :when (not-empty vs)] k)))))

(defn mm-filter [f mm]
  (mm-fmap f mm filter))

(defn mm-kv-filter [f mm]
  (mm-kv-fmap f mm filter))

(defn- not-in "((not-in :a :b) :d)->true ((not-in :a :b) :a)->false"
  ([v] (fn [x] (not= v x)))
  ([v & vs] (fn [x]
              (every? true?
                      (map (not-in x) (cons v vs))))))

(defn mm-rem-val
  ([mm v] (mm-filter (not-in v) mm))
  ([mm v & vs] (mm-filter (apply not-in v vs) mm)))

(defn- mm-seq-impl [kvs k vs]
  (cond
   (not-empty vs) (cons [k (first vs)] (lazy-seq (mm-seq-impl kvs k (rest vs))))
   (not-empty kvs) (let [p (first kvs)] (recur (rest kvs) (first p) (second p)))
   :else nil))

(defn mm-seq [mm]
  (let [kvs (seq mm)]
    (if-let [p (first kvs)]
      (mm-seq-impl (rest kvs) (first p) (second p))
      '())))

(defn mm-to-map [f mm]
  (fmap f mm))

(defn mm-contains-key
  ([mm k] (contains? mm k))
  ([mm k & ks] (every? true?
                       (cons (mm-contains-key mm k)
                             (map #(mm-contains-key mm %) ks)))))

(defn mm-contains-val
  ([mm v] (not-every? false? (map #(contains? % v) (vals mm))))
  ([mm v & vs] (every? true?
                       (cons (mm-contains-val mm v)
                             (map #(mm-contains-val mm %) vs)))))

(defn mm-contains-kv
  ([mm k v] (contains? (get mm k) v))
  ([mm k v & kvs] (every? true?
                          (cons (mm-contains-kv mm k v)
                                (map (fn [[k v]] (mm-contains-kv mm k v)) (partition-all 2 kvs))))))
