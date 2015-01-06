(ns graph.mapgraph
  (:gen-class))

(defn- append [mm k v] (assoc mm k (conj (get mm k #{}) v)))
(defn- appendMulti [mm k vs] (reduce #(append %1 k %2) mm vs))
(defn- appendBetween [mm ks] (reduce (fn [mm k] (appendMulti mm k (filter #(not (= k %)) ks))) mm ks))
(defn connect-all [graph nodes] (appendBetween graph nodes))

(defn graph[] {})

(deftype PropertyNode [properties-map]
  clojure.lang.ILookup
  (valAt [this item] (get properties-map item))
  (valAt [this item not-found] (get properties-map item not-found))
  Object
  (toString[this] (str properties-map)))

(defn property-node [properties-map]
  (PropertyNode. properties-map))
