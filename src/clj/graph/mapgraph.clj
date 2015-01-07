(ns graph.mapgraph
  (:gen-class))

(defn- joinBetween [mm ks] (reduce (fn [mm k] (util.set_multimap/add mm k (filter #(not (= k %)) ks))) mm ks))
(defn connect-all [graph nodes] (joinBetween graph nodes))

(defn graph[] (util.set_multimap/multimap))

(deftype PropertyNode [properties-map]
  clojure.lang.ILookup
  (valAt [this item] (get properties-map item))
  (valAt [this item not-found] (get properties-map item not-found))
  Object
  (toString[this] (str properties-map)))

(defn property-node [properties-map]
  (PropertyNode. properties-map))
