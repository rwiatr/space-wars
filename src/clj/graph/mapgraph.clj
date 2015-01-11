(ns graph.mapgraph
  (:use util.set_multimap)
  (:gen-class))

(defn connect
  ([graph n1 n2] (util.set_multimap/add graph n1 n2))
  ([graph n1 n2 & np] (apply connect (connect graph n1 n2) np)))

(defn graph[] (util.set_multimap/multimap))

(deftype PropertyNode [properties-map]
  clojure.lang.ILookup
  (valAt [this item] (get properties-map item))
  (valAt [this item not-found] (get properties-map item not-found))
  Object
  (toString[this] (str properties-map)))

(defn property-node [properties-map]
  (PropertyNode. properties-map))
