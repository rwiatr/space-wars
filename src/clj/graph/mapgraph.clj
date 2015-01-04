(ns graph.mapgraph
  (:gen-class))

(defprotocol IGraph
  (neighbours [this node])
  (nodes [this])
  (connectBetween [this nodes])
  (connectToMultiple [this n1 nodes])
  (connect [this n1 n2]))

(defn- append [mm k v] (assoc mm k (conj (get mm k #{}) v)))
(defn- appendMulti [mm k vs] (reduce #(append %1 k %2) mm vs))
(defn- appendBetween [mm ks] (reduce (fn [mm k vs]
                                        (appendMulti k vs (filter (not (= k)) ks))
                                        ks)))

(deftype MapGraph [node-multimap]

  IGraph
  (neighbours [this node] (get node-multimap node))
  (nodes [this] (keys node-multimap))
  (connectBetween [this nodes] (MapGraph. (appendBetween node-multimap nodes)))
  (connectToMultiple [this n1 nodes] (MapGraph. (appendMulti node-multimap n1 nodes)))
  (connect [this n1 n2] (MapGraph. (append node-multimap n1 n2)))

  clojure.lang.ILookup
  (valAt [this item] (get node-multimap item))
  (valAt [this item not-found] (get node-multimap item not-found)))

(defn- mapgraph []
  (MapGraph. {}))

(defn graph [] (mapgraph))

(deftype PropertyNode [properties-map]
  clojure.lang.ILookup
  (valAt [this item] (get properties-map item))
  (valAt [this item not-found] (get properties-map item not-found)))

(defn- property-node [properties-map]
  (PropertyNode. properties-map))

(defn node [& _]
  (property-node {}))
