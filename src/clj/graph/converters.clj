(ns graph.converters
  (:require [clojure.set :refer :all]
            [graph.mapgraph :refer [node]]
            [graph.geometries :refer [triangle point]]))

(defn- point->triangles [triangles]
  (->> triangles
       (map (fn [t] {(:p1 t) (list t)
                     (:p2 t) (list t)
                     (:p3 t) (list t)}))
       (reduce #(merge-with concat %1 %2))))

(defn- neighbours [triangle triangles]
  (filter (fn [neighbour] (empty? (clojure.set/intersection (:edges triangle) (:edges neighbour)))) triangles))

(defn- neighbour-index [triangles]
  (index (fn [triangle] (neighbours triangle triangles)) triangles))


(defn- append [mm k v] (assoc mm k (conj (get mm k #{}) v)))
(defn- appendMulti [mm k vs] (reduce #(append %1 k %2) mm vs))
(defn- appendBetween [mm ks] (reduce (fn [mm k]
                                       (appendMulti mm k (filter (not (= k)) ks)))
                                     mm
                                     ks))
(defn- connect [graph nodes]
  (appendBetween graph nodes))

(defn- as-nodes [triangles & {:keys [graph connect-fn] :or [graph {} connect-fn connect]}]
  (let [indx (point->triangles triangles)]
    (->> (vals indx) ; [point (triangles)]
         (map (connect graph #(second %))))))

(defn as-graph "converts triangles produced by bowyers watson algorithm into graph" [triangles] (into #{} (as-nodes triangles)))

(def ts #{(triangle (point 0 100) (point -100 -100) (point 0 25))
          (triangle (point 100 -100) (point -100 -100) (point 48 0))
          (triangle (point 0 25) (point -100 -100) (point 48 0))
          (triangle (point 0 100) (point 100 -100) (point 48 0))
          (triangle (point 0 100) (point 0 25) (point 48 0))})

(as-nodes ts :graph {} :connect-fn connect)
