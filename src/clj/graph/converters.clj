(ns graph.converters
  (:require [clojure.set :refer :all]
            [graph.mapgraph :refer [property-node]]
            [graph.geometries :refer [triangle point polygon]]))

(defn- point->triangles [triangles]
  (->> triangles
       (map (fn [t] {(:p1 t) (list t)
                     (:p2 t) (list t)
                     (:p3 t) (list t)}))
       (reduce #(merge-with concat %1 %2))))

(defn- point->points [point->triangles-indx]
  (into {} (for [[k v] m] [k (fn [triangles] (into #{}))])))

(defn- neighbours [triangle triangles]
  (filter (fn [neighbour] (empty? (clojure.set/intersection (:edges triangle) (:edges neighbour)))) triangles))

(defn- neighbour-index [triangles]
  (index (fn [triangle] (neighbours triangle triangles)) triangles))

(defn- as-ordered-points [triangles]
  (loop [triangles triangles
         points ()]
    (if (empty? triangles) points
      (let [triangle (first triangles)]
        (if-let [other (neighbours triangle triangles)]
          (recur (rest triangles)
                 (cons (->> triangle :c :p) points))
          points)))))

(defn- an-node [triangles]
  (->> triangles
       as-ordered-points
       polygon
       #(assoc {} :geometry %)
       property-node))

(defn- as-nodes [triangles & {:keys [graph connect-fn] :or {graph {} connect-fn graph.mapgraph/connect-all}}]
  (let [indx (point->triangles triangles)]
    (->> (vals indx) ; [point (triangles)]
         (map as-node))))

(defn as-graph "converts triangles produced by bowyers watson algorithm into graph" [triangles] (into #{} (as-nodes triangles)))

(def ts #{(triangle (point 0 100) (point -100 -100) (point 0 25))
          (triangle (point 100 -100) (point -100 -100) (point 48 0))
          (triangle (point 0 25) (point -100 -100) (point 48 0))
          (triangle (point 0 100) (point 100 -100) (point 48 0))
          (triangle (point 0 100) (point 0 25) (point 48 0))})
