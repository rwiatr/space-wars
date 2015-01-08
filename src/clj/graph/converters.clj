(ns graph.converters
  (:require [clojure.set :refer :all]
            [clojure.algo.generic.functor :refer [fmap]]
            [graph.mapgraph :refer [property-node]]
            [util.set_multimap :refer [mm-reverse mm-index mm-merge mm-to-map mm-fmap]]
            [geom.point :refer [point]]
            [geom.triangle :refer [triangle points]]
            [geom.polygon :refer [polygon]]))

(defn- point->triangles [triangles]
  (mm-merge (mm-index #(:p1 %) triangles)
            (mm-index #(:p2 %) triangles)
            (mm-index #(:p3 %) triangles)))

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

(defn- as-node [triangles]
  (->> triangles
       as-ordered-points
       polygon
       (assoc {} :geometry)
       property-node))

(defn- point->polygon [point->triangles-indx]
  (mm-to-map as-node point->triangles-indx))

(defn- point->points [point->triangles-indx]
  (mm-fmap #(points %) point->triangles-indx mapcat))

(defn- polygon->polygons [point->points-indx
                          point->polygon-indx]
   (->> point->points-indx
        (mm-fmap #(get point->polygon-indx %))
        (mm-reverse)
        (mm-fmap #(get point->polygon-indx %))))

(defn as-graph "converts triangles produced by bowyers watson algorithm into graph" [triangles]
  (let [point->triangles-indx (point->triangles triangles)
        point->points-indx (point->points point->triangles-indx)
        point->polygon-indx (point->polygon point->triangles-indx)]
    (println point->polygon-indx)
    (polygon->polygons point->points-indx point->polygon-indx)))

(def ts #{(triangle (point -100 -100) (point 100 -100) (point 0 0))
          (triangle (point -100 -100) (point -100 100) (point 0 0))
          (triangle (point 100 100) (point 100 -100) (point 0 0))
          (triangle (point 100 100) (point -100 100) (point 0 0))})

(count (as-graph ts))
