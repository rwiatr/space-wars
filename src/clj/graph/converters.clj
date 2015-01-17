(ns graph.converters
  (:require [clojure.set :refer :all]
            [clojure.algo.generic.functor :refer [fmap]]
            [graph.mapgraph :refer [property-node]]
            [util.set_multimap :refer [mm-reverse mm-index mm-merge mm-to-map mm-fmap mm-kv-fmap mm-filter del mm-kv-filter]]
            [geom.point :refer [point]]
            [geom.triangle :refer [triangle points to-circumcircle-center]]
            [geom.polygon :refer [polygon]]))

(defn- neighbours? [t1 t2]
  (empty? (clojure.set/intersection (:edges t1) (:edges t2))))

(defn- neighbours [triangle triangles]
  (filter (partial neighbours? triangle) triangles))

(defn- as-ordered-points [triangles]
  (loop [triangles triangles
         points ()]
    (if (empty? triangles) points
      (let [triangle (first triangles)]
        (if-let [other (neighbours triangle triangles)]
          (recur (rest triangles)
                 (cons (to-circumcircle-center triangle) points))
          points)))))

(defn- as-node [triangles]
  (->> triangles
       as-ordered-points
       polygon
       (assoc {} :geometry)))

(defn point->triangles [triangles]
  (mm-merge (mm-index #(:p1 %) triangles)
            (mm-index #(:p2 %) triangles)
            (mm-index #(:p3 %) triangles)))

(defn point->polygon [point->triangles-indx]
  (mm-to-map as-node point->triangles-indx))

(defn point->points [point->triangles-indx]
  (mm-kv-fmap #(filter (partial not= %1) (points %2)) point->triangles-indx mapcat))

(defn polygon->polygons [point->points-indx
                         point->polygon-indx]
  (->> point->points-indx
       (mm-fmap #(get point->polygon-indx %))
       (mm-reverse)
       (mm-fmap #(get point->polygon-indx %))
       (mm-filter some?)))

(defn voronoi-polygons->neighbours
  "transforms bowyer-watson triangles into voronoi diagram polygons mapped into their neighbours"
  [bowyer-watson-triangles]
  (mm-kv-filter not=
                (let [point->triangles-indx (point->triangles bowyer-watson-triangles)
                      point->points-indx (point->points point->triangles-indx)
                      point->polygon-indx (point->polygon point->triangles-indx)]
                  (polygon->polygons point->points-indx point->polygon-indx))))
