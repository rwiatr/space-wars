(ns graph.converters
  (:require [clojure.set :refer :all]
            [clojure.algo.generic.functor :refer [fmap]]
            [util.set_multimap :refer [mm-reverse mm-index mm-merge mm-to-map mm-fmap mm-kv-fmap mm-filter del mm-kv-filter]]
            [geom.point :refer [point]]
            [geom.triangle :refer [triangle points to-circumcircle-center]]
            [geom.polygon :refer [polygon]]
            [graph.mapgraph :refer [graph g-connect-to-many g-add-prop]]))

(defn neighbours? [t1 t2]
  (not-empty (clojure.set/intersection (:edges t1) (:edges t2))))

(defn- neighbours [triangle triangles]
  (filter (partial neighbours? triangle) triangles))

(defn ordered-circumcircle-centers [triangles]
  (loop [current (first triangles)
         candidates (rest triangles)
         points (list (to-circumcircle-center current))]
    (if-let [neighbour (first (neighbours current candidates))]
      (recur neighbour
             (filter (partial not= neighbour) candidates)
             (cons (to-circumcircle-center neighbour) points))
      points)))

(defn- as-node [triangles]
  (->> triangles
       ordered-circumcircle-centers
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

(defn- key-mem [key-fn]
  (let [ks (atom {})]
    (fn [v]
      (swap! ks #(if (% v) % (assoc % v (key-fn v))))
      (@ks v))))

(defn- inc-key [prefix]
  (let [cnt (atom 0)]
    (fn [v]
      (swap! cnt inc)
      (keyword (str prefix "-" @cnt)))))

(defn as-graph
  ([point->point-mm point->polygon-m] (as-graph (key-mem (inc-key "node")) point->point-mm point->polygon-m))
  ([node-fn point->point-mm point->polygon-m]
   (let [g (atom (graph))
         pts (keys point->point-mm)
         point->node-m (into {} (map #(vector % (node-fn %)) pts))]
     (doseq [p pts] (do (let [n (point->node-m p)
                              nds (map point->node-m (point->point-mm p))
                              gm (:geometry (point->polygon-m p))]
                          (swap! g g-connect-to-many n nds)
                          (swap! g g-add-prop n :geometry gm))))
     @g)))


(defn voronoi-polygons->neighbours
  "transforms bowyer-watson triangles into voronoi diagram polygons mapped into their neighbours"
  [bowyer-watson-triangles]
  (mm-kv-filter not=
                (let [point->triangles-indx (point->triangles bowyer-watson-triangles)
                      point->points-indx (point->points point->triangles-indx)
                      point->polygon-indx (point->polygon point->triangles-indx)]
                  (polygon->polygons point->points-indx point->polygon-indx))))
