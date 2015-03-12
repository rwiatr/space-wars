(ns space-wars.game-state
  (:require [clojure.tools.logging :as log]
            [geom.triangle :refer [triangle]]
            [geom.point :refer [point]]
            [geom.polygon :refer [to-bbox]]
            [graph.bowyer-watson :refer [bowyer-watson_2d]]
            [graph.converters :refer [point->triangles, point->points, point->polygon, as-graph]]
            [graph.mapgraph :refer [g-get-prop, g-nodes]]
            [spatial.spattree :refer [tree, tree-add]])
  (:gen-class))

(defn- random-points [min_ size_] (cons (point (+ (rand-int size_) min_) (+ (rand-int size_) min_)) (lazy-seq (random-points min_ size_))))


(defn- spiral-points [cx cy a b angle] (let [r (* a angle)] (cons (point (+ cx (* r (Math/cos angle))) (+ cy (* r (Math/sin angle))))
                                                                  (lazy-seq (spiral-points cx cy a b (+ (* b Math/PI) angle))))))


(defn- as-tree [graph]
  (doall (reduce (fn [rtree node]
                   (tree-add rtree {:bbox (to-bbox (g-get-prop graph node :geometry))
                                    :value node}))
                 (tree :split-size 10)
                 (g-nodes graph))))

(defn generate-level
  ([] (generate-level 100 600))
  ([min_ size_]
   (let [max_ (+ min_ size_)
         points (take 400 (spiral-points 400 400 0.5 0.32711 0))
         ;(take 1000 (random-points min_ size_))
         triangles (bowyer-watson_2d points :boundries #{(triangle min_ min_ max_ min_ max_ max_) (triangle min_ min_ min_ max_ max_ max_)})
         point->triangles-mm (point->triangles triangles)
         graph (as-graph (point->points point->triangles-mm) (point->polygon point->triangles-mm))
         rtree (as-tree graph)]
     {:graph graph
      :rtree rtree})))
