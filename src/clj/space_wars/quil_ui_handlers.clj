(ns space-wars.quil-ui-handlers
  (require [quil.core :as q]
           [geom.bbox :refer [bbox]]
           [geom.polygon :refer [point-in-poly?]]
           [geom.point :refer [point]]
           [spatial.spattree :refer [tree-values-containing]])
  (:gen-class))

(defn build-state [map-rtree, map-graph]
  {:map-rtree map-rtree
   :map-graph map-graph})

(defn- select-map-polygon [state event]
  (if-let [map-rtree (:map-rtree state)]
    (if-let [selected (not-empty (tree-values-containing map-rtree (bbox {:x event} {:y event} 0 0)))]
      (filter (partial point-in-poly? (point {:x event} {:y event})) selected))))

(defn map-select [state event]
  (assoc state :map-selected-polygon (select-map-polygon state event)))


(defn draw-line [p1 p2]
  (q/line (:x p1) (:y p1) (:x p2) (:y p2)))

(defn draw-polygon
  ([points] (draw-polygon points (first points)))
  ([points p0])
  (loop [p1 (first points)
         p2 (second points)
         points (rest points)]))


(defn draw-map-selected [state])
  (if-let [map-selected-polygon (:map-selected-polygon state)]
    (do
      (q/stroke-weight 3)

      (q/line
      )

    state))
