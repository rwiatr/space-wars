(ns geom.polygon
  (:require [geom.bbox :refer [bbox-xy]])
  (:gen-class))

(defrecord Polygon [points])
(defn polygon [points] (Polygon. points))

(defn to-bbox [polygon]
  (loop [points (:points polygon)
         max-x nil
         max-y nil
         min-x nil
         min-y nil]
    (if-let [point (first points)]
      (recur (next points)
             (max (or max-x (:x point)) (:x point))
             (max (or max-y (:y point)) (:y point))
             (min (or min-x (:x point)) (:x point))
             (min (or min-y (:y point)) (:y point)))
      (if (and max-x max-y min-x min-y)
        (bbox-xy min-x min-y max-x max-y)))))
