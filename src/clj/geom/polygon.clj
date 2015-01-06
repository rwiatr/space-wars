(ns geom.polygon
  (:gen-class))

(defrecord Polygon [points])
(defn polygon [points] (Polygon. points))
