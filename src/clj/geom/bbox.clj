(ns geom.circle
  (:gen-class))

(defn- abs [n] (max n (- n)))

(defrecord Bbox [x y width height])
(defn bbox [x y width height] (Bbox. x y width height))

(defn intersect? [bbox1 bbox2]
  (and
   (<= (* 2 (abs (- (:x bbox1) (:x bbox2)))) (+ (:width bbox1) (:width bbox2)))
   (<= (* 2 (abs (- (:y bbox1) (:y bbox2)))) (+ (:height bbox1) (:height bbox2)))))

(defn intersecting [bbox & other]
  (filter (partial intersect? bbox) other))
