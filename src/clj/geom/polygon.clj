(ns geom.polygon
  (:require [geom.bbox :refer [bbox-xy]])
  (:gen-class))

(defrecord Polygon [points])
(defn polygon [points]
  ;; TODO enable and fix defects in code
  ;(if (> 3 (count points)) (throw (Exception. (str "Polygon can't be build from less then three points. Input=[" points "]"))))
  (Polygon. points))

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

(defn polygon->point-pairs
  ([{points :points}] (polygon->point-pairs points (first points)))
  ([points p0]
   (if-let [p1 (first points)]
     (if-let [p2 (second points)]
       (cons [p1 p2] (lazy-seq (polygon->point-pairs (rest points) p0)))
       (list [p1 p0])))))

(defn- ray-intersect? [p b1 b2]
  (if
    (and (or (<= (:x b1) (:x p)) (<= (:x b2) (:x p)))
         (or (and (< (:y b1) (:y p)) (>= (:y b2) (:y p)))
             (and (< (:y b2) (:y p)) (>= (:y b1) (:y p)))))
    (< (+ (:x b1)
          (* (/ (- (:y p) (:y b1))
                (- (:y b2) (:y b1)))
             (- (:x b2) (:x b1)))) (:x p))))

(defn point-in-poly? [point poly]
  (let [poly-bbox (to-bbox poly)]
    (odd? (reduce + (for [[a b] (polygon->point-pairs poly) :when (ray-intersect? point a b)] 1)))))
