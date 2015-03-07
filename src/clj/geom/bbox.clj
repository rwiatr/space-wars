(ns geom.bbox
  (:gen-class))

(defn- abs [n] (max n (- n)))

(defrecord Bbox [x y width height])
(defn bbox [x y width height] (Bbox. x y width height))
(defn bbox-xy [x1 y1 x2 y2]
  (let [x (min x1 x2)
        y (min y1 y2)
        width (- (max x1 x2) x)
        height (- (max y1 y2) y)]
    (bbox x y width height)))
(defn bbox-max [bbox1 bbox2]
  (bbox-xy (min (:x bbox1) (:x bbox2))
           (min (:y bbox1) (:y bbox2))
           (max (+ (:x bbox1) (:width bbox1)) (+ (:x bbox2) (:width bbox2)))
           (max (+ (:y bbox1) (:height bbox1)) (+ (:y bbox2) (:height bbox2)))))

(defn area [bbox] (* (:width bbox) (:height bbox)))

(defn intersect?
  "returns true if bbox1 has a common part with bbox2 - this includes contained?"
  [bbox1 bbox2]
  (and
   (<= (* 2 (abs (- (:x bbox1) (:x bbox2)))) (+ (:width bbox1) (:width bbox2)))
   (<= (* 2 (abs (- (:y bbox1) (:y bbox2)))) (+ (:height bbox1) (:height bbox2)))))

(defn contained? [bbox1 bbox2]
  (and (<= (:x bbox1) (:x bbox2))
       (<= (:y bbox1) (:y bbox2))
       (>= (+ (:width bbox1) (:x bbox1)) (+ (:width bbox2) (:x bbox2)))
       (>= (+ (:height bbox1) (:y bbox1)) (+ (:height bbox2) (:y bbox2)))))

(defn intersecting [bbox & other]
  (filter (partial intersect? bbox) other))
