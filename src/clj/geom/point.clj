(ns geom.point
  (:gen-class))

(defn- cmp [x y] (if (< x y) -1 (if (> x y) 1 0)))
(defn- cmpp [p1 p2] (let [dx (cmp (:x p1) (:x p2))
                          dy (cmp (:y p1) (:y p2))]
                      (if (zero? dx) dy dx)))

(defrecord Point2d [x y]
  java.lang.Comparable
  (compareTo [this other] (cmpp this other)))
(defn point[x y] (Point2d. x y))

(defn distance [x y]
  (let [a (- (:x x) (:x y))
        b (- (:y x) (:y y))]
    (java.lang.Math/sqrt (+ (* a a) (* b b)))))
