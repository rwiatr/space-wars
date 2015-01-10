(ns geom.triangle
  (:require [geom.edge :refer [edge]]
            [geom.point :refer [point distance]]
            [geom.circle :refer [circle]])
  (:gen-class))

(defn circumcircle-p
  ([t] (circumcircle-p (:p1 t) (:p2 t) (:p3 t)))
  ([p1 p2 p3]
   (let [a1 (- (:x p1) (:x p2))
         b1 (- (:y p1) (:y p2))
         c1 (- (+ (* a1 (/ (+ (:x p1) (:x p2)) 2)) (* b1 (/ (+ (:y p1) (:y p2)) 2))))

         a2 (- (:x p2) (:x p3))
         b2 (- (:y p2) (:y p3))
         c2 (- (+ (* a2 (/ (+ (:x p2) (:x p3)) 2)) (* b2 (/ (+ (:y p2) (:y p3)) 2))))]
     (point (/ (- (* c2 b1) (* c1 b2)) (- (* a1 b2) (* a2 b1)))
            (/ (- (* a2 c1) (* a1 c2)) (- (* a1 b2) (* a2 b1)))))))

(defn circumcircle
  ([t] (circumcircle (:p1 t) (:p2 t) (:p3 t)))
  ([p1 p2 p3]
   (let [c (circumcircle-p p1 p2 p3)]
     (circle c (distance c p1)))))

(defn points [triangle] (list (:p1 triangle) (:p2 triangle) (:p3 triangle)))

(defrecord Triangle [p1 p2 p3 edges c])
(defn triangle
  ([p1 p2 p3]
   (let [points #{p1 p2 p3} ;; ensure unique
         edges  #{(edge p1 p2) (edge p2 p3) (edge p3 p1)}]
     (Triangle. (first points) (second points) (last points) edges (circumcircle p1 p2 p3))))

  ([p e]
   (let [points #{p (:p1 e) (:p2 e)} ;; ensure unique
         edges  #{e (edge p (:p1 e)) (edge p (:p2 e))}
         [p1 p2 p3] [(first points) (second points) (last points)]]
     (Triangle. p1 p2 p3 edges (circumcircle p1 p2 p3)))))

(defn to-circumcircle [t] (->> t :c))
(defn to-circumcircle-center [t] (->> t :c :p))
(defn to-circumcircles [ts] (map to-circumcircle ts))
(defn to-circumcircles-centers [ts] (map to-circumcircle-center ts))
