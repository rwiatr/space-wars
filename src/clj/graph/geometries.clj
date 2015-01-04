(ns graph.geometries
  (:gen-class))

(defn distance [x y]
  (let [a (- (:x x) (:x y))
        b (- (:y x) (:y y))]
    (java.lang.Math/sqrt (+ (* a a) (* b b)))))

(defn- cmp [x y] (if (< x y) -1 (if (> x y) 1 0)))
(defn- cmpp [p1 p2] (let [dx (cmp (:x p1) (:x p2))
                          dy (cmp (:y p1) (:y p2))]
                      (if (zero? dx) dy dx)))
(defrecord Point2d [x y]
  java.lang.Comparable
  (compareTo [this other] (cmpp this other)))
(defn point[x y] (Point2d. x y))

(defrecord Edge[p1 p2])
(defn edge [p1 p2]
  (let [comparision (.compareTo p1 p2)]
    (if (zero? comparision)
      nil
      (if (> 0 (.compareTo p1 p2))
        (Edge. p1 p2)
        (Edge. p2 p1)))))

(defrecord Circumcircle [p r])
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
     (Circumcircle. c (distance c p1)))))

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
