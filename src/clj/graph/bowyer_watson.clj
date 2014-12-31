(ns graph.bowyer-watson
  (:require [clojure.set :refer :all])
  (:require [util.monit :refer :all])
  (:gen-class))

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

(defn distance [x y]
  (let [a (- (:x x) (:x y))
        b (- (:y x) (:y y))]
    (java.lang.Math/sqrt (+ (* a a) (* b b)))))

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

(defn- recreate [point edges]
  (map #(triangle point %) edges))

(defn- timmed-frequencies [timer-monitor arg]
  (timer (frequencies arg) timer-monitor))

(defmacro timed
  ([timer-monitor func arg]
   `(timer (~func ~arg) ~timer-monitor))
  ([timer-monitor func farg arg]
   `(timer (~func ~farg ~arg) ~timer-monitor)))

(defn- timmed-reduce [timer-monitor arg]
  (timer (reduce arg) timer-monitor))

(defn- boundry-edges [triangles timer-monitor]
  (->> triangles
       (timed timer-monitor mapcat :edges)
       (frequencies)
       (filter #(= (second %) 1))
       (map key)))

(defn- triangles-with-intersecting-circumcircle [point triangles]
  (filter #(not (> (distance (:p (:c %)) point) (:r (:c %)))) triangles))

(defn bowyer-watson_2d [points & {:keys [boundries timer-monitor]
                                  :or {boundries (triangle (point -100 -100) (point 100 -100) (point 0 100))
                                       timer-monitor nil-monit}}]
  (loop [triangles #{boundries}
         points points]
    (if (empty? points) triangles ;; end of points - stop condition
      (let [point (first points)
            badTriangles (timer (triangles-with-intersecting-circumcircle point triangles) timer-monitor)
            boundries (timer (boundry-edges badTriangles timer-monitor) timer-monitor)
            newTriangles (timer (recreate point boundries) timer-monitor)]
        (recur (-> triangles
                   (clojure.set/difference (into #{} badTriangles))
                   (clojure.set/union (into #{} newTriangles)))
               (rest points))))))

(defn add
  "Adds key-value pairs the multimap."
  ([mm k v]
   (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs]
   (apply add (add mm k v) kvs)))

(defn as-voronoi-diagram [triangles & {:keys [connect
                                              graph
                                              edge-filter]
                                       :or {connect (fn [graph p1 p2]
                                                      (let [g (or graph {})]
                                                        (-> graph
                                                            (add p1 p2)
                                                            (add p2 p1))))
                                            graph {}
                                            edge-filter (let [exclude #{(point -100 -100) (point 100 -100) (point 0 100)}] ;;same points as in bowyer-watson_2d boundry
                                                          (fn [e] (not (or (contains? exclude (:p1 e))
                                                                           (contains? exclude (:p2 e))))))}}]
  (loop [triangles triangles
         graph graph]
    (if-let [triangle (first triangles)]
      (let [newGraph (loop [neighbours (rest triangles)
                            graph graph]
                       (if-let [neighbour (first neighbours)]
                         (if
                           (empty? (clojure.set/intersection (:edges triangle) (:edges neighbour)))
                           (recur (rest neighbours) graph)
                           (recur (rest neighbours) (connect graph (-> triangle :c :p) (-> neighbour :c :p))))
                         graph))]
        (recur (rest triangles) newGraph))
      graph)))

(defn as-graph [triangles & {:keys [connect
                                    graph
                                    edge-filter]
                             :or {connect (fn [graph p1 p2]
                                            (let [g (or graph {})]
                                              (-> graph
                                                  (add p1 p2)
                                                  (add p2 p1))))
                                  graph {}
                                  edge-filter (let [exclude #{(point -100 -100) (point 100 -100) (point 0 100)}] ;;same points as in bowyer-watson_2d boundry
                                                (fn [e] (not (or (contains? exclude (:p1 e))
                                                                 (contains? exclude (:p2 e))))))}}]
  (loop [edges (->> triangles
                    (map :edges)
                    (reduce concat)
                    (into #{})
                    (filter edge-filter))
         graph graph]
    (if (empty? edges) graph
      (recur (rest edges)
             (connect graph
                      (:p1 (first edges))
                      (:p2 (first edges)))))))

(defn- breath-first-seq-impl
  ([node neighbour-fn head-fn]
   (cons (head-fn node nil)
         (lazy-seq (breath-first-seq-impl node (neighbour-fn node) #{node} neighbour-fn head-fn))))
  ([pred nodes ignored neighbour-fn head-fn]
   (loop [nodes nodes]
     (let [node (first nodes)]
       (cond (empty? nodes) nil; EOF seq
             (contains? ignored node) (recur (rest nodes)) ;try next node
             :else (cons (head-fn node pred)
                         (lazy-seq (breath-first-seq-impl
                                    node
                                    (concat (rest nodes) (neighbour-fn node))
                                    (conj ignored node)
                                    neighbour-fn
                                    head-fn))))))))

(defn breath-first-seq
  ([node neighbour-fn] (breath-first-seq node neighbour-fn :nodes-only))
  ([node neighbour-fn option]
   (case option
     :predecessors (breath-first-seq-impl node neighbour-fn (fn [node pred] [node pred]))
     :nodes-only (breath-first-seq-impl node neighbour-fn (fn [node pred] node)))))

(take 12 (breath-first-seq 'a (fn [x] (let [result (cond (= x 'a) '(b c q)
                                                         (= x 'q) '()
                                                         (= x 'b) '(d)
                                                         (= x 'c) '(e)
                                                         (= x 'e) '(a f)
                                                         (= x 'd) '(b)
                                                         (= x 'f) '())]
                                        result)) :predecessors))

