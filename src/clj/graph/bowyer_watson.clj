(ns graph.bowyer-watson
  (:require [clojure.set :refer :all]
            [geom.triangle :refer [triangle]]
            [geom.edge :refer [edge]]
            [geom.point :refer [point distance]]
            [util.monit :refer :all])
  (:gen-class))

(defmacro timed
  ([timer-monitor func arg]
   `(timer (~func ~arg) ~timer-monitor))
  ([timer-monitor func farg arg]
   `(timer (~func ~farg ~arg) ~timer-monitor)))

(defn- timmed-frequencies [timer-monitor arg]
  (timer (frequencies arg) timer-monitor))

(defn- timmed-reduce [timer-monitor arg]
  (timer (reduce arg) timer-monitor))

(defn- boundry-edges [triangles timer-monitor]
  (->> triangles
       (timed timer-monitor mapcat :edges)
       (frequencies)
       (filter #(= (second %) 1))
       (map key)))

(defn- recreate [point edges]
  (map #(triangle point %) edges))

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

(defn- add
  "Adds key-value pairs the multimap."
  ([mm k v]
   (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs]
   (apply add (add mm k v) kvs)))

(defn- neighbours [triangle triangles]
  (filter (fn [neighbour] (empty? (clojure.set/intersection (:edges triangle) (:edges neighbour)))) triangles))

(defn as-voronoi-diagram [triangles & {:keys [connect
                                              graph
                                              edge-filter]
                                       :or {connect (fn [graph p1 p2 cell-keys]
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
                         (if-let [edge (first (clojure.set/intersection (:edges triangle) (:edges neighbour)))]
                           (recur (rest neighbours) (connect graph
                                                             (-> triangle :c :p)
                                                             (-> neighbour :c :p)
                                                             (list (:p1 edge) (:p2 edge))))
                           (recur (rest neighbours) graph))
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
