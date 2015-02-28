(ns graph.bowyer-watson
  (:require [clojure.set :refer :all]
            [geom.triangle :refer [triangle points circumcircle-defined?]]
            [geom.edge :refer [edge]]
            [geom.point :refer [point distance]]
            [util.monit :refer :all]
            [util.set_multimap :refer [add multimap]])
  (:gen-class))

(defn- default-boundries
  ([] (default-boundries 100))
  ([r] [(point (- r) (- r)) (point r (- r)) (point 0 r)]))

(defn bw-standard-filter
  ([triangles] (bw-standard-filter 100 triangles))
  ([r triangles]
    (let [excluded (into #{} (default-boundries r))]
    (filter #(not (some excluded (points %))) triangles))))

(defmacro timed
  ([timer-monitor func arg]
   `(timer (~func ~arg) ~timer-monitor))
  ([timer-monitor func farg arg]
   `(timer (~func ~farg ~arg) ~timer-monitor)))

(defn- boundry-edges [triangles timer-monitor]
  (->> triangles
       (timed timer-monitor mapcat :edges)
       (frequencies)
       (filter #(= (second %) 1))
       (map key)))

(defn- recreate [point edges]
  (for [e edges :when (circumcircle-defined? point (:p1 e) (:p2 e))] (triangle point e)))

(defn- triangles-with-intersecting-circumcircle [point triangles]
  (filter #(not (> (distance (:p (:c %)) point) (:r (:c %)))) triangles))

(defn bowyer-watson_2d [points & {:keys [boundries timer-monitor]
                                  :or {boundries #{(triangle (point -100 -100) (point 100 -100) (point 0 100))}
                                       timer-monitor nil-monit}}]
  (loop [triangles boundries
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

(defn as-graph2 [triangles & {:keys [connect
                                    graph
                                    edge-filter]
                             :or {connect (fn [graph p1 p2]
                                            (let [g (or graph {})]
                                              (-> graph
                                                  (add p1 p2)
                                                  (add p2 p1))))
                                  graph (multimap)
                                  edge-filter (let [exclude (into #{} (default-boundries))] ;;same points as in bowyer-watson_2d boundry
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
