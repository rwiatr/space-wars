(ns geom.edge
  (:gen-class))

(defrecord Edge[p1 p2])
(defn edge [p1 p2]
  (let [comparision (.compareTo p1 p2)]
    (if (zero? comparision)
      nil
      (if (> 0 (.compareTo p1 p2))
        (Edge. p1 p2)
        (Edge. p2 p1)))))
