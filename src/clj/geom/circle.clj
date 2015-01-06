(ns geom.circle
  (:gen-class))

(defrecord Circle [p r])
(defn circle [p r] (Circle. p r))
