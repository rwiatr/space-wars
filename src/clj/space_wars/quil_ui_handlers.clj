(ns space-wars.quil-ui-handlers
  (require [quil.core :as q]
           [spatial.spattree :refer [tree-values-containing]])
  (:gen-class))

(defn select-map-polygon[state event]
  (if-let [rtree (:rtree state)]
    (if-let [selected (not-empty (tree-values-containing rtee (bbox {:x event} {:y event} 0 0)))]
      (filter
      )

  (defn select[state event])
