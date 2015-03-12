(ns space-wars.draw
  (:require [quil.core :as q]
            [space-wars.game-state :refer [generate-level]]
            [quil.middleware :as m]
            [space-wars.quil-ui-handlers :as handlers])
  (:gen-class))

(def lvl (generate-level))

(q/defsketch example
  :title "MAP"
  :setup (handlers/setup-factory (-> lvl :rtree) (-> lvl :graph))
  :renderer :opengl
  :draw handlers/draw-ui
  :mouse-pressed handlers/map-select
  :size [820 820]
  :key-pressed handlers/monitoring-key-handler
  :middleware [m/fun-mode])

