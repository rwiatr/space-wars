(ns space-wars.quil-ui-handlers
  (:import [java.awt.event KeyEvent])
  (:require [quil.core :as q]
            [graph.mapgraph :refer [g-nodes g-get-prop]]
            [geom.bbox :refer [bbox]]
            [geom.polygon :refer [point-in-poly? polygon->point-pairs]]
            [geom.point :refer [point]]
            [spatial.spattree :refer [tree-values-containing]]
            [util.monit :as monit]
            [clojure.tools.logging :as log])
  (:gen-class))

;; QUIL UTILS
(defn quil-draw-polygon [polygon]
  (doseq [[a b] (polygon->point-pairs polygon)]
    (q/line (:x a) (:y a) (:x b) (:y b))))
(def m (long-array 64))

(defn quil-fps []
  (q/stroke 255)
  (q/text (format "%.2f fps" (q/current-frame-rate)) 0 11))

;; SETUP
(defn build-state [map-rtree, map-graph]
  {:map-rtree map-rtree
   :map-graph map-graph
   :main-monitor-display false
   :main-monitor (monit/atom-map-monit)
   :fps-counter-display true})

(defn setup-factory [map-rtree map-graph]
  (fn []
    (q/smooth)
    (q/frame-rate 60)
    (q/background 0)
    (build-state map-rtree map-graph)))

;; EVENTS
(defn monitoring-key-handler [state event]
  (if (= (q/key-code) KeyEvent/VK_M) (update-in state [:main-monitor-display] not)
    (if (= (q/key-code) KeyEvent/VK_K) (update-in state [:fps-counter-display] not) state)))

;; SELECTION EVENTS
(defn- select-map-polygon [state event]
  (if-let [map-rtree (:map-rtree state)]
    (if-let [selected (not-empty (tree-values-containing map-rtree {:bbox (bbox (:x event) (:y event) 0 0)}))]
      (let [selected (filter (partial point-in-poly? (point (:x event) (:y event)))
                             (map :value selected))]
        (log/info "event " event " selected " selected)
        selected))))

(defn map-select [state event]
  (assoc state :map-selected-polygon (first (select-map-polygon state event))))

;; DRAWING
(defn draw-map [state]
  (if-let [map-graph (:map-graph state)]
    (let [polygons (for [node (g-nodes map-graph)] (g-get-prop map-graph node :geometry))]
      (doseq [polygon polygons]
        (q/stroke-weight 2)
        (q/stroke 0 0 255)
        (quil-draw-polygon polygon)))
    state))

(defn draw-map-selected [state]
  (if-let [map-selected-polygon (:map-selected-polygon state)]
    (do
      (q/stroke-weight 3)
      (q/stroke 255 0 255)
      (quil-draw-polygon map-selected-polygon))
    state))

(defn draw-monitoring [state]
  (if (:main-monitor-display state) (q/text (str (:main-monitor state)) 0 24))
  (if (:fps-counter-display state) (quil-fps)))

(defn draw-ui [state]
  (monit/timer (q/background-float 0x20) (:main-monitor state))
  (monit/timer (draw-map state) (:main-monitor state))
  (monit/timer (draw-map-selected state) (:main-monitor state))
  (monit/timer (draw-monitoring state) (:main-monitor state)))
