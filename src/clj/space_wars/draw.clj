(ns space-wars.draw
  (:require [graph.bowyer-watson :refer :all]
            [graph.converters :refer :all]
            [graph.mapgraph :refer :all]
            [geom.point :refer :all]
            [geom.bbox :refer :all]
            [geom.polygon :refer :all]
            [geom.triangle :refer :all]
            [util.set_multimap :refer :all]
            [util.key_gen :refer :all]
            [spatial.spattree :refer [tree, tree-add, tree-breath-first-bbox-seq, tree-values-containing]]
            [quil.core :as q]
            [space-wars.game-state :refer [generate-level]]
            [quil.middleware :as m]
            [space-wars.quil-ui-handlers :as handlers])
  (:gen-class))

(def min_ 10)
(def size_ 800)
(def max_ (+ min_ size_))

(defn random-points [] (cons (point (+ (rand-int size_) min_) (+ (rand-int size_) min_)) (lazy-seq (random-points))))

(defn trn [a b c d e f]
  (triangle (point a b) (point c d) (point e f)))

(def pts (take 300 (random-points))); (point 0 0) (point 800 0) (point 0 600) (point 800 600)))
(def bwt (bowyer-watson_2d pts :boundries #{(triangle min_ min_ max_ min_ max_ max_) (triangle min_ min_ min_ max_ max_ max_)}))
;#{(triangle (point -100000 -100000) (point 100000 -100000) (point 0 100000))})))
(def p->t (point->triangles bwt))

(def g (as-graph (point->points p->t) (point->polygon p->t)))

(defn boxes [g]
  (let [kfn ((key-mem inc-key) "ndx")]
    (loop [nodes (:nodes g)
           t (tree :node-factory-fn (fn [{b :bbox v :geometry}] {:bbox b :value v})
                   :split-size 10)]
      (if-let [n (first nodes)]
        (recur (rest nodes)
               (tree-add t {:bbox (to-bbox (g-get-prop g n :geometry)) :geometry (g-get-prop g n :geometry)}))
        t))))

(def t (boxes g))
(def box-vals (reverse (tree-breath-first-bbox-seq t)))
(println (second (first box-vals)))

(def colors (cycle [[0 0 204] [255 153 153] [0 204 204] [204 102 0] [204 0 0]]))
(defn drawbox [box lvl]
    (q/stroke-weight 1)
    (let [[r g b] (nth colors lvl)] (q/stroke r g b))
    (q/line (:x box) (:y box) (+ (:x box) (:width box)) (:y box))
    (q/line (:x box) (:y box) (:x box) (+ (:y box) (:height box)))
    (q/line (+ (:x box) (:width box)) (:y box) (+ (:x box) (:width box)) (+ (:y box) (:height box)))
    (q/line (:x box) (+ (:y box) (:height box)) (+ (:x box) (:width box)) (+ (:y box) (:height box))))

(defn drawpoints [points]
  (loop [points (concat points [(first points)])
         prev nil]
    (if-let [curr (first points)]
      (do
        (if prev (q/line (:x prev) (:y prev) (:x curr) (:y curr)))
        (recur (rest points) curr)))))

(defn get-param [g p]
  (for [n (:nodes g) :let [geom (g-get-prop g n p)]] geom))

(defn drawpoly [polygon]
  (if (< 2 (count (:points polygon)))
    (drawpoints (:points polygon))))

(defn drawpoint [point]
  (q/point (:x point) (:y point)))

(defn draw-geoms [g]
  (doseq [p (get-param g :geometry)] (drawpoly p)))

(defn draw-points [g]
  (doseq [p (get-param g :point)] (drawpoint p)))

(defn draw-t [bwt]
  (doseq [t bwt]
    (drawpoints (points t))))

(def selected (atom nil))

(defn press [old-state event]
  (swap! selected #(do %2) event))

(defn draw-selected []
  (if-let [selected-val @selected]
    (if-let [values (not-empty (map :value (tree-values-containing t {:bbox (bbox (:x selected-val) (:y selected-val) 1 1)})))]
      (doall (map drawpoly values)))))

(tree-values-containing t {:bbox (bbox 262 355 1 1)})

(defn draw [s]
  (q/background-float 0x20)
  (q/stroke 120)
  (q/stroke-weight 1)
  (q/stroke 255 0 0)
  (draw-t bwt)
  (q/stroke 0 255 0)
  (draw-geoms g)
  (q/stroke-weight 10)
  (q/stroke 0 255 255)
  (draw-points g)
  (doseq [[b l] box-vals] (drawbox b l))
  (q/stroke 255 0 255)
  (draw-selected))

;(def lvl (generate-level))

(q/defsketch example                  ;; Define a new sketch named example
  :title "Oh so many grey circles"    ;; Set the title of the sketch
  :setup (handlers/setup-factory t g)
  ;:renderer :opengl
  ;(handlers/setup-factory (-> lvl :map :rtree) (-> lvl :map :graph))
  :draw handlers/draw-ui                          ;; Specify the draw fn
  :mouse-pressed handlers/map-select
  :size [820 820]
  :key-pressed handlers/monitoring-key-handler
  :middleware [m/fun-mode])

