(ns space-wars.draw
  (:require [graph.bowyer-watson :refer :all]
            [graph.converters :refer :all]
            [graph.mapgraph :refer :all]
            [geom.point :refer :all]
            [geom.polygon :refer :all]
            [geom.triangle :refer :all]
            [util.set_multimap :refer :all]
            [quil.core :as q])
  (:gen-class))

(defn setup []
  (q/smooth)                          ;; Turn on anti-aliasing
  (q/frame-rate 1)                    ;; Set framerate to 1 FPS
  (q/background 0))                 ;; Set the background colour to
;; a nice shade of grey.
(def min_ 10)
(def size_ 800)
(def max_ (+ min_ size_))

(defn random-points [] (cons (point (+ (rand-int size_) min_) (+ (rand-int size_) min_)) (lazy-seq (random-points))))

(defn trn [a b c d e f]
  (triangle (point a b) (point c d) (point e f)))

(def pts (take 1000 (random-points))); (point 0 0) (point 800 0) (point 0 600) (point 800 600)))
(def bwt (bowyer-watson_2d pts :boundries #{(triangle min_ min_ max_ min_ max_ max_) (triangle min_ min_ min_ max_ max_ max_)}))
                                               ;#{(triangle (point -100000 -100000) (point 100000 -100000) (point 0 100000))})))
(def p->t (point->triangles bwt))

(def g (as-graph (point->points p->t) (point->polygon p->t)))

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

(defn draw []
  (q/stroke 120)
  (q/stroke-weight 1)
  (q/stroke 255 0 0)
  (draw-t bwt)
  (q/stroke 0 255 0)
  (draw-geoms g)
  (q/stroke-weight 10)
  (q/stroke 0 255 255)
  (draw-points g))

(q/defsketch example                  ;; Define a new sketch named example
  :title "Oh so many grey circles"    ;; Set the title of the sketch
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [820 820])

