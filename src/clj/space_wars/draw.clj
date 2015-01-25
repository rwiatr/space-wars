(ns space-wars.draw
  (:require [graph.bowyer-watson :refer :all]
            [graph.converters :refer :all]
            [geom.point :refer :all]
            [geom.polygon :refer :all]
            [geom.triangle :refer :all]
            [util.set_multimap :refer :all]
            [quil.core :as q])
  (:gen-class))
(comment
(defn setup []
  (q/smooth)                          ;; Turn on anti-aliasing
  (q/frame-rate 1)                    ;; Set framerate to 1 FPS
  (q/background 0))                 ;; Set the background colour to
;; a nice shade of grey.
(defn random-points [] (cons (point (rand-int 800) (rand-int 600)) (lazy-seq (random-points))))

(defn trn [a b c d e f]
  (triangle (point a b) (point c d) (point e f)))

(def pts (take 500 (random-points))); (point 0 0) (point 800 0) (point 0 600) (point 800 600)))
(def bwt (bw-standard-filter 100000
                             (bowyer-watson_2d pts :boundries (triangle (point -100000 -100000) (point 100000 -100000) (point 0 100000)))))
;(def bwt [(trn 100 100, 100 200, 200 100) (trn 100 200, 200 100, 300 200) (trn 300 200, 300 100, 200 100) (trn 300 200, 100 200, 200 300)])
(comment def bwt [(trn 80 90, 0 100, 0 0)
          (trn 80 90, 0 0, 100 0)
          (trn 80 90, 200 0, 100 0)
          (trn 80 90, 200 0, 200 100)
          (trn 80 90, 200 100, 200 200)
          (trn 80 90, 100 200, 200 200)
          (trn 80 90, 100 200, 0 200)
          (trn 80 90, 0 100, 0 200)

          ;(trn 300 100, 200 100, 200 0)
          ;(trn 300 100, 200 0, 300 0)
          ;(trn 300 100, 400 0, 300 0)
          ;(trn 300 100, 400 0, 400 100)
          ;(trn 300 100, 400 100, 400 200)
          ;(trn 300 100, 300 200, 400 200)
          ;(trn 300 100, 300 200, 200 200)
          ;(trn 300 100, 200 100, 200 200)

          ;(trn 100 300, 0 300, 0 200)
          ;(trn 100 300, 0 200, 100 200)
          ;(trn 100 300, 200 200, 100 200)
          ;(trn 100 300, 200 200, 200 300)
          ;(trn 100 300, 200 300, 200 400)
          ;(trn 100 300, 100 400, 200 400)
          ;(trn 100 300, 100 400, 0 400)
          ;(trn 100 300, 0 300, 0 400)
          ])

(def geoms (keys (voronoi-polygons->neighbours bwt)))



;(def graph (as-graph bw :edge-filter (let [exclude #{(point -100000 -100000) (point 100000 -100000) (point 0 100000)}] ;;same points as in bowyer-watson_2d boundry
;                                                (fn [e] (not (or (contains? exclude (:p1 e))
;                                                                 (contains? exclude (:p2 e))))))))
;(def voronoi (as-voronoi-diagram bw :edge-filter (let [exclude #{(point -100000 -100000) (point 100000 -100000) (point 0 100000)}] ;;same points as in bowyer-watson_2d boundry
;                                                (fn [e] (not (or (contains? exclude (:p1 e))
;                                                                (contains? exclude (:p2 e))))))))

(def graph (as-graph bwt :edge-filter (let [exclude #{(point -100000 -100000) (point 100000 -100000) (point 0 100000)}] ;;same points as in bowyer-watson_2d boundry
                                        (fn [e] (not (or (contains? exclude (:p1 e))
                                                         (contains? exclude (:p2 e))))))))
(def voronoi (as-voronoi-diagram bwt :edge-filter (let [exclude #{(point -100000 -100000) (point 100000 -100000) (point 0 100000)}] ;;same points as in bowyer-watson_2d boundry
                                                    (fn [e] (not (or (contains? exclude (:p1 e))
                                                                     (contains? exclude (:p2 e))))))))

(defn drawpoints [points]
  (loop [points (concat points [(first points)])
         prev nil]
    (if-let [curr (first points)]
      (do
        (if prev (q/line (:x prev) (:y prev) (:x curr) (:y curr)))
        (recur (rest points) curr)))))

(defn drawpoly [polygon]
  (if (< 2 (count (:points polygon)))
    (drawpoints (:points polygon))
    (println (count (:points polygon)))))

(defn draw []
  (q/stroke 120)
  (q/stroke-weight 1)
  (q/stroke 0 255 0)
  (doseq [k (keys voronoi)]
    (doseq [p (get voronoi k)]
      (q/line (+ 1 (:x k)) (+ 1 (:y k)) (+ 1 (:x p)) (+ 1 (:y p)))))
  (q/stroke 120)
  (doseq [g geoms] (drawpoly (:geometry g)))
  (q/stroke 255 0 0)
  ;(doseq [t bwt] (drawpoints (points t)))
  )

(q/defsketch example                  ;; Define a new sketch named example
  :title "Oh so many grey circles"    ;; Set the title of the sketch
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [800 600])
)
