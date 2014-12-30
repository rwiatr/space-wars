(ns space-wars.draw
  (:require [graph.bowyer-watson :refer :all]
            [quil.core :as q])
  (:gen-class))

(defn setup []
  (q/smooth)                          ;; Turn on anti-aliasing
  (q/frame-rate 1)                    ;; Set framerate to 1 FPS
  (q/background 0))                 ;; Set the background colour to
                                      ;; a nice shade of grey.
(defn random-points [] (cons (graph.bowyer-watson/point (rand-int 800) (rand-int 600)) (lazy-seq (random-points))))


(def points (take 20 (random-points))); (point 0 0) (point 800 0) (point 0 600) (point 800 600)))
(def bw (bowyer-watson_2d points :boundries (triangle (point -100000 -100000) (point 100000 -100000) (point 0 100000))))

(def graph (as-graph bw :edge-filter (let [exclude #{(point -100000 -100000) (point 100000 -100000) (point 0 100000)}] ;;same points as in bowyer-watson_2d boundry
                                                (fn [e] (not (or (contains? exclude (:p1 e))
                                                                 (contains? exclude (:p2 e))))))))
(def voronoi (as-voronoi-diagram bw :edge-filter (let [exclude #{(point -100000 -100000) (point 100000 -100000) (point 0 100000)}] ;;same points as in bowyer-watson_2d boundry
                                                (fn [e] (not (or (contains? exclude (:p1 e))
                                                                 (contains? exclude (:p2 e))))))))
(defn draw []
  (q/stroke 120)
  (q/stroke-weight 1)
    (doseq [k (keys graph)]
      (doseq [p (get graph k)]
       (q/line (:x k) (:y k) (:x p) (:y p))))
  (q/stroke 255 0 0)
  (q/stroke-weight 1)
    (doseq [k (keys voronoi)]
      (doseq [p (get voronoi k)]
       (q/line (:x k) (:y k) (:x p) (:y p)))))       ;; Draw a circle at x y with the correct diameter

(q/defsketch example                  ;; Define a new sketch named example
  :title "Oh so many grey circles"    ;; Set the title of the sketch
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [800 600])
