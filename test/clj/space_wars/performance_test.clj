(ns space-wars.performance-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [space-wars.bowyer-watson :refer :all]
            [clojure.set :refer :all]))

(defn random-points [] (cons (space-wars.bowyer-watson/point (rand-int 800) (rand-int 600)) (lazy-seq (random-points))))
(def points (into #{} (conj (take 1 (random-points)) (point 0 0) (point 800 0) (point 0 600) (point 800 600))))

(defrecord timeholder [start operations stop])
(defn start [] (timeholder. (new java.util.concurrent.atomic.AtomicLong (System/nanoTime))
                      (new java.util.concurrent.atomic.AtomicLong)
                      (new java.util.concurrent.atomic.AtomicLong)))
(defn stop  [timeholder] (doto (:stop timeholder) (.set (System/nanoTime))))
(defn update [timeholder] (doto (:operations timeholder) (.incrementAndGet)))
(defn as-str [timeholder]
  (let [t (/ (- (.get (:stop timeholder)) (.get (:start timeholder))) 1000000000)
        o (.get (:operations timeholder))]
  (str "Timeholder:"
       "\n\ttotal: " (format "%.10f" (.doubleValue t))
       "\n\tops:   " (format "%.10f" (.doubleValue o))
       "\n\to/t: " (format "%.10f" (.doubleValue (/ o t)))
       "\n\tt/o: " (format "%.10f" (.doubleValue (/ t o))))))



(println)
(def t (start))
(defn u [] (update t) u)
(comment (bowyer-watson_2d points :boundries (triangle (point -100000 -100000) (point 100000 -100000) (point 0 100000))
                  :timer-fn u)
(stop t)
(println (as-str t))

(def graph (as-graph (bowyer-watson_2d points :boundries (triangle (point -100000 -100000) (point 100000 -100000) (point 0 100000)))
                     :edge-filter (let [exclude #{(point -100000 -100000) (point 100000 -100000) (point 0 100000)}] ;;same points as in bowyer-watson_2d boundry
                                                (fn [e] (not (or (contains? exclude (:p1 e))
                                                                 (contains? exclude (:p2 e)))))))))

