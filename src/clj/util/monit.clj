(ns util.monit
  (:gen-class))

(defprotocol Monitors
  (monit [this id t]))

(deftype AtomMapMonitors [atom-map]
  Monitors
  (monit [this id t]
         (swap! atom-map #(assoc % id (+ (% id 0) t))))
  Object
  (toString [_]
            (let [result (into (sorted-set-by #(> (val %1) (val %2))) @atom-map)
                  total (reduce + (vals result))]
              (str (format "TOTAL: %d\n" total)
                   (clojure.string/join "\n" (doall (map #(do [(format "%.10f" (double (/ (second %) total))) (first %)])
                                                         result)))))))
;; API

(defn atom-map-monit []
  (AtomMapMonitors. (atom {})))

(defn monit-nil [id t])

(defn monit-fn [monitor]
  (fn [id t] (monit monitor id t)))

(defmacro timer [exec monit]
  `(let [start# (System/nanoTime)
         execution-result# ~exec
         end# (System/nanoTime)]
     (~monit '~exec (- end# start#))
     execution-result#))
