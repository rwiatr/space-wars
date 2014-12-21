(ns space-wars.monit
  (:gen-class))

(defmacro timer [exec monit]
  `(let [start# (System/nanoTime)
         execution-result# ~exec
         end# (System/nanoTime)]
     (~monit '~exec (- end# start#))
     execution-result#))

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

(defn atom-map-monit []
  (AtomMapMonitors. (atom {})))

(defn monit-nil [id t])

(defn monit-fn [monitor]
  (fn [id t] (monit monitor id t)))


(def monitor (atom-map-monit))
(def monitor-fn (monit-fn monitor))
(macroexpand '(timer (str "123" "123") monitor-fn))
(def timer-code (timer (str "123" "123") monitor-fn))
(def timer-code (timer (str "123") monitor-fn))

(def nul nil)
(macroexpand '(timer (str "1") nul))

monitor
