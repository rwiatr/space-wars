(ns util.monit
  (:gen-class))

(defprotocol Monitors
  (monit [this id t]))

(deftype NilMonitor []
  Monitors
  (monit [this _ _] this))

(deftype AtomMapMonitors [atom-map formatter]
  Monitors
  (monit [this id t]
         (swap! atom-map #(assoc % id (+ (% id 0) t)))
         this)
  Object
  (toString [_] (formatter @atom-map)))

(defn- map-to-percents [input-map]
  (let [result (into (sorted-set-by #(> (val %1) (val %2))) input-map)
        total (reduce + (vals result))]
    (str (clojure.string/join "\n" (doall (map #(do [(format "%.10f" (double (/ (second %) total))) (first %)])
                                               result))))))

(defn- map-to-total [input-map]
  (let [result (into (sorted-set-by #(> (val %1) (val %2))) input-map)
        total (reduce + (vals result))]
    (str (format "TOTAL: %d" total))))

(defn- multi-formatter
  ([formatters] (multi-formatter formatters "\n"))
  ([formatters separator]
   (fn [input-map]
     (->> formatters
          (map #(% input-map))
          (filter not-empty)
          (clojure.string/join separator)))))

(def atom-map-formatters {:percent map-to-percents
                          :total map-to-total})

;; API

(defn atom-map-monit
  [& {:keys [formatters]
      :or {formatters [:total :percent]}}]
  (let [instances (doall (filter some? (map #(% atom-map-formatters) formatters)))
        formatter (multi-formatter instances)]
    (AtomMapMonitors. (atom {}) formatter)))

(def nil-monit (NilMonitor.))

(defmacro timer [exec monitor]
  `(let [start# (System/nanoTime)
         execution-result# ~exec
         end# (System/nanoTime)]
     (monit ~monitor '~exec (- end# start#))
     execution-result#))
