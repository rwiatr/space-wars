(ns util.key_gen
  (:gen-class))

(defn key-mem [key-fn]
  (let [ks (atom {})]
    (fn [v]
      (swap! ks #(if (% v) % (assoc % v (key-fn v))))
      (@ks v))))

(defn inc-key [prefix]
  (let [cnt (atom 0)]
    (fn [v]
      (swap! cnt inc)
      (keyword (str prefix "-" @cnt)))))
