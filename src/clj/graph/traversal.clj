(ns graph.traversal
  (:gen-class))

(defn- breath-first-seq-impl
  ([node neighbour-fn head-fn]
   (cons (head-fn node nil)
         (lazy-seq (breath-first-seq-impl node (neighbour-fn node) #{node} neighbour-fn head-fn))))
  ([pred nodes ignored neighbour-fn head-fn]
   (loop [nodes nodes]
     (let [node (first nodes)]
       (cond (empty? nodes) nil ; EOF seq
             (contains? ignored node) (recur (rest nodes)) ; try next node
             :else (cons (head-fn node pred)
                         (lazy-seq (breath-first-seq-impl
                                    pred
                                    (concat (rest nodes) (neighbour-fn node))
                                    (conj ignored node)
                                    neighbour-fn
                                    head-fn))))))))

(defn breath-first-seq
  ([node neighbour-fn] (breath-first-seq node neighbour-fn :nodes-only))
  ([node neighbour-fn option]
   (case option
     :predecessors (breath-first-seq-impl node neighbour-fn (fn [node pred] [node pred]))
     :nodes-only (breath-first-seq-impl node neighbour-fn (fn [node pred] node)))))
