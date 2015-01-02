(ns graph.traversal
  (:gen-class))

(defn- breath-first-seq-impl
  ([node neighbour-fn]
   (cons node (lazy-seq (breath-first-seq-impl (neighbour-fn node) #{node} neighbour-fn))))
  ([queue ignored neighbour-fn]
   (loop [nodes queue]
     (let [node (first nodes)]
       (cond (empty? nodes) nil ; EOF seq
             (contains? ignored node) (recur (rest nodes)) ; try next node
             :else (cons node (lazy-seq (breath-first-seq-impl
                                         (concat (rest nodes) (neighbour-fn node))
                                         (conj ignored node)
                                         neighbour-fn))))))))

(defn breath-first-seq
  ([node neighbour-fn] (breath-first-seq-impl node neighbour-fn)))
