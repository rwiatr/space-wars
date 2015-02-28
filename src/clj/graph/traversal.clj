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

(defn- breath-first-seq-with-lvls-impl
  ([node neighbour-fn]
   (cons [node 0] (lazy-seq (breath-first-seq-with-lvls-impl (neighbour-fn node) (list) #{node} neighbour-fn 1))))
  ([queue child-queue ignored neighbour-fn level]
   (if (and (empty? child-queue) (empty? queue)) nil
     (if (empty? queue) (recur child-queue queue ignored neighbour-fn (inc level))
       (let [node (first queue)]
         (if (contains? ignored node) (recur (rest queue) child-queue ignored neighbour-fn level)
           (cons [node level] (lazy-seq (breath-first-seq-with-lvls-impl
                                         (rest queue)
                                         (concat child-queue (neighbour-fn node))
                                         (conj ignored node)
                                         neighbour-fn
                                         level)))))))))

(defn breath-first-seq
  ([node neighbour-fn & {:keys [with-level] :or [with-level false]}]
   (if with-level
     (breath-first-seq-with-lvls-impl node neighbour-fn)
     (breath-first-seq-impl node neighbour-fn))))
