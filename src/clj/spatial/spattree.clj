(ns spatial.spattree
  (:use [geom.bbox :refer [area, bbox, bbox-max, bbox-xy, contained?, intersect?]]
        [graph.traversal :refer [breath-first-seq]])
  (:gen-class))

(defn- node? [node]
  (-> node :value nil?))

(defn- leaf? [node]
  (not (node? node)))

(defn- succ [node]
  (:sub node))

(defn- succ-node [node]
  (filter node? (:sub node)))

(defn bbox-interact? [interact? {b1 :bbox} {b2 :bbox}]
  (interact? b1 b2))

(defn contains-bbox? [node value]
  (bbox-interact? contained? node value))

(defn intersects-bbox? [node value]
  (bbox-interact? intersect? node value))

(defn- multi-bbox-max [bbox & bboxs]
  (if (nil? bbox) (throw (Exception. "bbox can't be nil")))
  (if-let [bbox2 (first bboxs)]
    (recur (bbox-max bbox bbox2) (rest bboxs))
    bbox))

(defn into-node [node & values]
  (if (empty? values) node
    (recur (-> node
               (update-in [:sub] conj (first values))
               (update-in [:bbox] (partial multi-bbox-max (:bbox (first values))))
               (update-in [:total-leafs] + (or (:total-leafs (first values)) 1)))
           (rest values))))

(defn create-node
  ([& values] (apply into-node (create-node) values))
  ([] {:sub #{} :total-leafs 0}))

;; splitting algorithm
(defn cost-fn [{bbox1 :bbox} {bbox2 :bbox}]
  (- (area (bbox-max bbox1 bbox2)) (area bbox1) (area bbox2)))

(defn- area-diff [{bbox1 :bbox} {bbox2 :bbox}]
  (- (area bbox1) (area bbox2)))
(def k [:a :b :c :d])

(defn- s-seq [s]
  (if (empty? s) nil
    (cons s (lazy-seq (s-seq (rest s))))))

(defn- p-seq [s]
  (apply concat
         (for [ss (s-seq s) :while (not-empty (rest ss))]
           (for [p (rest ss)] [(first ss) p]))))

(defn choose-seeds
  "choose two items with biggest cost-fn"
  [items]
  (let [pairs (p-seq items)]
    (if (empty? pairs) (throw (Exception. (str "Can't build any pairs from input [" (clojure.string/join "," items) "]"))))
    (loop [pair (first pairs)
           cost (cost-fn (first pair) (second pair))
           pairs (rest pairs)]
      (if (empty? pairs) pair
        (let [next-pair (first pairs)
              next-cost (cost-fn (first next-pair) (second next-pair))]
          (if (> next-cost cost)
            (recur next-pair next-cost (rest pairs))
            (recur pair cost (rest pairs))))))))

(defn split-node
  "splits items into two nodes with optimal bbox"
  [{items :sub}]
  (let [[seed1, seed2] (choose-seeds items)]
    (loop [node1 (create-node seed1)
           node2 (create-node seed2)
           items (filter #(not (or (identical? % seed2) (identical? % seed1))) items)]
      (if (empty? items) (vector node1 node2)
        (if (< (area-diff (into-node node1 (first items)) node1)
               (area-diff (into-node node2 (first items)) node2))
          (recur (into-node node1 (first items)) node2 (rest items))
          (recur (into-node node2 (first items)) node1 (rest items)))))))

(defn rebuild
  "rebuilds the path in the tree by updating every item on the path"
  [new old path split]
  (loop [new new
         old old
         path path]
    (let [for-split (> (count (succ new)) split)
          is-root (empty? path)]
      (if (and for-split is-root) (let [[new1 new2] (split-node new)] (create-node new1 new2))
        (if is-root new
          (if for-split
            (let [[new1 new2] (split-node new)
                  head (first path)
                  updated (apply create-node (succ (-> head
                                                       (update-in [:sub] disj old)
                                                       (update-in [:sub] conj new1)
                                                       (update-in [:sub] conj new2))))]
              (recur updated head (rest path)))
            (let [new new
                  head (first path)
                  updated (apply create-node (succ (-> head
                                                       (update-in [:sub] disj old)
                                                       (update-in [:sub] conj new))))]
              (recur updated head (rest path)))))))))

(defn insert
  ([target value split] (insert target value (list) split))
  ([target value path split]
   (if-let [fit (or (not-empty (filter #(contains-bbox? % value) (succ-node target)))
                    (not-empty (filter #(intersects-bbox? % value) (succ-node target)))
                    (not-empty (succ-node target)))]
     (recur (first (->> fit
                        (min-key #(area-diff % (into-node % value)))
                        (min-key #(count (succ-node %)))))
            value
            (cons target path)
            split)
     (rebuild (into-node target value) target path split))))

(defn successors-interacting-bbox [interaction node bbox]
  (for [child (:sub node) :when (interaction child bbox)] child))

(defn leafs-interacting-bbox
  ([interaction root bbox] (leafs-interacting-bbox interaction (list root) bbox (list)))
  ([interaction nodes bbox result]
   (if (empty? nodes) result
     (let [node (first nodes)
           children (successors-interacting-bbox interaction node bbox)]
       (recur interaction
              (concat (rest nodes) (filter node? children))
              bbox
              (into result (filter leaf? children)))))))

(defn tree [& {:keys [split-size, node-factory-fn]
               :or {split-size 5
                    node-factory-fn identity}}]
  {:root (create-node)
   :split-size split-size
   :node-factory-fn node-factory-fn})

(defn tree-values-contained-by [tree bbox])

(defn tree-values-intersecting [tree bbox]
  (leafs-interacting-bbox intersects-bbox? (:root tree) bbox))

(defn tree-values-containing [tree bbox]
  (leafs-interacting-bbox contains-bbox? (:root tree) bbox))

(defn tree-add [tree & values]
  (assoc tree :root
    (let [split (:split-size tree)
          factory (:node-factory-fn tree)]
      (loop [root (:root tree)
             values values]
        (if-let [value (first values)]
          (recur (insert root (factory value) split)
                 (rest values))
          root)))))

(defn tree-breath-first-bbox-seq [tree]
  (for [[{bbox :bbox} i] (breath-first-seq (:root tree) succ :with-level true)] [bbox i]))
