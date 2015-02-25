(ns spatial.spattree
  (:use [geom.bbox :refer [area, bbox, bbox-max, bbox-xy, contained?]])
  (:gen-class))

(defn- node? [node]
  (-> node :value nil?))

(defn- leaf? [node]
  (not (node? node)))

(defn- succ [node]
  (:sub node))

(defn- contains-bbox? [node value]
  (contained? (:bbox node) (:bbox value)))

(defn- node-area [node] (area (:bbox node)))

(defn- multi-bbox-max [bbox & bboxs]
  (if (nil? bbox) (throw (Exception. "bbox can't be nil")))
  (if-let [bbox2 (first bboxs)]
    (recur (bbox-max bbox bbox2) (rest bboxs))
    bbox))

(defn into-node [node & values]
  (if (empty? values) node
    (recur (-> node
               (update-in [:sub] conj (first values))
               (update-in [:bbox] (partial multi-bbox-max (:bbox (first values)))))
           (rest values))))

(defn create-node
  ([& values] (apply into-node (create-node) values))
  ([] {:sub #{}}))

(defn create-leaf [value bbox]
  {:value value, :bbox bbox})

;; splitting algorithm
(defn cost-fn [{bbox1 :bbox} {bbox2 :bbox}]
  (- (area (bbox-max bbox1 bbox2)) (area bbox1) (area bbox2)))

(defn choose-seeds
  "choose two items with biggest cost-fn"
  [items]
  (let [pairs (for [a items, b items :when (not (identical? a b))] (vector a b))] ;; TODO refactor for into more efficient
    (loop [pair (first pairs)
           cost (cost-fn (first pair) (second pair))
           pairs (rest pairs)]
      (if (empty? pairs) pair
        (let [next-pair (first pairs)
              next-cost (cost-fn (first next-pair) (second next-pair))]
          (if (> next-cost cost)
            (recur next-pair next-cost (rest pairs))
            (recur pair cost (rest pairs))))))))

(defn split-node [{items :sub}]
  "splits items into two nodes with optimal bbox"
  (let [[seed1, seed2] (choose-seeds items)]
    (loop [node1 (create-node seed1)
           node2 (create-node seed2)
           items items]
      (if (empty? items) (vector node1 node2)
        (if (< (- (node-area (into-node node1 (first items))) (node-area node1)) (- (node-area (into-node node2 (first items))) (node-area node2)))
          (recur (into-node node1 (first items)) node2 (rest items))
          (recur (into-node node2 (first items)) node1 (rest items)))))))

(defn rebuild [new old path split]
  "rebuilds the path in the tree by updating every item on the path"
  (loop [new new
         old old
         path path]
    (let [for-split (> (count (succ new)) split)
          is-root (empty? path)]
      (case
        (and for-split is-root) (let [[new1 new2] (split-node (succ new))] (create-node new1 new2))
        is-root new
        for-split (let [[new1 new2] (split-node (succ new))
                        head (first path)
                        updated (-> head
                                    (update-in [:sub] disj old)
                                    (update-in [:sub] conj new))]
                    (recur updated head (rest path)))))))

(defn ok-to-add? [node properties]
  (> (:split-size properties) (count (:values node))))

(defn- min-size-node [nodes]
  (loop [min-node (first nodes)
         nodes nodes]
    (if (empty? nodes) min-node
      (if (> (count (:sub min-node)) (count (:sub (first min-node))))
        (recur (first min-node) (rest nodes))
        (recur min-node (rest nodes))))))

(defn- bbox-extension-cost [b1 b2]
  (- (area (bbox-max b1 b2) (area b1))))

(defn- into-node2
  ([node value properties path]
   (if-let [children (not-empty (filter node? (succ node)))]
     (let [notin (println children)
           fit (or (not-empty (filter #(contains-bbox? % value) children)) ; bbox perfect match
                   (apply min-key #(bbox-extension-cost % (:bbox value)) children)) ; bbox extension cheapest
           notin (println fit)
           fit (apply min-key #(count (succ %)) fit)] ; minimum children
       (if-let [target (first fit)]
         (recur target value properties (cons node path))))
     (let [new-node (into-node node value)]
       (rebuild new-node node path)))))
(comment
  (def properties {:split-size 3})
  (def root {:bbox (bbox-xy 0 0 1000 1000) :sub #{}})
  (def root2 (into-node root (create-leaf :2 (bbox-xy 20 20 120 120)) properties (list)))
  (def root3 (into-node root2 (create-leaf :3 (bbox-xy 20 20 120 120)) properties (list)))
  (def root4 (into-node root3 (create-leaf :4 (bbox-xy 20 20 120 120)) properties (list)))
  (def root5 (into-node root4 (create-leaf :5 (bbox-xy 20 20 120 120)) properties (list)))
  (def root6 (into-node root5 (create-leaf :6 (bbox-xy 20 20 120 120)) properties (list)))
  root
  root2
  root3
  root4
  root5
  root6
  )

(defn tree [& {:keys [split-size, val-factory-fn]
               :or {split-size 5
                    val-factory-fn identity}}]
  {:root (create-node) :properties {:split-size split-size
                                    :val-factory val-factory-fn}})

(defn t-add [tree value]
  (assoc tree :root (into-node (:root tree)
                                 ((:val-factory tree) value)
                                 (:properties tree)
                                 (list))))
