(ns spattree
  (:use [geom.point :refer [point distance]])
  (:gen-class))

(defn- area [t])
(defn- area-valid [n] true)
(defn- subtrees[t g])

; node = {:children #{c}, :properties {p}, :bb #{}}
(defn create-node
  ([child] {:children #{}, :bboxs #{child}})
  ([] {:children #{}, :bboxs #{}}))

(defn into-node [node bbox]
  (let [new-node (update-in node [:bboxs] conj bbox)] new-node))

(def a {:a :x :children #{}})
(def b {:b :b :children #{a}})
(def c {:c :c :children #{}})
(def d {:d :d :children #{c b}})

(defn- rebuild [new old path]
  "rebuilds the path in the tree by updating every item on the path"
  (loop [new new
         old old
         path path]
    (if (empty? path) new
      (let [updated (-> (first path)
                        (update-in [:children] disj old)
                        (update-in [:children] conj new))]
        (recur updated (first path) (rest path))))))

(defn ok-to-add? [node properties]
  (> (:split-size properties) (count (:bboxs node))))

(defn- best-fit-node [nodes bbox]

(defn- min-size-node [nodes]
  (loop [min-node (first nodes)
         nodes nodes]
    (if (empty? nodes) min-node
      (if (> (count (:children min-node)) (count (:children (first min-node))))
        (recur (first min-node) (rest nodes))
        (recur min-node (rest nodes))))))

(defn- add-to-node
  ([node bbox properties path]
   (if (ok-to-add? node properties)
     (rebuild (into-node node bbox) node path)
     (if-let [mnode (min-size-node (:children node))]
       (recur mnode bbox properties (cons node path))
       (rebuild (create-node bbox) nil (cons node path))))))

(defn tree [& {:keys [split-size, val-factory-fn]
               :or {split-size 5
                    val-factory-fn identity}}]
  {:root (create-node) :properties {:split-size split-size
                                    :val-factory val-factory-fn}})

(defn t-add [tree value]
  (assoc tree :root (add-to-node (:root tree)
                                 ((:val-factory tree) value)
                                 (:properties tree)
                                 (list))))
