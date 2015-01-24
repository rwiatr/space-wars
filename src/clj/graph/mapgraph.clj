(ns graph.mapgraph
  (:use util.set_multimap)
  (:use clojure.math.combinatorics)
  (:gen-class))

(defn connect
  ([graph n1 n2] (util.set_multimap/add graph n1 n2))
  ([graph n1 n2 & nps] (apply connect (connect graph n1 n2) nps)))

(defn connect-seq [graph nps] (apply connect graph nps))

(defn graph[] {:connections (util.set_multimap/multimap),
               :nodes #{}
               :data {}})

(defn g-empty? [g]
  (-> g :nodes empty?))

(defn g-contains?
  ([g n] (-> g :nodes (contains? n)))
  ([g n & nds] (and (g-contains? g n)
                    (every? true? (map (partial g-contains? g) nds)))))

(defn g-add
  ([g n] (update-in g [:nodes] #(conj % n)))
  ([g n & nds] (apply g-add (g-add g n) nds)))

(defn connect [g n1 n2]
  (-> (g-add g n1 n2)
      (update-in [:connections] #(add % n1 n2))))

(defn connect-to-many [g n & nds]
   (-> (apply g-add g nds)
       (g-add n)
       (update-in [:connections] #(addseq % n nds))))

(defn bi-connect
  ([g n1 n2] (-> g (connect n1 n2) (connect n2 n1)))
  ([g n1 n2 & nds] (apply  bi-connect (bi-connect g n1 n2) nds)))

(defn- array-bi-connect
  ([g [n1 n2]] (bi-connect g n1 n2))
  ([g [n1 n2] & nds] (apply array-bi-connect (array-bi-connect g [n1 n2]) nds)))

(defn all-connect [g & nds]
  (apply array-bi-connect g (clojure.math.combinatorics/combinations nds 2)))

(defn disconnect [g n1 n2]
  (update-in g [:connections] #(del % n1 n2)))

(disconnect {:connections {:a #{:b :c}}} :a :b)

(defn bi-disconnect [g n1 n2]
  (-> (disconnect g n1 n2)
      (disconnect n2 n1)))

(defn disconnect-all [g & nds]
  (apply bi-disconnect g (clojure.math.combinatorics/combinations nds 2)))

(defn prop-del [g pk n]
  (if (g-contains? g n)
    (let [ng (update-in g [:data pk] dissoc n)]
      (println (empty? (get-in ng [:data pk])))
      (if (empty? (get-in ng [:data pk])) ; removes {property {}} entries
        (update-in ng [:data] dissoc pk)
        ng))))

(defn prop-add [g pk n v]
  (if (g-contains? g n)
    (assoc-in g [:data pk n] v)
    g))
