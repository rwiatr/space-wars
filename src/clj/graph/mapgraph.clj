(ns graph.mapgraph
  (:use util.set_multimap)
  (:use clojure.math.combinatorics)
  (:gen-class))

(defn graph[] {:connections (util.set_multimap/multimap),
               :nodes #{}
               :data {}})

(defn g-empty? [g]
  (-> g :nodes empty?))

(defn g-contains?
  ([g n] (-> g :nodes (contains? n)))
  ([g n & nds] (and (g-contains? g n)
                    (every? true? (map (partial g-contains? g) nds)))))

(defn g-connected? [g n1 n2]
  (-> g :connections (mm-contains-kv n1 n2)))

(defn g-connected-any? [g n1 n2]
  (or (g-connected? g n1 n2) (g-connected? g n2 n1)))

(defn g-bi-connected? [g n1 n2]
  (and (g-connected? g n1 n2) (g-connected? g n2 n1)))

(defn g-add-prop [g n pk v]
  (if (g-contains? g n)
    (assoc-in g [:data n pk] v)
    g))

(defn g-get-prop [g n pk]
  (get-in g [:data n pk]))

(defn g-del-prop [g pk n]
  (if (g-contains? g n)
    (let [ng (update-in g [:data n] dissoc pk)]
      (if (empty? (get-in ng [:data n])) ; removes {property {}} entries
        (update-in ng [:data] dissoc n)
        ng))))

(defn g-add
  ([g n] (update-in g [:nodes] #(conj % n)))
  ([g n & nds] (apply g-add (g-add g n) nds)))

(defn g-rem [g n] (-> (update-in g [:nodes] #(disj % n))
                      (update-in [:data] #(dissoc % n))
                      (update-in [:connections] #(-> (del % n)
                                                     (mm-rem-val n)))))

(defn g-connect [g n1 n2]
  (-> (g-add g n1 n2)
      (update-in [:connections] #(add % n1 n2))))

(defn g-connect-to-many [g n & nds]
  (-> (apply g-add g nds)
      (g-add n)
      (update-in [:connections] #(addseq % n nds))))

(defn g-bi-connect
  ([g n1 n2] (-> g (g-connect n1 n2) (g-connect n2 n1)))
  ([g n1 n2 & nds] (apply g-bi-connect (g-bi-connect g n1 n2) nds)))

(defn- array-bi-connect
  ([g [n1 n2]] (g-bi-connect g n1 n2))
  ([g [n1 n2] & nds] (apply array-bi-connect (array-bi-connect g [n1 n2]) nds)))

(defn g-connect-all [g & nds]
  (apply array-bi-connect g (clojure.math.combinatorics/combinations nds 2)))

(defn g-disconnect [g n1 n2]
  (update-in g [:connections] #(del % n1 n2)))

(defn g-bi-disconnect [g n1 n2]
  (-> (g-disconnect g n1 n2)
      (g-disconnect n2 n1)))

(defn- array-bi-disconnect
  ([g [n1 n2]] (g-bi-disconnect g n1 n2))
  ([g [n1 n2] & nds] (apply array-bi-disconnect (array-bi-disconnect g [n1 n2]) nds)))

(defn g-disconnect-all [g & nds]
  (apply array-bi-disconnect g (clojure.math.combinatorics/combinations nds 2)))
