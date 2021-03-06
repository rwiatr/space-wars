(ns spatial.spattree-test
  (:require [clojure.test :refer :all]
            [spatial.spattree :refer :all]
            [geom.bbox :refer :all]
            [util.set_multimap :refer :all]))

(deftest test.node
  (testing "creation"
    (is (= {:total-leafs 0 :sub #{}} (create-node)))
    (is (thrown-with-msg? Exception #"bbox can't be nil" (create-node {:v :a})))
    (is (= {:total-leafs 1 :sub #{{:val 'value :bbox (bbox 0 0 100 100)}}, :bbox (bbox 0 0 100 100)}
           (create-node {:val 'value :bbox (bbox 0 0 100 100)})))
    (is (= {:total-leafs 2 :sub #{{:val 'v1 :bbox (bbox 0 0 100 100)}
                                  {:val 'v2 :bbox (bbox -150 -10 -5 -5)}}, :bbox (bbox -150 -10 250 110)}
           (create-node {:val 'v1 :bbox (bbox 0 0 100 100)}
                        {:val 'v2 :bbox (bbox -150 -10 -5 -5)}))))
  (testing "modification"
    (is (= {:total-leafs 1 :sub #{{:val 'value :bbox (bbox 0 0 100 100)}}, :bbox (bbox 0 0 100 100)}
           (-> (create-node) (into-node {:val 'value :bbox (bbox 0 0 100 100)}))))
    (is (= {:total-leafs 2 :sub #{{:val 'v1 :bbox (bbox 0 0 100 100)}
                                  {:val 'v2 :bbox (bbox -150 -10 -5 -5)}}, :bbox (bbox -150 -10 250 110)}
           (-> (create-node) (into-node {:val 'v1 :bbox (bbox 0 0 100 100)}
                                        {:val 'v2 :bbox (bbox -150 -10 -5 -5)}))))))

(deftest test.cost-fn
  (testing  "cost"
    (is (= 2050 (cost-fn {:bbox (bbox 0 0 10 5)} {:bbox (bbox 10 10 100 100)})))
    (is (= 2000 (cost-fn {:bbox (bbox 0 0 10 10)} {:bbox (bbox 10 10 100 100)})))
    (is (= 950 (cost-fn {:bbox (bbox 0 0 10 5)} {:bbox (bbox 0 10 100 100)})))
    (is (= -100 (cost-fn {:bbox (bbox 0 0 10 10)} {:bbox (bbox 0 0 100 100)})))
    (is (= -1000 (cost-fn {:bbox (bbox 0 0 100 10)} {:bbox (bbox 0 0 100 100)})))))

(deftest test.choose-seeds
  (testing "chooste a pair with the biggest cost-fn"
    (is (= [{:bbox (bbox 10 10 100 100)} {:bbox (bbox 0 0 10 5)}]
           (choose-seeds [{:bbox (bbox 0 0 10 10)} {:bbox (bbox 10 10 100 100)}
                          {:bbox (bbox 0 0 10 5)} {:bbox (bbox 0 10 100 100)}
                          {:bbox (bbox 0 0 10 10)} {:bbox (bbox 0 0 100 100)}
                          {:bbox (bbox 0 0 100 10)} {:bbox (bbox 0 0 100 100)}]))))
  (testing "cost-fn with bad input"
    (is (thrown-with-msg? Exception #"Can't build any pairs from input \[\]"
                          (choose-seeds [])))
    (is (thrown-with-msg? Exception #"Can't build any pairs from input \[\{:bbox #geom.bbox.Bbox\{:x 0, :y 0, :width 10, :height 10\}\}\]"
                          (choose-seeds [{:bbox (bbox 0 0 10 10)}])))))

(deftest test.split-node
  (testing "split node into smaller pieces"
    (is (= [(create-node {:bbox (bbox-xy 0 10 15 20)})
            (create-node {:bbox (bbox-xy 5 20 15 50)})]
           (split-node (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 5 20 15 50)}))))
    (is (= [(create-node {:bbox (bbox 0 10 100 100)}
                         {:bbox (bbox 10 10 100 100)}
                         {:bbox (bbox 0 0 100 100)}
                         {:bbox (bbox 0 0 100 10)})
            (create-node {:bbox (bbox 0 0 10 5)}
                         {:bbox (bbox 0 0 10 10)})]
           (split-node (create-node {:bbox (bbox 0 0 10 10)} {:bbox (bbox 10 10 100 100)}
                                    {:bbox (bbox 0 0 10 5)} {:bbox (bbox 0 10 100 100)}
                                    {:bbox (bbox 0 0 10 10)} {:bbox (bbox 0 0 100 100)}
                                    {:bbox (bbox 0 0 100 10)} {:bbox (bbox 0 0 100 100)}))))
    (is (= [(create-node {:bbox (bbox-xy 5 0 15 50)}
                         {:bbox (bbox-xy 20 0 30 50)})
            (create-node {:bbox (bbox-xy 0 10 15 20)}
                         {:bbox (bbox-xy 20 10 35 20)})]
           (split-node (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 5 0 15 50)}
                                    {:bbox (bbox-xy 20 10 35 20)} {:bbox (bbox-xy 20 0 30 50)}))))))

(deftest test.rebuild
  (testing "splitting root"
    (is (= (create-node
            (create-node {:bbox (bbox-xy 5 0 15 50)} {:bbox (bbox-xy 20 0 30 50)})
            (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 20 10 35 20)}))
           (rebuild (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 5 0 15 50)}
                                 {:bbox (bbox-xy 20 10 35 20)} {:bbox (bbox-xy 20 0 30 50)})
                    (create-node)
                    (list)
                    3))))
  (testing "don't split root"
    (is (= (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 5 0 15 50)}
                        {:bbox (bbox-xy 20 10 35 20)} {:bbox (bbox-xy 20 0 30 50)})
           (rebuild (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 5 0 15 50)}
                                 {:bbox (bbox-xy 20 10 35 20)} {:bbox (bbox-xy 20 0 30 50)})
                    (create-node)
                    (list)
                    4))))
  (testing "splits new node and rebuild path"
    (is (= (create-node
            (create-node {:bbox (bbox -5 1 20 20)})
            (create-node {:bbox (bbox-xy 5 0 15 50)} {:bbox (bbox-xy 20 0 30 50)})
            (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 20 10 35 20)}))
           (let [new-node (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 5 0 15 50)}
                                       {:bbox (bbox-xy 20 10 35 20)} {:bbox (bbox-xy 20 0 30 50)})
                 old-node (create-node {:bbox (bbox-xy 0 10 15 20)})
                 path (list (create-node old-node (create-node {:bbox (bbox -5 1 20 20)})))]
             (rebuild new-node old-node path 3)))))
  (testing "don't splits new node and rebuild path"
    (is (= (create-node (create-node {:bbox (bbox -5 1 20 20)})
                        (create-node {:bbox (bbox-xy 0 10 15 20)}
                                     {:bbox (bbox-xy 5 0 15 50)}))
           (let [new-node (create-node {:bbox (bbox-xy 0 10 15 20)} {:bbox (bbox-xy 5 0 15 50)})
                 old-node (create-node {:bbox (bbox-xy 0 10 15 20)})
                 path (list (create-node old-node (create-node {:bbox (bbox -5 1 20 20)})))]
             (rebuild new-node old-node path 3))))))

(deftest test.insert
  (testing "insertion into empty root node"
    (is (= (create-node {:bbox (bbox -5 1 20 20) :value 1})
           (insert (create-node) {:bbox (bbox -5 1 20 20) :value 1} 3))))
  (testing "insertion into root node with values and does not split"
    (is (= (create-node {:bbox (bbox 5 0 15 50) :value 1}
                        {:bbox (bbox -5 1 20 20) :value 2})
           (insert (create-node {:bbox (bbox 5 0 15 50) :value 1})
                   {:bbox (bbox -5 1 20 20) :value 2} 3)))
    (is (= (create-node {:bbox (bbox 5 0 15 50) :value 1}
                        {:bbox (bbox -5 1 20 20) :value 2}
                        {:bbox (bbox -5 -10 20 20) :value 3})
           (insert (create-node {:bbox (bbox 5 0 15 50) :value 1}
                                {:bbox (bbox -5 1 20 20) :value 2})
                   {:bbox (bbox -5 -10 20 20) :value 3} 3))))
  (testing "insertion into root node with values and splits the node"
    (is (= (create-node (create-node {:bbox (bbox -5 1 20 20) :value 2}
                                     {:bbox (bbox -5 -10 20 20) :value 3})
                        (create-node {:bbox (bbox 5 0 15 50) :value 1}
                                     {:bbox (bbox 0 0 10 25) :value 4}))
           (insert (create-node {:bbox (bbox 5 0 15 50) :value 1}
                                {:bbox (bbox -5 1 20 20) :value 2}
                                {:bbox (bbox -5 -10 20 20) :value 3})
                   {:bbox (bbox 0 0 10 25) :value 4} 3))))
  (testing "insertion into child node with values and does not split"
    (is (= (create-node (create-node {:bbox (bbox -5 1 20 20) :value 2}
                                     {:bbox (bbox -5 -10 20 20) :value 3})
                        (create-node {:bbox (bbox 5 0 15 50) :value 1}
                                     {:bbox (bbox 0 0 10 25) :value 4}
                                     {:bbox (bbox 25 5 2 2) :value 5}))
           (insert (create-node (create-node {:bbox (bbox -5 1 20 20) :value 2}
                                             {:bbox (bbox -5 -10 20 20) :value 3})
                                (create-node {:bbox (bbox 5 0 15 50) :value 1}
                                             {:bbox (bbox 0 0 10 25) :value 4}))
                   {:bbox (bbox 25 5 2 2) :value 5} 3)))))

(deftest test.successors-interacting-bbox
  (testing "returns all children containing the bbox or an empty list"
    (is (empty? (successors-interacting-bbox (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                          {:bbox (bbox 5 -10 20 20) :value 2})
                                             contains-bbox?
                                             {:bbox (bbox -50 1 20 20)})))
    (is (= (list {:bbox (bbox -5 1 20 20) :value 1})
           (successors-interacting-bbox contains-bbox?
                                        (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                     {:bbox (bbox 5 -10 20 20) :value 2})
                                        {:bbox (bbox -5 1 20 20)})))
    (is (= (list {:bbox (bbox -5 1 20 20) :value 1})
           (successors-interacting-bbox contains-bbox?
                                        (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                     {:bbox (bbox 5 -10 20 20) :value 2})
                                        {:bbox (bbox 0 1 15 20)})))
    (is (= #{{:bbox (bbox -5 1 20 20) :value 1}
             {:bbox (bbox 5 -10 20 20) :value 2}}
           (into #{} (successors-interacting-bbox contains-bbox?
                                                  (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                               {:bbox (bbox 5 -10 20 20) :value 2})
                                                  {:bbox (bbox 5 1 10 5)}))))))

(deftest test.leafs-interacting-bbox
  (testing "returns only leafs that contain bbox or nothing"
    (is (empty? (leafs-interacting-bbox (create-node (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                                  {:bbox (bbox 5 -10 20 20) :value 2}))
                                        contains-bbox?
                                        {:bbox (bbox -50 1 20 20)})))
    (is (= (list {:bbox (bbox -5 1 20 20) :value 1})
           (leafs-interacting-bbox contains-bbox?
                                   (create-node (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                             {:bbox (bbox 5 -10 20 20) :value 2}))
                                   {:bbox (bbox -5 1 20 20)})))
    (is (= (list {:bbox (bbox -5 1 20 20) :value 1})
           (leafs-interacting-bbox contains-bbox?
                                   (create-node (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                             {:bbox (bbox 5 -10 20 20) :value 2}))
                                   {:bbox (bbox 0 1 15 20)})))
    (is (= #{{:bbox (bbox -5 1 20 20) :value 1}
             {:bbox (bbox 5 -10 20 20) :value 2}}
           (into #{} (leafs-interacting-bbox contains-bbox?
                                             (create-node (create-node {:bbox (bbox -5 1 20 20) :value 1}
                                                                       {:bbox (bbox 5 -10 20 20) :value 2}))
                                             {:bbox (bbox 5 1 10 5)}))))))

(deftest test.tree
  (testing "creation"
    (is (= {:root (create-node), :split-size 5, :node-factory-fn identity}
           (tree)))
    (is (= {:root (create-node), :split-size 10, :node-factory-fn zero?}
           (tree :node-factory-fn zero? :split-size 10))))
  (testing "add value"
    (is (= {:root (into-node (create-node) {:bbox (bbox 5 0 15 50) :value 1}), :split-size 5, :node-factory-fn identity}
           (tree-add (tree) {:bbox (bbox 5 0 15 50) :value 1}))))
  (testing "breath first seq"
    (is (= (mm-index (multimap) first second
                     [[(bbox 1 0 19 50) 0]
                      [(bbox 1 0 16 50) 1] [(bbox 3 0 17 50) 1]
                      [(bbox 5 0 15 50) 2] [(bbox 2 0 15 50) 2] [(bbox 1 0 15 50) 2] [(bbox 3 0 15 50) 2] [(bbox 4 0 15 50) 2]])
           (mm-index (multimap) first second
                     (tree-breath-first-bbox-seq (tree-add (tree :split-size 3)
                                                           {:bbox (bbox 1 0 15 50) :value 1}
                                                           {:bbox (bbox 2 0 15 50) :value 2}
                                                           {:bbox (bbox 3 0 15 50) :value 3}
                                                           {:bbox (bbox 4 0 15 50) :value 4}
                                                           {:bbox (bbox 5 0 15 50) :value 5}))))))
  (testing "get values containing bbox or nothing"
    (is (= #{{:bbox (bbox-xy 20 20 25 25) :value 1}
             {:bbox (bbox-xy 23 23 30 30) :value 2}}
           (into #{} (tree-values-containing (tree-add (tree)
                                                       {:bbox (bbox-xy 20 20 25 25) :value 1}
                                                       {:bbox (bbox-xy 23 23 30 30) :value 2}
                                                       {:bbox (bbox-xy -23 -23 -30 -30) :value 3}
                                                       {:bbox (bbox-xy -20 -20 -30 -30) :value 4}
                                                       {:bbox (bbox-xy 20 20 23 23) :value 5})
                                             {:bbox (bbox-xy 23 23 24 24)}))))
    (is (empty? (tree-values-containing (tree-add (tree)
                                                  {:bbox (bbox-xy 20 20 25 25) :value 1}
                                                  {:bbox (bbox-xy 23 23 30 30) :value 2}
                                                  {:bbox (bbox-xy -23 -23 -30 -30) :value 3}
                                                  {:bbox (bbox-xy -20 -20 -30 -30) :value 4}
                                                  {:bbox (bbox-xy 20 20 23 23) :value 2})
                                        {:bbox (bbox-xy 23 23 24 240)}))))
  (testing "get values intersecting bbox or nothing"
    (is (= #{{:bbox (bbox-xy 20 20 25 25) :value 1}
             {:bbox (bbox-xy 23 23 30 30) :value 2}}
           (into #{} (tree-values-intersecting (tree-add (tree)
                                                         {:bbox (bbox-xy 20 20 25 25) :value 1}
                                                         {:bbox (bbox-xy 23 23 30 30) :value 2}
                                                         {:bbox (bbox-xy -23 -23 -30 -30) :value 3}
                                                         {:bbox (bbox-xy -20 -20 -30 -30) :value 4}
                                                         {:bbox (bbox-xy 20 20 23 23) :value 5})
                                               {:bbox (bbox-xy 23 23 24 24)}))))
    (is (= #{{:bbox (bbox-xy 20 20 25 25) :value 1}
             {:bbox (bbox-xy 23 23 30 30) :value 2}}
           (into #{} (tree-values-intersecting (tree-add (tree)
                                                         {:bbox (bbox-xy 20 20 25 25) :value 1}
                                                         {:bbox (bbox-xy 23 23 30 30) :value 2}
                                                         {:bbox (bbox-xy -23 -23 -30 -30) :value 3}
                                                         {:bbox (bbox-xy -20 -20 -30 -30) :value 4}
                                                         {:bbox (bbox-xy 20 20 23 23) :value 5})
                                               {:bbox (bbox-xy 23 23 24 240)}))))
    (is (empty? (tree-values-intersecting (tree-add (tree)
                                                    {:bbox (bbox-xy 20 20 25 25) :value 1}
                                                    {:bbox (bbox-xy 23 23 30 30) :value 2}
                                                    {:bbox (bbox-xy -23 -23 -30 -30) :value 3}
                                                    {:bbox (bbox-xy -20 -20 -30 -30) :value 4}
                                                    {:bbox (bbox-xy 20 20 23 23) :value 5})
                                          {:bbox (bbox-xy 230 230 240 240)})))))
