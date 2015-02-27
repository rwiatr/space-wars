(ns spatial.spattree-test
  (:require [clojure.test :refer :all]
            [spatial.spattree :refer :all]
            [geom.bbox :refer :all]))

(deftest test.node
  (testing "creation"
    (is (= {:sub #{}} (create-node)))
    (is (thrown-with-msg? Exception #"bbox can't be nil" (create-node {:v :a})))
    (is (= {:sub #{{:val 'value :bbox (bbox 0 0 100 100)}}, :bbox (bbox 0 0 100 100)}
           (create-node {:val 'value :bbox (bbox 0 0 100 100)})))
    (is (= {:sub #{{:val 'v1 :bbox (bbox 0 0 100 100)}
                   {:val 'v2 :bbox (bbox -150 -10 -5 -5)}}, :bbox (bbox -150 -10 250 110)}
           (create-node {:val 'v1 :bbox (bbox 0 0 100 100)}
                        {:val 'v2 :bbox (bbox -150 -10 -5 -5)}))))
  (testing "modification"
    (is (= {:sub #{{:val 'value :bbox (bbox 0 0 100 100)}}, :bbox (bbox 0 0 100 100)}
           (-> (create-node) (into-node {:val 'value :bbox (bbox 0 0 100 100)}))))
    (is (= {:sub #{{:val 'v1 :bbox (bbox 0 0 100 100)}
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
    (is (= [{:bbox (bbox 0 0 110 110),
             :sub
             #{{:bbox (bbox 0 10 100 100)}
               {:bbox (bbox 10 10 100 100)}
               {:bbox (bbox 0 0 100 100)}
               {:bbox (bbox 0 0 100 10)}}}
            {:bbox (bbox 0 0 10 10),
             :sub
             #{{:bbox (bbox 0 0 10 5)}
               {:bbox (bbox 0 0 10 10)}}}]
           (split-node (create-node {:bbox (bbox 0 0 10 10)} {:bbox (bbox 10 10 100 100)}
                                    {:bbox (bbox 0 0 10 5)} {:bbox (bbox 0 10 100 100)}
                                    {:bbox (bbox 0 0 10 10)} {:bbox (bbox 0 0 100 100)}
                                    {:bbox (bbox 0 0 100 10)} {:bbox (bbox 0 0 100 100)}))))
    (is (= [{:bbox (bbox-xy 5 0 30 50),
             :sub
             #{{:bbox (bbox-xy 5 0 15 50)}
               {:bbox (bbox-xy 20 0 30 50)}}}
            {:bbox (bbox-xy 0 10 35 20),
             :sub
             #{{:bbox (bbox-xy 0 10 15 20)}
               {:bbox (bbox-xy 20 10 35 20)}}}]
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
             (rebuild new-node old-node path 3))))))


