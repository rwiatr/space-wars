(ns graph.traversal-test
  (:require [clojure.test :refer :all]
            [graph.traversal :refer :all]))


(deftest test.breath-first
  (testing "one element graph"
    (is (= '(a) (breath-first-seq 'a {} :nodes-only)))
    (is (= '([a nil]) (breath-first-seq 'a {} :predecessors))))
  (testing "two element directed graph"
    (is (= '(a b) (breath-first-seq 'a {'a '(b)} :nodes-only)))
    (is (= '([a nil] [b a]) (breath-first-seq 'a {'a '(b)} :predecessors))))
  (testing "two element undirected graph"
    (is (= '(a b) (breath-first-seq 'a {'a '(b) 'b '(a)} :nodes-only)))
    (is (= '([a nil] [b a]) (breath-first-seq 'a {'a '(b) 'b '(a)} :predecessors))))
  (testing "three element directed graph"
    (is (= '(a b c) (breath-first-seq 'a {'a '(b) 'b '(c)} :nodes-only)))
    (is (= '([a nil] [b a] [c b]) (breath-first-seq 'a {'a '(b) 'b '(c)} :predecessors)))
    (is (= '(a b c) (breath-first-seq 'a {'a '(b c)} :nodes-only)))
    (is (= '([a nil] [b a] [c a]) (breath-first-seq 'a {'a '(b c)} :predecessors)))))
