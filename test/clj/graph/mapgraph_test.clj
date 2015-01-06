(ns graph.traversal-test
  (:require [clojure.test :refer :all]
            [graph.traversal :refer :all]))

(deftest test.mapgraph
  (testing "is empty when no elements added"
    (is (empty? (graph))))
  (testing "connecting all conected"
    (is (= {'a '(b c) 'b '(a c) 'c '(a b)} (connect-all (graph) '(a b c)))))
  (testing "conencting one elment"
    (is (= {'a '()} (connect-all (graph) '(a)))))
  (testing "conencting no elements"
    (is (empty? (connect-all (graph) '())))))
