(ns graph.mapgraph-test
  (:require [clojure.test :refer :all]
            [graph.mapgraph :refer :all]))

(deftest test.mapgraph
  (testing "is empty when no elements added"
    (is (empty? (graph))))
  (testing "conencting two elments"
    (is (= {'a #{'b}} (connect (graph) 'a 'b))))
  (testing "conencting multiple elments"
    (is (= {'a #{'b} 'b #{'c}} (connect (graph) 'a 'b 'b 'c)))
    (is (= {'a #{'b} 'b #{'c}} (connect-seq (graph) ['a 'b 'b 'c])))))
