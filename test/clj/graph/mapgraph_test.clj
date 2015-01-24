(ns graph.mapgraph-test
  (:require [clojure.test :refer :all]
            [graph.mapgraph :refer :all]))

(deftest test.g-empty
  (testing "is empty when no elements added"
    (is (g-empty? (graph))))
  (testing "after adding to graph its not empty"
    (is (not (-> (graph) (g-add :a) g-empty?)))))

(deftest test.g-contains?
  (testing "empty graph does not contain node"
    (is (not (g-contains? (graph) :a)))
    (is (not (g-contains? (graph) :a :b :c))))
  (testing "after adding to graph it contains only added items"
    (is (-> (graph) (g-add :a) (g-contains? :a)))
    (is (-> (graph) (g-add :a :b) (g-contains? :a :b)))
    (is (not (-> (graph) (g-add :a :b) (g-contains? :a :b :c))))
    (is (not (-> (graph) (g-add :a :b) (g-contains? :c))))))

