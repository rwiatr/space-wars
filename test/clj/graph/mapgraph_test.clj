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

(deftest test.g-connect
  (testing "connection adds nodes to graph"
    (is (is (-> (graph) (g-connect :a :b) (g-contains? :a :b))))
    (is (is (-> (graph) (g-connect :a :b) (g-connect :a :c) (g-contains? :a :b :c)))))
  (testing "connection adds one way connection to graph"
    (is (is (-> (graph) (g-connect :a :b) (g-connected? :a :b))))
    (is (is (not (-> (graph) (g-connect :a :b) (g-bi-connected? :a :b)))))
    (is (is (not (-> (graph) (g-connect :a :b) (g-connected? :b :a)))))))

(deftest test.g-connect-to-many
    (testing "connect-to-many adds nodes to graph"
    (is (is (-> (graph) (g-connect-to-many :a :b :c :d) (g-contains? :a :b :c :d))))
    (is (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-contains? :a :b :c :d :e)))))
  (testing "connect-to-many adds one way connection to graph"
    (is (is (-> (graph) (g-connect-to-many :a :b :c) (g-connected? :a :b))))
    (is (is (-> (graph) (g-connect-to-many :a :b :c) (g-connected? :a :c))))
    (is (is (not (-> (graph) (g-connect-to-many :a :b :c) (g-connected? :b :c)))))
    (is (is (not (-> (graph) (g-connect-to-many :a :b :c) (g-bi-connected? :a :b)))))
    (is (is (not (-> (graph) (g-connect-to-many :a :b :c) (g-bi-connected? :a :c))))))
  (testing "connect-to-many does not overide existing connections"
    (is (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :b))))
    (is (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :c))))
    (is (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :d))))
    (is (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :e))))))
