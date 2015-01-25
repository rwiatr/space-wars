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
    (is (-> (graph) (g-connect :a :b) (g-contains? :a :b)))
    (is (-> (graph) (g-connect :a :b) (g-connect :a :c) (g-contains? :a :b :c))))
  (testing "connection adds one way connection to graph"
    (is (-> (graph) (g-connect :a :b) (g-connected? :a :b)))
    (is (not (-> (graph) (g-connect :a :b) (g-bi-connected? :a :b))))
    (is (not (-> (graph) (g-connect :a :b) (g-connected? :b :a))))))

(deftest test.g-connect-to-many
  (testing "connect-to-many adds nodes to graph"
    (is (-> (graph) (g-connect-to-many :a :b :c :d) (g-contains? :a :b :c :d))))
    (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-contains? :a :b :c :d :e)))
  (testing "connect-to-many adds one way connection to graph"
    (is (-> (graph) (g-connect-to-many :a :b :c) (g-connected? :a :b)))
    (is (-> (graph) (g-connect-to-many :a :b :c) (g-connected? :a :c)))
    (is (not (-> (graph) (g-connect-to-many :a :b :c) (g-connected? :b :c))))
    (is (not (-> (graph) (g-connect-to-many :a :b :c) (g-bi-connected? :a :b))))
    (is (not (-> (graph) (g-connect-to-many :a :b :c) (g-bi-connected? :a :c)))))
  (testing "connect-to-many does not overide existing connections"
    (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :b)))
    (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :c)))
    (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :d)))
    (is (-> (graph) (g-connect-to-many :a :b :c) (g-connect-to-many :a :d :e) (g-connected? :a :e)))))

(deftest test.g-bi-connect
  (testing "connection adds nodes to graph"
    (is (-> (graph) (g-bi-connect :a :b) (g-contains? :a :b)))
    (is (-> (graph) (g-bi-connect :a :b :c :d) (g-contains? :a :b :c :d)))
    (is (-> (graph) (g-bi-connect :a :b) (g-bi-connect :a :c) (g-contains? :a :b :c))))
  (testing "bi-connect adds two way connection to graph"
    (is (-> (graph) (g-bi-connect :a :b) (g-bi-connected? :a :b)))
    (is (-> (graph) (g-bi-connect :a :b :c :d) (g-bi-connected? :a :b)))
    (is (-> (graph) (g-bi-connect :a :b :c :d) (g-bi-connected? :c :d)))
    (is (not (-> (graph) (g-bi-connect :a :b :c :d) (g-connected? :c :b))))))

(deftest test.g-connect-all
  (testing "connect-all adds nodes to graph"
    (is (-> (graph) (g-connect-all :a :b) (g-contains? :a :b)))
    (is (-> (graph) (g-connect-all :a :b :c :d) (g-contains? :a :b :c :d)))
    (is (-> (graph) (g-connect-all :a :b) (g-connect-all :a :c) (g-contains? :a :b :c))))
  (testing "connectes all nodes with two way connections"
    (is (-> (graph) (g-connect-all :a :b) (g-bi-connected? :a :b)))
    (is (-> (graph) (g-connect-all :a :b :c) (g-bi-connected? :a :b)))
    (is (-> (graph) (g-connect-all :a :b :c) (g-bi-connected? :a :c)))
    (is (-> (graph) (g-connect-all :a :b :c) (g-bi-connected? :b :c)))))
