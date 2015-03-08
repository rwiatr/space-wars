(ns graph.mapgraph-test
  (:require [clojure.test :refer :all]
            [graph.mapgraph :refer :all]))

(deftest test.g-empty
  (testing "is empty when no elements added"
    (is (g-empty? (graph))))
  (testing "after adding to graph its not empty"
    (is (not (-> (graph) (g-add :a) g-empty?)))))

(deftest test.g-empty
  (testing "is empty when no elements added"
    (is (empty? (g-nodes (graph)))))
  (testing "after adding to graph its not empty"
    (is (= #{:a} (-> (graph) (g-add :a) g-nodes)))))

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
  (testing "self connection is two way"
    (is (-> (graph) (g-connect :a :a) (g-contains? :a)))
    (is (-> (graph) (g-connect :a :a) (g-connected? :a :a)))
    (is (-> (graph) (g-connect :a :a) (g-bi-connected? :a :a))))
  (testing "connection adds one way connection to graph"
    (is (-> (graph) (g-connect :a :b) (g-connected? :a :b)))
    (is (not (-> (graph) (g-connect :a :b) (g-bi-connected? :a :b))))
    (is (not (-> (graph) (g-connect :a :b) (g-connected? :b :a))))))

(deftest test.g-connect-to-many
  (testing "no nodes to connect"
    (is (-> (graph) (g-connect-to-many :a) (g-contains? :a))))
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

(deftest test.g-disconnect
  (testing "disconnect does not remove nodes from graph"
    (is (-> (graph) (g-connect :a :b) (g-disconnect :a :b) (g-contains? :a)))
    (is (-> (graph) (g-connect :a :b) (g-disconnect :a :b) (g-contains? :b)))
    (is (not (-> (graph) (g-connect :a :b) (g-disconnect :c :b) (g-contains? :c)))))
  (testing "disconnect removes one way connection"
    (is (not (-> (graph) (g-connect :a :b) (g-disconnect :a :b) (g-connected? :a :b))))
    (is (-> (graph) (g-bi-connect :a :b) (g-disconnect :a :b) (g-connected? :b :a)))
    (is (-> (graph) (g-bi-connect :a :b) (g-disconnect :a :c) (g-bi-connected? :a :b)))))

(deftest test.g-bi-disconnect
  (testing "bi-disconnect does not remove nodes from graph"
    (is (-> (graph) (g-connect :a :b) (g-bi-disconnect :a :b) (g-contains? :a)))
    (is (-> (graph) (g-connect :a :b) (g-bi-disconnect :a :b) (g-contains? :b))))
  (testing "disconnect removes two way connection"
    (is (not (-> (graph) (g-bi-connect :a :b) (g-bi-disconnect :a :b) (g-connected? :a :b))))
    (is (not (-> (graph) (g-bi-connect :a :b) (g-bi-disconnect :a :b) (g-connected? :b :a))))
    (is (-> (graph) (g-bi-connect :a :b) (g-bi-disconnect :c :b) (g-bi-connected? :a :b)))))

(deftest test.g-disconnect-all
  (testing "disconnect does not remove nodes from graph"
    (is (-> (graph) (g-connect-all :a :b) (g-disconnect-all :a :b) (g-contains? :a :b)))
    (is (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :d) (g-contains? :a :b :c :d)))
    (is (-> (graph) (g-connect-all :a :b) (g-disconnect-all :a :f) (g-contains? :a :b))))
  (testing "disconnect removes all two way connection"
    (is (not (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :a :b))))
    (is (not (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :a :c))))
    (is (not (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :c :b))))
    (is (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :d :a)))
    (is (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :d :b)))
    (is (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :d :c)))
    (is (not (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :a :f))))
    (is (not (-> (graph) (g-connect-all :a :b :c :d) (g-disconnect-all :a :b :c :f) (g-connected? :d :f))))))

(deftest test.g-connected-any?
  (testing "not existing is disconected"
    (is (not (-> (graph) (g-connected-any? :a :b))))
    (is (not (-> (graph) (g-connect :a :c) (g-connected-any? :a :b)))))
  (testing "two way and one way connections are valid"
    (is (-> (graph) (g-connect :a :b) (g-connected-any? :a :b)))
    (is (-> (graph) (g-connect :b :a) (g-connected-any? :a :b)))
    (is (-> (graph) (g-bi-connect :a :b) (g-connected-any? :a :b)))
    (is (-> (graph) (g-bi-connect :a :b) (g-connected-any? :b :a)))))

(deftest test.props
  (testing "get property when it is not set or deleted"
    (is (nil? (-> (graph) (g-get-prop :a :pk))))
    (is (nil? (-> (graph) (g-add :a) (g-get-prop :a :pk))))
    (is (nil? (-> (graph) (g-add :a) (g-add-prop :a :pk :v) (g-del-prop :a :pk) (g-get-prop :a :pk))))
    (is (nil? (-> (graph) (g-add :b) (g-add-prop :b :pk :v) (g-get-prop :a :pk)))))
  (testing "add and get property"
    (is (= :v1 (-> (graph) (g-add :a) (g-add-prop :a :pk :v1) (g-get-prop :a :pk))))
    (is (= :v2 (-> (graph) (g-add :a) (g-add-prop :a :pk :v1) (g-add-prop :a :pk :v2) (g-get-prop :a :pk))))))

(deftest test.rem
  (testing "node is deleted from :nodes"
    (is (not (-> (graph) (g-add :a :b) (g-rem :a) (g-contains? :a))))
    (is (-> (graph) (g-add :a :b) (g-rem :a) (g-contains? :b)))
    (is (-> (graph) (g-add :a) (g-rem :c) (g-contains? :a))))
  (testing "node is deleted from properties"
    (is (nil? (-> (graph) (g-add :a :b) (g-add-prop :a :key :val) (g-rem :a) (g-get-prop :a :key))))
    (is (-> (graph) (g-add :a :b) (g-add-prop :a :key :val) (g-rem :b) (g-get-prop :a :key))))
  (testing "node is deleted from :connections"
    (is (not (-> (graph) (g-connect :a :b) (g-rem :a) (g-connected? :a :b))))
    (is (not (-> (graph) (g-connect :a :b) (g-rem :a) (g-connected? :b :a))))
    (is (not (-> (graph) (g-bi-connect :a :b) (g-rem :a) (g-connected? :a :b))))
    (is (not (-> (graph) (g-bi-connect :a :b) (g-rem :a) (g-connected? :b :a))))
    (is (-> (graph) (g-bi-connect :a :b) (g-bi-connect :a :c) (g-rem :b) (g-connected? :a :c)))
    (is (-> (graph) (g-bi-connect :a :b) (g-bi-connect :a :c) (g-rem :b) (g-connected? :c :a)))))
