(ns util.set_multimap-test
  (:require [clojure.test :refer :all]
            [util.set_multimap :refer :all]))

(deftest test.set-multimap
  (testing "creates empty multimap"
    (is (empty? (multimap))))
  (testing "nil behaviour"
    (is (= {:k1 #{nil}} (add (multimap) :k1 nil)))
    (is (= {} (del (add (multimap) :k1 nil) :k1 nil)))
    (is (= {nil #{:v1}} (add (multimap) nil :v1)))
    (is (= {} (del (add (multimap) nil :v1) nil :v1)))
    (is (= {nil #{nil}} (add (multimap) nil nil))))
  (testing "add one element to mulimap"
    (is (= 1 (count (add (multimap) :k1 :v1))))
    (is (= #{:v1} (:k1 (add (multimap) :k1 :v1)))))
  (testing "add two elements to mulimap"
    (is (= 2 (count (add (multimap) :k1 :v1 :k2 :v2))))
    (is (= 1 (count (add (multimap) :k1 :v1 :k1 :v2))))
    (is (= #{:v1} (:k1 (add (multimap) :k1 :v1 :k2 :v2))))
    (is (= #{:v2} (:k2 (add (multimap) :k1 :v1 :k2 :v2))))
    (is (= #{:v1 :v2} (:k1 (add (multimap) :k1 :v1 :k1 :v2)))))
  (testing "test delete"
    (is (= {} (del (multimap) :k1)))
    (is (= {} (del (add (multimap) :k1 :v1) :k1)))
    (is (= {:k2 #{:v2}} (del (add (multimap) :k1 :v1 :k2 :v2) :k1)))
    (is (= {:k1 #{:v2}} (del (add (multimap) :k1 :v1 :k1 :v2) :k1 :v1)))
    (is (= {} (del (add (multimap) :k1 :v1) :k1 :v1))))
  (testing "test add sequence"
    (is (= {:k1 #{:v1 :v2}} (addseq (multimap) :k1 [:v1 :v1 :v2])))
    (is (= {:k1 #{:v1 :v2 :v3}} (addseq (multimap) :k1 [:v1 :v2 :v3])))))
