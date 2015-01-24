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
    (is (= {:k2 #{:v2}} (del (add (multimap) :k2 :v2) :k1)))
    (is (= {:k1 #{:v2}} (del (add (multimap) :k1 :v1 :k1 :v2) :k1 :v1)))
    (is (= {:k1 #{:v2}} (del (add (multimap) :k1 :v2) :k1 :v1)))
    (is (= {} (del (add (multimap) :k1 :v1 :k1 :v2) :k1 :v1 :k1 :v2)))
    (is (= {} (del (add (multimap) :k1 :v1) :k1 :v1))))
  (testing "test add sequence"
    (is (= {:k1 #{:v1 :v2}} (addseq (multimap) :k1 [:v1 :v1 :v2])))
    (is (= {:k1 #{:v1 :v2 :v3}} (addseq (multimap) :k1 [:v1 :v2 :v3]))))
  (testing "reverse multimap"
    (is (empty? (mm-reverse (multimap))))
    (is (= {:v1 #{:k1}} (mm-reverse (add (multimap) :k1 :v1))))
    (is (= {:v1 #{:k1} :v2 #{:k1}} (mm-reverse (add (multimap) :k1 :v1 :k1 :v2)))))
  (testing "mm-index"
    (is (= {true #{1 3} false #{0 2}} (mm-index odd? [0 1 2 3])))
    (is (= {true #{2 4} false #{1 3}} (mm-index (multimap) odd? (partial + 1) [0 1 2 3])))
    (is (= {true #{1 3} false #{0 2} nil #{'k}} (mm-index (add (multimap) nil 'k) odd? [0 1 2 3])))
    (is (= {:v1 #{:k1} :v2 #{:k1}} (mm-reverse (add (multimap) :k1 :v1 :k1 :v2)))))
  (testing "mm-fmap"
    (is (= {true #{4 2} false #{1 3}} (mm-fmap (partial + 1) (mm-index odd? [0 1 2 3]))))
    (is (= {true #{'(3 4) '(1 2)}, false #{'(2 3) '(0 1)}} (mm-fmap #(list % (+ 1 %)) (mm-index odd? [0 1 2 3])))))
  (testing "mm-fmap with different mapper"
    (is (= {true #{1 4 3 2} false #{0 1 3 2}} (mm-fmap #(list % (+ 1 %)) (mm-index odd? [0 1 2 3]) mapcat))))
  (testing "mm-filter"
    (is (= {true #{1 3} false #{2}} (mm-filter #(not (zero? %)) (mm-index odd? [0 1 2 3]))))
    (is (= {false #{0}} (mm-filter (partial = 0) (mm-index odd? [0 1 2 3])))))
  (testing "mm-kv-fmap"
    (is (= {true #{4 2} false #{0 2}} (mm-kv-fmap #(if %1 (+ 1 %2) %2) (mm-index odd? [0 1 2 3]))))
    (is (= {true #{'(3 4) '(1 2)}, false #{'(2 2) '(0 0)}} (mm-kv-fmap #(list %2 (if %1 (+ 1 %2) %2)) (mm-index odd? [0 1 2 3])))))
  (testing "mm-kv-fmap with different mapper"
    (is (= {true #{1 4 3 2} false #{0 2}} (mm-kv-fmap #(list %2 (if %1 (+ 1 %2) %2)) (mm-index odd? [0 1 2 3]) mapcat)))
    (is (= {true #{1 3}} (mm-kv-fmap #(if %1 (list %2) (list)) (mm-index odd? [0 1 2 3]) mapcat))))
  (testing "mm-kv-filter"
    (is (= {true #{1}} (mm-kv-filter #(and (not (= %2 3)) %1) (mm-index odd? [0 1 2 3])))))
  (testing "mm-to-map"
    (is (= {true 4 false 2} (mm-to-map (partial reduce +) (mm-index odd? [0 1 2 3])))))
  (testing "mm-seq"
    (is (= (list [:k2 :v2] [:k1 :v1]) (mm-seq (add (multimap) :k1 :v1 :k2 :v2))))
    (is (= (list [:k1 :v2] [:k1 :v1]) (mm-seq (add (multimap) :k1 :v1 :k1 :v2))))
    (is (= (list) (mm-seq (multimap)))))
  (testing "contains key"
    (is (mm-contains-key (add (multimap) :k1 :v1 :k2 :v2) :k1))
    (is (mm-contains-key (add (multimap) :k1 :v1 :k2 :v2) :k1 :k2))
    (is (not (mm-contains-key (add (multimap) :k1 :v1 :k2 :v2) :k3)))
    (is (not (mm-contains-key (add (multimap) :k1 :v1 :k2 :v2) :k1 :k2 :k3))))
  (testing "contains val"
    (is (mm-contains-val (add (multimap) :k1 :v1 :k2 :v2) :v1))
    (is (mm-contains-val (add (multimap) :k1 :v1 :k2 :v2) :v1 :v2))
    (is (not (mm-contains-val (add (multimap) :k1 :v1 :k2 :v2) :v3)))
    (is (not (mm-contains-val (add (multimap) :k1 :v1 :k2 :v2) :v1 :v2 :v3))))
  (testing "contains key-val"
    (is (mm-contains-kv (add (multimap) :k1 :v1 :k2 :v2 :k1 :v2) :k1 :v1))
    (is (mm-contains-kv (add (multimap) :k1 :v1 :k2 :v2 :k1 :v2) :k1 :v1 :k2 :v2))
    (is (not (mm-contains-kv (add (multimap) :k1 :v1 :k2 :v2 :k1 :v2) :k1 :v3)))
    (is (not (mm-contains-kv (add (multimap) :k1 :v1 :k2 :v2 :k1 :v2) :k3 :v1)))
    (is (not (mm-contains-kv (add (multimap) :k1 :v1 :k2 :v2 :k1 :v2) :k1 :v1 :k2 :v2 :k1 :v3)))
    (is (not (mm-contains-kv (add (multimap) :k1 :v1 :k2 :v2 :k1 :v2) :k1 :v1 :k2 :v2 :k3 :v1)))))

