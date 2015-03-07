(ns geom.edge_test
  (:require [clojure.test :refer :all]
            [geom.edge :refer :all]
            [geom.point :refer :all]
            [graph.bowyer-watson :refer :all]))

(deftest test.edge
  (testing "edge contains ordered points"
    (let [e (edge (point 1 0) (point 0 0))]
      (is (and (= (point 0 0) (:p1 e))
               (= (point 1 0) (:p2 e))))))
  (testing "edge returns nil when points are the same"
    (is (nil? (edge (point 0 0) (point 0 0)))))
  (testing "equal points give equal edges"
    (is (= (edge (point 1 0) (point 0 0))
           (edge (point 1 0) (point 0 0)))))
  (testing "equal points in different oder give equal edges"
    (is (= (edge (point 1 0) (point 0 0))
           (edge (point 0 0) (point 1 0)))))
  (testing "different points give different edges"
    (is (not (= (edge (point 1 0) (point 0 0))
                (edge (point 0 1) (point 0 0))))))
  (testing "different points give different edges"
    (is (not (= (edge (point 0 0) (point 1 0))
                (edge (point 0 0) (point 0 1)))))))
