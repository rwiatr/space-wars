(ns geom.circle_test
  (:require [clojure.test :refer :all]
            [geom.circle :refer :all]))

(deftest test.circle
  (testing "equal"
    (is (= (circle 15 16) (circle 15 16))))
  (testing "not equal when p differs"
    (is (not (= (circle 16 16) (circle 15 16)))))
  (testing "not equal when equal r differs"
     (is (not (= (circle 15 17) (circle 15 16))))))
