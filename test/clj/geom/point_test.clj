(ns geom.point_test
  (:require [clojure.test :refer :all]
            [geom.point :refer :all]))

(deftest test.point2d
  (testing "compareTo when equal"
    (is (= 0 (.compareTo (point 15 16) (point 15 16))))
    (is (= 0 (.compareTo (point Long/MIN_VALUE Long/MIN_VALUE) (point Long/MIN_VALUE Long/MIN_VALUE)))))
  (testing "compareTo when equal x is bigger"
    (is (= 1 (.compareTo (point 16 16) (point 15 16)))))
  (testing "compareTo when equal x is smaller"
    (is (= -1 (.compareTo (point 14 16) (point 15 16)))))
  (testing "compareTo when equal y is bigger"
    (is (= 1 (.compareTo (point 15 17) (point 15 16)))))
  (testing "compareTo when equal y is smaller"
    (is (= -1 (.compareTo (point 15 15) (point 15 16))))))

(deftest test.distance
  (testing "test distance"
    (is (= 0.0 (distance (point 10 16) (point 10 16))))
    (is (= 1.4142135623730951 (distance (point 10 16) (point 11 17))))
    (is (= 10.0 (distance (point 20 16) (point 10 16))))
    (is (= 10.0 (distance (point 10 16) (point 10 26))))))
