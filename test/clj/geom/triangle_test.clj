(ns geom.triangle_test
  (:require [clojure.test :refer :all]
            [geom.triangle :refer :all]
            [geom.point :refer [point]]
            [geom.edge :refer [edge]]
            [geom.circle :refer [circle]]))

(deftest test.triangle
  (testing "triangle equality"
    (is (= (triangle (point 0 0) (point 0 1) (point 1 0))
           (triangle (point 0 0) (point 0 1) (point 1 0))))
    (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
           (triangle (point 0 0) (point 0 1) (point 1 0))))
    (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
           (triangle (point 1 0) (point 0 1) (point 0 0))))
    (is (= (triangle (point 0 0) (point 0 1) (point 1 0))
           (triangle (point 0 0) (edge (point 0 1) (point 1 0)))))
    (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
           (triangle (point 0 0) (edge (point 0 1) (point 1 0)))))
    (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
           (triangle (point 1 0) (edge (point 0 1) (point 0 0))))))
  (testing "illegal attributes"
    (is (thrown-with-msg? IllegalArgumentException #"Duplicate key: .*\.Point2d" (triangle (point 0 0) (point 0 0) (point 1 0))))
    (is (thrown-with-msg? IllegalArgumentException #"Duplicate key: .*\.Point2d" (triangle (point 0 0) (edge (point 0 0) (point 1 0)))))))

(deftest test.points
  (testing "triangle equality"
    (is (list (point 0 0) (point 0 1) (point 1 0)) (= (triangle (point 0 0) (point 0 1) (point 1 0))))))

(deftest test.circumcircle
  (testing "center point"
    (is (= (point 0 0) (circumcircle-p (triangle (point -1 0) (point 1 0) (point 0 1)))))
    (is (= (point 0 0) (circumcircle-p (triangle (point -10 0) (point 10 0) (point 0 10)))))
    (is (= (point (/ -2521 70) (/ -561 14)) (circumcircle-p (triangle (point -50 20) (point 10 1) (point 0 10))))))
  (testing "points on one line"
    (is (thrown-with-msg? ArithmeticException #"Divide by zero" (circumcircle-p (triangle (point 0 1) (point 0 2) (point 0 3))))))
  (testing "create"
    (let [circ (circumcircle (triangle (point -50 20) (point 10 1) (point 0 10)))]
      (is (= (point (/ -2521 70) (/ -561 14)) (:p circ)))
      (is (= 61.67800851757357 (:r circ))))))

(deftest test.to-circumcircle
  (testing "converts triangle into circumcircle"
    (is (= (circle (point (/ 1 2) (/ 1 2)) 0.7071067811865476) (to-circumcircle (triangle (point 0 0) (point 0 1) (point 1 0))))))
  (testing "converts triangles into circumcircles"
    (is (= (list (circle (point (/ 1 2) (/ 1 2)) 0.7071067811865476)
                 (circle (point (/ 1 2) (/ 1 2)) 0.7071067811865476)) (to-circumcircles [(triangle (point 0 0) (point 0 1) (point 1 0))
                                                                                         (triangle (point 0 0) (point 0 1) (point 1 0))]))))
  (testing "converts triangle into circumcircle center"
    (is (= (point (/ 1 2) (/ 1 2)) (to-circumcircle-center (triangle (point 0 0) (point 0 1) (point 1 0))))))
  (testing "converts triangles into circumcircles centers"
    (is (= (list (point (/ 1 2) (/ 1 2))
                 (point (/ 1 2) (/ 1 2))) (to-circumcircles-centers [(triangle (point 0 0) (point 0 1) (point 1 0))
                                                                     (triangle (point 0 0) (point 0 1) (point 1 0))])))))
