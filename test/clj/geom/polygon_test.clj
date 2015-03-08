(ns geom.polygon_test
  (:require [clojure.test :refer :all]
            [geom.polygon :refer :all]
            [geom.point :refer :all]
            [geom.bbox :refer :all]))

(deftest test.polygon
  ;(testing "throws excpetion"
  ;  (is (thrown-with-msg? Exception #"Polygon can't be build from less then three points. Input=" (polygon [1 15]))))
  (testing "equal"
    (is (= (polygon [1 15 16]) (polygon [1 15 16]))))
  (testing "not-equal"
    (is (not (= (polygon [10 15 16]) (polygon [1 15 16])))))
  (testing "contents"
    (is (= '(1 2 3) (:points (polygon [1 2 3]))))))

(deftest test.to-bbox
  (testing "polygon to bbox"
    (is (= (bbox 0 0 100 200) (to-bbox (polygon [(point 0 0) (point 100 0) (point 0 200)]))))))

(deftest test.polygon->point-pairs
  (testing "iterate using every point pair"
    (is (= (list [(point 0 0) (point 0 100)] [(point 0 100) (point 13 10)] [(point 13 10) (point 0 0)])
           (polygon->point-pairs (polygon [(point 0 0) (point 0 100) (point 13 10)]))))))

(deftest test.point-in-poly?
  (testing "point in polygon"
    (is (point-in-poly? (point 10 50) (polygon [(point 0 0) (point 100 0) (point 0 200)]))))
  (testing "point not in polygon"
    (is (not (point-in-poly? (point 200 200) (polygon [(point 0 0) (point 100 0) (point 0 200)]))))))
