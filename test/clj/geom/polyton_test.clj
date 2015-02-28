(ns geom.polygon
  (:require [clojure.test :refer :all]
            [geom.polygon :refer :all]
            [geom.point :refer :all]
            [geom.bbox :refer :all]))

(deftest test.polygon
  (testing "equal"
    (is (= (polygon 15 16) (polygon 15 16))))
  (testing "not-equal"
    (is (not (= (polygon 1 16) (polygon 15 16)))))
  (testing "contents"
    (is (= '(1 2) (:points (polygon 1 2))))))

(deftest test.to-bbox
  (testing "polygon to bbox"
    (is (= (bbox 0 0 100 200) (to-bbox (polygon [(point 0 0) (point 100 0) (point 0 200)]))))))
