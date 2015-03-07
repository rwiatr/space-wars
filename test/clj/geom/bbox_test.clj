(ns geom.bbox_test
  (:require [clojure.test :refer :all]
            [geom.bbox :refer :all]))

(deftest test.bbox
  (testing "creation of bbox"
    (is (= 102 (:x (bbox 102 100 101 104))))
    (is (= 100 (:y (bbox 102 100 101 104))))
    (is (= 101 (:width (bbox 102 100 101 104))))
    (is (= 104 (:height (bbox 102 100 101 104))))
    (is (= (bbox 1 2 100 105) (bbox-xy 1 2 101 107)))
    (is (= (bbox 1 2 100 105) (bbox-xy 101 107 1 2)))
    (is (= (bbox-xy -100 -100 200 200) (bbox-max (bbox-xy -100 -100 0 0) (bbox-xy 199 199 200 200)))))
  (testing "area"
    (is (= 600 (area (bbox 10 10 20 30)))))
  (testing "bbox max"
    (is (= (bbox-xy 10 0 31 100) (bbox-max (bbox-xy 21 0 31 100) (bbox-xy 10 0 20 100)))))
  (testing "intersections"
    (is (intersect? (bbox 0 0 100 100) (bbox 10 10 200 200)))
    (is (intersect? (bbox 0 0 0 0) (bbox 0 0 0 0)))
    (is (not (intersect? (bbox 100 0 10 10) (bbox 0 0 0 0))))
    (is (= (list (bbox 0 0 0 0) (bbox 1 0 0 0) (bbox 0 2 0 0))
           (intersecting (bbox 0 0 10 10) (bbox 0 0 0 0) (bbox 1 0 0 0) (bbox 0 2 0 0) (bbox 20 20 10 10)))))
  (testing "contained?"
    (is (contained? (bbox 0 0 10 10) (bbox 0 0 10 10)))
    (is (contained? (bbox 0 0 10 10) (bbox 1 1 9 9)))
    (is (not (contained? (bbox 0 0 10 10) (bbox 2 1 9 9))))))
