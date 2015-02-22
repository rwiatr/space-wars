(ns geom.circle_test
  (:require [clojure.test :refer :all]
            [geom.bbox :refer :all]))

(deftest test.bbox
  (testing "creation of bbox"
    (is (= 102 (:x (bbox 102 100 101 104))))
    (is (= 100 (:y (bbox 102 100 101 104))))
    (is (= 101 (:width (bbox 102 100 101 104))))
    (is (= 104 (:height (bbox 102 100 101 104)))))
  (testing "intersections"
    (is (intersect? (bbox 0 0 100 100) (bbox 10 10 200 200)))
    (is (intersect? (bbox 0 0 0 0) (bbox 0 0 0 0)))
    (is (not (intersect? (bbox 100 0 10 10) (bbox 0 0 0 0))))
    (is (= (list (bbox 0 0 0 0) (bbox 1 0 0 0) (bbox 0 2 0 0))
           (intersecting (bbox 0 0 10 10) (bbox 0 0 0 0) (bbox 1 0 0 0) (bbox 0 2 0 0) (bbox 20 20 10 10))))))
