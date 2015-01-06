(ns geom.polygon
  (:require [clojure.test :refer :all]
            [geom.polygon :refer :all]))

(deftest test.polygon
  (testing "equal"
    (is (= (polygon 15 16) (polygon 15 16))))
  (testing "not-equal"
    (is (not (= (polygon 1 16) (polygon 15 16)))))
  (testing "contents"
    (is (= '(1 2) (:points (polygon 1 2))))))
