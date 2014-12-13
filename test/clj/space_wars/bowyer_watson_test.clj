(ns space-wars.bowyer-watson-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [space-wars.bowyer-watson :refer :all]
            [clojure.set :refer :all]))

(deftest test-point2d
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

(deftest test-edge-factory
  (testing "edge creates with ordered points"
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

(deftest test-triangle-factory
  (testing "triangle equality"
    (is (= (triangle (point 0 0) (point 0 1) (point 1 0))
           (triangle (point 0 0) (point 0 1) (point 1 0)))))

  (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
         (triangle (point 0 0) (point 0 1) (point 1 0))))

  (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
         (triangle (point 1 0) (point 0 1) (point 0 0))))

  (is (= (triangle (point 0 0) (point 0 1) (point 1 0))
         (triangle (point 0 0) (edge (point 0 1) (point 1 0)))))

  (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
         (triangle (point 0 0) (edge (point 0 1) (point 1 0)))))

  (is (= (triangle (point 0 1) (point 0 0) (point 1 0))
         (triangle (point 1 0) (edge (point 0 1) (point 0 0)))))

  (testing "illegal attributes"
    (is (thrown-with-msg? IllegalArgumentException #"Duplicate key: space_wars.bowyer_watson.Point2d" (triangle (point 0 0) (point 0 0) (point 1 0))))
    (is (thrown-with-msg? IllegalArgumentException #"Duplicate key: space_wars.bowyer_watson.Point2d" (triangle (point 0 0) (edge (point 0 0) (point 1 0)))))))

(deftest test-circumcircle-center
  (testing "center point"
    (is (= (point 0 0) (circumcircle-p (triangle (point -1 0) (point 1 0) (point 0 1)))))
    (is (= (point 0 0) (circumcircle-p (triangle (point -10 0) (point 10 0) (point 0 10)))))
    (is (= (point (/ -2521 70) (/ -561 14)) (circumcircle-p (triangle (point -50 20) (point 10 1) (point 0 10))))))
  (testing "points on one line"
    (is (thrown-with-msg? ArithmeticException #"Divide by zero" (circumcircle-p (triangle (point 0 1) (point 0 2) (point 0 3)))))))

(deftest test-circumcircle
  (testing "create"
    (let [circ (circumcircle (triangle (point -50 20) (point 10 1) (point 0 10)))]
      (is (= (point (/ -2521 70) (/ -561 14)) (:p circ)))
      (is (= 61.67800851757357 (:r circ))))))

(deftest test-distance
  (testing "test distance"
    (is (= 0.0 (distance (point 10 16) (point 10 16))))
    (is (= 1.4142135623730951 (distance (point 10 16) (point 11 17))))
    (is (= 10.0 (distance (point 20 16) (point 10 16))))
    (is (= 10.0 (distance (point 10 16) (point 10 26))))))

(defn- sets-equal [expected actual]
  (is (= #{} (clojure.set/difference actual expected)))
  (is (= #{} (clojure.set/difference expected actual))))

(deftest test-bowyer-watson_2d
  (testing "counts"
    (is (= 3 (count (bowyer-watson_2d [(point 0 0)]))))
    (is (= 5 (count (bowyer-watson_2d [(point 0 0) (point 0 1)]))))
    (is (= 7 (count (bowyer-watson_2d [(point 0 0) (point 0 1) (point 2 1)]))))
    (is (= 1 (count (bowyer-watson_2d [])))))
  (testing "triangles"
    (is (sets-equal
         #{(triangle (point -100 -100) (point 100 -100) (point 0 25))
           (triangle (point 0 100) (point 100 -100) (point 0 25))
           (triangle (point 0 100) (point -100 -100) (point 0 25))}
         (bowyer-watson_2d [(point 0 25)])))
    (let [expected #{(triangle (point 0 100) (point -100 -100) (point 0 25))
                     (triangle (point 100 -100) (point -100 -100) (point 48 0))
                     (triangle (point 0 25) (point -100 -100) (point 48 0))
                     (triangle (point 0 100) (point 100 -100) (point 48 0))
                     (triangle (point 0 100) (point 0 25) (point 48 0))}
          actual (bowyer-watson_2d [(point 0 25) (point 48 0)])]
      (sets-equal actual expected)
      (sets-equal expected actual))))

(deftest test-as-graph
  (testing "standard graph"
    (is (= {(point 0 0) #{(point 0 1) (point 2 1)}
            (point 0 1) #{(point 0 0) (point 2 1)}
            (point 2 1) #{(point 0 1) (point 0 0)}} (as-graph (bowyer-watson_2d [(point 0 0) (point 0 1) (point 2 1)]))))))

