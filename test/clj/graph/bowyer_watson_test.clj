(ns graph.bowyer-watson_test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [geom.triangle :refer [triangle]]
            [geom.edge :refer [edge]]
            [geom.point :refer [point distance]]
            [graph.bowyer-watson :refer :all]))

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

