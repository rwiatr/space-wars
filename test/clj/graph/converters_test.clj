(ns graph.converters-test
  (:require [clojure.test :refer :all]
            [graph.converters :refer :all]
            [util.set_multimap :refer :all]
            [geom.point :refer [point]]
            [geom.triangle :refer [triangle points to-circumcircle-center to-circumcircles-centers]]
            [geom.polygon :refer [polygon]]
            [graph.mapgraph :refer [property-node]]
            [graph.bowyer-watson :refer [bowyer-watson_2d bw-standard-filter]]))

(deftest test.indexing-finctions
  (testing "create point->triangles multimap from triangle seq"
    (is (= {(point 0 0) #{(triangle (point 0 0) (point 0 1) (point 1 0))}
            (point 1 0) #{(triangle (point 0 0) (point 0 1) (point 1 0))}
            (point 0 1) #{(triangle (point 0 0) (point 0 1) (point 1 0))}}
           (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))])))
    (is (= {(point 0 0) #{(triangle (point 0 0) (point 0 1) (point 1 0))}
            (point 1 0) #{(triangle (point 0 0) (point 0 1) (point 1 0))}
            (point 0 1) #{(triangle (point 0 0) (point 0 1) (point 1 0))}}
           (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))
                              (triangle (point 0 0) (point 0 1) (point 1 0))])))
    (is (= {(point 0 0) #{(triangle (point 0 0) (point 0 1) (point 1 0)) (triangle (point 0 0) (point 2 1) (point 1 2))}
            (point 1 0) #{(triangle (point 0 0) (point 0 1) (point 1 0))}
            (point 0 1) #{(triangle (point 0 0) (point 0 1) (point 1 0))}
            (point 1 2) #{(triangle (point 0 0) (point 2 1) (point 1 2))}
            (point 2 1) #{(triangle (point 0 0) (point 2 1) (point 1 2))}}
           (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))
                              (triangle (point 0 0) (point 2 1) (point 1 2))]))))
  (testing "create point->polygon map from point->triangles index"
    (is (= {(point 0 0) {:geometry (polygon (list (to-circumcircle-center (triangle (point 0 0) (point 0 1) (point 1 0)))))}
            (point 1 0) {:geometry (polygon (list (to-circumcircle-center (triangle (point 0 0) (point 0 1) (point 1 0)))))}
            (point 0 1) {:geometry (polygon (list (to-circumcircle-center (triangle (point 0 0) (point 0 1) (point 1 0)))))}}
           (point->polygon (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))]))))
    (is (= {(point 0 0) {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 0 1) (point 1 0)) (triangle (point 0 0) (point 2 1) (point 1 2))]))}
            (point 1 0) {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 0 1) (point 1 0))]))}
            (point 0 1) {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 0 1) (point 1 0))]))}
            (point 1 2) {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 2 1) (point 1 2))]))}
            (point 2 1) {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 2 1) (point 1 2))]))}}
           (point->polygon (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))
                                              (triangle (point 0 0) (point 2 1) (point 1 2))])))))
  (testing "create point->points multimap from point->triangles index"
    (is (= {(point 0 0) #{(point 0 1) (point 1 0)}
            (point 1 0) #{(point 0 0) (point 0 1)}
            (point 0 1) #{(point 0 0) (point 1 0)}}
           (point->points (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))]))))
    (is (= {(point 0 0) #{(point 0 1) (point 1 0) (point 2 1) (point 1 2)}
            (point 1 0) #{(point 0 0) (point 0 1)}
            (point 0 1) #{(point 0 0) (point 1 0)}
            (point 1 2) #{(point 0 0) (point 2 1)}
            (point 2 1) #{(point 0 0) (point 1 2)}}
           (point->points (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))
                                             (triangle (point 0 0) (point 2 1) (point 1 2))]))))))


(deftest test.polygon->polygons
  (testing "polygon->polygons when a<->b and a and b different polygons"
    (is (= {:2 #{:1} :1 #{:2}} (polygon->polygons (add (multimap) :a :b :b :a) {:a :1 :b :2}))))
  (testing "polygon->polygons when a<->b and a and b same"
    (is (= {:2 #{:2}} (polygon->polygons (add (multimap) :a :b :b :a) {:a :2 :b :2}))))
  (testing "polygon->polygons when a and b not connected"
    (is (= {nil #{:1 :2}} (polygon->polygons (add (multimap) :a :c :c :a :b :d :d :b) {:a :1 :b :2})))))

(deftest test.voronoi-polygons->neighbours
  (testing "voronoi-polygons->neighbours produces a valid voronoi diagram out of triangles"
    (is (= {{:geometry (polygon [(point 15 (/ 75 2))])}
            #{{:geometry (polygon [(point 15 (/ 75 2)) (point (/ 935 54) (/ 625 18))])}}
            {:geometry (polygon [(point (/ 935 54) (/ 625 18))])}
            #{{:geometry (polygon [(point 15 (/ 75 2)) (point (/ 935 54) (/ 625 18))])}}
            {:geometry (polygon [(point 15 (/ 75 2)) (point (/ 935 54) (/ 625 18))])}
            #{{:geometry (polygon [(point (/ 935 54) (/ 625 18))])} {:geometry (polygon [(point 15 (/ 75 2))])}}}
           (voronoi-polygons->neighbours (bw-standard-filter (bowyer-watson_2d [(point 0 25) (point 0 50) (point 30 50) (point 15 15)]))))))
  (testing "voronoi-polygons->neighbours produces empty multimap when only one triangle is given"
    (is (empty? (voronoi-polygons->neighbours (bw-standard-filter (bowyer-watson_2d [(point 0 25) (point 0 50) (point 30 50)])))))))
