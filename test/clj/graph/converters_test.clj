(ns graph.converters-test
  (:require [clojure.test :refer :all]
            [graph.converters :refer :all]
            [util.set_multimap :refer :all]
            [geom.point :refer [point points-from]]
            [geom.triangle :refer [triangle points to-circumcircle-center to-circumcircles-centers]]
            [geom.polygon :refer [polygon]]
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
    (is (= {(point 0 0) {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 2 1) (point 1 2))]))}
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

(deftest test.neighbours?
  (testing "are neighbours"
    (is (neighbours? (triangle 100 100, 100 1000, 0 0) (triangle 100 100, 100 1000, 200 0))))
  (testing "are not neighbours"
    (is (not (neighbours? (triangle 100 100, 100 0, 0 0) (triangle 100 100, 100 1000, 200 0))))))

(deftest test.ordered-circumcircle-centers
  (testing "will return points in 'neighbour' order"
    (is (= (points-from {:x 375/8, :y 150}
                        {:x 50, :y 1675/11}
                        {:x 150, :y 1475/11}
                        {:x 1625/12, :y 150}
                        {:x 575/4, :y 50}
                        {:x 150, :y 175/3}
                        {:x 50, :y 325/9}
                        {:x 275/8, :y 50})
           (ordered-circumcircle-centers [(triangle 80 90, 0 100, 0 0)
                                          (triangle 80 90, 0 0, 100 0)
                                          (triangle 80 90, 200 0, 100 0)
                                          (triangle 80 90, 200 0, 200 100)
                                          (triangle 80 90, 200 100, 200 200)
                                          (triangle 80 90, 100 200, 200 200)
                                          (triangle 80 90, 100 200, 0 200)
                                          (triangle 80 90, 0 100, 0 200)])))
    (is (= (points-from {:x 100, :y 0}
                        {:x 650, :y 550}
                        {:x -450, :y 550})
           (ordered-circumcircle-centers [(triangle 100 100, 100 1000, 0 0)
                                          (triangle 100 100, 100 1000, 200 0)
                                          (triangle 100 100, 200 0, 0 0)])))))

(deftest test.as-graph
  (testing "empty input"
    (is (= {:connections {}, :nodes #{}, :data {}} (as-graph {} {}))))
  (testing "building a graph from valid data"
    (is (= {:connections {},
            :nodes #{:node-1},
            :data {:node-1 {:geometry :g_A, :point :A}}}
           (as-graph {:A #{}} {:A {:geometry :g_A, :point :A}})))
    (is (= {:connections {},
            :nodes #{:node-2 :node-1},
            :data {:node-2 {:geometry :g_B, :point :B}, :node-1 {:geometry :g_A, :point :A}}}
           (as-graph {:A #{}, :B #{}} {:A {:geometry :g_A, :point :A}, :B {:geometry :g_B, :point :B}})))
    (is (= {:connections {:node-1 #{:node-2}, :node-2 #{:node-1}},
            :nodes #{:node-1 :node-2},
            :data {:node-2 {:geometry :g_B, :point :B}, :node-1 {:geometry :g_A, :point :A}}}
           (as-graph {:A #{:B}, :B #{:A}} {:A {:geometry :g_A}, :B {:geometry :g_B}})))
    (is  (= {:connections {:node-3 #{:node-1}, :node-2 #{:node-1}, :node-1 #{:node-2 :node-3}},
             :nodes #{:node-2 :node-1 :node-3},
             :data {:node-3 {:geometry :g_C, :point :C}, :node-2 {:geometry :g_B, :point :B}, :node-1 {:geometry :g_A, :point :A}}}
            (as-graph {:A #{:B :C}, :B #{:A}, :C #{:A}} {:A {:geometry :g_A, :point :A}, :B {:geometry :g_B, :point :B}, :C {:geometry :g_C, :point :C}})))))
