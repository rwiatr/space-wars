(ns graph.converters-test
  (:require [clojure.test :refer :all]
            [graph.converters :refer :all]
            [geom.point :refer [point]]
            [geom.triangle :refer [triangle points to-circumcircle-center to-circumcircles-centers]]
            [geom.polygon :refer [polygon]]
            [graph.mapgraph :refer [property-node]]))

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
    (is (= {(point 0 0) (property-node {:geometry (polygon (list (to-circumcircle-center (triangle (point 0 0) (point 0 1) (point 1 0)))))})
            (point 1 0) (property-node {:geometry (polygon (list (to-circumcircle-center (triangle (point 0 0) (point 0 1) (point 1 0)))))})
            (point 0 1) (property-node {:geometry (polygon (list (to-circumcircle-center (triangle (point 0 0) (point 0 1) (point 1 0)))))})}
           (point->polygon (point->triangles [(triangle (point 0 0) (point 0 1) (point 1 0))]))))
    (is (= {(point 0 0) (property-node {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 0 1) (point 1 0)) (triangle (point 0 0) (point 2 1) (point 1 2))]))})
            (point 1 0) (property-node {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 0 1) (point 1 0))]))})
            (point 0 1) (property-node {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 0 1) (point 1 0))]))})
            (point 1 2) (property-node {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 2 1) (point 1 2))]))})
            (point 2 1) (property-node {:geometry (polygon (to-circumcircles-centers [(triangle (point 0 0) (point 2 1) (point 1 2))]))})}
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
