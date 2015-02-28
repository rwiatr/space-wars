(ns graph.traversal-test
  (:require [clojure.test :refer :all]
            [graph.traversal :refer :all]))


(deftest test.breath-first
  (testing "one element graph"
    (is (= '(a) (breath-first-seq 'a {}))))
  (testing "one element with loop graph"
    (is (= '(a) (breath-first-seq 'a {'a '(a)}))))
  (testing "two element directed graph"
    (is (= '(a b) (breath-first-seq 'a {'a '(b)}))))
  (testing "two element undirected graph"
    (is (= '(a b) (breath-first-seq 'a {'a '(b) 'b '(a)}))))
  (testing "three element directed graph"
    (is (= '(a b c) (breath-first-seq 'a {'a '(b) 'b '(c)})))
    (is (= '(a b c) (breath-first-seq 'a {'a '(b) 'b '(c) 'c '(a)})))
    (is (= '(a b c) (breath-first-seq 'a {'a '(b c)})))
    (is (= '(a b c) (breath-first-seq 'a {'a '(b c) 'c '(b)}))))
  (testing "three element undirected graph"
    (is (= '(a b c) (breath-first-seq 'a {'a '(b)
                                          'b '(a c)
                                          'c '(b)})))
    (is (= '(a b c) (breath-first-seq 'a {'a '(b c)
                                          'b '(a c)
                                          'c '(a b)})))
    (is (= '(a b c) (breath-first-seq 'a {'a '(b c)
                                          'b '(a)
                                          'c '(a)})))
    (is (= '(a b c) (breath-first-seq 'a {'a '(b c)
                                          'b '(a c)
                                          'c '(a b)})))))

(deftest test.breath-first-with-level
  (testing "one element graph"
    (is (= '([a 0]) (breath-first-seq 'a {} :with-level true))))
  (testing "one element with loop graph"
    (is (= '([a 0]) (breath-first-seq 'a {'a '(a)} :with-level true))))
  (testing "two element directed graph"
    (is (= '([a 0] [b 1]) (breath-first-seq 'a {'a '(b)} :with-level true))))
  (testing "two element undirected graph"
    (is (= '([a 0] [b 1]) (breath-first-seq 'a {'a '(b) 'b '(a)} :with-level true))))
  (testing "three element directed graph"
    (is (= '([a 0] [b 1] [c 2]) (breath-first-seq 'a {'a '(b) 'b '(c)} :with-level true)))
    (is (= '([a 0] [b 1] [c 2]) (breath-first-seq 'a {'a '(b) 'b '(c) 'c '(a)} :with-level true)))
    (is (= '([a 0] [b 1] [c 1]) (breath-first-seq 'a {'a '(b c)} :with-level true)))
    (is (= '([a 0] [b 1] [c 1]) (breath-first-seq 'a {'a '(b c) 'c '(b)} :with-level true))))
  (testing "three element undirected graph"
    (is (= '([a 0] [b 1] [c 2]) (breath-first-seq 'a {'a '(b)
                                                      'b '(a c)
                                                      'c '(b)} :with-level true)))
    (is (= '([a 0] [b 1] [c 1]) (breath-first-seq 'a {'a '(b c)
                                                      'b '(a c)
                                                      'c '(a b)} :with-level true)))
    (is (= '([a 0] [b 1] [c 1]) (breath-first-seq 'a {'a '(b c)
                                                      'b '(a)
                                                      'c '(a)} :with-level true)))
    (is (= '([a 0] [b 1] [c 1]) (breath-first-seq 'a {'a '(b c)
                                                      'b '(a c)
                                                      'c '(a b)} :with-level true)))))
