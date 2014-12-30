(ns util.monit-test
  (:require [clojure.test :refer :all]
            [util.monit :refer :all]))

(deftest test.atom-map-monit
  (testing "create without arguments"
    (is (= "TOTAL: 0" (str (atom-map-monit))))
    (is (= "TOTAL: 300\n[\"1,0000000000\" \"TMP\"]" (str (monit (atom-map-monit) "TMP" 300))))
    (is (= "TOTAL: 900\n[\"0,8333333333\" \"FIRST\"]\n[\"0,1666666667\" \"SECOND\"]"
           (str (-> (atom-map-monit)
                    (monit "SECOND" 150)
                    (monit "FIRST" 750))))))
  (testing "with formatter :total"
    (is (= "TOTAL: 0" (str (atom-map-monit :formatters [:total]))))
    (is (= "TOTAL: 900" (str (-> (atom-map-monit :formatters [:total])
                                 (monit "SECOND" 150)
                                 (monit "FIRST" 750))))))
  (testing "with formatter :perent"
    (is (= "" (str (atom-map-monit :formatters [:percent]))))
    (is (= "[\"0,8333333333\" \"FIRST\"]\n[\"0,1666666667\" \"SECOND\"]"
           (str (-> (atom-map-monit :formatters [:percent])
                    (monit "SECOND" 150)
                    (monit "FIRST" 750)))))))

(deftype TestingMonitors[a_id a_t]
  Monitors
  (monit [this id t]
         (reset! a_id id)
         (reset! a_t t)
         this))

(defn- append-abc [s] (do (Thread/sleep 100) (str s "ABC")))

(deftest test.timer-macro
  (testing "timer macro will return with original result"
    (let [append-abc-t (timer append-abc util.monit/nil-monit)]
      (is (= "ZZZABC" (append-abc-t "ZZZ")))))
  (testing "passes code as id and execution time to monitor"
    (let [id (atom nil)
          t (atom nil)
          append-abc-t (timer append-abc (TestingMonitors. id t))]
      (do (= "ZZZABC" (append-abc-t "ZZZ"))
        (is (= 'append-abc @id))
        (is (> @t 100))))))


(deftest test.nil-objects
  (testing "nil-monit"
    (is (.startsWith (str util.monit/nil-monit) "util.monit.NilMonitor@"))
    (is (.startsWith (str (-> util.monit/nil-monit
                              (monit "SECOND" 150)
                              (monit "FIRST" 750))) "util.monit.NilMonitor@"))))
