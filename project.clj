(defproject space-wars "0.1.0-SNAPSHOT"
  :source-paths ["src/clj"]
  :test-paths   ["test/clj"]
  :dependencies [[pjstadig/humane-test-output "0.6.0"]
                 [quil "2.2.4"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/math.combinatorics "0.0.8"]]
  :injections [(require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)]
  :plugins [[lein-cloverage "1.0.2"]]
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot space-wars.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
