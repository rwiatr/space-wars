(defproject space-wars "0.1.0-SNAPSHOT"
  :source-paths ["src/clj"]
  :test-paths   ["test/clj"]
  :dependencies [
                 [quil "2.2.4"]
  ;               [rm-hull/monet "0.2.1"]
                 [org.clojure/clojure "1.6.0"]
  ;               [org.clojure/clojurescript "0.0-2371"
  ;                :exclusions [org.apache.ant/ant]]
                 ]
  ;:hooks [leiningen.cljsbuild]
  ;:plugins [[lein-cljsbuild "1.0.3"]]
  :plugins [[lein-cloverage "1.0.2"]]
  ;:cljsbuild { :builds [{:source-paths ["src/cljs"]
  ;             :compiler {:output-to "resources/public/js/main.js"
  ;                        :optimizations :whitespace
  ;                        :pretty-print true}}]}

  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot space-wars.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
