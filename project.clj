(defproject ra "0.1.0-SNAPSHOT"
  :description "Risk analysis"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clocop "0.2.0"]
                 [net.mikera/vectorz-clj "0.28.0"]
                 [incanter/incanter-core "1.5.6"]
                 [incanter/incanter-charts "1.5.6"]]
  :main ^:skip-aot ra.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})