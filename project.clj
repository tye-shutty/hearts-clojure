(defproject hearts "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.trace "0.7.10"]]
  :main ^:skip-aot hearts.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
