(defproject stader "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [net.mikera/core.matrix "0.36.1"]
                 [net.mikera/imagez "0.6.0"]
                 [net.mikera/image-matrix "0.1.0"]
                 [quil/quil "2.2.6"]
                 [org.clojure/math.combinatorics "0.1.1"]]
  :main ^:skip-aot stader.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
