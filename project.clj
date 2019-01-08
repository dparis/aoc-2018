(defproject aoc-2018 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [com.climate/claypoole "1.1.4"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.threeten/threeten-extra "1.2"]
                 [com.taoensso/tufte "2.0.1"]]
  :main ^:skip-aot aoc-2018.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
