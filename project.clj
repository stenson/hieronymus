(defproject hieronymus "0.2.0-SNAPSHOT"
  :description "Weird characters"
  :url "http://github.com/stenson/hieronymus"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [clj-time "0.8.0"]
                 [circleci/clj-yaml "0.5.3"]]
  :profiles {:dev {:dependencies [[hiccup-bridge "1.0.1"]
                                  [mvxcvi/puget "0.8.1"]]}})