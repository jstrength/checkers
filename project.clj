(defproject checkers "0.1.0-SNAPSHOT"
  :description "Simple single and multiplayer checkers game"
  :url "https://github.com/jstrength"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil "2.8.0"]]
  :aot :all
  :profiles {:uberjar {:resource-paths ["data"]
                       :main checkers.core
                       :aot [checkers.core]}})
