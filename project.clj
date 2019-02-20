(defproject bandit "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/data.csv "0.1.4"]
                 [expound "0.7.2"]
                 [kixi/stats "0.5.0"]]
  :profiles {:dev {:jvm-opts ["-Xmx14G"]}}
  :jvm-opts ["-XX:+UseG1GC"]
  :plugins [[lein-codox "0.10.6"]]
  :codox {:metadata {:doc/format :markdown}})
