(defproject com.curbside/curbside-bandit "0.1.3"
  :description "Multi-armed bandit algorithms"
  :url "https://github.com/Curbside/curbside-bandit"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/data.csv "0.1.4"]
                 [expound "0.7.2"]
                 [kixi/stats "0.5.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/algo.generic "0.1.3"]
                 [com.taoensso/carmine "2.19.1"]]
  :profiles {:dev {:jvm-opts ["-Xmx14G"]}}
  :jvm-opts ["-XX:+UseG1GC"]
  :plugins [[lein-codox "0.10.6"]
            [lein-release "1.0.5"]]
  :codox {:metadata {:doc/format :markdown}}
  :deploy-repositories
  [["releases"
    {:url "https://curbside.jfrog.io/curbside/libs-release-local/"
     :username :env/artifactory_user
     :password :env/artifactory_pass}]])
