(defproject com.curbside/curbside-bandit "0.2.1"
  :description "Multi-armed bandit algorithms"
  :url "https://github.com/Curbside/curbside-bandit"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "0.3.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/test.check "0.10.0"]
                 [expound "0.7.2"]
                 [kixi/stats "0.5.2"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/algo.generic "0.1.3"]
                 [com.taoensso/carmine "2.19.1"]]
  :profiles {:dev {:jvm-opts ["-Xmx14G"]
                   :plugins [[lein-ancient "0.6.15"]
                             [jonase/eastwood "0.3.5"]
                             [lein-cljfmt "0.6.4"]
                             [com.gfredericks/lein-how-to-ns "0.2.3"]]
                   :how-to-ns {:require-docstring? false
                               :sort-clauses? true
                               :allow-refer-all? true
                               :allow-extra-clauses? false
                               :align-clauses? false
                               :import-square-brackets? false}
                   :cljfmt {:indents {instrumenting    [[:block 1]]
                                      mocking          [[:block 1]]
                                      stubbing         [[:block 1]]
                                      mocking-private  [[:block 1]]
                                      stubbing-private [[:block 1]]
                                      timed            [[:block 2]]
                                      for-all          [[:block 1]]}}}
             :ci [:test {:plugins [[test2junit "1.3.3"]]}]}
  :aliases {"fix" ["do" ["cljfmt" "fix"] ["how-to-ns" "fix"]]
            "check" ["do" ["cljfmt" "check"] ["how-to-ns" "check"]]}
  :jvm-opts ["-XX:+UseG1GC"]
  :plugins [[lein-codox "0.10.6"]
            [lein-release "1.0.5"]]
  :codox {:metadata {:doc/format :markdown}}
  :deploy-repositories
  [["releases"
    {:url "https://curbside.jfrog.io/curbside/libs-release-local/"
     :username :env/artifactory_user
     :password :env/artifactory_pass}]
   ["snapshots"
    {:url "https://curbside.jfrog.io/curbside/libs-snapshot-local/"
     :username :env/artifactory_user
     :password :env/artifactory_pass}]])
