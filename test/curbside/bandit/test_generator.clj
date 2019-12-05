(ns curbside.bandit.test-generator
  "Generators used for testing purpose. This is intended to avoid cluttering the
  main spec namespace and decouple test from source."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [curbside.bandit.spec :as spec]))

(s/def ::experiment-state (s/keys :req [::spec/reward-lower-bound
                                        ::spec/n
                                        ::spec/deleted?
                                        ::spec/mean-reward
                                        ::spec/max-reward]))

(def experiment-state
  "Generator for a map containing a partial state of an experiment.
  Tries to generate a valid value up to 100 times."
  (gen/such-that (fn [{::spec/keys [reward-lower-bound mean-reward max-reward]}]
                   (<= reward-lower-bound mean-reward max-reward))
                 (s/gen ::experiment-state)
                 100))
