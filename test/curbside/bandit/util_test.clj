(ns curbside.bandit.util-test
  (:require
   [clojure.algo.generic.functor :refer [fmap]]
   [clojure.test :refer :all]
   [curbside.bandit.util :as util]))

(deftest test-select-by-probability
  (let [probs {:0 0.25 :1 0.25 :2 0.50}
        selections (repeatedly 10000 #(util/select-by-probability probs))
        counts (fmap count (group-by identity selections))]
    (is (> (:2 counts) 4000))
    (is (< (:1 counts) 3000))
    (is (< (:0 counts) 3000))))
