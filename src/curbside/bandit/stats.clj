(ns curbside.bandit.stats
  "Probability and statistics helpers."
  (:require
   [clojure.algo.generic.functor :refer [fmap]]))

(def const-e 2.71828)

(defn select-by-probability
  "Given a map of keys where the values are probabilities (summing to one),
  randomly select a key with probability proportional to its assigned
  probability."
  [probs]
  (let [sorted-probs (sort-by val probs)
        cumulative-probs (reductions + (map val sorted-probs))
        rand-val (rand 1.0)
        chosen-index (->> cumulative-probs
                          (map (fn [i p] [i p]) (range))
                          (filter (fn [[i p]] (< rand-val p)))
                          first
                          first)]
    (first (nth sorted-probs chosen-index))))

(defn flip-probabilities
  "Given a map where the values are probabilities, invert the probabilities
   so that the least probable becomes the most probable and vice versa."
  [probs]
  (let [n (count probs)]
    (fmap
     (fn [p] (- (* 2 (/ 1.0 n)) p))
     probs)))
