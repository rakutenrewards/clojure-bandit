(ns curbside.bandit.core-test
  (:require
   [clojure.data.csv :as csv]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [curbside.bandit.core :as bandit]
   [curbside.bandit.spec :as spec]
   [kixi.stats.distribution :refer [draw sample normal]])
  (:import
   (java.util UUID)))

(defn stationary-problem
  "Generates n samples for the given distributions, in the format expected
   by the test helper functions."
  [n dists & {:keys [maximize?]}]
  {:time-series (repeatedly n #(mapv draw dists))
   :maximize? (if (nil? maximize?) true maximize?)})

(defn k-armed-normal-problem
  "Generates a k-armed problem with normal distributions"
  [k n & {:keys [maximize?]}]
  (let [dists (for [i (range k)]
                (normal {:mu (* 10 (rand-int (inc i))) :sd 2.0}))]
    (stationary-problem n dists :maximize? (if (nil? maximize?)
                                             true
                                             maximize?))))

(defn interpolate-params
  "Interpolates ranges of params into concrete param values.
   Example:
   (test/interpolate-params 5 10 {:foo [0 10]})
   =>
   {:foo 5}"
  [i n params]
  (into {}
        (for [[k [begin end]] params]
          (let [diff (- end begin)]
            [k (float (+ begin (* (/ i n) diff)))]))))

(defn non-stationary-sample
  [i n params]
  ;; TODO: parametrize on distribution type
  (draw (normal (interpolate-params i n params))))

(defn non-stationary-problem
  "Generates n samples for the given distribution specifications. The parameters
   of the input distributions will be smoothly interpolated from their initial
   to final values over the n samples.

   Example invocation
   (non-stationary-problem n [{:mu [1.5 2.0] :sd [0.2 0.4]}
                              {:mu [2.5 3.0] :sd [0.1 0.2]}])"
  [n dist-specs & {:keys [maximize?] :or {maximize? true}}]
  {:time-series
   (for [i (range n)]
     (mapv #(non-stationary-sample i n %) dist-specs))
   :maximize? (if (nil? maximize?) true maximize?)})

(defn optimal-total-reward
  "Returns the total reward of a learner that always chooses the best arm.
   For minimization problems, the optimal reward is the smallest reward
   possible, which is kind of confusing."
  [{:keys [time-series maximize?] :as _problem}]
  (reduce + (map #(apply (if maximize? max min) %) time-series)))

(defn cumulative-optimal-reward
  "Returns the reward over time of a learner that always chooses the best
   arm."
  [{:keys [time-series maximize?] :as _problem}]
  (reductions + (map #(apply (if maximize? max min) %) time-series)))

(defn max-regret
  "Returns the total regret of a learner that always chooses the worst arm.
   This is defined as the absolute difference between the worst attainable
   reward and the best attainable reward, so a larger number is worse, for both
   maximization and minimization problems."
  [{:keys [time-series maximize?] :as _problem}]
  (reduce + (map #(Math/abs (- (apply (if maximize? max min) %)
                               (apply (if maximize? min max) %)))
                 time-series)))

(defn cumulative-max-regret
  "Returns the regret over time of a learner that always chooses the worst
   arm."
  [{:keys [time-series maximize?] :as _problem}]
  (reductions + (map #(Math/abs (- (apply (if maximize? max min) %)
                                   (apply (if maximize? min max) %)))
                     time-series)))

(defn problem-arm-names
  [problem]
  (mapv str (range (inc (count (first problem))))))

(defn run-on-test-problem
  "Runs the given learner on a problem, with rewards delayed by the given
   number of time steps. Returns the index of the choice taken at each time
   step.

   Takes two optional maps from integer timestep to string arm id, specifying
   when to add or delete arms from the problem."
  [backend learner-algo algo-params problem reward-delay
   & {:keys [arm-deletion-times arm-addition-times]}]
  (let [learner {::spec/learner-algo learner-algo
                 ::spec/algo-params (assoc algo-params
                                           ::spec/maximize?
                                           (:maximize? problem)
                                           ::spec/learner-algo
                                           learner-algo)
                 ::spec/arm-names (problem-arm-names problem)
                 ::spec/experiment-name (str (UUID/randomUUID))}]
    (bandit/init backend learner)
    (loop [[choice & choices] (:time-series problem)
           reward-queue clojure.lang.PersistentQueue/EMPTY
           remaining-delay reward-delay
           result []
           t 0]
      (if (nil? choice)
        result
        (do
          (when-let [to-delete (get arm-deletion-times t)]
            (bandit/delete-arm backend learner to-delete))
          (when-let [to-add (get arm-addition-times t)]
            (bandit/create-arm backend learner to-add))
          (let [chosen-arm (bandit/choose backend learner)
                chosen-idx (Integer/parseInt chosen-arm)
                chosen (nth choice chosen-idx)
                chosen-reward {::spec/reward-value chosen
                               ::spec/arm-name chosen-arm}
                new-queue (conj reward-queue chosen-reward)
                new-result (conj result chosen-idx)]
            (if (= remaining-delay 0)
              (let [reward (peek new-queue)]
                (bandit/reward backend learner reward)
                (recur choices (pop new-queue) remaining-delay new-result (inc t)))
              (recur choices new-queue (dec remaining-delay) new-result (inc t)))))))))

(defn total-reward
  [{:keys [time-series maximize?] :as _problem} chosen-indices]
  (reduce + (map nth time-series chosen-indices)))

(defn cumulative-reward
  [{:keys [time-series maximize?] :as _problem} chosen-indices]
  (reductions + (map nth time-series chosen-indices)))

(defn total-regret
  [problem chosen-indices]
  (Math/abs
   (- (optimal-total-reward problem)
      (total-reward problem chosen-indices))))

(defn cumulative-total-regret
  [problem chosen-indices]
  (map (comp #(Math/abs %) -) (cumulative-optimal-reward problem)
                              (cumulative-reward problem chosen-indices)))

(defn algo-param->csv-column-name
  [algo params]
  (match [algo params]
    [::spec/epsilon-greedy {::spec/epsilon e}] (str "epsilon_" e)
    [::spec/random _] "random"
    [::spec/ucb1 _] "ucb1"))

(defn generate-regret-csvs
  "For a given problem, generate CSVs showing the growth in regret over time
   for different learner algorithms, with different amounts of delay in
   reward feedback."
  [filename backend problem algo-param-pairs reward-delay
   & {:keys [include-max-regret?
             arm-deletion-times
             arm-addition-times]}]
  (let [ixes (pmap
              (fn [[algo params]]
                (run-on-test-problem backend algo params problem reward-delay
                                     :arm-deletion-times arm-deletion-times
                                     :arm-addition-times arm-addition-times))
              algo-param-pairs)
        regrets (cond-> (mapv #(cumulative-total-regret problem %) ixes)
                  include-max-regret? (conj (cumulative-max-regret problem)))
        column-names (cond-> (mapv #(apply algo-param->csv-column-name %)
                                   algo-param-pairs)
                       include-max-regret? (conj "max_regret"))]
    (with-open [writer (io/writer filename)]
      (csv/write-csv writer [column-names])
      (csv/write-csv writer (apply map (partial conj []) regrets)))))

(deftest performance-comparison
  (let [prob (stationary-problem 100000
                                 [(normal {:mu 200.7 :sd 2.0})
                                  (normal {:mu 15.1 :sd 1.3})
                                  (normal {:mu 1.3 :sd 2.0})]
                                 :maximize? true)
        backend (atom {})
        params {::spec/maximize? true}
        ucb-ixes (future
                   (run-on-test-problem backend ::spec/ucb1 params prob 0))
        eps-ixes (future
                   (run-on-test-problem backend
                                        ::spec/epsilon-greedy
                                        {::spec/epsilon 0.05
                                         ::spec/maximize? true}
                                        prob
                                        0))
        random-ixes (future
                      (run-on-test-problem backend ::spec/random params prob 0))
        ucb-regret (total-regret prob @ucb-ixes)
        ucb-reward (total-reward prob @ucb-ixes)
        eps-regret (total-regret prob @eps-ixes)
        eps-reward (total-reward prob @eps-ixes)
        random-regret (total-regret prob @random-ixes)
        random-reward (total-reward prob @random-ixes)]
    (is (< ucb-regret eps-regret))
    (is (> ucb-reward eps-reward))
    (is (< eps-regret random-regret))
    (is (> eps-reward random-reward))))

(deftest performance-comparison-minimization
  (let [prob (stationary-problem 100000
                                 [(normal {:mu 200.7 :sd 2.0})
                                  (normal {:mu 15.1 :sd 1.3})
                                  (normal {:mu 1.3 :sd 2.0})]
                                 :maximize? false)
        backend (atom {})
        params {::spec/maximize? false}
        ucb-ixes (future
                   (run-on-test-problem backend
                                        ::spec/ucb1
                                        params
                                        prob
                                        0))
        eps-ixes (future
                   (run-on-test-problem backend
                                        ::spec/epsilon-greedy
                                        {::spec/epsilon 0.05
                                         ::spec/maximize? false}
                                        prob
                                        0))
        random-ixes (future
                      (run-on-test-problem backend
                                           ::spec/random
                                           params
                                           prob
                                           0))
        ucb-regret (total-regret prob @ucb-ixes)
        ucb-reward (total-reward prob @ucb-ixes)
        eps-regret (total-regret prob @eps-ixes)
        eps-reward (total-reward prob @eps-ixes)
        random-regret (total-regret prob @random-ixes)
        random-reward (total-reward prob @random-ixes)]
    (testing "with minimization, larger regrets are still worse"
      (is (< ucb-regret eps-regret))
      (is (< eps-regret random-regret)))
    (testing "with minimization, rewards are lower"
      (is (< ucb-reward eps-reward))
      (is (< eps-reward random-reward)))))
