(ns curbside.bandit.core-test
  (:require
   [clojure.algo.generic.functor :refer [fmap]]
   [clojure.data.csv :as csv]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [clojure.math.numeric-tower :refer [abs]]
   [clojure.test :refer :all]
   [curbside.bandit.core :as bandit]
   [curbside.bandit.ext :as ext]
   [curbside.bandit.spec :as spec]
   [curbside.bandit.stats :as stats]
   [kixi.stats.distribution :refer [draw sample normal]]
   [taoensso.carmine :as car :refer (wcar)])
  (:import
   (java.util UUID)))

(def redis-conn {:pool {} :spec {:uri "redis://localhost:6379/13"}})

(defn approx-eq
  [x y eps]
  (< (abs (- x y)) eps))

(use-fixtures :each
  (fn [test]
    (wcar redis-conn
          (car/flushdb))
    (test)))

(defn bulkify-rewards
  "Given a collection of ::spec/reward, return one ::spec/bulk-reward. Used to
   test that bulk-reward and reward yield approximately the same results.
   Assumes that all input rewards are for the same arm!"
  [rewards]
  (let [n (count rewards)
        reward-vals (map ::spec/reward-value rewards)
        mean (/ (reduce + reward-vals) n)
        max (apply max reward-vals)]
    {::spec/bulk-reward-mean mean
     ::spec/bulk-reward-count n
     ::spec/bulk-reward-max max
     ::spec/arm-name (::spec/arm-name (first rewards))}))

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
  (->> problem
       :time-series
       first
       count
       range
       (mapv str)))

(defn run-on-test-problem
  "Runs the given learner on a problem, with rewards delayed by the given
   number of time steps. Returns the index of the choice taken at each time
   step.

   Takes two optional maps from integer timestep to string arm id, specifying
   when to add or delete arms from the problem.

   Also takes an optional `bulk-rewards?` boolean. If true, delayed rewards will
   be updated in bulk by the bulk-reward API."
  [backend learner-algo algo-params problem reward-delay
   & {:keys [arm-deletion-times arm-addition-times bulk-rewards?]}]
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
            (cond

              (and (not bulk-rewards?) (= remaining-delay 0))
              (let [reward (peek new-queue)]
                (bandit/reward backend learner reward)
                (recur choices (pop new-queue) remaining-delay new-result (inc t)))

              (and bulk-rewards? (= 0 (mod t reward-delay)))
              (let [rewards (take reward-delay new-queue)]
                (doall
                 (for [[_ batch] (group-by ::spec/arm-name rewards)]
                   (let [bulk-reward (bulkify-rewards batch)]
                     (bandit/bulk-reward backend learner bulk-reward))))
                (recur choices
                       (ext/pop-n reward-delay new-queue)
                       remaining-delay
                       new-result
                       (inc t)))

              :else
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
    [::spec/ucb1 _] "ucb1"
    [::spec/softmax {::spec/temp-decay-per-step d}] (str "softmax_" d)))

(defn generate-regret-csvs
  "For a given problem, generate CSVs showing the growth in regret over time
   for different learner algorithms, with different amounts of delay in
   reward feedback."
  [filename backend problem algo-param-pairs reward-delay
   & {:keys [include-max-regret?
             arm-deletion-times
             arm-addition-times
             bulk-rewards?]}]
  (let [ixes (pmap
              (fn [[algo params]]
                (run-on-test-problem backend algo params problem reward-delay
                                     :arm-deletion-times arm-deletion-times
                                     :arm-addition-times arm-addition-times
                                     :bulk-rewards? bulk-rewards?))
              algo-param-pairs)
        regrets (cond-> (mapv #(cumulative-total-regret problem %) ixes)
                  include-max-regret? (conj (cumulative-max-regret problem)))
        column-names (cond-> (mapv #(apply algo-param->csv-column-name %)
                                   algo-param-pairs)
                       include-max-regret? (conj "max_regret"))]
    (with-open [writer (io/writer filename)]
      (csv/write-csv writer [column-names])
      (csv/write-csv writer (apply map (partial conj []) regrets)))))

(deftest test-arm-selection-probabilities
  (let [backend (atom {})
        maximize? true
        ucb-learner {::spec/learner-algo ::spec/ucb1
                     ::spec/algo-params {::spec/maximize? maximize?
                                         ::spec/learner-algo ::spec/ucb1}
                     ::spec/arm-names ["1" "2" "3"]
                     ::spec/experiment-name (str (UUID/randomUUID))}
        eps-learner {::spec/learner-algo ::spec/epsilon-greedy
                     ::spec/algo-params {::spec/maximize? maximize?
                                         ::spec/learner-algo ::spec/epsilon-greedy
                                         ::spec/epsilon 0.05}
                     ::spec/arm-names ["1" "2" "3"]
                     ::spec/experiment-name (str (UUID/randomUUID))}
        softmax-learner {::spec/learner-algo ::spec/softmax
                         ::spec/algo-params {::spec/starting-temperature 1.0
                                             ::spec/temp-decay-per-step (/ 1.0 100000)
                                             ::spec/min-temperature 0.01
                                             ::spec/maximize? maximize?
                                             ::spec/learner-algo ::spec/softmax}
                         ::spec/arm-names ["1" "2" "3"]
                         ::spec/experiment-name (str (UUID/randomUUID))}]
    (doseq [learner [ucb-learner eps-learner softmax-learner]]
      (bandit/init backend learner)
      (bandit/bulk-reward backend learner {::spec/bulk-reward-mean 14.5
                                           ::spec/bulk-reward-count 25
                                           ::spec/bulk-reward-max 16
                                           ::spec/arm-name "1"})
      (bandit/bulk-reward backend learner {::spec/bulk-reward-mean 11.5
                                           ::spec/bulk-reward-count 25
                                           ::spec/bulk-reward-max 16
                                           ::spec/arm-name "2"})
      (bandit/bulk-reward backend learner {::spec/bulk-reward-mean 10.5
                                           ::spec/bulk-reward-count 25
                                           ::spec/bulk-reward-max 16
                                           ::spec/arm-name "3"})
      (let [n 1000000
            chosen (repeatedly n #(bandit/choose backend learner))
            chosen-histogram (group-by identity chosen)
            chosen-frequencies (fmap #(double (/ (count %) n)) chosen-histogram)
            probabilities (bandit/arm-selection-probabilities
                           backend learner)
            arm-states (bandit/get-arm-states backend learner)]
        (doseq [arm-name ["1" "2" "3"]]
          (testing (str arm-name
                        " chosen frequency approximates expectation for "
                        (::spec/learner-algo learner))
            (is (approx-eq
                 (or (get chosen-frequencies arm-name) 0)
                 (get probabilities arm-name)
                 0.005))))))))

(defn performance-comparison
  [backend
   & {:keys [delay bulk-rewards?]}]
  (let [prob (stationary-problem 100000
                                 [(normal {:mu 200.7 :sd 2.0})
                                  (normal {:mu 15.1 :sd 1.3})
                                  (normal {:mu 1.3 :sd 2.0})]
                                 :maximize? true)
        params {::spec/maximize? true}
        ucb-ixes (future
                   (run-on-test-problem backend
                                        ::spec/ucb1
                                        params
                                        prob
                                        (or delay 0)
                                        :bulk-rewards? bulk-rewards?))
        eps-ixes (future
                   (run-on-test-problem backend
                                        ::spec/epsilon-greedy
                                        {::spec/epsilon 0.05
                                         ::spec/maximize? true}
                                        prob
                                        (or delay 0)
                                        :bulk-rewards? bulk-rewards?))
        random-ixes (future
                      (run-on-test-problem backend
                                           ::spec/random
                                           params
                                           prob
                                           (or delay 0)
                                           :bulk-rewards? bulk-rewards?))
        softmax-params {::spec/starting-temperature 1.0
                        ::spec/temp-decay-per-step (/ 1.0 100000)
                        ::spec/min-temperature 0.01
                        ::spec/maximize? true}
        softmax-ixes (future
                       (run-on-test-problem backend
                                            ::spec/softmax
                                            softmax-params
                                            prob
                                            (or delay 0)
                                            :bulk-rewards? bulk-rewards?))
        ucb-regret (total-regret prob @ucb-ixes)
        ucb-reward (total-reward prob @ucb-ixes)
        eps-regret (total-regret prob @eps-ixes)
        eps-reward (total-reward prob @eps-ixes)
        random-regret (total-regret prob @random-ixes)
        random-reward (total-reward prob @random-ixes)
        softmax-regret (total-regret prob @softmax-ixes)
        softmax-reward (total-reward prob @softmax-ixes)]
    (is (< ucb-regret eps-regret))
    (is (> ucb-reward eps-reward))
    (is (< eps-regret random-regret))
    (is (> eps-reward random-reward))
    (is (< softmax-regret random-regret))
    (is (> softmax-reward random-reward))))

(deftest test-performance-comparison
  (testing "When maximizing, more sophisticated algorithms perform better"
    (testing "atom backend"
      (performance-comparison (atom {})))
    (testing "redis backend"
      (performance-comparison redis-conn))
    (testing "batch rewards, 5000 at a time"
      (testing "atom backend"
        (performance-comparison (atom {}) :delay 5000 :bulk-rewards? true))
      (testing "redis backend"
        (performance-comparison redis-conn :delay 5000 :bulk-rewards? true)))))

(defn performance-comparison-minimization
  [backend]
  (let [prob (stationary-problem 100000
                                 [(normal {:mu 200.7 :sd 2.0})
                                  (normal {:mu 15.1 :sd 1.3})
                                  (normal {:mu 1.3 :sd 2.0})]
                                 :maximize? false)
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
        softmax-params {::spec/starting-temperature 1.0
                        ::spec/temp-decay-per-step (/ 1.0 100000)
                        ::spec/min-temperature 0.01
                        ::spec/maximize? false}
        softmax-ixes (future
                       (run-on-test-problem backend
                                            ::spec/softmax
                                            softmax-params
                                            prob
                                            0))
        ucb-regret (total-regret prob @ucb-ixes)
        ucb-reward (total-reward prob @ucb-ixes)
        eps-regret (total-regret prob @eps-ixes)
        eps-reward (total-reward prob @eps-ixes)
        random-regret (total-regret prob @random-ixes)
        random-reward (total-reward prob @random-ixes)
        softmax-regret (total-regret prob @softmax-ixes)
        softmax-reward (total-reward prob @softmax-ixes)]
    (testing "with minimization, larger regrets are still worse"
      (is (< ucb-regret eps-regret))
      (is (< eps-regret random-regret))
      (is (< softmax-regret random-regret)))
    (testing "with minimization, lower rewards are better"
      (is (< ucb-reward eps-reward))
      (is (< eps-reward random-reward))
      (is (< softmax-reward random-reward)))))

(deftest test-performance-comparison-minimization
  (testing "When minimizing, more sophisticated algorithms perform better"
    (performance-comparison-minimization (atom {}))
    (performance-comparison-minimization redis-conn)))

(deftest test-ucb-deterministic-wrt-backend
  (testing "UCB gives same results regardless of backend"
    (let [prob (k-armed-normal-problem 2 100)
          atom-ixes (run-on-test-problem (atom {}) ::spec/ucb1 {} prob 0)
          redis-ixes (run-on-test-problem redis-conn ::spec/ucb1 {} prob 0)]
      (is (= atom-ixes redis-ixes)))))

(deftest test-bulk-reward-same-effect
  (testing "Sending rewards in bulk gives (roughly) the same results as sending
            rewards one-by-one."
    (let [r (fn [x] {::spec/reward-value x ::spec/arm-name "arm1"})
          rewards (map r [1.0 0.5 0.2 0.7 0.3 1.0 0.3 0.7 0.9 0.14])
          bulk-reward (bulkify-rewards rewards)
          ;; Also try sending two separate batches
          bulk-reward2-1 (bulkify-rewards (take 5 rewards))
          bulk-reward2-2 (bulkify-rewards (drop 5 rewards))
          learner {::spec/algo-params {::spec/maximize? true
                                       ::spec/learner-algo ::spec/epsilon-greedy
                                       ::spec/epsilon 0.05}
                   ::spec/arm-names ["arm1"]
                   ::spec/experiment-name "learner"
                   ::spec/learner-algo ::spec/epsilon-greedy}
          bulk-learner (assoc learner ::spec/experiment-name "bulk-learner")
          bulk-learner2 (assoc learner ::spec/experiment-name "bulk-learner2")
          backend (atom {})]
      (bandit/init backend learner)
      (bandit/init backend bulk-learner)
      (bandit/init backend bulk-learner2)
      (bandit/bulk-reward backend bulk-learner bulk-reward)
      (bandit/bulk-reward backend bulk-learner2 bulk-reward2-1)
      (bandit/bulk-reward backend bulk-learner2 bulk-reward2-2)
      (doall (map #(bandit/reward backend learner %) rewards))
      (let [learner-state (get (bandit/get-arm-states backend learner) "arm1")
            bulk-learner-state (get (bandit/get-arm-states backend bulk-learner)
                                    "arm1")
            bulk-learner2-state (get (bandit/get-arm-states backend bulk-learner)
                                     "arm1")]
        (is (approx-eq (:mean-reward learner-state)
                       (:mean-reward bulk-learner-state)
                       0.0005))
        (is (approx-eq (:mean-reward learner-state)
                       (:mean-reward bulk-learner2-state)
                       0.0005))
        (is (= (:n learner-state)
               (:n bulk-learner-state)
               (:n bulk-learner2-state)))))))

(defn init-idempotent*
  [backend backend-name]
  (testing (str "init is idempotent for " backend-name)
    (let [learner {::spec/algo-params {::spec/maximize? true
                                       ::spec/learner-algo ::spec/epsilon-greedy
                                       ::spec/epsilon 0.05}
                   ::spec/arm-names ["arm1" "arm2"]
                   ::spec/experiment-name "learner"
                   ::spec/learner-algo ::spec/epsilon-greedy}]
      (bandit/init backend learner)
      (bandit/reward backend learner {::spec/reward-value 0.5
                                      ::spec/arm-name "arm1"})
      (bandit/reward backend learner {::spec/reward-value 0.2
                                      ::spec/arm-name "arm2"})
      (let [curr-state (bandit/get-arm-states backend learner)
            _ (bandit/init backend learner)
            state-after-init (bandit/get-arm-states backend learner)]
        (is (= curr-state state-after-init))))))

(deftest init-idempotent
  (init-idempotent* (atom {}) "atom")
  (init-idempotent* redis-conn "redis"))

(deftest ucb1-exploration
  (let [state-1 {:more-explored-smaller-reward  {:n 1000000 :mean-reward 0.1}
                 :less-explored-bigger-reward   {:n 10  :mean-reward 0.5}}]
    (testing "When maximizing, the less explored arm with large reward is chosen"
      (is (= :less-explored-bigger-reward
             (bandit/choose-ucb1 state-1 {::spec/maximize? true}))))
    (testing "When minimizing, the less explored arm with large reward is chosen"
      (is (= :less-explored-bigger-reward
             (bandit/choose-ucb1 state-1 {::spec/maximize? false}))))))
