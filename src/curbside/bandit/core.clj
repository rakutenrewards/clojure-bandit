(ns curbside.bandit.core
  "A library of functions for handling multi-armed bandit (MAB) problems. A MAB
   is defined as a problem where we repeatedly must choose between `k` choices
   (called arms). Once we have made a choice, we receive a reward based on that
   choice. The goal is to learn which arm to choose to maximize our rewards over
   time. There are many algorithms for trying different arms (exploring) and
   then settling on the best arm and choosing it repeatedly (exploitation). This
   library implements some of the more popular ones.

   Aside from the basic functionality of choosing arms and rewarding the
   learner, this library also includes support for delayed rewards, adding and
   removing arms from an ongoing problem, and persisting learner state, so that
   we can run long-term experiments.

   Usage is rather simple. We can create a new experiment with [[init]].

   ```
   (require '[bandit.core :as bandit])
   (require '[bandit.spec :as spec])
   (def bandit-state (atom {}))
   (def learner {::spec/learner-algo ::spec/ucb1
                 ::spec/experiment-name \"my-experiment\"
                 ::spec/arm-names [\"arm1\" \"arm2\"]})
   (bandit/init bandit-state learner)
   ```

   then ask it to [[choose]] an arm

   ```
   (bandit/choose bandit-state learner)
   ;; => \"arm1\"
   ```

   then [[reward]] it based on the value that arm gave us

   ```
   (bandit/reward bandit-state learner {::spec/reward-value 1.5
                                        ::spec/arm-name \"arm1\"})
   ```

   Over time, calls to [[choose]] will begin to return the arm that gives the
   best rewards.

   This namespace implements the core bandit API, consisting of [[init]],
   [[choose]], [[reward]], [[create-arm]], and [[delete-arm]]. These functions
   are wrappers around multimethods, which are responsible for dispatching to
   algorithm-specific logic. At the moment, most of the implemented bandit
   algorithms are similar enough that there is little difference in the
   multimethod implementations, and some just use a :default case. In the
   future, however, if we implement Thompson-sampling, the logic in the
   multimethods will look significantly different.

   Algorithm state is abstracted over by an opaque `storage-backend`. See
   bandit.learner-state for the implementation."
  (:require
   [clojure.algo.generic.functor :refer [fmap]]
   [clojure.math.numeric-tower :as math]
   [curbside.bandit.ext :as ext]
   [curbside.bandit.learner-state :as state]
   [curbside.bandit.spec :as spec]
   [curbside.bandit.stats :as stats]))

(defmulti arm-selection-probabilities*
  (fn [storage-backend learner]
    (::spec/learner-algo learner)))

(defmulti choose*
  "Chooses an ::spec/arm-name for the given learner. See documentation of
   [[choose]] for details."
  (fn [storage-backend learner]
    (::spec/learner-algo learner)))

(defn arm-states->arm-means
  "Computes the mean reward of each arm."
  [arm-states]
  (fmap :mean-reward arm-states))

(defn arm-states->unrewarded-arm-names
  "Returns all arm names that have not yet received a reward."
  [arm-states]
  (sort
   (for [[arm-name {:keys [n]}] arm-states
         :when (= 1 n)]
     arm-name)))

(defn choose-epsilon-greedy
  "Chooses an arm according to the epsilon-greedy algorithm -- chooses the
   best arm with probability (1 - epsilon). Otherwise, chooses a random arm."
  [arm-means {::spec/keys [epsilon maximize?]}]
  {:pre [epsilon (not (nil? maximize?))]}
  (let [k (count (keys arm-means))
        arm-names (keys arm-means)
        threshold (+ (- 1.0 epsilon) (/ epsilon k))
        best-key (if maximize? max-key min-key)
        best-arm (key (apply best-key val arm-means))
        other-arms (filter #(not= % best-arm) arm-names)
        rand-arm (nth other-arms (rand-int (- k 1)))]
    (if (< (rand) threshold) best-arm rand-arm)))

(defmethod choose* ::spec/epsilon-greedy
  [storage-backend learner]
  ;; TODO: it is likely that all choose* implementations will look like this, in
  ;; which case we don't need the state-fetching step in the multimethod.
  (let [arm-states (state/get-arm-states storage-backend
                                         (::spec/experiment-name learner))
        params (state/get-learner-params storage-backend
                                         (::spec/experiment-name learner))]
    (state/incr-choose-calls storage-backend (::spec/experiment-name learner))
    (choose-epsilon-greedy (arm-states->arm-means arm-states) params)))

(defmethod arm-selection-probabilities* ::spec/epsilon-greedy
  [storage-backend {::spec/keys [experiment-name]}]
  (when-let [arm-states (not-empty (state/get-arm-states storage-backend
                                                         experiment-name))]
    (let [{::spec/keys [epsilon maximize?]}
          (state/get-learner-params storage-backend
                                    experiment-name)
          arm-means (arm-states->arm-means arm-states)
          best-key (if maximize? max-key min-key)
          best-arm (key (apply best-key val arm-means))
          k (count arm-means)]
      (ext/map-kvs
       (fn [arm-name _mean-reward]
         (if (= arm-name best-arm)
           (+ (- 1.0 epsilon) (/ epsilon k))
           (/ epsilon k)))
       arm-means))))

(defn choose-round-robin
  "Chooses an arm-name in round-robin order."
  [arm-names call-count]
  (let [k (count arm-names)]
    (nth arm-names (mod call-count k))))

(defn- upper-confidence-bounds
  "Compute the upper confidence bound for each arm."
  [arm-states {::spec/keys [maximize? exploration-mult]}]
  (let [total-iterations (reduce + (map (comp :n val) arm-states))]
    (into {}
          (for [[arm-name {:keys [mean-reward n]}] arm-states]
            (let [const
                  (* (or exploration-mult 1.0)
                     (Math/sqrt (/ (* 2 (Math/log total-iterations))
                                   n)))]
              [arm-name ((if maximize? + -) mean-reward const)])))))

(defn choose-ucb1
  "Chooses an arm using the upper confidence bound algorithm. This chooses the
   arm that maximizes the expected reward, where the expected reward of the arm
   is defined as the historical mean reward of the arm, plus a constant
   exploration term."
  [arm-states {::spec/keys [maximize?] :as params}]
  {:pre [(not (nil? maximize?))]}
  (let [total-iterations (reduce + (map (comp :n val) arm-states))
        ucbs (upper-confidence-bounds arm-states params)
        best-key (if maximize? max-key min-key)
        best-arm (key (apply best-key val ucbs))]
    best-arm))

(defmethod choose* ::spec/ucb1
  [storage-backend learner]
  (when-let [arm-states (not-empty
                         (state/get-arm-states storage-backend
                                               (::spec/experiment-name learner)))]
    (let [params (state/get-learner-params storage-backend
                                           (::spec/experiment-name learner))
          call-count (state/incr-choose-calls storage-backend
                                              (::spec/experiment-name learner))
          unrewarded-arms (arm-states->unrewarded-arm-names arm-states)
          k (count arm-states)
          num-unrewarded (count unrewarded-arms)]
      (assert k)
      (assert num-unrewarded)
      (assert call-count)
      (cond
        ;; If we haven't received any rewards yet, round-robin between arms.
        ;; This helps us converge faster when rewards are delayed.
        (= num-unrewarded k)
        (choose-round-robin unrewarded-arms call-count)

        ;; If a new arm has been added, choose it 1 out of k times.
        (and (> num-unrewarded 0)
             (< (mod call-count k) num-unrewarded))
        (choose-round-robin unrewarded-arms call-count)

        ;; Otherwise, use standard UCB1 behavior.
        :else
        (choose-ucb1 arm-states params)))))

(defmethod arm-selection-probabilities* ::spec/ucb1
  [storage-backend {::spec/keys [experiment-name]}]
  (when-let [arm-states (not-empty
                         (state/get-arm-states storage-backend
                                               experiment-name))]
    (let [{::spec/keys [maximize?]}
          (state/get-learner-params storage-backend
                                    experiment-name)
          call-count (state/incr-choose-calls storage-backend
                                              experiment-name)
          unrewarded-arms (arm-states->unrewarded-arm-names arm-states)
          k (count arm-states)
          num-unrewarded (count unrewarded-arms)]
      (cond
        (= num-unrewarded k)
        (fmap (constantly (/ 1.0 k)) arm-states)

        (and (> num-unrewarded 0)
             (< (mod call-count k) num-unrewarded))
        (fmap (constantly (/ 1.0 k)) arm-states)

        :else
        (let [ucbs (upper-confidence-bounds arm-states maximize?)
              best-key (if maximize? max-key min-key)
              best-arm (key (apply best-key val ucbs))]
          (ext/map-kvs
           (fn [arm-name _]
             (if (= arm-name best-arm)
               1.0
               0.0))
           arm-states))))))

(defmethod choose* ::spec/random
  [storage-backend learner]
  (when-let [arm-states (not-empty
                         (state/get-arm-states storage-backend
                                               (::spec/experiment-name learner)))]
    (state/incr-choose-calls storage-backend (::spec/experiment-name learner))
    (nth (keys arm-states) (rand-int (count arm-states)))))

(defmethod arm-selection-probabilities* ::spec/random
  [storage-backend learner]
  (when-let [arm-states (not-empty
                         (state/get-arm-states storage-backend
                                               (::spec/experiment-name learner)))]
    (let [p (/ 1.0 (count arm-states))]
      (fmap (constantly p) arm-states))))

(defn- arm-states->softmax
  [arm-states {::spec/keys [starting-temperature
                            temp-decay-per-step
                            min-temperature
                            maximize?]}]
  {:pre [starting-temperature temp-decay-per-step min-temperature]}
  (let [arm-means (arm-states->arm-means arm-states)
        total-iterations (reduce + (map (comp :n val) arm-states))
        current-temp (max min-temperature
                          (- starting-temperature (* temp-decay-per-step
                                                     total-iterations)))
        adjust-by-temp #(math/expt stats/const-e (/ % current-temp))
        adjusted-values (fmap adjust-by-temp arm-means)
        adjusted-values-sum (reduce + (vals adjusted-values))
        softmax (fmap #(/ % adjusted-values-sum)
                      adjusted-values)]
    softmax))

(defn choose-softmax
  "Softmax arm selection. Begins by choosing arms randomly with roughly equal
   probability. As time passes, begins to choose the best arm with increasing
   probability. The speed of convergence is based on the temperature parameter,
   in a process analogous to annealing. Higher temperatures lead to more random
   selection. Low temperatures lead to selecting the historically best arm.

   Softmax is very sensitive to the choice of temperature parameters. Some
   choices can lead to out-of-range floating point values, which cause
   exceptions. For our problems, a good choice of starting temperature is 1.0,
   with temp-decay-per-step set to `(/ 1.0 n)`, where n is equal to the number
   of rewards after which the learner will stop exploring, and min-temperature
   is set to 0.01.

   See http://incompleteideas.net/book/ebook/node17.html for more information
   about this algorithm."
  [arm-states {::spec/keys [maximize?] :as learner-params}]
  {:pre [(boolean? maximize?)]}
  (let [softmaxes (arm-states->softmax arm-states learner-params)
        best-arm (stats/select-by-probability
                  (if maximize?
                    softmaxes
                    (stats/flip-probabilities softmaxes)))]
    best-arm))

(defmethod choose* ::spec/softmax
  [storage-backend learner]
  (when-let [arm-states (not-empty
                         (state/get-arm-states storage-backend
                                               (::spec/experiment-name learner)))]
    (let [params (state/get-learner-params storage-backend
                                           (::spec/experiment-name learner))]
      (state/incr-choose-calls storage-backend (::spec/experiment-name learner))
      (choose-softmax arm-states params))))

(defmethod arm-selection-probabilities* ::spec/softmax
  [storage-backend {::spec/keys [experiment-name] :as learner}]
  (when-let [arm-states (not-empty
                         (state/get-arm-states storage-backend
                                               experiment-name))]
    (let [params (state/get-learner-params storage-backend
                                           experiment-name)
          softmaxes (arm-states->softmax arm-states params)]
      (if (::spec/maximize? params)
        softmaxes
        (stats/flip-probabilities softmaxes)))))

(defmulti reward*
  "Updates the learner state with the given reward. See [[reward]] for details."
  (fn [storage-backend learner reward]
    (::spec/learner-algo learner)))

(defmethod reward* ::spec/random
  [_ _ _])

(defmethod reward* :default
  [storage-backend learner {::spec/keys [reward-value arm-name]}]
  (state/record-reward storage-backend
                       (::spec/experiment-name learner)
                       arm-name
                       reward-value))

(defmulti create-arm*
  "Creates a new arm for the given experiment. See [[create-arm]] for details."
  (fn [storage-backend learner arm-name]
    (::spec/learner-algo learner)))

(defmethod create-arm* :default
  [storage-backend learner arm-name]
  (state/create-arm storage-backend learner arm-name))

(defmulti delete-arm*
  "Deletes an arm for the given experiement. See [[delete-arm]] for details."
  (fn [storage-backend learner arm-name]
    (::spec/learner-algo learner)))

(defmethod delete-arm* :default
  [storage-backend learner arm-name]
  (state/delete-arm storage-backend learner arm-name))

(defn arm-selection-probabilities
  "Reports the current probability that each arm will be chosen."
  [storage-backend learner-info]
  {:pre [(spec/check ::spec/learner-minimal-info learner-info)]}
  {:post [map?]}
  (arm-selection-probabilities* storage-backend learner-info))

;; TODO: real code will pass in just the experiment-name and look up learner
;; from storage-backend.
;; TODO: if this service ends up needing multiple external services, use
;; integrant.
(defn choose
  "Asks the learner to choose an arm. Returns an arm-name string.
   Example invocation:
   ```
   (choose {::spec/learner-algo ::spec/ucb1 ::spec/experiment-name \"exp\"})
   ```"
  [storage-backend learner-info]
  {:pre [(spec/check ::spec/learner-minimal-info learner-info)]}
  {:post [string?]}
  (choose* storage-backend learner-info))

(defn reward
  "Gives a learner the reward for a particular arm. Example invocation:
   ```
   (reward {::spec/learner-algo ::spec/ucb1 ::spec/experiment-name \"exp\"}
           {::spec/reward-value 12.5 ::spec/arm-name \"arm1\"})
   ```"
  [storage-backend learner-info reward]
  {:pre [(spec/check ::spec/learner-minimal-info learner-info)
         (spec/check ::spec/reward reward)]}
  (reward* storage-backend learner-info reward))

(defn bulk-reward
  "Gives a learner a set of rewards for a particular arm. The caller must
   supply the number of rewards, the mean of the rewards, and the variance of
   the rewards. Example invocation:
   ```
   (bulk-reward {::spec/learner-algo ::spec/ucb1 ::spec/experiment-name \"exp\"}
            {::spec/bulk-reward-mean 12.5
             ::spec/bulk-reward-count 10
             ::spec/bulk-reward-max 15
             ::spec/arm-name \"arm1\"})
   ```"
  [storage-backend learner-info bulk-reward]
  {:pre [(spec/check ::spec/learner-minimal-info learner-info)
         (spec/check ::spec/bulk-reward bulk-reward)]}
  (state/bulk-reward storage-backend
                     (::spec/experiment-name learner-info)
                     (::spec/arm-name bulk-reward)
                     bulk-reward))

(defn init
  "Initializes the state of a learner. Example invocation:
   ```
   (init (atom {})
         {::spec/learner-algo ::spec/ucb1
          ::spec/experiment-name \"exp\"
          ::spec/arm-names [\"arm1\" \"arm2\" \"arm3\"]})
   ```"
  [storage-backend learner]
  {:pre [(spec/check ::spec/learner learner)]}
  (state/init-experiment storage-backend learner))

(defn create-arm
  "Adds a new arm to the set of arms the learner can return from `choose`
   calls. Example invocation:
   ```
   (create-arm {::spec/learner-algo ::spec/ucb1 ::spec/experiment-name \"exp\"}
               \"cool-new-arm\")
   ```"
  [storage-backend learner-info arm-name]
  {:pre [(spec/check ::spec/learner-minimal-info learner-info)
         (spec/check ::spec/arm-name arm-name)]}
  (create-arm* storage-backend learner-info arm-name))

(defn delete-arm
  "Removes an arm from the set of arms the learner can return from `choose`
   calls. All learner-specific state for the arm is deleted. Example invocation:
   ```
   (delete-arm {::spec/learner-algo ::spec/ucb1 ::spec/experiment-name \"exp\"}
               \"cool-new-arm\")
   ```"
  [storage-backend learner-info arm-name]
  {:pre [(spec/check ::spec/learner-minimal-info learner-info)
         (spec/check ::spec/arm-name arm-name)]}
  (delete-arm* storage-backend learner-info arm-name))

(defn get-arm-states
  "Gets the state of all arms for the given learner. This can be used to create
   reports or summaries of bandit results."
  [storage-backend learner-info]
  {:pre [(spec/check ::spec/learner-minimal-info learner-info)]}
  (state/get-arm-states storage-backend (::spec/experiment-name learner-info)))
