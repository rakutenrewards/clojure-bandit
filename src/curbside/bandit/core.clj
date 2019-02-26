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
   [curbside.bandit.learner-state :as state]
   [curbside.bandit.spec :as spec]
   [curbside.bandit.stats :as stats]))

(defmulti choose*
  "Chooses an ::spec/arm-name for the given learner. See documentation of
   [[choose]] for details."
  (fn [storage-backend learner]
    (::spec/learner-algo learner)))

(defn arm-states->arm-means
  "Computes the mean reward of each arm."
  [arm-states]
  (fmap :mean-reward arm-states))

(defn arm-states->arm-variances
  "Computes the variance of the reward of each arm."
  [arm-states]
  (fmap #(/ (:mean-sq-dist %) (:n %)) arm-states))

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
        rand-arm (nth arm-names (rand-int k))]
    (if (< (rand) threshold) best-arm rand-arm)))

(defmethod choose* ::spec/epsilon-greedy
  [storage-backend learner]
  ;; TODO: it is likely that all choose* implementations will look like this, in
  ;; which case we don't need the state-fetching step in the multimethod.
  (let [arm-states (state/get-arm-states storage-backend
                                         (::spec/experiment-name learner))
        params (state/get-learner-params storage-backend
                                         (::spec/experiment-name learner))]
    (choose-epsilon-greedy (arm-states->arm-means arm-states) params)))

(defn choose-ucb1
  "Chooses an arm using the upper confidence bound algorithm. This chooses the
   arm that maximizes the expected reward, where the expected reward of the arm
   is defined as the historical mean reward of the arm, plus a constant
   exploration term."
  [arm-states {::spec/keys [maximize?] :as _params}]
  {:pre [(not (nil? maximize?))]}
  (let [total-iterations (reduce + (map (comp :n val) arm-states))
        ucbs (into {}
                   (for [[arm-name {:keys [mean-reward n]}] arm-states]
                     (let [const (Math/sqrt (/ (* 2 (Math/log total-iterations))
                                               n))]
                       [arm-name (+ mean-reward const)])))
        best-key (if maximize? max-key min-key)
        best-arm (key (apply best-key val ucbs))]
    best-arm))

(defmethod choose* ::spec/ucb1
  [storage-backend learner]
  (let [arm-states (state/get-arm-states storage-backend
                                         (::spec/experiment-name learner))
        params (state/get-learner-params storage-backend
                                         (::spec/experiment-name learner))]
    (choose-ucb1 arm-states params)))

(defmethod choose* ::spec/random
  [storage-backend learner]
  (let [arm-states (state/get-arm-states storage-backend
                                         (::spec/experiment-name learner))]
    (nth (keys arm-states) (rand-int (count arm-states)))))

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
  [arm-states {::spec/keys [starting-temperature
                            temp-decay-per-step
                            min-temperature
                            maximize?]}]
  {:pre [starting-temperature temp-decay-per-step min-temperature
         (boolean? maximize?)]}
  (let [arm-means (arm-states->arm-means arm-states)
        total-iterations (reduce + (map (comp :n val) arm-states))
        current-temp (max min-temperature
                          (- starting-temperature (* temp-decay-per-step
                                                     total-iterations)))
        adjust-by-temp #(math/expt stats/const-e (/ % current-temp))
        adjusted-values (fmap adjust-by-temp arm-means)
        softmaxes (fmap #(/ (adjust-by-temp %)
                            (reduce + (vals adjusted-values)))
                        arm-means)
        best-arm (stats/select-by-probability (if maximize?
                                                softmaxes
                                                (fmap #(- 1.0 %) softmaxes)))]
    best-arm))

(defmethod choose* ::spec/softmax
  [storage-backend learner]
  (let [arm-states (state/get-arm-states storage-backend
                                         (::spec/experiment-name learner))
        params (state/get-learner-params storage-backend
                                         (::spec/experiment-name learner))]
    (choose-softmax arm-states params)))

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
