(ns curbside.bandit.learner-state
  "Contains various state backends that handle persisting
   state for multi-armed bandit algorithms. Currently contains a
   non-persistent in-memory implementation, for testing and proof-of-concept."
  (:require
   [curbside.bandit.spec :as spec]))

(defmulti get-arm-states
  "Gets the state of all arms for the given experiment. The shape of the
   state structure depends on the learner-algorithm."
  (fn [backend experiment-name]
    (type backend)))

(defmethod get-arm-states clojure.lang.Atom
  [backend experiment-name]
  (get-in @backend [experiment-name :arm-states]))

(defmulti record-reward
  "For a given experiment and arm, increment the total number of times the
   arm has been chosen, and add the given reward to the total reward the arm
   has earned so far.
   TODO: this is only sufficient for bandit algorithms that make decisions based
   on average reward per arm. This is insufficient for Thompson sampling. To
   fix, we need to dispatch on [(type backend) learner-algo]"
  (fn [backend experiment-name arm-name reward]
    (type backend)))

(defmethod record-reward clojure.lang.Atom
  [backend experiment-name arm-name reward]
  (swap! backend
         (fn [b]
           (if-let [{:keys [total-reward n]} (get-in b [experiment-name
                                                        :arm-states
                                                        arm-name])]
             (let [old-max-reward (get-in b [experiment-name :max-reward])
                   max-reward (max reward old-max-reward)
                   scaled-reward (/ reward max-reward)
                   new-state {:total-reward (+ total-reward scaled-reward)
                              :n (inc n)}]
               (cond-> b
                 (> max-reward old-max-reward)
                 (assoc-in [experiment-name :max-reward] max-reward)
                 true
                 (assoc-in [experiment-name :arm-states arm-name] new-state)))
             b))))

(defmulti get-learner-params
  "Get the parameters of the learner."
  (fn [backend experiment-name]
    (type backend)))

(defmethod get-learner-params clojure.lang.Atom
  [backend experiment-name]
  (get-in @backend [experiment-name :params]))

(defmulti init-experiment
  "Initialize a new learner for a new experiment."
  (fn [backend learner]
    (type backend)))

(def default-arm-state {:total-reward 1.0 :n 1})

(defmethod init-experiment clojure.lang.Atom
  [backend {::spec/keys [learner-algo algo-params
                         arm-names experiment-name]
            :as _learner-map}]
  (let [arm-states (into {} (for [arm-name arm-names]
                              [arm-name default-arm-state]))]
    (swap! backend
           (fn [b]
             (-> b
                 (assoc-in [experiment-name :params] algo-params)
                 (assoc-in [experiment-name :arm-states] arm-states)
                 (assoc-in [experiment-name :max-reward] 1.0))))))

(defmulti create-arm
  "Adds an arm to an existing experiment."
  (fn [backend learner arm-name]
    (type backend)))

(defmethod create-arm clojure.lang.Atom
  [backend {::spec/keys [experiment-name]} arm-name]
  (swap! backend
         (fn [b]
           (assoc-in b
                     [experiment-name :arm-states arm-name]
                     default-arm-state))))

(defmulti delete-arm
  "Deletes an arm from an existing experiment."
  (fn [backend learner arm-name]
    (type backend)))

(defmethod delete-arm clojure.lang.Atom
  [backend {::spec/keys [experiment-name]} arm-name]
  (swap! backend
         (fn [b]
           (update-in b [experiment-name :arm-states] dissoc arm-name))))
