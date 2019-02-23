(ns curbside.bandit.learner-state
  "Contains various state backends that handle persisting
   state for multi-armed bandit algorithms. Currently contains a
   non-persistent in-memory implementation, for testing and a
   a Redis implementation using Carmine."
  (:require
   [clojure.algo.generic.functor :refer [fmap]]
   [curbside.bandit.spec :as spec]
   [curbside.bandit.util :as util]
   [taoensso.carmine :as car :refer (wcar)]))

(def ^:private carmine-conn-type
  "Alias for the type of Carmine's connection specification maps, to make
   defmethod calls more readable. A simple example of a valid carmine connection
   is
   {:pool {} :spec {:uri \"redis://localhost:6379/0\"}}"
  clojure.lang.PersistentArrayMap)

(defn- arm-names-key
  "For a given experiment, creates the key containing the names of all arms for
   the experiment. This is a Redis set."
  [experiment-name]
  (util/fmt-key "bandit:experiment:%s:arm-names" experiment-name))

(defn- arm-state-key
  "For a given experiment and arm name, creates the key containing the state of
   the arm. This is a Redis hashmap."
  [experiment-name arm-name]
  (util/fmt-key "bandit:experiment:%s:arm-states:%s" experiment-name arm-name))

(defn- max-reward-key
  "For a given experiment name, creates the key containing the max reward value
   ever seen for the given experiment. Stored as a plain Redis key/value."
  [experiment-name]
  (util/fmt-key "bandit:experiment:%s:max-reward" experiment-name))

(defn- params-key
  "For a given experiment name, creates the key to contain the parameters for
   that experiment. Stored as a Redis hashmap."
  [experiment-name]
  (util/fmt-key "bandit:experiment:%s:params" experiment-name))

(defn- ->redis-map
  "Convert the raw result of an `hgetall` command to a map."
  [x]
  (->> x
       (partition 2)
       (map #(into [] %))
       (into {})))

(defmulti get-arm-states
  "Gets the state of all arms for the given experiment. The shape of the
   state structure depends on the learner-algorithm."
  (fn [backend experiment-name]
    (type backend)))

(defmethod get-arm-states clojure.lang.Atom
  [backend experiment-name]
  (get-in @backend [experiment-name :arm-states]))

(defn- redis-get-arm-names
  "Get the names of all arms for an experiment from Redis."
  [conn experiment-name]
  (wcar conn (car/smembers (arm-names-key experiment-name))))

(defn redis-get-arm-state
 [conn experiment-name arm-name]
 (let [k (arm-state-key experiment-name arm-name)]
   (->> (wcar conn (car/hgetall k))
        ->redis-map
        (fmap util/parse-double)
        util/keywordify-keys
        ((fn [x] (update x :n int))))))

(defmethod get-arm-states carmine-conn-type
  [backend experiment-name]
  (let [arm-names (redis-get-arm-names backend experiment-name)]
    (->> (for [arm-name arm-names]
           (let [arm-state (redis-get-arm-state backend
                                                experiment-name
                                                arm-name)]
             [arm-name arm-state]))
         (into {}))))

(defmulti record-reward
  "For a given experiment and arm, increment the total number of times the
   arm has been chosen, and add the given reward to the total reward the arm
   has earned so far. Uses Welford's Online Algorithm for improved numerical
   stability and so that we can compute the variance of the rewards. See
   https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_Online_algorithm

   Note: rewards are scaled to be between 0 and 1.0 before being recorded. This
   is needed for UCB1."
  (fn [backend experiment-name arm-name reward]
    (type backend)))

(defn record-reward*
  "Updates an arm's reward state using Welford's Algorithm."
  [reward max-reward {:keys [mean-reward n mean-sq-dist] :as old-arm-state}]
  (let [new-max-reward (max reward max-reward)
        scaled-reward (/ reward new-max-reward)
        delta (- scaled-reward mean-reward)
        new-mean-reward (+ mean-reward (/ delta (inc n)))
        delta2 (- scaled-reward new-mean-reward)
        new-state {:n (inc n)
                   :mean-reward new-mean-reward
                   :mean-sq-dist (+ mean-sq-dist (* delta delta2))}]
    [new-max-reward new-state]))

(defmethod record-reward clojure.lang.Atom
  [backend experiment-name arm-name reward]
  (swap! backend
         (fn [b]
           (if-let [old-arm-state
                    (get-in b [experiment-name
                               :arm-states
                               arm-name])]
             (let [max-reward
                   (get-in b [experiment-name :max-reward])
                   [new-max-reward new-state]
                   (record-reward* reward max-reward old-arm-state)]
               (-> b
                 (assoc-in [experiment-name :max-reward] new-max-reward)
                 (assoc-in [experiment-name :arm-states arm-name] new-state)))))))

;; Note: this uses a lua script to ensure that all the steps of Welford's
;; algorithm are performed atomically.
(defmethod record-reward carmine-conn-type
  [backend experiment-name arm-name reward]
  (let [arm-key (arm-state-key experiment-name arm-name)
        max-reward-key (max-reward-key experiment-name)]
    (wcar backend
          (car/lua
           "local max_reward = redis.call('get',_:max-reward-key)
            local mean_reward = redis.call ('hget',_:arm-key,'mean-reward')
            local mean_sq_dist = redis.call ('hget',_:arm-key,'mean-sq-dist')
            local new_n = 1 + redis.call ('hget',_:arm-key,'n')

            local new_max_reward = math.max(_:reward,max_reward)
            local scaled_reward = _:reward/new_max_reward
            local delta = scaled_reward - mean_reward
            local new_mean_reward = mean_reward + (delta / new_n)
            local delta2 = scaled_reward - new_mean_reward
            local new_mean_sq_dist = mean_sq_dist + delta * delta2

            redis.call('set',_:max-reward-key,new_max_reward)
            redis.call('hset',_:arm-key,'n',new_n)
            redis.call('hset',_:arm-key,'mean-reward',new_mean_reward)
            redis.call('hset',_:arm-key,'mean-sq-dist',new_mean_sq_dist)"
           {:arm-key arm-key
            :max-reward-key max-reward-key}
           {:reward reward}))))

(defmulti get-learner-params
  "Get the parameters of the learner."
  (fn [backend experiment-name]
    (type backend)))

(defmethod get-learner-params clojure.lang.Atom
  [backend experiment-name]
  (get-in @backend [experiment-name :params]))

(defmethod get-learner-params carmine-conn-type
  [conn experiment-name]
  (-> (wcar conn (car/hgetall (params-key experiment-name)))
      (->redis-map)
      (->> (util/keywordify-keys "curbside.bandit.spec"))
      (util/update-in-if-contains [::spec/epsilon] util/parse-double)
      (util/update-in-if-contains [::spec/starting-temperature]
                                  util/parse-double)
      (util/update-in-if-contains [::spec/temp-decay-per-step]
                                  util/parse-double)
      (util/update-in-if-contains [::spec/min-temperature]
                                  util/parse-double)))

(defmulti create-arm
  "Adds an arm to an existing experiment."
  (fn [backend learner arm-name]
    (type backend)))

(def default-arm-state {:n 1
                        :mean-reward 1.0
                        :mean-sq-dist 10.0})

(defmethod create-arm clojure.lang.Atom
  [backend {::spec/keys [experiment-name]} arm-name]
  (swap! backend
         (fn [b]
           (assoc-in b
                     [experiment-name :arm-states arm-name]
                     default-arm-state))))

(defn- redis-init-arm
  "Initializes a new arm in Redis for a given experiment."
  [experiment-name arm-name]
  (car/hmset* (arm-state-key experiment-name arm-name)
              (util/stringify-keys default-arm-state)))

(defmethod create-arm carmine-conn-type
  [backend {::spec/keys [experiment-name]} arm-name]
  (wcar backend
        (car/sadd (arm-names-key experiment-name) arm-name)
        (redis-init-arm experiment-name arm-name)))

(defmulti init-experiment
  "Initialize a new learner for a new experiment."
  (fn [backend learner]
    (type backend)))

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

(defmethod init-experiment carmine-conn-type
  [conn {::spec/keys [learner-algo algo-params
                      arm-names experiment-name]
         :as _learner-map}]
  (wcar conn
        (car/multi)
        (car/hmset* (params-key experiment-name)
                    (util/stringify-keys algo-params))
        (car/set (max-reward-key experiment-name) 1.0)
        (apply car/sadd (arm-names-key experiment-name) arm-names)
        (dorun (map #(redis-init-arm experiment-name %) arm-names))
        (car/exec)))

(defmulti delete-arm
  "Deletes an arm from an existing experiment."
  (fn [backend learner arm-name]
    (type backend)))

(defmethod delete-arm clojure.lang.Atom
  [backend {::spec/keys [experiment-name]} arm-name]
  (swap! backend
         (fn [b]
           (update-in b [experiment-name :arm-states] dissoc arm-name))))

(defmethod delete-arm carmine-conn-type
  [conn {::spec/keys [experiment-name]} arm-name]
  (wcar conn
        (car/multi)
        (car/srem (arm-names-key experiment-name) arm-name)
        (car/del (arm-state-key experiment-name arm-name))
        (car/exec)))
