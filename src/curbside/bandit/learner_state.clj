(ns curbside.bandit.learner-state
  "Contains various state backends that handle persisting
   state for multi-armed bandit algorithms. Currently contains a
   non-persistent in-memory implementation, for testing and a
   a Redis implementation using Carmine."
  (:require
   [clojure.algo.generic.functor :refer [fmap]]
   [curbside.bandit.ext :as ext]
   [curbside.bandit.redis :as redis]
   [curbside.bandit.spec :as spec]
   [taoensso.carmine :as car :refer [wcar]])
  (:import
   (clojure.lang PersistentArrayMap Atom)))

(def ^:private carmine-conn-type
  "Alias for the type of Carmine's connection specification maps, to make
   defmethod calls more readable. A simple example of a valid carmine connection
   is
   {:pool {} :spec {:uri \"redis://localhost:6379/0\"}}"
  PersistentArrayMap)

(defn- arm-names-key
  "For a given experiment, creates the key containing the names of all arms for
   the experiment. This is a Redis set."
  [experiment-name]
  (redis/fmt-key "bandit:experiment:%s:arm-names" experiment-name))

(defn- arm-state-key
  "For a given experiment and arm name, creates the key containing the state of
   the arm. This is a Redis hashmap."
  [experiment-name arm-name]
  (redis/fmt-key "bandit:experiment:%s:arm-states:%s" experiment-name arm-name))

(defn- max-reward-key
  "For a given experiment name, creates the key containing the max reward value
   ever seen for the given experiment. Stored as a plain Redis key/value."
  [experiment-name]
  (redis/fmt-key "bandit:experiment:%s:max-reward" experiment-name))

(defn- params-key
  "For a given experiment name, creates the key to contain the parameters for
   that experiment. Stored as a Redis hashmap."
  [experiment-name]
  (redis/fmt-key "bandit:experiment:%s:params" experiment-name))

(defn- choose-count-key
  "For a given experiment name, creates the key counting the number of times
   `choose` has been called on the given experiment."
  [experiment-name]
  (redis/fmt-key "bandit:experiment:%s:choose-count" experiment-name))

(defmulti get-arm-states
  "Gets the state of all arms for the given experiment. The shape of the
   state structure depends on the learner-algorithm."
  (fn [backend _experiment-name]
    (type backend)))

(defn- remove-deleted-arms
  "Given a map from arm ids to arm states, remove all arms that have been
   soft-deleted."
  [arm-states]
  (->> arm-states
       (filter (fn [[_ v]] (not (:deleted? v))))
       (into {})
       (fmap #(dissoc % :deleted?))))

(defn- convert-arm-state-types
  "converts data in arm-state to expected types for core fns."
  [arm-state]
  (->> arm-state
       (fmap ext/parse-double)
       ((fn [x] (update x :n int)))))

(defmethod get-arm-states Atom
  [backend experiment-name]
  (->> (get-in @backend [experiment-name :arm-states])
       remove-deleted-arms))

(defn- redis-get-arm-names
  "Get the names of all arms for an experiment from Redis."
  [conn experiment-name]
  (wcar conn (car/smembers (arm-names-key experiment-name))))

(defn- redis-get-arm-state
  [conn experiment-name arm-name]
  (let [k (arm-state-key experiment-name arm-name)]
    (->> (wcar conn (car/hgetall k))
         redis/->redis-map
         ext/keywordify-keys)))

(defmethod get-arm-states carmine-conn-type
  [backend experiment-name]
  (let [arm-names (redis-get-arm-names backend experiment-name)]
    (->> (for [arm-name arm-names]
           (let [arm-state (redis-get-arm-state backend
                                                experiment-name
                                                arm-name)]
             [arm-name arm-state]))
         (into {})
         remove-deleted-arms
         (fmap convert-arm-state-types))))

(defmulti record-reward
  "For a given experiment and arm, increment the total number of times the
   arm has been chosen, and add the given reward to the total reward the arm
   has earned so far. Uses Welford's Online Algorithm for improved numerical
   stability and so that we can compute the variance of the rewards. See
   https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_Online_algorithm

   Note: rewards are scaled to be between 0 and 1.0 before being recorded. This
   is needed for UCB1."
  (fn [backend _experiment-name _arm-name _reward-lower-bound _reward]
    (type backend)))

(defn- record-reward*
  "Updates an arm's reward state using Welford's Algorithm."
  [reward max-reward {:keys [mean-reward n deleted?] :as _old-arm-state} reward-lower-bound]
  (let [adjusted-reward (max reward reward-lower-bound)
        new-max-reward (max adjusted-reward max-reward)
        scaled-reward (if (== new-max-reward reward-lower-bound)
                        reward-lower-bound
                        (/ (- adjusted-reward reward-lower-bound)
                           (- new-max-reward reward-lower-bound)))
        delta (- scaled-reward mean-reward)
        new-mean-reward (+ mean-reward (/ delta (inc n)))
        new-state {:n (inc n)
                   :mean-reward new-mean-reward
                   :deleted? deleted?}]
    [new-max-reward new-state]))

(defmethod record-reward Atom
  [backend experiment-name arm-name reward-lower-bound reward]
  (swap! backend
         (fn [b]
           (if-let [old-arm-state
                    (get-in b [experiment-name
                               :arm-states
                               arm-name])]
             (let [max-reward (get-in b [experiment-name :max-reward])
                   [new-max-reward new-state] (record-reward* reward
                                                              max-reward
                                                              old-arm-state
                                                              reward-lower-bound)]
               (-> b
                   (assoc-in [experiment-name :max-reward] new-max-reward)
                   (assoc-in [experiment-name :arm-states arm-name] new-state)))
             b))))

;; Note: this uses a lua script to ensure that all the steps of Welford's
;; algorithm are performed atomically.
(defmethod record-reward carmine-conn-type
  [backend experiment-name arm-name reward-lower-bound reward]
  {:pre [backend experiment-name arm-name reward-lower-bound reward]}
  (let [arm-key (arm-state-key experiment-name arm-name)
        max-reward-key (max-reward-key experiment-name)]
    (wcar backend
          (car/lua
           "if redis.call('exists',_:arm-key) == 1 then
              local max_reward = redis.call('get',_:max-reward-key)
              local mean_reward = redis.call ('hget',_:arm-key,'mean-reward')
              local new_n = 1 + redis.call ('hget',_:arm-key,'n')

              local adjusted_reward = math.max(_:reward,_:reward_lower_bound)
              local new_max_reward = math.max(adjusted_reward,max_reward)
              local scaled_reward = (new_max_reward == _:reward_lower_bound) and _:reward_lower_bound or (adjusted_reward - _:reward_lower_bound) / (new_max_reward - _:reward_lower_bound)
              local delta = scaled_reward - mean_reward
              local new_mean_reward = mean_reward + (delta / new_n)

              redis.call('set',_:max-reward-key,new_max_reward)
              redis.call('hset',_:arm-key,'n',new_n)
              redis.call('hset',_:arm-key,'mean-reward',new_mean_reward)
            end"
           {:arm-key arm-key
            :max-reward-key max-reward-key}
           {:reward reward
            :reward_lower_bound reward-lower-bound}))))

(defmulti bulk-reward
  "For a given experiment and arm, increment the total number of times the
   arm has been chosen, and combine the existing mean and variance data for this
   arm with the supplied values. This uses an extension of Welford's algorithm
   that allows statistics for a subset of the rewards to be computed by the
   client, so that we don't need to call Redis n times. See
   https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm

   Note: rewards are scaled to be between 0 and 1.0 before being recorded. This
   is needed for UCB1.

   Note: In some cases (especially when large new numbers appear mid-stream),
   this will have different results than calling reward on each incoming reward
   one-by-one. This is because bulk rewards are scaled by the highest number in
   the batch, rather than by each new highest number as it comes in. For
   example, if we have four rewards

   [1 3 2 5]

   if we call bulk-reward, the mean reward before scaling will be 2.75. We scale
   it by the biggest number we know of yet (5), and get a scaled mean reward of
   0.55.

   However, if we call reward(1), reward(3), reward(2), reward(5), we can only
   scale by the highest number we have seen so far.

   reward(1) will be scaled by 1 for a reward of 1.0
   reward(3) will be scaled relative to 3 for a reward of 1.0
   reward(2) will be scaled relative to 3 for a reward of 0.666
   reward(5) will be scaled relative to 5 for a reward of 1.0
   and so the total average stored in the arm after these calls will be 0.9165.

   This seems like a big difference, but it tends to even out as the number of
   rewards increases. Here is an informal argument:

   After an infinite number of steps, our probability
   of having seen the global maximum reward value (assuming rewards are bounded)
   is 1.0. Furthermore, there will be infinitely many additional rewards after
   we've seen the global maximum, so we are guaranteed to eventually converge
   on the true scaled mean."
  (fn [backend _experiment-name _arm-name _reward-lower-bound _reward]
    (type backend)))

(defn- bulk-reward*
  [{::spec/keys [bulk-reward-mean bulk-reward-count bulk-reward-max] :as _bulk-reward}
   old-max-reward
   {old-mean-reward :mean-reward old-n :n :as _old-arm-state}
   reward-lower-bound]
  (let [new-n (+ old-n bulk-reward-count)
        adjusted-mean-reward (max bulk-reward-mean reward-lower-bound)
        adjusted-max-reward (max bulk-reward-max reward-lower-bound)
        new-max-reward (max old-max-reward adjusted-max-reward)
        scaled-bulk-reward-mean (if (= new-max-reward reward-lower-bound)
                                  reward-lower-bound
                                  (/ (- adjusted-mean-reward reward-lower-bound)
                                     (- new-max-reward reward-lower-bound)))
        delta (- scaled-bulk-reward-mean old-mean-reward)
        new-mean-reward (+ old-mean-reward
                           (* delta (/ bulk-reward-count new-n)))
        new-state {:n new-n
                   :mean-reward new-mean-reward}]
    [new-max-reward new-state]))

(defmethod bulk-reward Atom
  [backend experiment-name arm-name reward-lower-bound bulk-reward]
  {:pre [backend experiment-name arm-name reward-lower-bound bulk-reward]}
  (swap! backend
         (fn [b]
           (if-let [old-arm-state (get-in b [experiment-name :arm-states arm-name])]
             (let [max-reward (get-in b [experiment-name :max-reward])
                   [new-max-reward new-state] (bulk-reward* bulk-reward
                                                            max-reward
                                                            old-arm-state
                                                            reward-lower-bound)]
               (-> b
                   (assoc-in [experiment-name :max-reward] new-max-reward)
                   (assoc-in [experiment-name :arm-states arm-name] new-state)))
             b))))

(defmethod bulk-reward carmine-conn-type
  [backend experiment-name arm-name reward-lower-bound {::spec/keys [bulk-reward-mean
                                                                     bulk-reward-count
                                                                     bulk-reward-max]
                                                        :as bulk-reward}]
  {:pre [backend experiment-name arm-name reward-lower-bound bulk-reward]}
  (let [arm-key (arm-state-key experiment-name arm-name)
        max-reward-key (max-reward-key experiment-name)]
    (wcar backend
          (car/lua
           "if redis.call('exists',_:arm_key) == 1 then
              local old_max_reward = redis.call('get',_:max_reward_key)
              local old_mean_reward = redis.call('hget',_:arm_key,'mean-reward')
              local new_n = _:bulk_reward_count + redis.call('hget',_:arm_key,'n')

              local adjusted_mean_reward = math.max(_:bulk_reward_mean,_:reward_lower_bound)
              local adjusted_max_reward = math.max(_:bulk_reward_max,_:reward_lower_bound)

              local new_max_reward = math.max(old_max_reward, adjusted_max_reward)
              local scaled_bulk_reward_mean = (new_max_reward == _:reward_lower_bound) and _:reward_lower_bound or (adjusted_mean_reward - _:reward_lower_bound) / (new_max_reward - _:reward_lower_bound)
              local delta = scaled_bulk_reward_mean - old_mean_reward
              local new_mean_reward = old_mean_reward + (delta * (_:bulk_reward_count / new_n))

              redis.call('set',_:max_reward_key,new_max_reward)
              redis.call('hset',_:arm_key,'n',new_n)
              redis.call('hset',_:arm_key,'mean-reward',new_mean_reward)
            end"
           {:arm_key arm-key
            :max_reward_key max-reward-key}
           {:bulk_reward_count bulk-reward-count
            :bulk_reward_max bulk-reward-max
            :bulk_reward_mean bulk-reward-mean
            :reward_lower_bound reward-lower-bound}))))

(defmulti get-learner-params
  "Get the parameters of the learner."
  (fn [backend _experiment-name]
    (type backend)))

(defmethod get-learner-params Atom
  [backend experiment-name]
  (get-in @backend [experiment-name :params]))

(defmethod get-learner-params carmine-conn-type
  [conn experiment-name]
  (-> (wcar conn (car/hgetall (params-key experiment-name)))
      (redis/->redis-map)
      (->> (ext/keywordify-keys "curbside.bandit.spec"))
      (ext/update-in-if-contains [::spec/epsilon] ext/parse-double)
      (ext/update-in-if-contains [::spec/starting-temperature]
                                 ext/parse-double)
      (ext/update-in-if-contains [::spec/temp-decay-per-step]
                                 ext/parse-double)
      (ext/update-in-if-contains [::spec/min-temperature]
                                 ext/parse-double)
      (ext/update-in-if-contains [::spec/exploration-mult]
                                 ext/parse-double)
      (ext/update-in-if-contains [::spec/reward-lower-bound]
                                 ext/parse-double)
      (ext/update-in-if-contains [::spec/learner-algo]
                                 keyword)))

(defmulti create-arm
  "Adds an arm to an existing experiment."
  (fn [backend _learner _arm-name]
    (type backend)))

(def ^:private default-arm-state
  {:n 1
   :mean-reward 0.0
   :deleted? false})

(def ^:private default-algo-params
  {::spec/reward-lower-bound 0.0})

(defmethod create-arm Atom
  [backend {::spec/keys [experiment-name]} arm-name]
  (swap! backend
         (fn [b]
           (if (get-in b [experiment-name :arm-states arm-name :deleted?])
             (assoc-in b [experiment-name :arm-states arm-name :deleted?] false)
             (assoc-in b [experiment-name :arm-states arm-name] default-arm-state)))))

(defn- redis-init-arm
  "Initializes a new arm in Redis for a given experiment."
  [backend experiment-name arm-name]
  (let [key (arm-state-key experiment-name arm-name)]
    (if (first (wcar backend (car/hmget key "deleted?")))
      (car/hmset key "deleted?" false)
      (car/hmset* key
                  (ext/stringify-keys default-arm-state)))))

(defmethod create-arm carmine-conn-type
  [backend {::spec/keys [experiment-name]} arm-name]
  (wcar backend
        (car/sadd (arm-names-key experiment-name) arm-name)
        (redis-init-arm backend experiment-name arm-name)))

(defmulti exists?
  "Returns true if an experiment exists."
  (fn [backend _learner]
    (type backend)))

(defmethod exists? Atom
  [backend {::spec/keys [experiment-name]
            :as _learner-map}]
  (not (nil? (get @backend experiment-name))))

(defmethod exists? carmine-conn-type
  [conn {::spec/keys [experiment-name]
         :as _learner-map}]
  (let [[redis-result] (wcar conn
                             ;; any key that is set when the experiment is
                             ;; created could be used here.
                             (car/get (max-reward-key experiment-name)))]
    (not (nil? redis-result))))

(defmulti init-experiment
  "Initialize a new learner for a new experiment."
  (fn [backend _learner]
    (type backend)))

(defmethod init-experiment Atom
  [backend {::spec/keys [algo-params arm-names experiment-name]
            :as learner-map}]
  (when-not (exists? backend learner-map)
    (let [algo-params-with-defaults (merge default-algo-params algo-params)
          arm-states (into {} (for [arm-name arm-names]
                                [arm-name default-arm-state]))]
      (swap! backend
             (fn [b]
               (-> b
                   (assoc-in [experiment-name :params] algo-params-with-defaults)
                   (assoc-in [experiment-name :arm-states] arm-states)
                   (assoc-in [experiment-name :max-reward] 1.0)
                   (assoc-in [experiment-name :choose-count] 0)))))))

(defmethod init-experiment carmine-conn-type
  [conn {::spec/keys [algo-params arm-names experiment-name]
         :as learner-map}]
  (when-not (exists? conn learner-map)
    (let [algo-params-with-defaults (merge default-algo-params algo-params)]
      (wcar conn
            (car/multi)
            (car/hmset* (params-key experiment-name)
                        (ext/stringify-keys algo-params-with-defaults))
            (car/set (max-reward-key experiment-name) 1.0)
            (apply car/sadd (arm-names-key experiment-name) arm-names)
            (dorun (map #(redis-init-arm conn experiment-name %) arm-names))
            (car/set (choose-count-key experiment-name) 0)
            (car/exec)))))

(defmulti delete-arm
  "Deletes an arm from an existing experiment."
  (fn [backend _learner _arm-name]
    (type backend)))

(defmethod delete-arm Atom
  [backend {::spec/keys [experiment-name]} arm-name]
  (swap! backend
         (fn [b]
           (assoc-in b [experiment-name :arm-states arm-name :deleted?] true))))

(defmethod delete-arm carmine-conn-type
  [conn {::spec/keys [experiment-name]} arm-name]
  (wcar conn
        (car/multi)
        (car/hmset (arm-state-key experiment-name arm-name) "deleted?" true)
        (car/exec)))

(defmulti incr-choose-count
  "Increments a counter which represents the number of times `choose` has
   been called on the given experiment name. Returns the new count, after
   it has been incremented."
  (fn [backend _experiment-name]
    (type backend)))

(defmethod incr-choose-count Atom
  [backend experiment-name]
  (let [path [experiment-name :choose-count]]
    (get-in (swap! backend update-in path inc) path)))

(defmethod incr-choose-count carmine-conn-type
  [conn experiment-name]
  (wcar conn (car/incr (choose-count-key experiment-name))))

(defmulti get-choose-count
  "Gets the current number of times `choose` has been called on the given
   experiment name."
  (fn [backend _experiment-name]
    (type backend)))

(defmethod get-choose-count Atom
  [backend experiment-name]
  (let [path [experiment-name :choose-count]]
    (get-in @backend path)))

(defmethod get-choose-count carmine-conn-type
  [conn experiment-name]
  (ext/parse-int
   (wcar conn (car/get (choose-count-key experiment-name)))))
