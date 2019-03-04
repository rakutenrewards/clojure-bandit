(ns curbside.bandit.spec
  (:require
   [clojure.spec.alpha :as spec]
   [expound.alpha :as expound]))

(defn check [type data]
  (if (spec/valid? type data)
    true
    (throw (IllegalArgumentException. (expound/expound-str type data)))))

(spec/def ::finite-double
  (spec/and number?
            #(not= % Double/POSITIVE_INFINITY)
            #(not= % Double/NEGATIVE_INFINITY)
            #(not= % Double/NaN)))

(spec/def ::maximize? boolean?)

(spec/def ::common-params
  (spec/keys :req [::maximize?]))

(spec/def ::learner-algo #{::epsilon-greedy ::ucb1 ::random ::softmax})

(spec/def ::starting-temperature ::finite-double)

(spec/def ::temp-decay-per-step ::finite-double)

(spec/def ::min-temperature ::finite-double)

(spec/def ::epsilon float?)

(spec/def ::epsilon-greedy-params
  (spec/and ::common-params
            (spec/keys :req [::epsilon])
            #(= ::epsilon-greedy (::learner-algo %))))

(spec/def ::ucb1-params
  (spec/and ::common-params
            #(= ::ucb1 (::learner-algo %))))

(spec/def ::random-params
  (spec/and ::common-params
            #(= ::random (::learner-algo %))))

(spec/def ::softmax-params
  (spec/and ::common-params
            (spec/keys :req [::starting-temperature
                             ::temp-decay-per-step
                             ::min-temperature])
            #(= ::softmax (::learner-algo %))))

(spec/def ::algo-params
  (spec/and (spec/keys :req [::learner-algo])
            (spec/or :epsilon-greedy ::epsilon-greedy-params
                     :ucb1 ::ucb1-params
                     :random ::random-params
                     :softmax ::softmax-params)))

(spec/def ::arm-name string?)

(spec/def ::arm-names (spec/coll-of ::arm-name :kind vector? :distinct true))

(spec/def ::experiment-name string?)

(spec/def ::learner
  (spec/keys :req [::algo-params
                   ::arm-names
                   ::experiment-name]))

;; minimal keys required to dispatch learner logic -- the algorithm used
;; (for code dispatch) and the experiment name (for backing store lookup).
(spec/def ::learner-minimal-info
  (spec/keys :req [::learner-algo
                   ::experiment-name]))

(spec/def ::reward-value ::finite-double)

(spec/def ::reward
  (spec/keys :req [::reward-value
                   ::arm-name]))

(spec/def ::bulk-reward-mean ::finite-double)

(spec/def ::bulk-reward-count int?)

(spec/def ::bulk-reward-max ::finite-double)

(spec/def ::bulk-reward
  (spec/keys :req [::bulk-reward-mean
                   ::bulk-reward-count
                   ::bulk-reward-max
                   ::arm-name]))
