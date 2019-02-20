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

(spec/def ::learner-algo #{::epsilon-greedy ::ucb1 ::random})

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

(spec/def ::algo-params
  (spec/and (spec/keys :req [::learner-algo])
            (spec/or :epsilon-greedy ::epsilon-greedy-params
                     :ucb1 ::ucb1-params
                     :random ::random-params)))

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
