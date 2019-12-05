(ns curbside.bandit.spec
  (:require
   [clojure.spec.alpha :as spec]
   [clojure.spec.alpha :as s]
   [expound.alpha :as expound]))

(defn check [type data]
  (if (spec/valid? type data)
    true
    (throw (IllegalArgumentException. ^String (expound/expound-str type data)))))

(spec/def ::real-number
  (spec/and number?
            #(Double/isFinite %)))

(spec/def ::maximize? boolean?)

(spec/def ::reward-lower-bound ::real-number)

(spec/def ::common-params
  (spec/keys :req [::maximize?]
             :opt [::reward-lower-bound]))

(spec/def ::learner-algo #{::epsilon-greedy ::ucb1 ::random ::softmax})

(spec/def ::starting-temperature ::real-number)

(spec/def ::temp-decay-per-step ::real-number)

(spec/def ::min-temperature ::real-number)

(spec/def ::epsilon float?)

(spec/def ::exploration-mult ::real-number)

(spec/def ::epsilon-greedy-params
  (spec/and ::common-params
            (spec/keys :req [::epsilon])
            #(= ::epsilon-greedy (::learner-algo %))))

(spec/def ::ucb1-params
  (spec/and ::common-params
            (spec/keys :opt [::exploration-mult])
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

(spec/def ::reward-value ::real-number)

(spec/def ::reward
  (spec/keys :req [::reward-value
                   ::arm-name]))

(spec/def ::n pos-int?)

(spec/def ::mean-reward (s/double-in :min 0 :max 1))

(spec/def ::max-reward ::real-number)

(spec/def ::deleted? boolean?)

(spec/def ::bulk-reward-mean ::real-number)

(spec/def ::bulk-reward-count ::n)

(spec/def ::bulk-reward-max ::real-number)

(spec/def ::bulk-reward
  (s/and (spec/keys :req [::bulk-reward-mean
                          ::bulk-reward-count
                          ::bulk-reward-max
                          ::arm-name])
         (fn [{::keys [bulk-reward-mean bulk-reward-max]}]
           (<= bulk-reward-mean bulk-reward-max))))
