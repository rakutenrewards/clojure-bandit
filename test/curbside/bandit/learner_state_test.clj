(ns curbside.bandit.learner-state-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer :all]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [curbside.bandit.learner-state :as state]
   [curbside.bandit.spec :as spec]
   [curbside.bandit.test-generator :as tgen]
   [taoensso.carmine :as car :refer [wcar]]))

(def redis-conn {:pool {} :spec {:uri "redis://localhost:6379/13"}})

(use-fixtures :each
  (fn [run-test]
    (state/reset-state redis-conn)
    (run-test)))

(def test-learner {::spec/learner-algo ::spec/epsilon-greedy
                   ::spec/algo-params {::spec/epsilon 0.05
                                       ::spec/maximize? true
                                       ::spec/reward-lower-bound 0.0
                                       ::spec/learner-algo ::spec/epsilon-greedy}
                   ::spec/arm-names ["arm1" "arm2"]
                   ::spec/experiment-name "test-learner"})

(def test-learner-lower-bound {::spec/learner-algo ::spec/ucb1
                               ::spec/algo-params {::spec/maximize? false
                                                   ::spec/reward-lower-bound -1.0
                                                   ::spec/learner-algo ::spec/ucb1}
                               ::spec/arm-names ["arm1" "arm2"]
                               ::spec/experiment-name "test-learner-lower-bound"})

(def default-arm-state (dissoc (deref #'state/default-arm-state) :deleted?))

(defn test-init-backend
  [backend]
  (is (false? (state/exists? backend test-learner)))
  (state/init-experiment backend test-learner)
  (let [params (state/get-learner-params backend "test-learner")
        arm-states (state/get-arm-states backend "test-learner")]
    (is (= {"arm1" default-arm-state
            "arm2" default-arm-state} arm-states))
    (is (= {::spec/epsilon 0.05
            ::spec/learner-algo ::spec/epsilon-greedy
            ::spec/maximize? true
            ::spec/reward-lower-bound 0.0}
           params))))

(deftest test-init
  (test-init-backend (atom {}))
  (test-init-backend redis-conn))

(defn test-reset-backend
  [backend]
  (state/init-experiment backend test-learner)
  (is (true? (state/exists? backend test-learner)))
  (state/reset-state backend)
  (is (false? (state/exists? backend test-learner))))

(deftest test-reset
  (test-reset-backend (atom {}))
  (test-reset-backend redis-conn))

(defspec rewards-are-scaled-correctly
  100
  (prop/for-all [reward-value (s/gen ::spec/reward-value)
                 {::spec/keys [reward-lower-bound n deleted? mean-reward max-reward]}
                 tgen/experiment-state]
    (let [old-arm-state {:mean-reward mean-reward :n n :deleted? deleted?}
          [_new-max-reward arm-state] (#'state/record-reward* reward-value max-reward old-arm-state reward-lower-bound)]
      (is (<= 0 (:mean-reward arm-state) 1)))))

(defspec bulk-rewards-are-scaled-correctly
  100
  (prop/for-all [bulk-reward (s/gen ::spec/bulk-reward)
                 {::spec/keys [reward-lower-bound n deleted? mean-reward max-reward]} tgen/experiment-state]
    (let [old-arm-state {:mean-reward mean-reward :n n}
          [_new-max-reward arm-state] (#'state/bulk-reward* bulk-reward max-reward old-arm-state reward-lower-bound)]
      (is (<= 0 (:mean-reward arm-state) 1)))))

(defn test-arm-crud-backend
  [backend backend-name]
  (state/init-experiment backend test-learner)
  (testing backend-name
    (testing "create arm"
      (state/create-arm backend test-learner "arm3")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" default-arm-state
                "arm2" default-arm-state
                "arm3" default-arm-state}
               arm-states))))
    (testing "get arm names"
      (let [arm-names (state/get-arm-names backend "test-learner")]
        (is (= #{"arm1" "arm2" "arm3"} arm-names))))
    (testing "reward arm"
      (state/record-reward backend "test-learner" "arm3" 0 0.5)
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" default-arm-state
                "arm2" default-arm-state
                "arm3" {:mean-reward 0.25 :n 2}}
               arm-states))))
    (testing "soft delete arm"
      (state/soft-delete-arm backend test-learner "arm3")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" default-arm-state
                "arm2" default-arm-state}
               arm-states))))
    (testing "bulk reward arm"
      (state/bulk-reward backend
                         "test-learner"
                         "arm1"
                         0
                         {::spec/bulk-reward-mean 0.5
                          ::spec/bulk-reward-max 0.5
                          ::spec/bulk-reward-count 1})
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" {:mean-reward 0.25 :n 2}
                "arm2" default-arm-state}
               arm-states))))
    (testing "undelete arm restores its prior state"
      (state/create-arm backend test-learner "arm3")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" {:mean-reward 0.25 :n 2}
                "arm2" default-arm-state
                "arm3" {:mean-reward 0.25 :n 2}}
               arm-states))))
    (testing "rewarding an arm before it is created has no effect"
      (state/record-reward backend "test-learner" "arm4" 0 0.5)
      (state/create-arm backend test-learner "arm4")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= default-arm-state
               (get arm-states "arm4")))))
    (testing "bulk rewarding an arm before it is created has no effect"
      (state/bulk-reward backend
                         "test-learner"
                         "arm5"
                         0
                         {::spec/bulk-reward-mean 0.5
                          ::spec/bulk-reward-max 0.5
                          ::spec/bulk-reward-count 1})
      (state/create-arm backend test-learner "arm5")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= default-arm-state
               (get arm-states "arm5")))))))

(deftest test-arm-crud
  (test-arm-crud-backend (atom {}) "ATOM:")
  (test-arm-crud-backend redis-conn "REDIS:"))

(defn test-reward-lower-bound-backend
  [backend backend-name]
  (state/init-experiment backend test-learner-lower-bound)
  (testing backend-name
    (testing "reward arm with negative value"
      (state/record-reward backend "test-learner-lower-bound" "arm1" -1 -0.5)
      (let [arm-states (state/get-arm-states backend "test-learner-lower-bound")]
        (is (= {"arm1" {:mean-reward 0.125 :n 2}
                "arm2" default-arm-state}
               arm-states))))
    (testing "bulk reward arm with mean lower than lower bound"
      (state/bulk-reward backend
                         "test-learner-lower-bound"
                         "arm2"
                         -1
                         {::spec/bulk-reward-mean -4
                          ::spec/bulk-reward-max -2
                          ::spec/bulk-reward-count 3})
      (let [arm-states (state/get-arm-states backend "test-learner-lower-bound")]
        (is (= {"arm1" {:mean-reward 0.125 :n 2}
                "arm2" {:mean-reward 0.0 :n 4}} arm-states))))))

(deftest test-reward-lower-bound
  (test-reward-lower-bound-backend (atom {}) "ATOM:")
  (test-reward-lower-bound-backend redis-conn "REDIS:"))

(defn test-choose-call-counter-backend
  [backend backend-name]
  (testing backend-name
    (state/init-experiment backend test-learner)
    (let [experiment-name (::spec/experiment-name test-learner)
          first-get-result (state/get-choose-count backend experiment-name)
          incr-result (state/incr-choose-count backend experiment-name)
          get-result (state/get-choose-count backend experiment-name)]
      (is (zero? first-get-result))
      (is (= 1 incr-result))
      (is (= 1 get-result)))))

(deftest test-choose-call-counter
  (test-choose-call-counter-backend (atom {}) "ATOM:")
  (test-choose-call-counter-backend redis-conn "REDIS:"))

(defn test-hard-delete-backend
  [backend backend-name]
  (testing (str backend-name "mixed hard and soft delete")
    (state/init-experiment backend test-learner)
    (state/soft-delete-arm backend test-learner "arm2")
    (state/record-reward backend "test-learner" "arm2" 0 0.1)
    (state/hard-delete-arm backend test-learner "arm2")
    (state/record-reward backend "test-learner" "arm2" 0 0.1)
    (is (= {"arm1" default-arm-state}
           (state/get-arm-states backend "test-learner"))))
  (testing (str backend-name "hard delete")
    (state/hard-delete-arm backend test-learner "arm1")
    (state/record-reward backend "test-learner" "arm2" 0 0.1)
    (is (= {}
           (state/get-arm-states backend "test-learner")))))

(deftest test-hard-delete
  (test-hard-delete-backend (atom {}) "ATOM:")
  (test-hard-delete-backend redis-conn "REDIS:"))
