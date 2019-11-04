(ns curbside.bandit.learner-state-test
  (:require
   [clojure.test :refer :all]
   [curbside.bandit.learner-state :as state]
   [curbside.bandit.spec :as spec]
   [taoensso.carmine :as car :refer [wcar]]))

(def redis-conn {:pool {} :spec {:uri "redis://localhost:6379/13"}})

(use-fixtures :each
  (fn [run-test]
    (wcar redis-conn
          (car/flushdb))
    (run-test)))

(def test-learner {::spec/learner-algo ::spec/epsilon-greedy
                   ::spec/algo-params {::spec/epsilon 0.05
                                       ::spec/maximize?
                                       true
                                       ::spec/learner-algo
                                       ::spec/epsilon-greedy}
                   ::spec/arm-names ["arm1" "arm2"]
                   ::spec/experiment-name "test-learner"})

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
            ::spec/maximize? true}
           params))))

(deftest test-init
  (test-init-backend (atom {})))

(defn test-arm-crud-backend
  [backend backend-name]
  (state/init-experiment backend test-learner)
  (testing backend-name
    (testing "create arm"
      (state/create-arm backend test-learner "arm3")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" default-arm-state
                "arm2" default-arm-state
                "arm3" default-arm-state} arm-states))))
    (testing "reward arm"
      (state/record-reward backend "test-learner" "arm3" 0.5)
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" default-arm-state
                "arm2" default-arm-state
                "arm3" {:mean-reward 0.25 :n 2}} arm-states))))
    (testing "delete arm"
      (state/delete-arm backend test-learner "arm3")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" default-arm-state
                "arm2" default-arm-state} arm-states))))
    (testing "bulk reward arm"
      (state/bulk-reward backend
                         "test-learner"
                         "arm1"
                         {::spec/bulk-reward-mean 0.5
                          ::spec/bulk-reward-max 0.5
                          ::spec/bulk-reward-count 1})
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" {:mean-reward 0.25 :n 2}
                "arm2" default-arm-state} arm-states))))
    (testing "undelete arm restores its prior state"
      (state/create-arm backend test-learner "arm3")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= {"arm1" {:mean-reward 0.25 :n 2}
                "arm2" default-arm-state
                "arm3" {:mean-reward 0.25 :n 2}} arm-states))))
    (testing "rewarding an arm before it is created has no effect"
      (state/record-reward backend "test-learner" "arm4" 0.5)
      (state/create-arm backend test-learner "arm4")
      (let [arm-states (state/get-arm-states backend "test-learner")]
        (is (= default-arm-state
               (get arm-states "arm4")))))
    (testing "bulk rewarding an arm before it is created has no effect"
      (state/bulk-reward backend
                         "test-learner"
                         "arm5"
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
