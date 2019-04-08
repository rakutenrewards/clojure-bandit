(ns curbside.bandit.learner-state-test
  (:require
   [curbside.bandit.learner-state :as state]
   [curbside.bandit.spec :as spec]
   [clojure.test :refer :all]
   [taoensso.carmine :as car :refer (wcar)]))

(def redis-conn {:pool {} :spec {:uri "redis://localhost:6379/13"}})

(use-fixtures :each
  (fn [test]
    (wcar redis-conn
          (car/flushdb))
    (test)))

(def test-learner {::spec/learner-algo ::spec/epsilon-greedy
                   ::spec/algo-params {::spec/epsilon 0.05
                                       ::spec/maximize?
                                       true
                                       ::spec/learner-algo
                                       ::spec/epsilon-greedy}
                   ::spec/arm-names ["arm1" "arm2"]
                   ::spec/experiment-name "test-learner"})

(def default-arm-state (dissoc state/default-arm-state :deleted?))

(defn test-init-backend
  [backend]
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
                "arm3" {:mean-reward 0.25 :n 2}} arm-states))))))

(deftest test-arm-crud
  (test-arm-crud-backend (atom {}) "ATOM:")
  (test-arm-crud-backend redis-conn "REDIS:"))
