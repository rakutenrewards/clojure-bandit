(ns curbside.bandit.learner-state-test
  (:require
   [curbside.bandit.learner-state :as state]
   [curbside.bandit.spec :as spec]
   [clojure.test :refer :all]))

(def test-learner {::spec/learner-algo ::spec/epsilon-greedy
                   ::spec/algo-params {::spec/epsilon 0.05
                                       ::spec/maximize?
                                       true
                                       ::spec/learner-algo
                                       ::spec/epsilon-greedy}
                   ::spec/arm-names ["arm1" "arm2"]
                   ::spec/experiment-name "test-learner"})

(defn test-init-backend
  [backend]
  (state/init-experiment backend test-learner)
  (let [params (state/get-learner-params backend "test-learner")
        arm-states (state/get-arm-states backend "test-learner")]
    (is (= {"arm1" state/default-arm-state
            "arm2" state/default-arm-state} arm-states))
    (is (= {::spec/epsilon 0.05
            ::spec/learner-algo ::spec/epsilon-greedy
            ::spec/maximize? true}
           params))))

(deftest test-init
  (test-init-backend (atom {})))

(defn test-arm-crud-backend
  [backend]
  (state/init-experiment backend test-learner)
  (testing "create arm"
    (state/create-arm backend test-learner "arm3")
    (let [arm-states (state/get-arm-states backend "test-learner")]
      (is (= {"arm1" state/default-arm-state
              "arm2" state/default-arm-state
              "arm3" state/default-arm-state} arm-states))))
  (testing "delete arm"
    (state/delete-arm backend test-learner "arm3")
    (let [arm-states (state/get-arm-states backend "test-learner")]
      (is (= {"arm1" state/default-arm-state
              "arm2" state/default-arm-state} arm-states))))
  (testing "reward arm"
    (state/record-reward backend "test-learner" "arm2" 0.5)
    (let [arm-states (state/get-arm-states backend "test-learner")]
      (is (= {"arm1" state/default-arm-state
              "arm2" {:mean-reward 0.75 :n 2 :mean-sq-dist 10.125}} arm-states)))))

(deftest test-arm-crud
  (test-arm-crud-backend (atom {})))
