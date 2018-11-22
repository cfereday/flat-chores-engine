(ns engine.chore_rules_test
  (:require [clojure.test :refer :all]
            [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))

(deftest checks-chores
  (testing "If a flatmate has completed a chore their chore status is marked as completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport :vacuum "Charlotte" nil nil nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :completed)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :vacuum)))))


  (testing "If a flatmate hired a cleaner to complete their chore, their chore status is completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" :vacuum nil nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :completed)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :vacuum)))))

  (testing "If another flatmate has done the task for the flatmate their chore status is completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" nil :vacuum nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :completed)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :vacuum)))))

  (testing "If a flatmate was ill their chore status is marked as exempt and their illness is down in lieu of a chore"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" nil nil :flu))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :exempt)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :missing)))))

  (testing "If a flatmate has not completed a chore & wasn't ill their chore status is marked as incomplete"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" nil nil nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (prn "HERE IS query" query)
      (prn "inspected" (clojure.pprint/pprint (:insertions (inspect session))))
      (is (and (= (:?chore-status mapped-query) :incomplete)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :missing))))))






