(ns engine.chore_rules_test
  (:require [clojure.test :refer :all]
            [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))

;:todo
;- how to have more meaningful assertions rather than just asserting not empty?
;- How to get the value of flatmate-name from the defrecord



(deftest checks-chores
  (testing "If a flatmate has completed a chore their chore status is marked as completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport :vacuum "Charlotte" nil nil false))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (= (:?chore-status mapped-query) :completed))))

  #_(testing "If a flatmate has not completed a chore or not added their name they do not have a completed status"
    (let [no-chore-session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" nil nil nil))
                    (fire-rules))

          no-name-session (-> (mk-session 'engine.chore-rules)
                            (insert (->WeeklyReport :vacuum nil nil nil nil))
                            (fire-rules))

          query-1 (query no-chore-session chore-outcomes)
          query-2 (query no-name-session chore-outcomes)
          mapped-query-1 (into {} query-2)
          mapped-query-2 (into {} query-1)]
      (is (and (= (:?chore-status mapped-query-1) nil) (= (:?chore-status mapped-query-2) nil)))))


  (testing "If a flatmate hired a cleaner to complete their chore, their chore status is completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" :vacuum nil nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (prn "mapped query" mapped-query)
      (is (= (:?chore-status mapped-query) :completed))))

  (testing "If another flatmate has done the task for the flatmate their chore status is completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" nil :vacuum nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (prn "inspected" (clojure.pprint/pprint (:insertions (inspect session))))
      (prn "mapped query" mapped-query)
      (is (= (:?chore-status mapped-query) :completed))))

  (testing "If a flatmate was ill their chore status is marked as exempt and their illness is down in lieu of a chore"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil "Charlotte" nil nil :flu))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (= (:?chore-status mapped-query) :exempt))))

  #_(testing "If a flatmate has not completed a chore & hired no cleaner their chore status is marked as incomplete"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport nil))
                    (fire-rules))
          query (query session chore-outcomes)]
      (prn "HERE IS query" query)
      (prn "inspected" (clojure.pprint/pprint (:insertions (inspect session))))
      (is (not-empty query)))))






