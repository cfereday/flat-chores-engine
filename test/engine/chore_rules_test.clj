(ns engine.chore_rules_test
  (:require [clojure.test :refer :all]
            [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))

(deftest checks-chores-for-individual-flatmates
  (testing "If a flatmate has completed a chore their chore status is marked as completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport :vacuum "Charlotte" nil nil nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :completed)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :vacuum)))))


  (testing "If a flatmate hired a cleaner to complete their chore, their chore status is completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" :vacuum nil nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :completed)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :vacuum)))))

  (testing "If another flatmate has done the task for the flatmate their chore status is completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" nil :vacuum nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :completed)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :vacuum)))))

  (testing "If a flatmate was ill their chore status is marked as exempt and their illness is down in lieu of a chore"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" nil nil "flu"))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and
            (= (:?chore-status mapped-query) :exempt)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :missing)))))

  (testing "If a flatmate has not completed a chore & wasn't ill their chore status is marked as incomplete"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" nil nil nil))
                    (fire-rules))
          query (query session chore-outcomes)
          mapped-query (into {} query)]
      (is (and (= (:?chore-status mapped-query) :incomplete)
            (= (:?flatmate-name mapped-query) "Charlotte")
            (= (:?chore mapped-query) :missing))))))


(deftest checks-chores-for-whole-flat
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->WeeklyReport
                              {:chore-type                        :vacuum
                               :flatmate-name                     "Charlotte"
                               :chore-completed-by-cleaner        nil
                               :chore-completed-by-other-flatmate nil
                               :flatmate-ill                      nil}
                              #_{:chore-type                        :kitchen
                               :flatmate-name                     "Felipe"
                               :chore-completed-by-cleaner        nil
                               :chore-completed-by-other-flatmate nil
                               :flatmate-ill                      nil}
                              #_{:chore-type                        :bathroom
                               :flatmate-name                     "Christoph"
                               :chore-completed-by-cleaner        nil
                               :chore-completed-by-other-flatmate nil
                               :flatmate-ill                      nil}))
                    (fire-rules))
          flatmate-report-query (query session flatmate-reports)
          all-records-report-query (query session all-records)
          mapped-query (into {} flatmate-report-query)
          mapped-query-all-records (into {} all-records-report-query)]

      (prn "HERE IS ALL RECORDS query" all-records)
      (prn "HERE IS FLATMATE REPORT query" flatmate-report-query)
      (prn "HERE IS MAPPED query" mapped-query)
      (prn "HERE IS ALL RECORDS MAPPED query" mapped-query-all-records)
      (prn "HERE IS TRYIONG TO GET A KEY" (:?all-records mapped-query-all-records))
      (prn "inspected session" (clojure.pprint/pprint (:insertions (inspect session))))
      (is (= (some? mapped-query-all-records))


        #_(and
            (= (:?chore-type mapped-query) :kitchen)
            (= (:?flatmate-name mapped-query) "Felipe")))))


(deftest checks-movie-picker
  (testing "If a flatmate completed their chore they are able to pick the sunday movie"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport :vacuum "Charlotte" nil nil nil))
                    (fire-rules))
          query (query session can-choose-a-movie?)
          mapped-query (into {} query)]
      (is (and
            (= (:?eligibility mapped-query) :pick-movie)
            (= (:?flatmate-name mapped-query) "Charlotte")))))

  (testing "If a flatmate didn't complete their chore or are exempt they aren't able to pick the sunday movie"
    (let [incomplete-session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" nil nil nil))
                    (fire-rules))
          exempt-session (-> (mk-session 'engine.chore-rules)
                           (insert (->FlatMateReport nil "Charlotte" nil nil "flu"))
                           (fire-rules))
          incomplete-query (query incomplete-session can-choose-a-movie?)
          exempt-query     (query exempt-session can-choose-a-movie?)
          mapped-incomplete-query (into {} incomplete-query)
          mapped-exempt-query (into {} exempt-query)]
      (is (and
            (= (:?eligibility mapped-incomplete-query) :no-movie-picking)
            (= (:?flatmate-name mapped-incomplete-query) "Charlotte")))
      (is (and
            (= (:?eligibility mapped-exempt-query) :no-movie-picking)
            (= (:?flatmate-name mapped-incomplete-query) "Charlotte")))))
  )






