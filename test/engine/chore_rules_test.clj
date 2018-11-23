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

                            {:chore-type                        :kitchen
                             :flatmate-name                     "Felipe"
                             :chore-completed-by-cleaner        nil
                             :chore-completed-by-other-flatmate nil
                             :flatmate-ill                      nil}

                            #_:chores-3 #_{:flatmate-3 {:chore-type                        :bathroom
                                                        :flatmate-name                     "Christoph"
                                                        :chore-completed-by-cleaner        nil
                                                        :chore-completed-by-other-flatmate nil
                                                        :flatmate-ill                      nil}}))
                  (fire-rules))
        report-query (query session flatmate-reports)
        movie-query (query session can-choose-a-movie?)
        movie-mapped-query (into {} movie-query)
        report-mapped-query (into [] (into {} report-query))
        charlotte-report (first report-mapped-query)
        felipe-report (first report-mapped-query)

        ]
    (prn "HERE IS REPORT query******" report-query)
    (prn "HERE IS MOVIE query----------" movie-query)
    (prn "HERE IS REPORT MAPPED query******" report-mapped-query)
    (prn "HERE IS MOVIE MAPPED query----------" movie-mapped-query)
    (prn "inspected session" (clojure.pprint/pprint (:insertions (inspect session))))
    (is (and
          (= (:?eligibility movie-mapped-query) :pick-movie)
          (= (:?flatmate-name movie-mapped-query) "Charlotte")))
    (is (and
          (= (:?eligibility movie-mapped-query) :pick-movie)
          (= (:?flatmate-name movie-mapped-query) "Felipe")))

    (is (and
          (= (:?chore-type charlotte-report) :vacuum)
          (= (:?flatmate-name charlotte-report) "Charlotte")))
    (is (and
          (= (:?chore-type felipe-report) :kitchen)
          (= (:?flatmate-name felipe-report) "Felipe")))))


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






