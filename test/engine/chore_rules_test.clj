(ns engine.chore_rules_test
  (:require [clojure.test :refer :all]
            [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))

(comment
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

                            {:chore-type                        :bathroom
                             :flatmate-name                     "Christoph"
                             :chore-completed-by-cleaner        nil
                             :chore-completed-by-other-flatmate nil
                             :flatmate-ill                      nil}))
                  (fire-rules))
        report-query (query session flatmate-reports)
        movie-query (set (query session can-choose-a-movie?))
        charlotte-movie (second movie-query)
        felipe-movie (last movie-query)
        christoph-movie (first movie-query)
        charlotte-report (first report-query)
        felipe-report (second report-query)
        christoph-report (last report-query)]

    (is (and
          (= (:?eligibility charlotte-movie) :pick-movie)
          (= (:?flatmate-name charlotte-movie) "Charlotte")))
    (is (and
          (= (:?eligibility felipe-movie) :pick-movie)
          (= (:?flatmate-name felipe-movie) "Felipe")))
    (is (and
          (= (:?eligibility christoph-movie) :pick-movie)
          (= (:?flatmate-name christoph-movie) "Christoph")))

    (is (and
          (= (:?chore-type charlotte-report) :vacuum)
          (= (:?flatmate-name charlotte-report) "Charlotte")))
    (is (and
          (= (:?chore-type felipe-report) :kitchen)
          (= (:?flatmate-name felipe-report) "Felipe")))
    (is (and
          (= (:?chore-type christoph-report) :bathroom)
          (= (:?flatmate-name christoph-report) "Christoph")))))


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
            (= (:?flatmate-name mapped-incomplete-query) "Charlotte"))))))
)

(deftest felipes-test-suite
  (testing "That a sick flatname is automagically exempted"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" nil nil :ear-infection))
                    (fire-rules))
          [{result :?exemption}] (query session exemptions-query?)]
      (is (= result (->Exemption "Charlotte")))))

  ;; this would need to be refactored into a blog that gets taken aparat, I guess?
  (testing "doing 4 chores gives you beer"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" nil nil nil))
                    (insert (->Chore "Charlotte" :bathroom))
                    (insert (->Chore "Charlotte" :kitchen))
                    (insert (->Chore "Charlotte" :laundry))
                    (insert (->Chore "Charlotte" :bins))
                    (fire-rules))
          [{result :?beer}] (query session beer?)
          ;; _x (explain-activations session)]
          ]
      (is (= result (->Beer "Charlotte")))))

  ;; this would need to be refactored into a blog that gets taken aparat, I guess?
  (testing "getting the cleaners"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" true nil nil))
                    (insert (->Chore "Charlotte" :bins))
                    (fire-rules))
          [{result :?beer}] (query session beer?)
          ;; _x (explain-activations session)]
          ]
      (is (= result (->Beer "Charlotte")))))

  (testing "sadly being ill"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Charlotte" true nil :ear-infection))
                    (insert (->Chore "Charlotte" :bins))
                    (insert (->Chore "Charlotte" :kitchen))
                    (insert (->Chore "Charlotte" :cooking))
                    (insert (->Chore "Charlotte" :bathroom))
                    (insert (->Chore "Charlotte" :balcony))
                    (fire-rules))
          result (query session beer?)
          ;; _x (explain-activations session)]
          ]
      (is (empty? result ))))
  

;;  (defn beerable? [person]
;;    (let [known-facts (derive-facts-from person)  ;; ---> derive-facts-from turns the person into a list of facts
;;          session (create-session-with known-facts)] ;; ---> inserts said facts into Clara and runs fires the rules
;;      (has-beer? session))) ;; ---> runs my query AND removes Clara'isms from it... aka ?foo should be gone!
;;
;;   {
;;    :name "Charlotte"
;;    :chores '(:bins :dishes :bedroom :kitchen)
;;   }
;; 
;;   {
;;    :name "Felipe"
;;    :chores '(:kitchen)
;;    :paid-cleaner true
;;   }


;;  (defn person-facts
;;    [person]
;;    (let [flatmate (make-flatmate person)
;;          chores (map #(Chore. (:name person) %) (:chores person))]
;;      '(flatmate chores)
;;      ))
;;
;;  (defn derive-facts-from
;;    [people]
;;    (mapcat person-facts people))
;;
;;  (defn create-session-with 
;;    [facts]
;;    (let [session (mk-session 'engine.chore-rules)
;;          with-facts (reduce insert session facts)]
;;          (fire-rules with-facts)))



  (testing "getting the cleaners"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateReport nil "Felipe" true nil nil))
                    (insert (->Chore "Felipe" :bills))
                    (insert (->FlatMateReport nil "Charlotte" nil nil nil))
                    (insert (->Chore "Charlotte" :bins))
                    (insert (->Chore "Charlotte" :dishes))
                    (insert (->Chore "Charlotte" :bedroom))
                    (insert (->Chore "Charlotte" :kitchen))
                    (fire-rules))
          [{result :?beer}] (query session beer?)
          ;; _x (explain-activations session)]
          ]
      (is (= result (->Beer "Charlotte"))))))



