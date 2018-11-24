(ns engine.weekly-chores-if-else-test
  (:require [clojure.test :refer :all]
            [engine.weekly-chores-if-else :refer :all]))


(deftest checks-chores
  (testing "If a flatmate has completed 4 chore their chore status is marked as completed"
    (is (= (chores-checker {:chores [:bathroom :kitchen :laundry :bins] :name "Charlotte"}) {:chores [:bathroom :kitchen :laundry :bins] :name "Charlotte" :status :completed})))
  )



(comment
  (deftest checks-chores-done
    (testing "If a flatmate has completed a chore their chore status is marked as completed"
      (is (= (chores-checker {:chore-type :vacuum :name "Charlotte"}) {:chore-type :vacuum :name "Charlotte" :chore-status :completed})))

    (testing "If a flatmate hired a cleaner to complete their chore, their chore status is completed"
      (is (= (chores-checker {:name "Charlotte" :cleaner-hired :vacuum}) {:chore-type :vacuum :name "Charlotte" :chore-status :completed})))


    (testing "If a flatemate has got their other flatmate to complete their chore, their chore status is completed"
      (is (= (chores-checker {:name "Charlotte" :flate-mate-completed :vacuum}) {:chore-type :vacuum :name "Charlotte" :chore-status :completed})))


    (testing "If a flatemate was ill their chore status is marked as exempt"
      (is (= (chores-checker {:name "Charlotte" :ill :flu}) {:name "Charlotte" :chore-status :exempt})))


    (testing "If a flatmate has not completed a chore & hired no cleaner their chore status is marked as incomplete"
      (is (= (chores-checker {:name "Felipe"}) {:name "Felipe" :chore-status :incomplete}))))

  (deftest check-chores-outcome
    (testing "If a flatmate has completed chores they receive a prize"
      (is (= (outcome-checker {:chore-type :vacuum :name "Charlotte" :chore-status :completed}) (prn "Congrats you did your chores, as a prize you can choose the flat sunday night movie!"))))

    (testing "If a flatmate has been ill they are exempt from completing their chores"
      (is (= (outcome-checker {:name "Charlotte" :chore-status :exempt}) (prn "Sorry you were ill, get better soon"))))

    (testing "If a flatmate has not done their chores"
      (is (= (outcome-checker {:name "Felipe" :chore-status :incomplete}) (prn "Oh no you didn't do your chores, please buy your flatmates pizza for the flat sunday night movie!"))))))
