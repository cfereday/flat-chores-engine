(ns engine.weekly-chores-if-else-test
  (:require [clojure.test :refer :all]
            [engine.weekly-chores-if-else :refer :all]))


(deftest checks-chores
  (testing "If a flatmate has completed 4 chore their chore status is marked as completed"
    (is (= (:status (chores-checker {:chores [:bathroom :kitchen :laundry :bins] :name "Charlotte"})) :completed)))

  (testing "If a flatmate has completed 4 chore their chore status is marked as incomplete"
    (is (= (:status (chores-checker {:chores [:bathroom] :name "Charlotte"}) :incomplete))))

  (testing "If you've completed all your chores you'll get a beer"
    (is (= (:gets-beer (chores-checker {:chores [:bathroom :kitchen :laundry :bins] :name "Charlotte"})) true)))

  (testing "If a flatmate hasn't completed enough chores they don't get a beer"
    (is (= (:gets-beer (chores-checker {:chores [:bathroom] :name "Charlotte"}) false))))


  #_(testing "If a flatmate has hired a cleaner then they get three chores done for them:  bathroom, living room, kitchen ")

  #_(testing "If a flatmate is ill they're exempt from doing a chore"
    (is (= (:status (chores-checker {:name "Charlotte" :cleaner-hired true}) :exempted)))


    )
  )
