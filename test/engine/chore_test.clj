(ns engine.chore_test
  (:require [clojure.test :refer :all]
            [engine.chore :refer :all]
            [clara.rules :refer :all]))


(deftest checks-chores
  (testing "Counts how many core chores are completed"
    (prn "result " (-> (mk-session 'engine.chore)
                     (insert (->CoreChore "Hoovering" :core))
                     (fire-rules)
                     (query all-chores)))))
