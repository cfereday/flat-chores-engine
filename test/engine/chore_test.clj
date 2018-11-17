(ns engine.chore_test
  (:require [clojure.test :refer :all]
            [engine.chore :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))


(deftest checks-chores
  (testing "Counts how many core chores are completed"

    (let [session (-> (mk-session 'engine.chore)
                    (insert (->CoreChore "Hoovering" :core))
                    (fire-rules))]
      (prn "inspected" (clojure.pprint/pprint (:insertions (inspect session))))
      (prn "all " (query session flatmate-chores))
      (prn "announcements " (query session chore-announcements))
      )))
