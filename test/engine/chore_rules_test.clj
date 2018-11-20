(ns engine.chore_rules_test
  (:require [clojure.test :refer :all]
            [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))


(deftest checks-chores
  (testing "If there are no choredetails a chorechecker fact won't be created by the rule"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->ChoreDetails nil nil))
                    (fire-rules))
          query (query session chore-outcomes)]
      (is (empty? query))))

  (testing "If a flatmate has completed a chore their chore status is marked as completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->ChoreDetails :vacuum "Charlotte"))
                    (fire-rules))
          query (query session chore-outcomes)]
      (is (not-empty query)))))
