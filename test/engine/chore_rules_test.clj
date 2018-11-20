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
                    (insert (->FlatMateName "Charlotte"))
                    (insert (->ChoreDetails :vacuum))
                    (fire-rules))
          query (query session chore-outcomes)]
      (is (not-empty query))))


  (testing "If a flatmate hired a cleaner to complete their chore, their chore status is completed"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateName "Charlotte"))
                    (insert (->CleanerDetails :vacuum))
                    (fire-rules))
          query (query session chore-outcomes)]
      (is (not-empty query)))
    )

  (testing "If a flatemate was ill their chore status is marked as exempt"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateName "Charlotte"))
                    (insert (->FlatMateIll :flu))
                    (fire-rules))
          query (query session chore-outcomes)]
      (is (not-empty query))))

  (testing "If a flatmate has not completed a chore & hired no cleaner their chore status is marked as incomplete"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMateName "Charlotte"))
                    (insert (->ChoreDetails nil))
                    (fire-rules))
          query (query session chore-outcomes)]
      (prn "HERE IS query" query)
      (prn "inspected" (clojure.pprint/pprint (:insertions (inspect session))))
      (is (not-empty query)))))






