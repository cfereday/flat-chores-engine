(ns engine.chore_test
  (:require [clojure.test :refer :all]
            [engine.chore :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))



#_(deftest checks-chores
  (testing "Counts how many core chores are completed"

    (let [session (-> (mk-session 'engine.chore)
                    (insert (->CoreChore "Hoovering" :core))
                    (fire-rules))
          query (query session chore-announcements :?status :completed)]
      (prn "inspected" (clojure.pprint/pprint (:insertions (inspect session))))
      (prn "announcement status" query)
      (is (some? query))))

  (testing "Counts which announcements completed"

    (let [session (-> (mk-session 'engine.chore)
                    (insert (->CoreChore "Hoovering" :core))
                    (fire-rules))
          query (query session chore-announcements :?status :completed)]
      (prn "inspected" (clojure.pprint/pprint (:insertions (inspect session))))
      (prn "announcement status" query)
      (is (some? query)))))
