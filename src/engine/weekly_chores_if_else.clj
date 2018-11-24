(ns engine.weekly-chores-if-else)

(def valid-chores
  {:bathroom true
   :kitchen  true
   :living-room true
   :laundry  true
   :bins true})

(defn valid-chore?
  [chore]
    (chore valid-chores))


(defn chores-checker
  [weekly-flat-report]
  (cond (and (= (count (:chores weekly-flat-report)) 4) (map (:chores weekly-flat-report) valid-chore?))
        (assoc weekly-flat-report :status :completed :gets-beer true)

        (= (:cleaner-hired weekly-flat-report) true)
        (assoc weekly-flat-report :chores [:bathroom :living-room :kitchen])

        :else (assoc weekly-flat-report :status :incomplete :gets-beer false)))

