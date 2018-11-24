(ns engine.weekly-chores-if-else)

(def valid-chores
  {:bathroom true
   :kitchen  true
   :laundry  true
   :bins true})

(defn valid-chore?
  [chore]
    (chore valid-chores))


(defn chores-checker
  [weekly-flat-report]
  (cond (and (= (count (:chores weekly-flat-report)) 4) (map (:chores weekly-flat-report) valid-chore?))
        (assoc weekly-flat-report :status :completed :gets-beer true)

        #_(= (:cleaner-hired weekly-flat-report) true)
        #_(assoc weekly-flat-report :chores [:bathroom :kitchen :bins])

        :else (assoc weekly-flat-report :status :incomplete :gets-beer false)))

