(ns engine.weekly-chores-if-else)



(defn chores-checker
  [weekly-flat-report]
  (cond (= (count (:chores weekly-flat-report)) 4)
        (assoc weekly-flat-report :status :completed)
        :else (assoc weekly-flat-report :status :incomplete)))




(comment
  (defn chores-checker
    [chores-information]
    (cond (some? (:chore-type chores-information))
          (assoc chores-information :chore-status :completed)

          (some? (:cleaner-hired chores-information))
          (dissoc (assoc (assoc chores-information :chore-status :completed) :chore-type (:cleaner-hired (assoc chores-information :chore-status :completed))) :cleaner-hired)

          (some? (:flate-mate-completed chores-information))
          (dissoc (assoc (assoc chores-information :chore-status :completed) :chore-type (:flate-mate-completed (assoc chores-information :chore-status :completed))) :flate-mate-completed)

          (some? (:ill chores-information))
          (dissoc (assoc chores-information :chore-status :exempt) :ill)

          :else (assoc chores-information :chore-status :incomplete)))

  (defn outcome-checker
    [chores-checker-outcome]
    (cond (= (:chore-status chores-checker-outcome) :completed)
          (prn "Congrats you did your chores, as a prize you can choose the flat sunday night movie!")

          (= (:chore-status chores-checker-outcome) :exempt)
          (prn "Sorry you were ill, get better soon")

          (= (:chore-status chores-checker-outcome) :incomplete)
          (prn "Oh no you didn't do your chores, please buy your flatmates pizza for the flat sunday night movie!"))))


