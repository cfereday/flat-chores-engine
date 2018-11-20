(ns engine.chore-rules
  (:require [clara.rules :refer :all]))


(defrecord ChoreDetails [chore-type flatmate-name])

(defrecord ChoreChecker [chore-status])



(defquery chore-outcomes
  "Query to find the completed chores"
  []
  [?result <- ChoreChecker])


(defrule is-core-chore
  "Marks the status of a chore"
  [ChoreDetails (some? chore-type)]
  =>
  (insert! (->ChoreChecker :completed)))
