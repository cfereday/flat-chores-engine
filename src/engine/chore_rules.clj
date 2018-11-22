(ns engine.chore-rules
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]))


(defrecord WeeklyReport [chore-type flatmate-name chore-completed-by-cleaner other-flatmate-completed-chore flatmate-ill])

(defrecord ChoreOutcome [chore-status flatmate-name chore])


(defquery chore-outcomes
  "Query to find the completed chores"
  []
  [ChoreOutcome (= ?chore-status chore-status)])


(defn is-legitimate-chore?
  [chore-type]
  (contains? #{:vacuum :kitchen :bathroom} chore-type))


(defrule flatmate-completed-chore
  "If a flatmate has completed a chore their chore status is completed"
  [:and
   [WeeklyReport (= ?chore chore-type) (is-legitimate-chore? chore-type)]
   [WeeklyReport (= ?name flatmate-name) (some? flatmate-name)]]
  =>
  (insert! (map->ChoreOutcome {:chore-status :completed :chore ?chore :flatmate-name ?name})))


#_(defrule cleaner-hired-chore-checker
  "If a flatmate has hired a cleaner to complete a chore their chore status is completed"
  [CleanerDetails (some? chore-type)]
  =>
  (insert! (->ChoreOutcome :completed FlatMateName)))


#_(defrule an-ill-flatmate-is-exempt
  "If a flatmate has been ill their chore status is marked as exempt"
  [FlatMateIll (= :flu illness)]
  =>
  (insert! (->ChoreOutcome :exempt FlatMateName)))

#_(defrule flatmate-skipped-chore
  "If a flatmate has skipped a chore their status is marked as incompleted"
  [WeeklyReport (nil? chore-type)]
  =>
  (insert! (->ChoreOutcome :incomplete FlatMateName)))




;:todo notes for talk
;Statements of facts based on other facts
;Pass in a lot of raw data that can be handled by rules
;Rules can handle multiple facts
;Able to grow complexity for the queries without these interacting
; flatmate & chore k
;certain level of intricacy
;chain of things - different ways to complete chores - do they need to be different rules? probably not, would want different queries
;queries
;count number of chores
;when enough chores are done , insert all done
;get to choose movie if all chores done & not exempt


