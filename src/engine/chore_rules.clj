(ns engine.chore-rules
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]))


(defrecord WeeklyReport [chore-type flatmate-name chore-completed-by-cleaner chore-completed-by-other-flatmate flatmate-ill])

(defrecord ChoreOutcome [chore-status flatmate-name chore])


(defquery chore-outcomes
  "Querying the chore outcome keys"
  []
  [ChoreOutcome (= ?chore-status chore-status) (= ?flatmate-name flatmate-name) (= ?chore chore)])


(defn is-legitimate-chore?
  [chore-type]
  (contains? #{:vacuum :kitchen :bathroom} chore-type))

(defn has-not-completed-a-chore?
  [chore-or-ill]
  (= '(true true true true) (map nil? chore-or-ill)))

(defrule chore-checker
  "Updates the chore outcome"
  ;:todo consider refactoring the :completed cases as repetitive
  [:and
   [:or
    [WeeklyReport (= ?chore chore-type) (= ?status :completed) (is-legitimate-chore? chore-type)]
    [WeeklyReport (= ?chore chore-completed-by-cleaner) (= ?status :completed) (is-legitimate-chore? chore-completed-by-cleaner)]
    [WeeklyReport (= ?chore chore-completed-by-other-flatmate) (= ?status :completed) (is-legitimate-chore? chore-completed-by-other-flatmate)]
    [WeeklyReport (= ?chore :missing) (= ?status :exempt) (= true flatmate-ill)]
    [WeeklyReport (= ?chore :missing) (= ?status :incomplete) (has-not-completed-a-chore? (take-while nil? [chore-type chore-completed-by-cleaner chore-completed-by-other-flatmate flatmate-ill]))]
    ]
   [:or
    [WeeklyReport (= ?name flatmate-name) (some? flatmate-name)]
    [WeeklyReport (= ?name :missing) (nil? flatmate-name)]]]
  =>
  (insert! (map->ChoreOutcome {:chore-status ?status :chore ?chore :flatmate-name ?name})))



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


