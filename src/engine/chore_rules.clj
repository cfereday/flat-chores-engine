(ns engine.chore-rules
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]))


(defrecord FlatMateReport [chore-type flatmate-name chore-completed-by-cleaner chore-completed-by-other-flatmate flatmate-ill])

(defrecord ChoreOutcome [chore-status flatmate-name chore])

(defrecord WeeklyReport [flatmate-1-chores #_flatmate-2-chores #_flatmate-3-chores])

(defrecord MoviePicker [eligibility flatmate-name])

(defrecord BigReport [all-records])

(defquery all-records
  "Query all records created"
  []
  [BigReport (= ?all-records all-records)])

(defquery flatmate-reports
  "Querying that the flatmate report has been correctly made"
  []
  [FlatMateReport (= ?chore-type chore-type) (= ?flatmate-name flatmate-name) (= ?chore-completed-by-cleaner chore-completed-by-cleaner) (= ?chore-completed-by-other-flatmate chore-completed-by-other-flatmate) (= ?flatmate-ill flatmate-ill)])

(defquery chore-outcomes
  "Querying the chore outcome keys"
  []
  [ChoreOutcome (= ?chore-status chore-status) (= ?flatmate-name flatmate-name) (= ?chore chore)])


(defquery can-choose-a-movie?
  "Checks if the flatmate is eligible to chose a movie"
  []
  [MoviePicker (= ?eligibility eligibility) (= ?flatmate-name flatmate-name)])


(defn is-legitimate-chore?
  [chore-type]
  (contains? #{:vacuum :kitchen :bathroom} chore-type))

(defn has-not-completed-a-chore?
  [chore-or-ill]
  (= '(true true true true) (map nil? chore-or-ill)))


(defn is-eligible?
  [chore-status flatmate-name]
  (and (= :completed chore-status) (not (= :missing flatmate-name))))


(defrule create-flatmate-report
  "Turns weekly report into individual flatmate report data"

  [WeeklyReport (= ?chore-type (:chore-type flatmate-1-chores)) (= ?flatmate-name (:flatmate-name flatmate-1-chores))
   (= ?chore-completed-by-cleaner (:chore-completed-by-cleaner flatmate-1-chores))
   (= ?chore-completed-by-other-flatmate (:chore-completed-by-other-flatmate flatmate-1-chores))
   (= ?flatmate-ill (:flatmate-ill flatmate-1-chores))]

  #_[WeeklyReport (= ?chore-type (:chore-type flatmate-2-chores)) (= ?flatmate-name (:flatmate-name flatmate-2-chores))
   (= ?chore-completed-by-cleaner (:chore-completed-by-cleaner flatmate-2-chores))
   (= ?chore-completed-by-other-flatmate (:chore-completed-by-other-flatmate flatmate-2-chores))
   (= ?flatmate-ill (:flatmate-ill flatmate-2-chores))]

  #_[WeeklyReport (= ?chore-type (:chore-type flatmate-3-chores)) (= ?flatmate-name (:flatmate-name flatmate-3-chores))
   (= ?chore-completed-by-cleaner (:chore-completed-by-cleaner flatmate-3-chores))
   (= ?chore-completed-by-other-flatmate (:chore-completed-by-other-flatmate flatmate-3-chores))
   (= ?flatmate-ill (:flatmate-ill flatmate-3-chores))]

  =>
  (insert! (map->BigReport {:result (map->FlatMateReport {:chore-type                        ?chore-type
                                                              :flatmate-name                     ?flatmate-name
                                                              :chore-completed-by-cleaner        ?chore-completed-by-cleaner
                                                              :chore-completed-by-other-flatmate ?chore-completed-by-other-flatmate
                                                              :flatmate-ill                      ?flatmate-ill})})))



  (defrule chore-checker
    "Updates the chore outcome for an individual flatmate"
    ;:todo consider refactoring the :completed cases as repetitive
    ;:todo make this work for multiple flatmates
    ;todo when enough chores add in an :chores-done
    [:and
     [:or
      [FlatMateReport (= ?chore chore-type) (= ?status :completed) (is-legitimate-chore? chore-type)]
      [FlatMateReport (= ?chore chore-completed-by-cleaner) (= ?status :completed) (is-legitimate-chore? chore-completed-by-cleaner)]
      [FlatMateReport (= ?chore chore-completed-by-other-flatmate) (= ?status :completed) (is-legitimate-chore? chore-completed-by-other-flatmate)]
      [FlatMateReport (= ?chore :missing) (= ?status :exempt) (some? flatmate-ill)]
      [FlatMateReport (= ?chore :missing) (= ?status :incomplete) (has-not-completed-a-chore? (take-while nil? [chore-type chore-completed-by-cleaner chore-completed-by-other-flatmate flatmate-ill]))]
      ]
     [:or
      [FlatMateReport (= ?name flatmate-name) (some? flatmate-name)]
      [FlatMateReport (= ?name :missing) (nil? flatmate-name)]]]
    =>
    (insert! (map->ChoreOutcome {:chore-status ?status :chore ?chore :flatmate-name ?name}))

    (defrule chore-outcome-checker
      "Updates the movie picker for an individual flatmate"
      ;:todo make this work for multiple flatmates
      ;:todo add a check to see if all house chores have been completed
      [:or
       [ChoreOutcome (= ?status :pick-movie) (= ?name flatmate-name flatmate-name) (is-eligible? chore-status flatmate-name)]
       [ChoreOutcome (= ?status :no-movie-picking) (= ?name flatmate-name) (not (is-eligible? chore-status flatmate-name))]]
      =>
      (insert! (map->MoviePicker {:eligibility ?status :flatmate-name ?name}))))



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


