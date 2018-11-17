(ns engine.chore
  (:require [clara.rules :refer :all]))

(defrecord FlatmateCompletedChore [flat-mate-name chore])

(defrecord CoreChore [chore rating])

(defrecord ChoreAnnouncement [status])

(defrecord FlatemateChore [name])


(defquery chore-announcements
  []
  [?announcement <- ChoreAnnouncement])


(defrule is-core-chore
  "Prints a message if a core chore is completed"
  [CoreChore (= rating :core)]
  =>
  (insert! (->ChoreAnnouncement :completed)))

(defrule notify-flatmates
  "Find the flatmate who completed the chore"
  [CoreChore (= ?chore chore)]
  [FlatmateCompletedChore (= ?chore chore) (= ?flat-mate-name flat-mate-name)]
  =>
  (insert! (->FlatemateChore (str ?flat-mate-name "has completed" ?chore))))

(defquery flatmate-chores
  []
  [?chore <- FlatemateChore])
