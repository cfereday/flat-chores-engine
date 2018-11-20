(ns engine.chore-rules
  (:require [clara.rules :refer :all]))


(defrecord ChoreDetails [chore-type])

(defrecord FlatMateName [flatmate-name])

(defrecord CleanerDetails [chore-type])

(defrecord ChoreChecker [chore-status flatmate-name])

(defrecord FlatMateIll [illness])


(defquery chore-outcomes
  "Query to find the completed chores"
  []
  [?result <- ChoreChecker])


(defrule flatmate-chore-checker
  "If a flatmate has completed a chore their chore status is completed"
  [ChoreDetails (some? chore-type)]
  [FlatMateName (some? flatmate-name)]
  =>
  (insert! (->ChoreChecker :completed FlatMateName)))


(defrule cleaner-hired-chore-checker
  "If a flatmate has hired a cleaner to complete a chore their chore status is completed"
  [CleanerDetails (some? chore-type)]
  [FlatMateName (some? flatmate-name)]
  =>
  (insert! (->ChoreChecker :completed FlatMateName)))


(defrule flatmate-illness-checker
  "If a flatmate has been ill their chore status is marked as exempt"
  [FlatMateIll (= :flu illness)]
  =>
  (insert! (->ChoreChecker :exempt FlatMateName)))

(defrule flatmate-skipped-chore
  "If a flatmate has skipped a chore their status is marked as incompleted"
  [ChoreDetails (nil? chore-type)]
  =>
  (insert! (->ChoreChecker :incomplete FlatMateName)))
