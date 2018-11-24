(ns engine.chore-rules
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]))

(defrecord FlatMate [name was-ill paid-cleaner])

(defn make-flatmate [person]
  (let [default-values {:was-ill false :paid-cleaner false}]
    (map->FlatMate (merge default-values person))))

(defrecord Exemption [name])

(defrecord Chore [name activity])

(defrecord CompletedChore [name amount])

(defrecord Beer [name])


(defquery exemptions-query?
  []
  [?exemption <- Exemption])

(defquery beer?
  []
  [?beer <- Beer])

(defquery completed-chores?
  []
  [?x <- CompletedChore])


(defrule ill-flatmates-are-exempt
  "When you are ill, you are exempt from doing any chores"
  [FlatMate (= ?name name) (= was-ill true)]
  =>
  (insert! (->Exemption ?name)))


(defrule cleaners-do-three-chores
  "they clean the bathrooms, living room, and kitchen"
  [FlatMate (= ?name name) (= paid-cleaner true)]
  =>
  (insert-all! [(->Chore ?name :bathroom)
                (->Chore ?name :living-room)
                (->Chore ?name :kitchen)]))


(defrule need-to-complete-four-chores
  "at least three chores per flatmates are needed"
  [FlatMate (= ?name name)]
  [?c <- (acc/count) from (Chore (= ?name name))]
  [:test (> ?c 3)]
  =>
  (insert! (->CompletedChore ?name ?c)))

(defrule completed-chores-grants-beer
  "get beer if you have completed your chores unless you are exempt"
  [CompletedChore (= ?name name)]
  [:not [Exemption (= ?name name)]]
  =>
  (insert! (->Beer ?name)))
