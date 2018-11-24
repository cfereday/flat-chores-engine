(ns engine.chore-rules
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]))

;; (defrecord FlatMate [name was-ill paid-cleaner])
;; 
;; (defn make-flatmate [values]
;;   (let [default-values {:was-ill false :paid-cleaner false}]
;;     (FlatMate. (merge default-values values))))

(defrecord FlatMateReport [chore-type flatmate-name chore-completed-by-cleaner chore-completed-by-other-flatmate flatmate-ill])

(defrecord ChoreOutcome [chore-status flatmate-name chore])

(defrecord WeeklyReport [chores1 chores2 chores3])

(defrecord MoviePicker [eligibility flatmate-name])

(defrecord Exemption [name])

(defrecord Chore [name activity])

(defrecord CompletedChore [name amount])

(defrecord Beer [name])


(defquery exemptions-query?
  "Hi there shnuggles"
  []
  [?exemption <- Exemption])

(defquery beer?
  "do you get beer"
  []
  [?beer <- Beer])

(defquery completed-chores?
  "do you get beer"
  []
  [?x <- CompletedChore])


(defrule ill-flatmates-are-exempt
  "When you are ill, you are exempt of doing any chores"
  [FlatMateReport (= ?name flatmate-name) (some? flatmate-ill)]
  =>
  (insert! (->Exemption ?name)))


(defrule cleaners-do-three-chores
  "they clean the bathrooms, living room, and kitchen"
  [FlatMateReport (= ?name flatmate-name) (some? chore-completed-by-cleaner)]
  =>
  (insert-all! [(->Chore ?name :bathroom)
                (->Chore ?name :living-room)
                (->Chore ?name :kitchen)]))


;; can use destructuring syntax: [FlatMateRepo [{:flatmate-name name...}]]
(defrule normal-chores-from-the-report
  "..."
  [FlatMateReport (= ?name flatmate-name) (= ?type chore-type) (some? chore-type)]
  =>
  (insert! (->Chore ?name ?type)))

(defrule need-to-complete-six-chores
  "..."
  [FlatMateReport (= ?name flatmate-name)]
  [?c <- (acc/count) from (Chore (= ?name name))]
  [:test (> ?c 3)]
  =>
  (insert! (->CompletedChore ?name ?c)))

(defrule flatmates-with-completed-chores-and-no-exemption-get-beer
  "..."
  [CompletedChore (= ?name name)]
  [:not [Exemption (= ?name name)]]
  =>
  (insert! (->Beer ?name)))
