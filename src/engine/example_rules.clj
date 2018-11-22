(ns engine.example-rules
  (:require [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))


(defrecord Temperature [temperature location])

(defrecord LocalTemperatureRecords [high low location])

(defrecord Cold [temperature])

(defrecord AlwaysOverZeroLocation [location])

(defrule insert-temperature-records
  [?min-temp <- (acc/min :temperature) :from [Temperature (= ?loc location)]]
  [?max-temp <- (acc/max :temperature) :from [Temperature (= ?loc location)]]
  =>
  (insert! (map->LocalTemperatureRecords {:high ?max-temp :low ?min-temp :location ?loc})))

[Temperature (= ?temperature temperature) (< temperature 30)]
