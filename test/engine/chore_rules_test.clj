(ns engine.chore_rules_test
  (:require [clojure.test :refer :all]
            [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]
            [clojure.pprint :refer :all]))

(defn person-facts
  [person]
  (let [flatmate (make-flatmate person)
        chores (map #(->Chore (:name person) %) (:chores person))]
    (conj chores flatmate)
    ))

(defn derive-facts-from
  [people]
  (mapcat person-facts people))

(defn create-session-with
  [facts]
  (let [session (mk-session 'engine.chore-rules)
        with-facts (reduce insert session facts)]
    (fire-rules with-facts)))

(defn beers [result]
  (map :?beer result))

(deftest felipes-test-suite
  (testing "that a sick flatmate is automatically exempted"
    (let [facts (derive-facts-from [{:name "Charlotte" :was-ill true}])
          session (create-session-with facts)
          [{result :?exemption}] (query session exemptions-query?)]
      (is (= result (->Exemption "Charlotte")))))

   (testing "doing 3 chores gives you beer"
     (let [facts (derive-facts-from [{:name "Charlotte" :chores [:bathroom :kitchen :laundry :bins]}])
           session (create-session-with facts)
           [{result :?beer}] (query session beer?)]
       (is (= result (->Beer "Charlotte")))))

  (testing "getting the cleaners"
    (let [facts (derive-facts-from [{:name "Charlotte" :paid-cleaner true :chores [:bins]}])
          session (create-session-with facts)
          [{result :?beer}] (query session beer?)]
      (is (= result (->Beer "Charlotte")))))

  (testing "sadly being ill"
    (let [facts (derive-facts-from [{:name "Charlotte" :was-ill true :chores [:bins :kitchen :cooking :bathroom :balcony]}])
          session (create-session-with facts)
          result (query session beer?)]
      (is (empty? result ))))

  (testing "multiple flatmates"
    (let [facts (derive-facts-from [{:name "Felipe" :paid-cleaner true :chores [:bills]} {:name "Charlotte" :chores [:bins :dishes :bedroom :kitchen]}])
          session (create-session-with facts)
          result (query session beer?)]
      (is (= (beers result) (list (->Beer "Felipe") (->Beer "Charlotte")) )))))
