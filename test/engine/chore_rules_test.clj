(ns engine.chore_rules_test
  (:require [clojure.test :refer :all]
            [engine.chore-rules :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]))

(defn person-facts
  [person]
  (let [flatmate (make-flatmate person)
        chores (map #(->Chore (:name person) %) (:chores person))]
    (list flatmate chores)
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
  (testing "That a sick flatname is automagically exempted"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (->FlatMate "Charlotte" true false))
                    (fire-rules))
          [{result :?exemption}] (query session exemptions-query?)]
      (is (= result (->Exemption "Charlotte")))))

   (testing "doing 4 chores gives you beer"
     (let [session (-> (mk-session 'engine.chore-rules)
                     (insert (make-flatmate {:name "Charlotte"}))
                     (insert (->Chore "Charlotte" :bathroom))
                     (insert (->Chore "Charlotte" :kitchen))
                     (insert (->Chore "Charlotte" :laundry))
                     (insert (->Chore "Charlotte" :bins))
                     (fire-rules))
           [{result :?beer}] (query session beer?)]
       (is (= result (->Beer "Charlotte")))))

  (testing "getting the cleaners"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (make-flatmate {:name "Charlotte" :paid-cleaner true}))
                    (insert (->Chore "Charlotte" :bins))
                    (fire-rules))
          [{result :?beer}] (query session beer?)]
      (is (= result (->Beer "Charlotte")))))

  (testing "sadly being ill"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (make-flatmate {:name "Charlotte" :was-ill true}))
                    (insert (->Chore "Charlotte" :bins))
                    (insert (->Chore "Charlotte" :kitchen))
                    (insert (->Chore "Charlotte" :cooking))
                    (insert (->Chore "Charlotte" :bathroom))
                    (insert (->Chore "Charlotte" :balcony))
                    (fire-rules))
          result (query session beer?)]
      (is (empty? result ))))
  

;;  (defn beerable? [person]
;;    (let [known-facts (derive-facts-from person)  ;; ---> derive-facts-from turns the person into a list of facts
;;          session (create-session-with known-facts)] ;; ---> inserts said facts into Clara and runs fires the rules
;;      (has-beer? session))) ;; ---> runs my query AND removes Clara'isms from it... aka ?foo should be gone!
;;
  (testing "multiple flatmates"
    (let [session (-> (mk-session 'engine.chore-rules)
                    (insert (make-flatmate {:name "Felipe" :paid-cleaner true}))
                    (insert (->Chore "Felipe" :bills))
                    (insert (make-flatmate {:name "Charlotte"}))
                    (insert (->Chore "Charlotte" :bins))
                    (insert (->Chore "Charlotte" :dishes))
                    (insert (->Chore "Charlotte" :bedroom))
                    (insert (->Chore "Charlotte" :kitchen))
                    (fire-rules))
          result (query session beer?)]
      (is (= (beers result) (list (->Beer "Felipe") (->Beer "Charlotte")) )))))
