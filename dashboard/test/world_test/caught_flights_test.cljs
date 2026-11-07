(ns world-test.caught-flights-test
  (:require [cljs.test :refer [deftest is]]
            [engine.settings :as settings]
            [jobs.lib.cost.caught :as caught]
            [jobs.lib.cost.threat :as threat]
            [world-test.caught-flights :as s]))

(defn decided [t health decision] {:k :decided :t t :gen "g" :mob "zombie" :decision decision :health health :job "r1"})
(defn flee [t health] (decided t health :flee))
(defn hurt [t hits] {:k :hurt :t t :gen "g" :causes #{"zombie"} :hits hits})
(defn fled [t] {:k :pos :t t :gen "g" :fled "r1"})

(deftest decisions-within-the-gap-are-one-episode-with-the-blows-and-death-after-it
  (is (= [[0 18 false 3 nil] [100000 12 false 1 nil] [200000 5 true 0 nil]]
         (map (juxt :t :health :died? :hits :weapon)
              (s/episodes [(flee 0 18) (hurt 5000 1) (flee 20000 15) (hurt 45000 2)
                           (flee 100000 12) (hurt 101000 1)
                           (flee 200000 5) {:k :died :t 210000 :gen "g" :cause "zombie"}]))))
  (is (= [["stone_sword" :fight]] (map (juxt :weapon :decision) (s/episodes [(decided 0 20 :fight) {:k :equip :t 10 :gen "g" :job "r1" :item "stone_sword"}] :fight)))
      "a fight's weapon is the one its run equipped"))

(deftest a-group-is-counted-as-its-prior-reads-it-a-death-by-the-blows-that-kill
  (settings/with-settings {::threat/difficulty 1}
    (fn []
      (let [rows (map #(assoc % :group :zombie)
                      (concat (repeat 10 {:health 20 :died? false :hits 0}) (repeat 3 {:health 20 :died? false :hits 4})
                              (repeat 4 {:health 4 :died? true :hits 2})))
            {:keys [flights fit]} (s/report (conj rows {:group :bow :health 6 :died? true :hits 1}) [])]
        (is (= {:zombie {:lived [[20 0 10] [20 4 3]] :died [[4 2 4]]} :bow {:lived [] :died [[6 3 1]]}} flights)
            "4 hp dies of 2 blows of 2.5; 6 hp of 3 arrows of 2.94")
        (is (= (s/r3 (:blows (caught/posterior (:zombie flights)))) (get-in fit [:zombie :blows])))))))

(deftest an-episode-is-counted-by-the-most-chasers-of-its-group-a-decision-names
  (let [rows (s/episodes [(assoc (flee 0 18) :near ["zombie" "skeleton"]) (assoc (flee 5000 17) :near ["zombie" "husk" "spider"]) (hurt 6000 2)
                          (assoc (flee 100000 20) :near ["zombie"])])]
    (is (= [2 1] (map :chasers (filter #(= :zombie (:group %)) rows))))
    (is (= {1 {:k 0 :n 1} 2 {:k 1 :n 1}} (:by-chasers (s/report rows []))))))

(deftest a-flight-held-still-before-it-stepped-is-no-sample-of-a-flights-catch
  (let [died {:k :died :t 8000 :gen "g" :cause "zombie"}
        held {:k :held :t 4000 :gen "g" :job "r1"}
        rows (concat (s/episodes [(flee 0 20) held died])
                     (s/episodes [(flee 0 20) (fled 500) held died])
                     (s/episodes [(flee 0 20) (assoc (fled 500) :fled "r2") held died]))]
    (is (= [false] (map :unlogged? (s/episodes [(fled -100) (flee 0 20) held died]))) "a step logged before it: the logs reach back far enough to show its own")
    (is (= [true false true] (map :held? rows)) "a step of its own run before the hold: the flight ran, then was cornered")
    (is (= {:flights {:zombie {:lived [] :died [[20 8 1]]}} :held {:zombie {:n 2 :died 2 :unlogged 2}}}
           (select-keys (settings/with-settings {::threat/difficulty 1} #(s/report rows [])) [:flights :held])))))

(deftest fights-are-counted-armed-and-dry
  (settings/with-settings {::threat/difficulty 1}
    (fn []
      (let [f (fn [weapon mob died?] {:group :zombie :health 20 :weapon weapon :mob mob :died? died? :hits (if died? 0 1)})]
        (is (= {:zombie {:lived [[20 1 1]] :died [[20 8 1]]}}
               (:fight-counts (s/report [] [(f "iron_sword" "zombie" false) (f "stone_sword" "husk" true) (f nil "zombie" true) (f "iron_sword" "drowned" true)])))
            "a fist or unknown weapon, and a drowned (cost.fight/wet-rate's), are not counted")))))
