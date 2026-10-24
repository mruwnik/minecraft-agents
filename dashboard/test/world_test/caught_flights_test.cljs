(ns world-test.caught-flights-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.caught-flights :as s]))

(defn flee [t health] {:k :decided :t t :gen "g" :mob "zombie" :decision :flee :health health})
(defn hurt [t hits] {:k :hurt :t t :gen "g" :causes #{"zombie"} :hits hits})

(deftest flee-decisions-within-the-gap-are-one-episode-caught-by-a-death-or-two-blows
  (is (= [{:group :zombie :t 0 :health 18 :chasers nil :died? false :hits 3} {:group :zombie :t 100000 :health 12 :chasers nil :died? false :hits 1} {:group :zombie :t 200000 :health 5 :chasers nil :died? true :hits 0}]
         (s/episodes [(flee 0 18) (hurt 5000 1) (flee 20000 15) (hurt 45000 2)
                      (flee 100000 12) (hurt 101000 1)
                      (flee 200000 5) {:k :died :t 210000 :gen "g" :cause "zombie"}])))
  (is (= [true false true true] (map s/caught? [{:group :zombie :hits 3} {:group :zombie :hits 1} {:group :zombie :died? true :hits 0} {:group :creeper :hits 1}])) "a creeper's one blast is a catch"))

(deftest a-band-counts-its-caught-episodes-and-the-blows-fit-the-deaths
  (let [rows (map #(assoc % :group :zombie)
                  (concat (repeat 10 {:health 20 :died? false :hits 0}) (repeat 3 {:health 20 :died? false :hits 4})
                          (repeat 4 {:health 4 :died? true :hits 2})))
        {:keys [bands blows]} (s/report rows [0 10])]
    (is (= {:zombie [{:from 0 :k 4 :n 4 :deaths 4} {:from 10 :k 3 :n 13 :deaths 0}]} bands))
    (is (< 0 blows 8) "four deaths at 4 hp and three lives at 20 hp: a few blows, under the 8 that kill at 20")))

(deftest an-episode-is-counted-by-the-most-chasers-of-its-group-a-decision-names
  (let [rows (s/episodes [(assoc (flee 0 18) :near ["zombie" "skeleton"]) (assoc (flee 5000 17) :near ["zombie" "husk" "spider"]) (hurt 6000 2)
                          (assoc (flee 100000 20) :near ["zombie"])])]
    (is (= [2 1] (map :chasers (filter #(= :zombie (:group %)) rows))))
    (is (= {1 {:k 0 :n 1} 2 {:k 1 :n 1}} (:by-chasers (s/report rows [0 10]))))))
