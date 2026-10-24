(ns world-test.caught-flights-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.caught-flights :as s]))

(defn flee [t health] {:k :decided :t t :gen "g" :mob "zombie" :decision :flee :health health})
(defn hurt [t hits] {:k :hurt :t t :gen "g" :causes #{"zombie"} :hits hits})

(deftest flee-decisions-within-the-gap-are-one-episode-caught-by-a-death-or-two-blows
  (is (= [{:t 0 :health 18 :died? false :hits 3} {:t 100000 :health 12 :died? false :hits 1} {:t 200000 :health 5 :died? true :hits 0}]
         (s/episodes [(flee 0 18) (hurt 5000 1) (flee 20000 15) (hurt 45000 2)
                      (flee 100000 12) (hurt 101000 1)
                      (flee 200000 5) {:k :died :t 210000 :gen "g" :cause "zombie"}])))
  (is (= [true false true] (map s/caught? [{:hits 3} {:hits 1} {:died? true :hits 0}]))))

(deftest a-band-counts-its-caught-episodes-and-the-blows-fit-the-deaths
  (let [rows (concat (repeat 10 {:health 20 :died? false :hits 0}) (repeat 3 {:health 20 :died? false :hits 4})
                     (repeat 4 {:health 4 :died? true :hits 2}))
        {:keys [bands blows]} (s/report rows [0 10])]
    (is (= [{:from 0 :k 4 :n 4 :deaths 4} {:from 10 :k 3 :n 13 :deaths 0}] bands))
    (is (< 0 blows 8) "four deaths at 4 hp and three lives at 20 hp: a few blows, under the 8 that kill at 20")))
