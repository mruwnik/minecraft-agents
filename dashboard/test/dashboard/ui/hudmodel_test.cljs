(ns dashboard.ui.hudmodel-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.hudmodel :as h]))

(deftest icons-cases
  (are [v expected] (= expected (h/icons v))
    20 (vec (repeat 10 :full))
    0 (vec (repeat 10 :empty))
    nil (vec (repeat 10 :empty))
    1 (into [:half] (repeat 9 :empty))
    3 (into [:full :half] (repeat 8 :empty))
    19 (conj (vec (repeat 9 :full)) :half)
    25 (vec (repeat 10 :full))
    -4 (vec (repeat 10 :empty))))

(deftest effect-text-cases
  (are [e expected] (= expected (h/effect-text e))
    {:name "strength" :amplifier 1 :duration 100} "strength II 0:05"
    {:name "speed" :amplifier 0 :duration 2400} "speed I 2:00"
    {:name "night_vision" :amplifier 0 :duration -1} "night_vision I"
    {:name "regeneration" :amplifier 4 :duration 20} "regeneration V 0:01"
    {:name "haste" :amplifier 11 :duration nil} "haste 12"))

(deftest held-text-cases
  (are [held expected] (= expected (h/held-text held))
    nil "empty hand"
    {:name "gravel" :count 64} "gravel ×64"
    {:name "bread" :count 1} "bread"))

(deftest xp-cases
  (are [xp expected] (= expected (h/xp-model xp))
    nil {:level 0 :percent 0}
    {:level 3 :progress 0.5} {:level 3 :percent 50}
    {:level 30 :progress 1.7} {:level 30 :percent 100}))

(def inv [{:slot 36 :name "diamond_sword" :count 1}
          {:slot 10 :name "gravel" :count 64}
          {:slot 6 :name "iron_chestplate" :count 1}
          {:slot 45 :name "shield" :count 1}])

(deftest slots-shape
  (let [s (h/slots inv)]
    (are [actual expected] (= expected actual)
      (map :slot (:hotbar s)) (range 36 45)
      (map #(map :slot %) (:main s)) [(range 9 18) (range 18 27) (range 27 36)]
      (map :slot (:armor s)) [5 6 7 8]
      (:slot (:offhand s)) 45
      (:name (first (:hotbar s))) "diamond_sword"
      (:label (first (:hotbar s))) "DS"
      (:title (second (first (:main s)))) "gravel ×64"
      (:icon (second (first (:main s)))) "/api/item-icon/gravel.png"
      (:empty? (second (:hotbar s))) true
      (:name (first (:armor s))) nil
      (:name (second (:armor s))) "iron_chestplate"
      (:name (:offhand s)) "shield")))
