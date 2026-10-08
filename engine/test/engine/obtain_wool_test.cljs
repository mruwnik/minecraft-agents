(ns engine.obtain-wool-test
  "Wool as a gathered need of items.obtain: the colour the body carries most of, taken from sheep of that colour."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]
            [jobs.items.obtain :as obtain]))

(defn sheep [id x color]
  {:id id :name "sheep" :kind "passive" :pos {:x x :y 64 :z 0} :color color
   :drops [{:name (str color "_wool") :count 1}]})

(def beds ["white_bed" "brown_bed" "black_bed"])

(defn carried [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(defn ^:async scenario [args world n]
  (let [s (h/setup world 50)]
    (core/submit! (:eng s) (list 'jobs.items.obtain args) {})
    (dotimes [_ n]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))

(deftest a-bed-need-hunts-a-sheep-of-the-colour-carried-most
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (into h/sword (carried "white_wool" 2 "oak_planks" 3 "crafting_table" 1))
              s (await (scenario {:any-of beds :count 1 :how #{:craft :gather}}
                                 {:inventory inv :entities [(sheep 1 3 "brown") (sheep 2 6 "white") (sheep 3 8 "white") (sheep 4 10 "white") (sheep 5 11 "white")]} 40))]
          (is (seq (attacked s)) "a sheep was attacked")
          (is (every? #{2 3 4 5} (attacked s)) "only white sheep"))))))

(deftest a-colour-with-no-sheep-in-range-falls-back-to-another-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (into h/sword (carried "white_wool" 2 "oak_planks" 3 "crafting_table" 1))
              s (await (scenario {:any-of beds :count 1 :how #{:craft :gather}}
                                 {:inventory inv :entities [(sheep 1 3 "brown") (sheep 2 6 "brown") (sheep 4 10 "brown") (sheep 5 11 "brown")]} 40))
              e (first (filter #(= :obtain.wool-colour (:kind %)) @(:seen s)))]
          (is (seq (attacked s)))
          (is (every? #{1 2 4 5} (attacked s)))
          (is (= "white" (:wanted e)))
          (is (= "brown" (:taking e))))))))

;; hunt keeps the last 2 sheep of the colour alive (its :keep), so each colour in the tests has 4.
