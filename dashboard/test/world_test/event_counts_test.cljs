(ns world-test.event-counts-test
  (:require [cljs.test :refer [deftest is are]]
            [world-test.event-counts :as c]))

(defn rec [t k & {:as more}] (merge {:t t :k k :gen "g" :job "j1"} more))

(def run
  [(rec 0 :decided :mob "creeper" :decision :fight)
   (rec 100 :equip :item "stone_sword")
   (rec 200 :hit) (rec 300 :hit) (rec 400 :hit)
   (rec 450 :hurt :amount 3 :causes #{"creeper"})
   (rec 460 :hurt :amount 1 :causes #{"zombie"})
   (rec 500 :ended)
   (rec 9000 :hurt :amount 8 :causes #{"creeper"})])

(deftest the-exchange-counts-swings-and-the-creeper-hp-inside-the-run-per-weapon
  (is (= [{:weapon "stone_sword" :swings 3 :hp 3 :blast? true}] (c/exchange run))))

(deftest a-run-with-another-mob-is-no-creeper-fight
  (is (empty? (c/exchange (conj run (rec 50 :decided :mob "zombie" :decision :fight))))))

(deftest a-fight-without-swings-in-the-logs-is-counted-apart
  (is (= {:runs 0 :no-swings 1 :swings 0 :hp 0 :blasts 0 :hp-per-swing nil}
         (-> (c/exchange (remove #(= :hit (:k %)) run)) c/tally (update :hp identity)))))

(defn pos [t x] (rec t :pos :x x :y 64 :z 0))
(defn flee [t job] (rec t :decided :mob "creeper" :decision :flee :job job))

(deftest a-start-is-the-time-to-a-block-off-less-the-block-at-sprint-speed
  (let [{:keys [starts]} (c/starts [(pos 0 0) (flee 100 "j1") (pos 600 0.5) (pos 1500 2)] 5)]
    (is (= [(- 1.4 0.2)] starts))))

(deftest only-the-first-flee-of-a-run-counts-and-a-decision-with-no-recent-position-is-dropped
  (is (= {:starts [] :no-pos 1 :never 0 :sparse 0} (c/starts [(flee 5000 "j1")] 5)))
  (is (= 1 (count (:starts (c/starts [(pos 0 0) (flee 10 "j1") (flee 20 "j1") (pos 500 3)] 5))))))

(deftest a-move-seen-only-across-a-long-gap-is-dropped
  (is (= {:starts [] :no-pos 0 :never 0 :sparse 1} (c/starts [(pos 0 0) (flee 100 "j1") (pos 3000 2)] 5))))

(deftest deciles-are-the-sorted-values-at-the-tenths-midpoints
  (is (= [0 1 2 3 4 5 6 7 8 9] (c/deciles (range 10)))))
