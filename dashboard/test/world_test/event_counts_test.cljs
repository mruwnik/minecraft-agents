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
  (is (= [{:id ["g" "j1"] :t 0 :weapon "stone_sword" :swings 3 :hp 3 :blast? true}] (c/exchange run))))

(deftest a-run-with-another-mob-is-no-creeper-fight
  (is (empty? (c/exchange (conj run (rec 50 :decided :mob "zombie" :decision :fight))))))

(deftest a-fight-without-swings-in-the-logs-is-counted-apart
  (is (= {:runs 0 :no-swings 1 :swings 0 :hp 0 :blasts 0 :hp-per-swing nil :sd nil}
         (-> (c/exchange (remove #(= :hit (:k %)) run)) c/tally))))

(deftest the-posterior-mean-is-the-hp-plus-a-half-over-the-swings-and-the-sd-shrinks-with-more-swings
  (let [t (fn [hp swings] (c/tally [{:swings swings :hp hp}]))]
    (is (= 0.583 (:hp-per-swing (t 3 6))))
    (is (< (:sd (t 30 60)) (:sd (t 3 6))))))

(deftest only-fights-with-a-sword-or-axe-pool-into-the-armed-tally
  (let [row (fn [w] {:weapon w :swings 2 :hp 1})
        r (c/exchange-report [(row "stone_sword") (row "iron_axe") (row nil) (row "stick")])]
    (is (= [2 8] [(get-in r [:armed :runs]) (get-in r [:all :swings])]))
    (is (= ["iron_axe" "stick" "stone_sword" "unknown"] (keys (:by-weapon r))))))

(defn pos [t x] (rec t :pos :x x :y 64 :z 0))
(defn flee [t job] (rec t :decided :mob "creeper" :decision :flee :job job))

(deftest a-start-is-the-time-to-a-block-off-less-the-block-at-sprint-speed
  (is (= [(- 1.4 0.2)] (map :s (c/starts [(pos 0 0) (flee 100 "j1") (pos 600 0.5) (pos 1500 2)] 5)))))

(deftest only-the-first-flee-of-a-run-counts-and-a-decision-with-no-recent-position-is-dropped
  (is (= [{:id ["g" "j1"] :t 5000 :drop :no-pos}] (c/starts [(flee 5000 "j1")] 5)))
  (is (= 1 (count (c/starts [(pos 0 0) (flee 10 "j1") (flee 20 "j1") (pos 500 3)] 5)))))

(deftest a-move-seen-only-across-a-long-gap-is-kept-at-the-middle-of-the-gap
  (let [[row :as rows] (c/starts [(pos 0 0) (flee 100 "j1") (pos 3000 2)] 5)]
    (is (= 1 (count rows)))
    (is (true? (:sparse? row)))
    (is (< (js/Math.abs (- 1.25 (:s row))) 1e-9)) ; the middle of (100, 3000] is 1.45 s after the decision, less the block at sprint speed
    (is (= {:n 1 :sparse 1 :no-pos 0 :never 0} (select-keys (c/starts-report rows) [:n :sparse :no-pos :never])))))

(deftest deciles-are-the-sorted-values-at-the-tenths-midpoints
  (is (= [0 1 2 3 4 5 6 7 8 9] (c/deciles (range 10)))))

(deftest folding-merges-rows-by-run-id-so-the-counts-only-grow
  (let [fight (fn [swings] {:id ["g" "j1"] :t 0 :swings swings :hp 1})
        start (fn [m] (merge {:id ["g" "j1"] :t 0} m))
        folded (-> c/empty-counts
                   (c/fold [(fight 0)] [(start {:drop :no-pos})])
                   (c/fold [(fight 3)] [(start {:s 0.4})])
                   (c/fold [(fight 1) {:id ["g" "j2"] :t 1 :swings 1 :hp 0}] [(start {:s 9})]))]
    (is (= [3 2] [(get-in folded [:exchange ["g" "j1"] :swings]) (count (:exchange folded))]))
    (is (= {:id ["g" "j1"] :t 0 :s 0.4} (get-in folded [:starts ["g" "j1"]]))))
  (let [sighting (fn [end] {:id ["g" 0 "zombie"] :t 0 :mob "zombie" :s 1 :end end})
        folded (-> c/empty-counts (c/fold [] [] [(sighting :open)]) (c/fold [] [] [(sighting :caught)]) (c/fold [] [] [(sighting :gap)]))]
    (is (= :caught (get-in folded [:notices ["g" 0 "zombie"] :end])) "a sighting open when its log ended is replaced, a closed one stays")))

(defn saw [t mob d] (rec t :saw :mob mob :distance d :at {:x d :y 64 :z 0}))

(deftest a-sighting-counts-its-weighted-seconds-until-its-kind-is-decided-about-or-left
  (is (= [{:id ["g" 0 "zombie"] :t 0 :mob "zombie" :s 1.5 :end :caught}]
         (c/notices [(saw 0 "zombie" 2) (pos 1000 8) (rec 2000 :decided :mob "zombie")]))
      "weight 1 within 3 steps: 1 s, then 1 s at 6 steps (weight 0.5)")
  (is (= [[1 :left]] (map (juxt :s :end) (c/notices [(saw 0 "zombie" 2) (pos 1000 40)]))))
  (is (= [[0 :gap]] (map (juxt :s :end) (c/notices [(saw 0 "zombie" 2) (pos 20000 2)])))))

(deftest a-sighting-out-of-follow-range-or-of-a-provoked-only-mob-is-not-followed
  (is (empty? (c/notices [(saw 0 "skeleton" 20) (saw 0 "enderman" 2)]))))
