(ns world-test.drowned-meets-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.drowned-meets :as d]
            [world-test.event-counts :as c]))

;; a pond at x 0: water y 60..63 over stone, sky light 15, block light 0 but at the cells in `lit`
(defn pond [lit]
  (fn [x y z]
    (when (and (= 0 x) (= 0 z))
      {:name (if (<= 60 y 63) "water" "stone") :block (if (lit y) 15 0) :sky 15 :biome "river"})))

(defn rec [t k & {:as more}] (merge {:t t :k k :gen "g" :job "j1"} more))
(def in-water {:x 0.5 :y 63.2 :z 0.5})

(deftest water-with-a-dark-cell-over-water-is-deep-and-a-lit-or-one-deep-column-is-not
  (is (= "river-ocean/dark-at-night/deep" (d/class-of (pond #{}) in-water)))
  (is (= "river-ocean/lit/deep" (d/class-of (pond #{63}) in-water)) "a torch-lit surface over dark depths")
  (is (= "river-ocean/lit/shallow" (d/class-of (pond #{60 61 62 63}) in-water)))
  (is (= "river-ocean/dark-at-night/shallow" (d/class-of (pond #{}) {:x 0.5 :y 60.1 :z 0.5})) "the floor cell has no water under it")
  (is (nil? (d/class-of (pond #{}) {:x 0.5 :y 64 :z 0.5})) "not water")
  (is (nil? (d/class-of (constantly nil) in-water)) "a column never recorded"))

(deftest exposure-counts-the-seconds-between-close-positions-of-one-run-in-water
  (let [at (pond #{})
        pos (fn [t gen] (merge (rec t :pos :gen gen) in-water))]
    (is (= [{"river-ocean/dark-at-night/deep" 5}] (map :seconds (d/exposure at [(pos 0 "g") (pos 5000 "g")]))))
    (is (empty? (d/exposure at [(pos 0 "g") (pos 20000 "g")])) "a gap hides where the body was")
    (is (empty? (d/exposure at [(pos 0 "g") (pos 5000 "h")])) "another run")))

(deftest a-meet-counts-the-drowned-hp-in-its-run-and-a-death-within-the-window
  (let [[m] (d/meets (pond #{}) [(merge (rec -500 :at) in-water)
                                 (rec 0 :decided :mob "drowned" :decision :fight :health 13 :at {:x 5 :y 70 :z 5})
                                 (rec 100 :miss) (rec 1000 :hurt :amount 5 :hits 2 :causes #{"drowned"})
                                 (rec 1200 :hurt :amount 1 :causes #{"fall"})
                                 (rec 4000 :ended)
                                 (rec 9000 :hurt :amount 8 :hits 3 :causes #{"drowned"})
                                 (rec 9100 :died :cause "drowned")])]
    (is (= {:id ["g" "j1"] :class "river-ocean/dark-at-night/deep" :health 13 :decisions [:fight] :seconds 4
            :hp 5 :hits 2 :hp-window 13 :died ["drowned" 9100] :tried 1 :landed 0}
           (select-keys m [:id :class :health :decisions :seconds :hp :hits :hp-window :died :tried :landed])))))

(deftest a-run-soon-after-another-about-a-drowned-is-the-same-meet-again
  (is (= [false true] (map :again? (sort-by :t (d/meets (pond #{}) [(rec 0 :decided :mob "drowned" :decision :fight)
                                                                    (rec 5000 :decided :mob "drowned" :decision :fight :job "j2")]))))))

(deftest a-run-that-first-decided-about-another-mob-is-no-drowned-meet
  (is (empty? (d/meets (pond #{}) [(rec 0 :decided :mob "zombie" :decision :fight) (rec 10 :decided :mob "drowned" :decision :fight)]))))

(deftest the-prior-pools-deep-and-other-water-as-k-plus-one-over-n-each-meet-once
  (let [p (d/prior [{:seconds {"other/lit/deep" 300 "river-ocean/dark-at-night/deep" 700 "other/lit/shallow" 500}}]
                   [{:class "other/lit/deep"} {:class "other/lit/shallow"} {:class "other/lit/shallow" :again? true} {:class nil}])]
    (is (= {:k 1 :n 1000 :per-s 0.002} (:deep p)))
    (is (= {:k 1 :n 500 :per-s 0.004} (:other p)))))

(deftest folding-keeps-the-meet-row-whose-chunk-was-recorded
  (let [row (fn [cls hits] {:id ["g" "j1"] :t 0 :class cls :hits hits})
        folded (-> c/empty-counts (d/fold [] [(row "other/lit/deep" 1)]) (d/fold [] [(row nil 3)]))]
    (is (= "other/lit/deep" (get-in folded [:drowned-meets ["g" "j1"] :class])))
    (is (= 3 (get-in (d/fold folded [] [(row "other/lit/deep" 3)]) [:drowned-meets ["g" "j1"] :hits])) "then the one that saw more")))

(deftest scan-line-reads-deaths-and-any-position
  (is (= {:k :died :t 5 :gen "g" :cause "drowned"}
         (c/scan-line "{:generation-id \"g\", :source :body, :kind :died, :time-ms 5, :data {:cause \"drowned\", :pos {:x 1, :y 2, :z 3}}}")))
  (is (= {:k :at :t 5 :gen "g" :x 1 :y 62.5 :z -3}
         (c/scan-line "{:generation-id \"g\", :source :job, :kind :waiting, :time-ms 5, :data {:pos {:x 1, :y 62.5, :z -3}}}"))))

(deftest the-wet-deaths-count-first-meets-in-water-the-drowned-dealt-and-their-price
  (let [{:keys [k n rows priced]} (d/wet-death [{:class "other/lit/deep" :health 20 :decisions [:fight] :died ["drowned" 1]}
                                                {:class "other/lit/deep" :health 20 :decisions [:fight] :died ["drown" 1]}
                                                {:class "other/lit/deep" :again? true :died ["drowned" 1]}
                                                {:class nil :died ["drowned" 2]}])]
    (is (= [1 2] [k n]) "a breath death is not the drowned's")
    (is (= [true false] (map last rows)))
    (is (< 1.9 priced 2.01) "bare-handed and swimming, each fight priced near a death")))

(deftest the-in-water-fight-is-fitted-to-the-deaths-its-sword-fights-gave
  (let [fight (fn [health died] {:class "river-ocean/dark-at-night/deep" :health health :weapon "stone_sword" :decisions [:fight] :died (when died ["drowned" 1])})
        rate #(:per-swing (d/wet-fight %))]
    (is (< (rate [(fight 20 false) (fight 20 false) (fight 20 false) (fight 20 true)])
           (rate [(fight 20 true) (fight 20 true) (fight 20 true) (fight 20 false)]))
        "more deaths: more blows a swing")
    (is (nil? (rate [(assoc (fight 20 true) :class "other/lit/shallow")])) "a wade is no swim")))
