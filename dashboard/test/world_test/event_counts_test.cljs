(ns world-test.event-counts-test
  (:require [cljs.test :refer [deftest is are]]
            [world-test.event-counts :as c]
            [world-test.gap-miss :as gm]))

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

(deftest a-start-lies-between-the-last-position-still-there-and-the-first-a-block-off-less-the-block-at-sprint-speed
  (is (= [[0.3 1.2]] (map (juxt :lo :hi) (c/starts [(pos 0 0) (flee 100 "j1") (pos 600 0.5) (pos 1500 2)] 5)))))

(deftest only-the-first-flee-of-a-run-counts-and-a-decision-with-no-recent-position-is-dropped
  (is (= [{:id ["g" "j1"] :mob "creeper" :t 5000 :drop :no-pos}] (c/starts [(flee 5000 "j1")] 5)))
  (is (= 1 (count (c/starts [(pos 0 0) (flee 10 "j1") (flee 20 "j1") (pos 500 3)] 5)))))

(deftest a-move-seen-only-across-a-long-gap-is-the-whole-gap
  (is (= [[0 59.8]] (map (juxt :lo :hi) (c/starts [(pos 0 0) (flee 0 "j1") (pos 60000 2)] 5)))))

(deftest a-flight-no-longer-watched-before-it-moves-is-right-censored-at-its-last-still-position
  (are [stop] (= [[2.8 nil]] (map (juxt :lo :hi) (c/starts [(pos 0 0) (flee 0 "j1") (pos 3000 0.5) stop (pos 9000 5)] 5)))
    (rec 4000 :ended)
    (rec 4000 :decided :mob "creeper" :decision :fight)
    (rec 4000 :died))
  (is (= [[0 nil]] (map (juxt :lo :hi) (c/starts [(pos 0 0) (flee 100 "j1")] 5))) "the logs end"))

(deftest a-run-that-holds-still-before-it-moves-answered-otherwise-and-gives-no-start
  (let [rows (c/starts [(pos 0 0) (flee 0 "j1") (pos 3000 0.5) (rec 4000 :held) (pos 9000 5)] 5)]
    (is (= [{:id ["g" "j1"] :mob "creeper" :t 0 :drop :held}] rows))
    (is (= 0 (get-in (c/starts-by-mob rows) [:melee :n])))))

(deftest a-respond-run-holding-still-is-read-as-held
  (is (= :held (:k (c/scan-line "{:generation-id \"g\", :source :job, :kind :holding, :context {:job-id \"j54\", :reflex-id :hostile-near}, :time-ms 5, :message \"holding: hiding\", :data {:pos {:x 1, :y 2, :z 3}}}"))))
  (is (= :pos (:k (c/scan-line "{:generation-id \"g\", :source :job, :kind :holding, :context {:job-id \"j54\", :reflex-id :hostile-near}, :time-ms 5, :message \"holding: searching\", :data {:pos {:x 1, :y 2, :z 3}}}")))
      "a search is part of the start; the engine stamped the body's place on it"))

(deftest each-mob-kind-has-its-own-first-flee-and-the-report-splits-them
  (let [zombie (fn [t job] (rec t :decided :mob "zombie" :decision :flee :job job))
        rows (c/starts [(pos 0 0) (flee 100 "j1") (zombie 150 "j1") (pos 600 3)] 5)
        r (c/starts-by-mob rows)]
    (is (= #{["g" "j1"] ["g" "j1" "zombie"]} (set (map :id rows))))
    (is (= [1 1] [(get-in r [:by-mob "creeper" :n]) (get-in r [:by-mob "zombie" :n])]))
    (is (= 2 (get-in r [:melee :n])))))

(deftest the-setting-holds-each-melee-kind-shrunk-toward-the-others-and-the-pool-of-all
  (let [row (fn [mob lo hi] {:id [mob lo] :mob mob :t 0 :lo lo :hi hi})
        rows (concat (map #(row "zombie" (* 0.05 %) (* 0.05 %)) (range 40)) (map #(row "creeper" (* 0.02 %) (* 0.02 %)) (range 80))
                     [(row "skeleton" 9 9) (row "creeper" 0.5 nil) {:id [0] :mob "zombie" :t 0 :drop :no-pos}])
        {:keys [setting melee]} (c/starts-by-mob rows)]
    (is (= #{"pooled" "creeper" "zombie"} (set (keys setting))) "no row for a ranged kind")
    (is (= [121 121 1] [(:n melee) (get-in setting ["pooled" :n]) (:censored melee)]) "the pool's n counts its censored start too")))

(deftest folding-merges-rows-by-run-id-so-the-counts-only-grow
  (let [fight (fn [swings] {:id ["g" "j1"] :t 0 :swings swings :hp 1})
        start (fn [m] (merge {:id ["g" "j1"] :t 0} m))
        folded (-> c/empty-counts
                   (c/fold [(fight 0)] [(start {:drop :no-pos})])
                   (c/fold [(fight 3)] [(start {:s 0.4})])
                   (c/fold [(fight 1) {:id ["g" "j2"] :t 1 :swings 1 :hp 0}] [(start {:drop :no-pos})]))]
    (is (= [3 2] [(get-in folded [:exchange ["g" "j1"] :swings]) (count (:exchange folded))]))
    (is (= {:id ["g" "j1"] :t 0 :s 0.4} (get-in folded [:starts ["g" "j1"]])) "a told start stays when its logs rotate away")
    (is (= {:id ["g" "j1"] :t 0 :lo 0.3 :hi 0.5}
           (get-in (-> folded (c/fold [] [(start {:lo 0.3 :hi 0.5})]) (c/fold [] [(start {:s 9})])) [:starts ["g" "j1"]]))
        "bounds replace a start told before they were kept, and stay")
    (is (= {:id ["g" "j1"] :t 0 :drop :held} (get-in (-> folded (c/fold [] [(start {:lo 2.8 :hi nil})]) (c/fold [] [(start {:drop :held})])) [:starts ["g" "j1"]]))
        "a run read again as held replaces the start it was folded as before held runs were left out")
    (are [later] (= later (get-in (-> folded (c/fold [] [(start {:lo 0.3 :hi nil :end :logs-end})]) (c/fold [] [later])) [:starts ["g" "j1"]]))
      (start {:lo 0.3 :hi 1.1 :end :moved})
      (start {:lo 0.9 :hi nil :end :logs-end})
      (start {:drop :held})))
  (let [sighting (fn [end] {:id ["g" 0 "zombie"] :t 0 :mob "zombie" :s 1 :end end})
        folded (-> c/empty-counts (c/fold [] [] [(sighting :open)]) (c/fold [] [] [(sighting :caught)]) (c/fold [] [] [(sighting :gap)]))]
    (is (= :caught (get-in folded [:notices ["g" 0 "zombie"] :end])) "a sighting open when its log ended is replaced, a closed one stays")))

(deftest a-start-whose-last-still-time-is-within-the-sprint-block-may-be-at-0
  (is (= [0 0 0 0 0 5 5 5 5 5] (:deciles (c/starts-report [{:lo 0 :hi 0} {:lo 0 :hi nil} {:lo 5 :hi 5}])))))

(deftest a-start-folded-before-bounds-were-kept-is-exact-or-censored-at-give-up
  (is (= [[0.4 0.4] [10 nil] nil] (map c/bounds [{:s 0.4} {:drop :never} {:drop :no-pos}]))))

(defn saw [t mob d] (rec t :saw :mob mob :distance d :at {:x d :y 64 :z 0}))

(deftest a-sighting-counts-its-weighted-seconds-until-its-kind-hurts-the-body-or-it-is-left
  (is (= [{:id ["g" 0 "zombie"] :t 0 :mob "zombie" :s 1.5 :end :caught}]
         (c/notices [(saw 0 "zombie" 2) (pos 1000 8) (rec 2000 :hurt :amount 3 :causes #{"zombie"})]))
      "weight 1 within 3 steps: 1 s, then 1 s at 6 steps (weight 0.5)")
  (is (= [:open] (map :end (c/notices [(saw 0 "zombie" 2) (pos 1000 8) (rec 2000 :hurt :amount 3 :causes #{"spider"})]))))
  (is (= [[1 :left]] (map (juxt :s :end) (c/notices [(saw 0 "zombie" 2) (pos 1000 40)]))))
  (is (= [[0 :gap]] (map (juxt :s :end) (c/notices [(saw 0 "zombie" 2) (pos 20000 2)])))))

(deftest a-decision-is-no-catch-unless-it-places-the-seen-mob-within-melee-reach-of-the-body
  (let [near {:x 7 :y 64 :z 0} far {:x 2 :y 64 :z 0}
        ends (fn [at] (map :end (c/notices [(saw 0 "zombie" 2) (pos 1000 8) (rec 1500 :decided :mob "zombie" :at at)])))]
    (is (= [:caught] (ends near)))
    (is (= [:open] (ends far)) "a respond decision about the kind, the mob 6 blocks off, is a sighting still open")
    (is (= [:open] (ends nil)) "a heard mob has no spot")))

(deftest one-hurt-or-decision-ends-one-sighting-of-the-kind-the-nearest-to-the-event
  (let [ends (fn [event] (map (juxt :t :end) (sort-by :t (c/notices [(saw 0 "zombie" 20) (saw 1000 "zombie" 2) (pos 1500 1) event]))))]
    (is (= [[0 :open] [1000 :caught]] (ends (rec 2000 :hurt :amount 3 :causes #{"zombie"} :at {:x 1 :y 64 :z 0})))
        "two zombies seen, one hurt: the one nearest the body is caught, the other stays open")
    (is (= [[0 :open] [1000 :caught]] (ends (rec 2000 :decided :mob "zombie" :at {:x 2 :y 64 :z 0})))
        "a decision catches the sighting nearest the mob it places")))

(deftest a-ranged-mob-is-caught-and-weighed-by-its-range-not-its-melee-reach
  (let [decided (fn [mob] (c/notices [(saw 0 mob 12) (pos 1000 0) (rec 1500 :decided :mob mob :at {:x 12 :y 64 :z 0})]))]
    (is (= [[1.5 :caught]] (map (juxt :s :end) (decided "skeleton"))) "12 blocks off, in range: weight 1, caught by a decision there")
    (is (= [:open] (map :end (decided "zombie"))) "a zombie at 12 blocks is out of melee reach"))
  (is (= [:other :ranged] (keys (:by-class (c/notice-report (c/notices [(saw 0 "skeleton" 12) (saw 0 "zombie" 12)])))))))

(deftest a-ranged-mob-is-caught-within-its-own-kinds-range
  (let [caught (fn [mob d] (map :end (c/notices [(saw 0 mob d) (pos 1000 0) (rec 1500 :decided :mob mob :at {:x d :y 64 :z 0})])))]
    (is (= [:caught] (caught "skeleton" 14)))
    (is (= [:open] (caught "skeleton" 16)) "past its 15")
    (is (= [:open] (caught "pillager" 12)) "a pillager's crossbow reaches 8")))

(deftest a-mob-known-again-is-one-sighting
  (is (= 1 (count (c/notices [(assoc (saw 0 "zombie" 10) :id 7) (pos 500 0) (assoc (saw 800 "zombie" 10) :id 7)]))))
  (is (= 2 (count (c/notices [(assoc (saw 0 "zombie" 10) :id 7) (pos 500 0) (assoc (saw 800 "zombie" 10) :id 8)])))))

(deftest a-sighting-out-of-follow-range-or-of-a-provoked-only-mob-is-not-followed
  (is (empty? (c/notices [(saw 0 "skeleton" 20) (saw 0 "enderman" 2)]))))

(deftest ore-seen-together-is-one-group-a-vein-s-deepslate-blocks-with-its-plain-ones
  (is (= [2 1] (sort > (c/groups [[0 0 0] [1 1 1] [5 0 0]]))))
  (is (= {:groups 3 :blocks 5 :per-group 1.67 :capped 1 :by-ore {"coal" {:groups 1 :blocks 3} "iron" {:groups 2 :blocks 2}}}
         (c/vein-seen [{:k :observed :capped? true :blocks [["coal_ore" [0 0 0]] ["deepslate_coal_ore" [0 -1 0]] ["coal_ore" [1 -2 0]]
                                                             ["iron_ore" [9 0 0]]]}
                       {:k :observed :blocks [["iron_ore" [0 0 0]]]}
                       {:k :hurt}]))))

(deftest the-flee-pace-is-blocks-over-the-steps-own-seconds-and-the-stops-between-them
  (let [step (fn [t job s blocks] (rec t :step :job job :slot :step :s s :blocks blocks))
        rows (c/flee-steps [(step 1000 "j1" 1 4) (step 2500 "j1" 1 3) (step 3000 "j1" 0.5 2) (step 9000 "j2" 1 5)])]
    (is (= {:flights 1 :steps 3 :blocks 9 :seconds 2.5 :bps 3.6 :gaps 2 :gap-s 0.5 :off-gaps 0 :stop-s 0.25} (c/pace-report rows))
        "a lone step is no flight; the stops run from a step's end to the next one's start")))

(deftest the-closing-rate-is-the-gap-growth-between-step-starts-per-mob-kind-and-sprint-state
  (let [step (fn [t job food chasers] (rec t :step :job job :slot :step :s 1 :blocks 4 :food food :chasers chasers))
        line (pr-str {:generation-id "g" :time-ms 5 :source :job :kind :retreat.step-speed :context {:job-id "j1" :reflex-id :hostile-near}
                      :data {:slot :step :seconds 1 :blocks 4 :bps 4 :food 18 :chasers [["zombie" 7 3.5]]}})
        rows (c/flee-steps [(step 1000 "j1" 20 [["zombie" 7 3] ["spider" 8 6]])
                            (step 2500 "j1" 20 [["zombie" 7 4] ["spider" 8 5]])
                            (step 3500 "j1" 20 [["zombie" 7 6]])
                            (step 5000 "j2" 5 [["zombie" 9 3]])
                            (step 7000 "j2" 5 [["zombie" 9 2]])
                            (rec 7500 :step :job "j3" :slot :step :s 1 :blocks 4)
                            (rec 9500 :step :job "j3" :slot :step :s 1 :blocks 4)])]
    (is (= {:food 18 :chasers [["zombie" 7 3.5]]} (select-keys (c/scan-line line) [:food :chasers])))
    (is (= {:flights 2
            :by-mob {"spider" {:sprint {:blocks -1 :s 1.5 :pieces 1 :bps -0.67}}
                     "zombie" {:sprint {:blocks 3 :s 2.5 :pieces 2 :bps 1.2} :walk {:blocks -1 :s 2 :pieces 1 :bps -0.5}}}
            :setting {"spider" {:sprint {:blocks -1 :s 1.5}} "zombie" {:sprint {:blocks 3 :s 2.5} :walk {:blocks -1 :s 2}}}}
           (c/closing-report rows))
        "each piece runs from one step's start to the next one's; a mob not seen at both starts and steps kept before gaps were are left out")
    (is (empty? (:setting (c/closing-report (c/flee-steps [(step 1000 "j1" 20 [["zombie" 7 3]]) (rec 2200 :step :job "j1" :slot :back-off :s 1 :blocks 1)
                                                            (step 3500 "j1" 20 [["zombie" 7 4]])]))))
        "a stop in which the flight ended (another answer's step) is no part of a flight")))

(deftest a-gap-in-which-the-flight-ended-is-no-stop
  (let [step (fn [t slot] (rec t :step :job "j1" :slot slot :s 1 :blocks 4))
        line (fn [t kind data] (pr-str {:generation-id "g" :time-ms t :source (if (= :started kind) :action :job) :kind kind
                                        :context {:job-id "j1/step" :reflex-id :hostile-near} :data data}))
        report (fn [between] (c/pace-report (c/flee-steps (sort-by :t (remove nil? (concat [(step 1000 :step) (step 4000 :step)] between))))))]
    (is (= {:gaps 1 :off-gaps 0 :stop-s 2} (select-keys (report [(c/scan-line (line 2000 :started {:name "look"}))]) [:gaps :off-gaps :stop-s]))
        "a look on the way: the flight stood, the chaser coming")
    (is (some? (c/scan-line (line 2000 :child_started {:slot :eat}))))
    (are [between] (= {:gaps 0 :off-gaps 1} (select-keys (report between) [:gaps :off-gaps]))
      [(c/scan-line (line 2000 :child_started {:slot :eat}))]
      [(c/scan-line (line 2000 :started {:name "place"}))]
      [(step 2000 :back-off)])))

(deftest a-fixture-sighting-runs-from-its-firing-to-the-first-hurt-from-its-kind-or-the-window
  (let [shown (fn [& rs] (map (juxt :s :end :label) (c/exposures (sort-by :t rs))))
        exposed (rec 1000 :exposed :mob "skeleton" :label "skeleton lit 9" :window-ms 20000)]
    (is (= [[1.5 :caught "skeleton lit 9"]] (shown exposed (rec 2500 :hurt :amount 3 :causes #{"skeleton"}) (rec 4000 :hurt :amount 3 :causes #{"skeleton"}))))
    (is (= [[20 :window "skeleton lit 9"]] (shown exposed (rec 30000 :hurt :amount 3 :causes #{"skeleton"}) (rec 2000 :hurt :amount 1 :causes #{"fall"}))))
    (is (empty? (shown exposed (rec 600 :hurt :amount 3 :causes #{"skeleton"}))) "shot before it was seen: left out")
    (is (= [[0.5 :caught "skeleton lit 9"]] (shown exposed (rec 2500 :hurt :amount 3 :causes #{"skeleton"} :since 1500)))
        "the hurt is emitted a merge window after its first hit, whose time is :since")
    (is (= 1 (get-in (c/notice-report (c/exposures [exposed (rec 2000 :hurt :amount 3 :causes #{"skeleton"})])) [:by-class :ranged :caught])))))

(deftest a-ranged-notice-text-is-a-fixture-sighting
  (is (= {:k :exposed :t 5 :gen "g" :mob "skeleton" :label "skeleton lit 9" :window-ms 20000}
         (c/scan-line "{:generation-id \"g\", :source :job, :kind :job.notify, :context {:job-id \"j1\", :reflex-id :hostile-near}, :time-ms 5, :message \"ranged-notice skeleton lit 9 20 | health 20\"}"))))

(defn steer-line [t kind job x & {:keys [flight?] :or {flight? false}}]
  (pr-str {:generation-id "g" :time-ms t :source :action :kind kind
           :context (cond-> {:job-id job :action-id (str job "-a")} flight? (assoc :reflex-id :hostile-near))
           :data (cond-> {:name "steer" :pos {:x x :y 64 :z 0}} (= :done kind) (assoc :status "done"))}))

(deftest the-walk-pace-is-blocks-over-the-seconds-of-steers-of-walks-outside-a-flight
  (let [steer (fn [job t0 x0 t1 x1 & more]
                [(c/scan-line (apply steer-line t0 :started job x0 more)) (c/scan-line (apply steer-line t1 :done job x1 more))])
        rows (c/walk-steers (concat (steer "j1/walk" 1000 0 3000 8)
                                    (steer "j1/collect/search" 4000 0 5000 9)
                                    (steer "j2/walk" 6000 0 7000 9 :flight? true)
                                    (steer "j3/c0/walk" 8000 0 9000 4)))]
    (is (= {:walks 2 :steers 2 :blocks 12 :seconds 3 :bps 4} (c/walk-report rows))
        "a search's steer and a flight's are left out")))

(deftest the-walk-detour-is-the-steers-blocks-over-the-straight-line-of-each-walk
  (let [steer (fn [job t0 x0 t1 x1 & more]
                [(c/scan-line (apply steer-line t0 :started job x0 more)) (c/scan-line (apply steer-line t1 :done job x1 more))])
        rows (c/walk-steers (concat (steer "j1/walk" 1000 0 2000 10) (steer "j1/walk" 3000 10 4000 5)
                                    (steer "j2/walk" 5000 0 6000 8)
                                    (steer "j3/walk" 7000 0 8000 2)
                                    (steer "j4/walk" 9000 0 10000 5) (steer "j4/walk" 11000 20 12000 30) (steer "j4/walk" 13000 30 14000 25)))]
    (is (= {:walks 4 :walked 43 :straight 23 :ratio 1.87 :median 2} (select-keys (c/walk-detour rows) [:walks :walked :straight :ratio :median]))
        "out 10 and back 5 is 15 blocks over 5, a straight 8 over 8; a walk under 4 blocks straight is left out; a steer starting over 3 blocks from the last end is another walk (5 straight, then out 10 and back 5); the median of 4 walks is the mean of the middle two")))

(defn gap-line [t out & {:keys [width dy sprint] :or {width 2 dy 0 sprint true}}]
  (pr-str {:generation-id "g" :time-ms t :source :job :kind :gap-jump :level :info
           :data {:width width :dy dy :sprint sprint :from [0 64 1] :to [0 64 4] :out out}}))

(deftest gap-miss-is-the-beta-posterior-of-falls-per-width-dy-and-sprint
  (let [lines (concat (map #(gap-line % :landed) (range 1000 1009))
                      [(gap-line 2000 :fell) (gap-line 2001 :stuck)
                       (gap-line 3000 :landed :width 1 :sprint false)])
        report (gm/report (gm/rows (keep c/scan-line lines)))]
    (is (= {:n 10 :fell 1 :stuck 1 :mean 0.06} (select-keys (report [2 0 true]) [:n :fell :stuck :mean]))
        "(0.2 + 1) / (0.2 + 9.8 + 10): stuck jumps are no miss and no landing")
    (is (= {:n 1 :fell 0} (select-keys (report [1 0 false]) [:n :fell])))
    (let [{:keys [lo hi mean]} (report [2 0 true])]
      (is (< 0 lo mean hi 1) "a 90% interval round the mean"))))

(deftest beta-quantile-inverts-the-beta-cumulative
  (is (< (js/Math.abs (- 0.5 (gm/beta-cdf 2 2 0.5))) 1e-6) "Beta(2,2) is symmetric")
  (is (< (js/Math.abs (- 0.9 (gm/beta-cdf 1 3 (gm/beta-quantile 1 3 0.9)))) 1e-6)))
