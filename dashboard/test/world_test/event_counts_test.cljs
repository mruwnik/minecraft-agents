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
    (rec 4000 :held)
    (rec 4000 :decided :mob "creeper" :decision :fight)
    (rec 4000 :died))
  (is (= [[0 nil]] (map (juxt :lo :hi) (c/starts [(pos 0 0) (flee 100 "j1")] 5))) "the logs end"))

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
    (is (= [121 120 1] [(:n melee) (get-in setting ["pooled" :n]) (:censored melee)]) "the pool's n counts the starts seen to end")))

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
        "bounds replace a start told before they were kept, and stay"))
  (let [sighting (fn [end] {:id ["g" 0 "zombie"] :t 0 :mob "zombie" :s 1 :end end})
        folded (-> c/empty-counts (c/fold [] [] [(sighting :open)]) (c/fold [] [] [(sighting :caught)]) (c/fold [] [] [(sighting :gap)]))]
    (is (= :caught (get-in folded [:notices ["g" 0 "zombie"] :end])) "a sighting open when its log ended is replaced, a closed one stays")))

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
