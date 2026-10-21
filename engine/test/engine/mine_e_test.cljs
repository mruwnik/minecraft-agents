(ns engine.mine-e-test
  "jobs.gather.mine against the fake world."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is async]]
            [clojure.string]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.blocks :as lb]
            [jobs.lib.look :as lk]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.tools :as tools]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.expr :as expr]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.gather.dig-for :as mine]
            [jobs.gather.mine :as mine-job]
            [jobs.gather.mine.descent :as mine-descent]
            [jobs.gather.mine.end :as mine-end]
            [jobs.gather.mine.targets :as mine-targets]
            [jobs.gather.mine.torches :as mine-torches]
            [jobs.gather.mine.way-set :as way-set]
            [jobs.gather.mine.tunnel :as mine-tunnel]
            [jobs.gather.mine-stay :as stay]
            [jobs.access.stair :as stair]
            [plan.shape :as shape]
            [engine.mine-support :refer [spec as-child floor-block sight-opts seeing start run-ticks scenario events-of done-events done-event finished? inv job-mem calls moved dig-count dug-cells block-at cells floor wide-floor sand-patch set-pos! bare-pickaxe pickaxe cobble stone-floor stone-hill ground-cell? dug-ground farm-zone pad-plan two-in-two-out zoned their-claim rock rock-world feet tunnel-ends open-line east start-x corridor bridge-run no-floor-end? deep-rock far-cave side-cave facing-west-run stay-run side-ore-run shutdown-at-cell! run-until! below-ore-run run-until shaft-plan shafted via-shaft ore-east drop-away! hide-drop-until-beside! walled-ore watched dark-ore-dug long-rock torch-world torch-xs torches8 side-room side-torch-cells branch-torches soil-over-stone soil-all-round descent-run stair-down-east classify-in walk-options spy-go-to  home-zone no-shaft-args deep-ore]]))

(deftest the-tunnel-cuts-beside-lava-it-has-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 3}
                                 (rock-world {"2,64,1" "lava"}) 80))]
          (is (some #(= 2 (first %)) (dug-cells s)) "the body cut past lava no sight reached"))))))

(deftest the-walk-to-a-target-is-go-near-and-a-waiting-walk-is-a-yield
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"15,64,0" "sand"}}})
              opts (atom [])]
          (await (tu/with-near-stub (fn ^:async f [_ _ _ o] (swap! opts conj o) :partial)
            (fn ^:async b [] (core/submit! (:eng s) (spec {:block "sand" :ways #{:seen}}) {})
            (await (run-ticks s 6)))))
          (is (seq @opts))
          (is (zero? (:failures (job-mem s) 0)) "a waiting walk is no failure")
          (is (zero? (dig-count s))))))))

(deftest a-mine-started-on-its-own-stair-ignores-the-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts (await (walk-options {:dim "overworld" :floors (vec (for [y (range 58 68)] [0 y 0]))}))]
          (is (seq opts))
          (is (= [{:safe? '(not (threatened))}] (distinct (map :checks opts)))))))))

(deftest a-mine-started-off-its-stair-keeps-the-default-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts (await (walk-options nil))]
          (is (seq opts))
          (is (= [nil] (distinct (map :checks opts)))))))))

(deftest no-tool-names-the-kind-the-block-needs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "cobweb" :count 2} {:blocks (cells "cobweb" [3 4] [64] [0]) :inventory []} 10))]
          (is (zero? (dig-count s)))
          (is (= "sword" (:tool (done-event s)))))))))

(deftest never-digs-the-cell-under-the-feet-onto-an-unseen-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"0,63,0" "sand" "0,62,0" "lava"}
                                :inventory [{:name "cobblestone" :count 2}]}})
              ys (atom [])]
          (add-watch (fake/state (:p s)) ::ys (fn [_ _ _ w] (swap! ys conj (second (get-in w [:self :pos])))))
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :ways #{:seen} :trip true :mend true}) {})
          (await (run-ticks s 30))
          (is (= "cobblestone" (block-at s 0 62 0)) "the lava the dig laid open is sealed from the rim")
          (is (= [[0 63 0]] (dug-cells s)) "the sand is dug, from beside")
          (is (every? #(>= % 63) @ys) "the body never goes down to the lava")
          (is (not= "air" (block-at s 0 63 0)) "the hole over the lava is mended")
          (is (#{{:x 1 :y 64 :z 0} {:x -1 :y 64 :z 0} {:x 0 :y 64 :z 1} {:x 0 :y 64 :z -1}} (:target (first (moved s))))
              "it steps to the floor beside before the dig, so it does not drop onto the unseen cell below")
          (is (not (contains? (:default (:accept mine/args)) :under-feet))))))))

(deftest a-cell-under-the-feet-with-no-side-stand-is-a-refusal-not-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 1 :ways #{:seen} :settings {:jobs.gather.dig-for/max-failures 1}}
                                 {:floor [0 0 0 0] :blocks {"0,63,0" "sand" "0,62,0" "stone"}} 20))]
          (is (empty? (dug-cells s)) "a pillar top is never dug from on top")
          (is (= [{:x 0 :y 63 :z 0}] (mapv :pos (events-of s :mine.refused))))
          (is (not= :failures (:reason (done-event s))) "no dig failure is booked"))))))

(deftest the-tunnel-heading-arg-names-a-way-or-a-cell-to-run-toward
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args cells-at] [[{:heading "north"} [[0 -1] [0 -2]]]
                                 [{:heading :e :direction "north"} [[1 0] [2 0]]]
                                 [{:heading [-30 64 3]} [[-1 0] [-2 0]]]
                                 [{:heading [2 64 -30]} [[0 -1] [0 -2]]]]]
          (let [s (await (scenario (merge {:block "iron_ore" :level false :count 1 :tunnel-length 2} args)
                                   (assoc (rock-world {}) :yaw 90) 40))]
            (is (= (set (for [[x z] cells-at y [64 65]] [x y z])) (set (dug-cells s))) (pr-str args))))))))

(deftest a-tunnel-that-cannot-walk-on-says-what-blocked-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 10}
                                 (assoc (rock-world {}) :noPath ["2.5,64,0.5"]) 80))
              e (first (tunnel-ends s))]
          (is (= :step-failed (:reason e)))
          (is (= [1 64 0] (:at e)))
          (is (= {:status :stopped :reason :unreachable} (select-keys (:walk e) [:status :reason])))
          (is (= {:cell "air" :floor "stone"} (select-keys (:walk e) [:cell :floor]))))))))

;; ------------------------------------------------------------------ low health
(deftest a-badly-hurt-body-waits-to-heal-before-it-mines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 6} {:blocks (merge floor sand-patch) :self {:health 0.5}} 10))
              world (.-world (:p s))
              waits (events-of s :waiting)]
          (is (zero? (dig-count s)) "nothing dug at 0.5 hp")
          (is (= [:low-health] (distinct (map :reason waits))))
          (is (= 0.5 (:health (first waits))))
          (is (re-find #"heal" (str (:text (first waits)))))
          (swap! (.. world -state) assoc-in [:self :health] 20)
          (await (run-ticks s 60))
          (is (>= (get (inv s) "sand") 6) "healed, it mines"))))))

(deftest a-caller-with-fit-always-digs-on-at-low-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s]} (await (as-child {:block "sand" :count 6 :checks {:fit? '(always)}}
                                           {:blocks (merge floor sand-patch) :self {:health 0.5}} nil 10))]
          (is (pos? (dig-count s)) "low health is fit?'s, which the caller overrode"))))))

(deftest a-body-with-its-head-in-water-digs-nothing-and-says-flooded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [flooded {:blocks (merge floor sand-patch {"0,65,0" "water" "0,64,0" "water"}) :self {:inWater true}}
              s (await (scenario {:block "sand" :count 6} flooded 40))]
          (is (zero? (dig-count s)))
          (is (= :flooded (:reason (done-event s))))
          (is (= 0 (:got (done-event s))))
          (is (finished? s)))))))

(deftest water-pouring-in-after-a-dig-stops-the-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing (tu/fake-on-floor {:world nil :floor-block floor-block :blocks (merge floor sand-patch)}))
              world (.-world p)
              _ (.override world "dig"
                           (fn ^:async f [token a impl]
                             (let [r (await (impl token a))]
                               (let [[x y z] (map #(js/Math.floor %) (get-in @(.-state world) [:self :pos]))]
                                 (swap! (.. world -state) update :blocks merge {[x (inc y) z] "water" [x y z] "water"}))
                               (swap! (.. world -state) assoc-in [:self :inWater] true)
                               r)))
              s (start {:p p})]
          (core/submit! (:eng s) (spec {:block "sand" :count 6}) {})
          (await (run-ticks s 40))
          (is (= 1 (dig-count s)) "nothing is dug once the head is wet")
          (is (= :flooded (:reason (done-event s)))))))))

;; ------------------------------------------------------------------ walks that cannot end

;; Alder j607: ore 50 blocks down, walks ended :off-plan (and, since go-to refuses big drops, :needs-health): three of them ended get-kit no-source.
(deftest ore-a-walk-cannot-reach-is-skipped-without-a-failure-whatever-the-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [why [:off-plan :needs-health]]
          (let [stub {:round (fn [c] (ctx/end c :nothing-possible {:reason :unreachable :why why} {:arrived false}))}
                s (start {:world {:blocks (cells "sand" [15] [64] [-1 0 1 2])} :jobs (assoc registry/jobs 'jobs.movement.go-to stub)})]
            (core/submit! (:eng s) (spec {:block "sand" :count 4 :ways #{:seen}}) {})
            (await (run-ticks s 60))
            (is (= 4 (count (events-of s :mine.unreachable))) (str why))))))))

;; Juniper j754: the walk to an ore waited for day for ever (go-to at night); mine gave the round back for 12 minutes.
(deftest a-walk-that-keeps-waiting-is-skipped-after-the-stall-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [waiting {:round (fn ^:async w [_] :continue)}
              s (start {:world {:blocks {"15,64,0" "sand"}} :jobs (assoc registry/jobs 'jobs.movement.go-to waiting)})]
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :ways #{:seen}}) {})
          (await (run-ticks s 60))
          (is (= 1 (count (events-of s :mine.stalled))))
          (is (finished? s)))))))

;; Sorrel #23: a tunnel through stone wore the iron pickaxe out and nothing said so.
(deftest a-tunnel-through-stone-holds-the-stone-pickaxe-not-the-iron-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (rock-world {}) :inventory [{:name "iron_pickaxe" :count 1 :durability 250 :max 250}
                                                   {:name "stone_pickaxe" :count 1 :durability 131 :max 131}])
              s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 4} w 80))
              held (mapv #(.-item (.-args %)) (calls s "equip"))]
          (is (seq held))
          (is (every? #{"stone_pickaxe"} held) (pr-str held)))))))

(deftest a-pickaxe-that-breaks-in-the-tunnel-says-tool-broke
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (rock-world {}) :inventory [{:name "stone_pickaxe" :count 1 :durability 4 :max 131} {:name "torch" :count 10}])
              s (start {:world w})
              world (.-world (:p s))
              n (atom 0)]
          (.override world "dig"
                     (fn ^:async f [token a impl]
                       (let [r (await (impl token a))]
                         (when (= 3 (swap! n inc))
                           (swap! (.. world -state) assoc :inventory []))
                         r)))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :trip true}) {})
          (await (run-ticks s 120))
          (is (= 1 (count (events-of s :tool.broke))))
          (is (= 1 (count (events-of s :tool.none)))))))))

(deftest the-body-walks-out-of-its-own-zone-before-it-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [home-zone])
              s (await (zoned {:block "sand" :count 2 :ways #{:seen} :trip true} {:floor [-30 -30 40 30] :blocks {"6,64,11" "sand" "7,64,11" "sand"}} w 60))
              left (first (events-of s :mine.left-zone))
              first-dig (first (dug-cells s))]
          (is (= "home" (:zone left)))
          (is (= 2 (count (dug-cells s))))
          (is (every? (fn [[_ _ z]] (> z 9)) (dug-cells s)))
          (is (some? first-dig)))))))

(deftest a-stair-is-not-started-beside-a-building
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {"pad" pad-plan} {} [])
              s (await (zoned {:block "stone" :count 1 :ways #{:seen :descend-stair :descend-shaft} :direction "east" :trip true :torches false}
                              {:blocks (soil-over-stone 63 59) :drops {"stone" "cobblestone"} :inventory pickaxe} w 60))
              left (first (events-of s :mine.left-zone))]
          (is (= "pad" (:zone left)))
          (is (< (first (:to left)) -1) "it walked west, away from the pad at x 6"))))))

(deftest a-stair-is-not-started-beside-another-owners-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "stone" :count 1 :ways #{:seen :descend-stair :descend-shaft} :direction "east" :trip true :torches false}
                              {:blocks (soil-over-stone 63 59) :drops {"stone" "cobblestone"} :inventory pickaxe}
                              (ew/of-data {} {} [(assoc farm-zone :min [6 60 -2] :max [8 70 2])]) 60))]
          (is (= "farm" (:zone (first (events-of s :mine.left-zone))))))))))

(deftest after-leaving-a-building-the-descent-still-walks-down-its-own-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (stair-down-east 4) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe
                                :self {:pos [0.5 64 0.5]}}
                        :shared (ew/of-data {"hut" {:id "hut" :parts [{:id "p" :box [[-5 64 0] [-5 64 0]] :want "sand"}]}} {} [])})]
          (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors (vec (for [i (range 1 5)] [i (- 63 i) 0]))})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level 58 :heading "east" :count 1 :tunnel-length 3 :torches false :trip true}) {})
          (await (run-ticks s 400))
          (is (seq (events-of s :mine.left-zone)))
          (is (= [[4 60 0]] (map :own-stair (events-of s :mine.stair-plan)))))))))

(deftest a-plain-dig-is-not-moved-from-a-building
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {"pad" pad-plan} {} [])
              s (await (zoned {:block "sand" :count 1 :ways #{:seen} :trip true} {:blocks {"2,64,0" "sand"}} w 20))]
          (is (empty? (events-of s :mine.left-zone)))
          (is (= 1 (count (dug-cells s)))))))))

(deftest ignoring-zones-digs-from-inside-the-own-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [home-zone])
              s (await (zoned {:block "sand" :count 1 :ways #{:seen} :ignore-zones? true} {:blocks {"2,64,0" "sand"}} w 20))]
          (is (empty? (events-of s :mine.left-zone)))
          (is (= 1 (count (dug-cells s)))))))))

(deftest the-tunnel-stops-at-the-edge-of-an-own-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [{:name "home" :min [8 60 -3] :max [12 70 3] :owner "Fake"}])
              s (await (zoned {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 20}
                              (rock-world {}) w 120))]
          (is (every? #(< (first %) 8) (dug-cells s)) "no cell of the zone is cut")
          (is (= [:own-zone] (map :reason (tunnel-ends s)))))))))

(deftest a-fresh-stair-with-no-shaft-suggests-declaring-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario no-shaft-args deep-ore 400))
              e (done-event s)
              {:keys [at heading to-y] :as suggest} (:shaft-suggest e)]
          (is (= {:at [0 64 0] :heading :east} (select-keys suggest [:at :heading])))
          (is (< to-y 64) "the bottom of the stair")
          (is (re-find (re-pattern (str "declare it with ./bin/plans mineshaft <id> --at=0,64,0 --heading east --to-y " to-y)) (:text e)))
          (is (empty? (shape/plan-errors {:id "x" :kind :mineshaft :parts [{:id "stair" :stair {:from at :heading heading :to-y to-y} :want :clear}]} "x"))
              "the suggestion is a valid shaft"))))))

(deftest a-shaft-in-use-or-shafts-off-suggest-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [used (await (shafted via-shaft ore-east [] 400))
              off (await (scenario (assoc no-shaft-args :ways #{:seen :descend-stair :strip-tunnel}) deep-ore 400))]
          (is (nil? (:shaft-suggest (done-event used))))
          (is (nil? (:shaft-suggest (done-event off))))
          (is (empty? (events-of off :mine.no-shaft)) "false never looks a shaft up"))))))

(deftest shaft-required-with-none-usable-stops-before-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario (assoc no-shaft-args :ways #{:seen :descend-shaft :strip-tunnel}) deep-ore 400))
              e (done-event s)]
          (is (= :no-shaft (:reason e)))
          (is (= 0 (:got e)))
          (is (empty? (dug-cells s)) "nothing dug")
          (is (nil? (:shaft-suggest e)))
          (is (finished? s)))))))

(deftest shaft-required-uses-a-usable-shaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (shafted (assoc via-shaft :ways #{:seen :descend-shaft :strip-tunnel}) ore-east [] 400))]
          (is (= :count (:reason (done-event s)))))))))

(deftest stay-level-declines-on-the-surface-and-says-what-to-do
  (let [stay {:ways #{:seen :strip-tunnel}}]
    (is (= :on-surface (:reason (mine/surface-cause stay 15))))
    (is (re-find #"access.stair" (:text (mine/surface-cause stay 15))))
    (is (nil? (mine/surface-cause stay 12)) "a cave mouth or overhang keeps spilled sky light")
    (is (nil? (mine/surface-cause stay 0)) "sealed from the sky")
    (is (nil? (mine/surface-cause stay nil)) "no light data")
    (is (nil? (mine/surface-cause {} 15)) "only a run without a descending way declines")))

(deftest the-ways-set-decides-what-a-dig-may-use
  (is (every? #(% {:tunnel-length 4}) [way-set/seen? way-set/stair? way-set/shaft? way-set/tunnel?]) "no :ways: all four")
  (is (not (way-set/tunnel? {:ways #{:seen} :tunnel-length 4})))
  (is (not (way-set/tunnel? {:tunnel-length 0})) "a tunnel of no blocks is no way")
  (is (not (way-set/stay-level? {})))
  (is (way-set/stay-level? {:ways #{:seen :strip-tunnel}}))
  (is (not (way-set/stay-level? {:ways #{:seen :strip-tunnel :descend-shaft}}))))

(deftest ore-in-view-is-left-when-seen-is-not-a-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :ways #{:descend-stair}} (rock-world {"1,64,0" "iron_ore"}) 40))]
          (is (empty? (dug-cells s)))
          (is (= :none (:reason (done-event s)))))))))

(deftest the-end-text-names-why-a-run-with-no-tunnel-blocks-stopped
  (is (= "; stair stopped: undercuts-way; tunnel stopped: way-out"
         (mine-end/dug-text {:descent {:steps 0 :stop :undercuts-way} :tunnel {:steps 0 :stop :way-out}} :tunnel-stopped)))
  (is (= "; tunnel 2 blocks east at y 5; tunnel stopped: lava"
         (mine-end/dug-text {:tunnel {:steps 2 :heading "east" :origin {:y 5} :stop :lava}} :tunnel-stopped)))
  (is (= "; tunnel 2 blocks east at y 5"
         (mine-end/dug-text {:tunnel {:steps 2 :heading "east" :origin {:y 5} :stop :tunnel-length}} :tunnel-length))))

;; ------------------------------------------------------------------ the contract
(deftest the-job-exports-the-contract-and-quotes-validly
  (let [def (get registry/jobs 'jobs.gather.mine)
        c {:primitives (tu/fake-on-floor {:blocks sand-patch}) :args (second (expr/leaf registry/jobs 'jobs.gather.mine {:block "sand" :count 3}))}
        q ((:quote-of def) c)]
    (is (= [:item :block :heading :center] (:target def)))
    (is (fn? (:can-start? def)))
    (is (nil? (:check def)))
    (is (nil? (contract/quote-problem q)) (pr-str q))
    (is (= [0 3 3] (get-in q [:yields :items "sand"])))
    (is (fn? (:done? def)))))

(deftest a-pickaxe-that-breaks-in-the-dig-phase-with-none-to-fetch-ends-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (rock-world {}) :inventory [{:name "stone_pickaxe" :count 1 :durability 4 :max 131} {:name "torch" :count 10}])
              n (atom 0)
              {:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8} w nil 120
                                             {:before (fn [s]
                                                        (let [world (.-world (:p s))]
                                                          (.override world "dig"
                                                                     (fn ^:async f [token a impl]
                                                                       (let [r (await (impl token a))]
                                                                         (when (= 3 (swap! n inc))
                                                                           (swap! (.. world -state) assoc :inventory []))
                                                                         r)))))}))]
          (is (= [:condition-failed :no-tool] [(:kind out) (contract/cause-key out)]) (pr-str out)))))))

(deftest a-side-cut-far-from-the-submit-cell-still-takes-the-wall-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world (update (rock-world {"0,65,-1" "air" "0,65,-2" "air" "1,65,-2" "iron_ore"}) :inventory into [{:name "cobblestone" :count 5}])})]
          (mem/write! (:store (:eng s)) :tunnel-way {:dim "overworld" :origin [0 64 -1] :delta [0 -1] :steps 0})
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4 :ways #{:seen :strip-tunnel} :settings {:jobs.gather.mine-stay/ore-reach 2} :submitted-at [-6 64 0]}) {})
          (await (run-ticks s 500))
          (is (= 1 (get (inv s) "raw_iron")) (pr-str (keep :text @(:seen s)))))))))

(deftest a-stay-level-walk-to-ore-plans-no-way-over-the-ground
  (is (= 8 (:y-margin (mine/dig-walk-opts {:ways #{:seen :strip-tunnel}} false nil))) "a way round over a hill is no way for a run that stays level")
  (is (nil? (:y-margin (mine/dig-walk-opts {} false nil))))
  (is (= #{:walk :clear-seal} (:ways (mine/dig-walk-opts {:ways #{:seen :strip-tunnel}} true nil))))
  (is (= {:safe? '(not (threatened))} (:checks (mine/dig-walk-opts {} false {:safe? '(not (threatened))})))))

(deftest a-walk-that-gives-up-for-a-passing-reason-is-told-and-counted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stub {:round (fn [c] (ctx/end c :nothing-possible {:reason :unreachable :why :no-progress} {:arrived false}))}
              s (start {:world {:blocks (cells "sand" [15] [64] [-1 0 1 2])} :jobs (assoc registry/jobs 'jobs.movement.go-to stub)})]
          (core/submit! (:eng s) (spec {:block "sand" :count 4 :ways #{:seen}}) {})
          (await (run-ticks s 60))
          (is (= [:no-progress :no-progress :no-progress] (mapv :why (events-of s :mine.walk-failed))))
          (is (= :gave-up (:reason (done-event s)))))))))
