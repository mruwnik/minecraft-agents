(ns engine.mine-d-test
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
            [jobs.gather.mine.descent :as mine-descent]
            [jobs.gather.mine.end :as mine-end]
            [jobs.gather.mine.targets :as mine-targets]
            [jobs.gather.mine.torches :as mine-torches]
            [jobs.gather.mine.tunnel :as mine-tunnel]
            [jobs.gather.mine-stay :as stay]
            [jobs.access.stair :as stair]
            [plan.shape :as shape]
            [engine.mine-support :refer [spec as-child floor-block sight-opts seeing start run-ticks scenario scenario-lit start-lit events-of done-events done-event finished? inv job-mem calls moved dig-count dug-cells block-at cells floor wide-floor sand-patch set-pos! bare-pickaxe pickaxe cobble stone-floor stone-hill ground-cell? dug-ground farm-zone pad-plan two-in-two-out zoned their-claim rock rock-world feet tunnel-ends open-line east start-x corridor bridge-run no-floor-end? deep-rock far-cave side-cave facing-west-run stay-run side-ore-run shutdown-at-cell! run-until! below-ore-run run-until shaft-plan shafted via-shaft ore-east drop-away! hide-drop-until-beside! walled-ore watched dark-ore-dug long-rock torch-world torch-xs torches8 side-room side-torch-cells branch-torches soil-over-stone soil-all-round descent-run stair-down-east classify-in walk-options spy-go-to home-zone no-shaft-args deep-ore]]))

(deftest a-strip-tunnel-looks-round-between-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east"}
                                 (rock-world {"3,64,1" "iron_ore"}) 80))]
          (is (seq (watched s)) "the tunnel watched, lit or not: torches do not stop a creeper walking up it"))))))

(deftest a-vein-dig-in-a-lit-place-does-not-look-round-and-in-the-dark-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (await (scenario {:block "iron_ore" :level false :count 1 :ways #{:seen}} (rock-world {"1,64,0" "iron_ore"}) 20))
              p (tu/fake-on-floor (assoc (rock-world {"1,64,0" "iron_ore"}) :floor-block floor-block))
              _ (swap! (fake/state p) assoc :light-default [0 0])
              dark (start {:p p})]
          (core/submit! (:eng dark) (spec {:block "iron_ore" :level false :count 1 :ways #{:seen}}) {})
          (await (run-ticks dark 20))
          (is (= [[1 64 0]] (dug-cells lit)))
          (is (empty? (watched lit)))
          (is (= [[1 64 0]] (dug-cells dark)))
          (is (seq (watched dark))))))))

(deftest the-cheapest-pickaxe-that-harvests-is-held-and-wear-is-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "stone"}
                                :inventory [{:name "iron_pickaxe" :count 1 :durability 200 :maxDurability 250}
                                            {:name "stone_pickaxe" :count 1 :durability 100 :maxDurability 131}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 1}) {})
          (await (run-ticks s 10))
          (is (= "stone_pickaxe" (.-held (.self (:p s))))))))))

(deftest ore-across-a-wide-trench-in-view-is-dug-from-the-cut-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [open (cells "air" (range 1 4) [64 65] [0])
              trench (cells "air" [4 5] (range 62 66) [0])
              s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8}
                                 (rock-world (merge open trench {"6,64,0" "iron_ore"})) 60))]
          (is (some #{[6 64 0]} (dug-cells s)) (pr-str (dug-cells s))))))))

(deftest a-torch-in-either-hand-lets-a-dark-ore-six-away-be-seen-and-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (some #{[6 64 0]} (await (dark-ore-dug {:offhand "torch"}))) "off hand")
        (is (some #{[6 64 0]} (await (dark-ore-dug {:held "torch"}))) "main hand")
        (is (not-any? #{[6 64 0]} (await (dark-ore-dug {}))) "no torch")))))

(deftest the-tunnel-hangs-a-torch-where-the-next-cell-would-stay-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 13}
                                 (torch-world torches8) 200 [0 0]))]
          (is (= [0 11] (torch-xs s)) "the entry, then where the next cell lies past its light (the torch at the entry reaches 12 cells on)")
          (is (= 6 (get (inv s) "torch")))
          (is (empty? (events-of s :mine.no-torches)))
          (is (= :tunnel-length (:reason (done-event s)))))))))

(deftest a-torch-that-could-not-be-placed-lights-nothing-so-the-next-step-asks-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake-on-floor (assoc (torch-world torches8) :floor-block floor-block))
              _ (swap! (fake/state p) assoc :light-default [0 0])
              _ (.override (.-world p) "place" (fn ^:async f [tok a impl] (if (= "torch" (.-item a)) #js {:status "failed" :reason "no"} (await (impl tok a)))))
              s (start {:p p})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 13}) {})
          (await (run-ticks s 200))
          (is (> (count (filter #(= :place-failed (:reason %)) (events-of s :mine.torch-left-out))) 3)
              "a failed torch is no light source: later steps are still owed torches"))))))

(deftest a-lit-cell-leaves-the-torch-out-once-and-the-tunnel-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake-on-floor (assoc (torch-world [{:name "iron_pickaxe" :count 1}]) :floor-block floor-block))
              lit (into {} (for [x (range 1 41) y [64 65] z [-1 0 1]] [[x y z] [0 8]]))
              _ (swap! (fake/state p) assoc :light-default [0 0] :light lit)
              s (start {:p p})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 13}) {})
          (await (run-ticks s 200))
          (is (every? #(= :lit (:reason %)) (events-of s :mine.torch-left-out)))
          (let [cells (map :cell (events-of s :mine.torch-left-out))]
            (is (= (count cells) (count (set cells))) "each owed site is left out once"))
          (is (some #(>= (first %) 10) (dug-cells s)) "the tunnel is dug on past the lit cells"))))))

;; the glance at the support before a torch is for support not yet seen: all is seen here, so hanging adds no looks
(deftest a-torch-on-seen-support-is-hung-without-a-glance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn [inv] (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 13}
                                      (torch-world inv) 200 [0 0]))
              lit (await (run torches8))
              dark (await (run pickaxe))]
          (is (= [0 11] (torch-xs lit)))
          (is (= (count (calls dark "look")) (count (calls lit "look")))))))))

(deftest torches-held-in-the-off-hand-are-hung-and-none-crafted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 3} {:name "stick" :count 3}])
                           :equipment {"offHand" {:name "torch" :count 8}})
              s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 13} world 200 [0 0]))]
          (is (= [0 11] (torch-xs s)) "hung from the off-hand stack")
          (is (empty? (events-of s :mine.no-torches)))
          (is (= 3 (get (inv s) "coal")) "nothing crafted"))))))

(deftest torches-false-hangs-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 9 :torches false}
                                 (torch-world torches8) 200 [0 0]))]
          (is (= [] (torch-xs s)))
          (is (empty? (events-of s :mine.no-torches))))))))

(deftest coal-and-sticks-are-crafted-into-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 15}
                                 (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "stick" :count 1}]) 300 [0 0]))]
          (is (= [0 11] (torch-xs s)))
          (is (= 2 (get (inv s) "torch")) "4 crafted, 2 hung")
          (is (nil? (get (inv s) "coal")))
          (is (empty? (events-of s :mine.no-torches))))))))

(deftest coal-and-a-log-are-crafted-into-sticks-and-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 15}
                                 (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "oak_log" :count 1}]) 300 [0 0]))]
          (is (= [0 11] (torch-xs s)) "the first one hung")
          (is (pos? (get (inv s) "torch" 0)))
          (is (empty? (events-of s :mine.no-torches))))))))

(deftest fetch-false-with-coal-and-a-log-but-no-stick-leaves-the-tunnel-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :ways #{:carried}}
                                 (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "oak_log" :count 1}]) 300 [0 0]))]
          (is (= [] (torch-xs s)))
          (is (= :no-torches (:reason (done-event s)))))))))

(deftest no-torches-and-no-materials-ends-no-torches-with-a-message
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 13}
                                 (torch-world bare-pickaxe) 200 [0 0]))]
          (is (= [] (torch-xs s)))
          (is (= [:no-torches 1] [(:reason (done-event s)) (:steps (:tunnel (done-event s)))]) "the tunnel stopped at its first torch")
          (is (re-find #"no torches left for the tunnel.*jobs.gather.mine" (:text (done-event s)))))))))

(deftest no-torches-in-a-lit-tunnel-digs-on-without-hanging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-lit {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 13}
                                 (torch-world bare-pickaxe) 200 [0 8]))]
          (is (= [] (torch-xs s)))
          (is (= :tunnel-length (:reason (done-event s)))))))))

(deftest a-missing-pickaxe-is-fetched-then-the-stone-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1 :trip true} {:blocks {"3,64,0" "stone"} :drops cobble} "stone_pickaxe" 40))]
          (is (= ["stone"] (mapv :block fetch)))
          (is (= [:done :count] [(:kind out) (:reason (:data out))]))
          (is (= 1 (get (inv s) "cobblestone"))))))))

(deftest the-tunnel-starts-where-the-body-stood-before-the-pickaxe-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [aisle (-> rock (dissoc "1,64,0" "2,64,0" "1,65,0" "2,65,0"))
              {:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "west" :tunnel-length 3 :trip true}
                                           {:blocks aisle :drops {"iron_ore" "raw_iron" "stone" "cobblestone"}}
                                           "stone_pickaxe" 120 {:move-to [2.5 64 0.5]}))]
          (is (= [0 64 0] (:origin (:tunnel (:data out)))) "the origin is the cell the job began on, not the fetch spot"))))))

(deftest a-pickaxe-that-cannot-be-fetched-ends-stopped-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1 :trip true} {:blocks {"3,64,0" "stone"} :drops cobble} nil 20))]
          (is (= 1 (count fetch)))
          (is (= [:condition-failed :no-tool "pickaxe" 0] [(:kind out) (contract/cause-key out) (:tool (:cause out)) (:got (:data out))]) (pr-str out))
          (is (zero? (dig-count s))))))))

(deftest a-failed-pickaxe-fetch-is-the-because-of-the-no-tool-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (as-child {:block "stone" :count 1 :trip true} {:blocks {"3,64,0" "stone"} :drops cobble} nil 20))]
          (is (= :no-tool (contract/cause-key out)))
          (is (= :no-source (contract/cause-key (:because out))) (pr-str out)))))))

(deftest a-bad-direction-is-refused-at-submit
  (is (re-find #"jobs.gather.mine :direction must be"
               (expr/problem registry/jobs '(jobs.gather.mine {:block "stone" :count 1 :direction "up"}))))
  (is (nil? (expr/problem registry/jobs '(jobs.gather.mine {:block "stone" :count 1 :direction :w})))))

(deftest dig-for-ends-no-tool-without-asking-get-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1} {:blocks {"3,64,0" "stone"} :drops cobble} "stone_pickaxe" 20))]
          (is (empty? fetch))
          (is (= [:condition-failed :no-tool] [(:kind out) (contract/cause-key out)]))
          (is (zero? (dig-count s))))))))

(deftest soil-in-the-tunnel-is-not-dug-with-the-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (merge (cells "dirt" (range 0 6) [63] [0]) (cells "dirt" (range 1 6) [64 65] [0]))
                                :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
          (core/submit! (:eng s) (spec {:block "stone" :count 1 :direction "east" :tunnel-length 3 :ways #{:seen :strip-tunnel}}) {})
          (await (run-ticks s 60))
          (is (pos? (dig-count s)) "it did dig the soil")
          (is (nil? (.-held (.self (:p s)))) "by hand")
          (is (= 1 (count (calls s "unequip")))))))))

(deftest stone-under-soil-is-reached-by-a-stair-down-then-mined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 59) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :trip true}) {})
          (await (run-ticks s 200))
          (is (<= 2 (get (inv s) "cobblestone" 0)) "the ore is kept")
          (is (= :count (:reason (done-event s))))
          (let [names (mapv #(.-name %) (.-calls (.-world (:p s))))
                soil-digs (filter #(< 59 (nth % 1)) (dug-cells s))]
            (is (seq soil-digs) "it dug soil")
            (is (< -1 (.indexOf names "unequip") (.indexOf names "equip")) "the hand was emptied before the pickaxe came out")))))))

(deftest stone-far-off-while-standing-on-soil-is-dug-down-to-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (merge (soil-over-stone 63 59) {"11,64,0" "stone"}) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :trip true}) {})
          (await (run-ticks s 200))
          (is (<= 2 (get (inv s) "cobblestone" 0)))
          (is (> 4 (first (first (dug-cells s)))) "it dug down where it stood before any walk to the far block")
          (is (< (nth (first (dug-cells s)) 1) 64)))))))

;; Sorrel j23: the crafting table the kit just placed stood in the first stair step of the faced heading.
(deftest a-table-in-the-first-step-of-the-faced-heading-turns-the-stair-to-another
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (merge soil-all-round {"-1,64,0" "crafting_table"}) :drops cobble :yaw 90
                                :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :tunnel-length 4}) {})
          (await (run-ticks s 300))
          (is (finished? s))
          (is (seq (events-of s :mine.turned)) "it turned")
          (is (= "crafting_table" (block-at s -1 64 0)) "the table stands")
          (is (<= 2 (get (inv s) "cobblestone" 0)))
          (is (= :count (:reason (done-event s)))))))))

(deftest a-table-on-every-side-ends-no-stone-found-after-trying-each-heading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tables {"-1,64,0" "crafting_table" "1,64,0" "crafting_table" "0,64,1" "crafting_table" "0,64,-1" "crafting_table"}
              s (start {:world {:blocks (merge (soil-over-stone 63 59) tables) :drops cobble :yaw 90
                                :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :tunnel-length 4}) {})
          (await (run-ticks s 300))
          (is (finished? s))
          (is (= 3 (count (events-of s :mine.turned))))
          (is (= :no-stone-found (:reason (done-event s)))))))))

(deftest a-given-heading-is-not-turned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (merge (soil-over-stone 63 59) {"-1,64,0" "crafting_table"}) :drops cobble
                                :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :heading "west" :tunnel-length 4}) {})
          (await (run-ticks s 300))
          (is (empty? (events-of s :mine.turned)))
          (is (= :no-stone-found (:reason (done-event s)))))))))

(deftest a-given-heading-is-turned-when-water-fills-its-first-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (merge (soil-over-stone 63 59) {"-1,64,0" "water"}) :drops cobble
                                :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :heading "west" :tunnel-length 4}) {})
          (await (run-ticks s 300))
          (is (seq (events-of s :mine.turned)))
          (is (<= 2 (get (inv s) "cobblestone" 0))))))))

;; Alder j48: between two stair steps something moved the body off the stair line; the stair stopped :off-stair and mine ended with no stone.
(deftest a-stair-that-finds-the-body-off-its-line-goes-on-along-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 59) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})
              world (.-world (:p s))
              moved (atom false)]
          (.override world "dig"
                     (fn ^:async f [token a impl]
                       (let [r (await (impl token a))]
                         (when-not @moved
                           (reset! moved true)
                           (swap! (.. world -state) assoc-in [:self :pos] [0.5 64 2.5]))
                         r)))
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :direction "east" :tunnel-length 4}) {})
          (await (run-ticks s 300))
          (is (empty? (events-of s :mine.restair)) "the stair walks back onto its line itself")
          (is (<= 2 (get (inv s) "cobblestone" 0)))
          (is (= :count (:reason (done-event s)))))))))

;; Rowan j119: mine dug the step-up cells and floors round its pit until the body stood on a one-cell column.
(deftest mine-leaves-the-block-that-is-the-bodys-last-way-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"1,63,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe}})]
          (doseq [c [[-1 63 0] [0 63 1] [0 63 -1]]] (fake/set-block! (:p s) c "air"))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :ways #{:seen}}) {})
          (await (run-ticks s 40))
          (is (= "iron_ore" (block-at s 1 63 0)) "the only floor to step onto is left")
          (is (seq (events-of s :mine.spared-exit))))))))

(deftest soil-deeper-than-the-descent-limit-ends-no-stone-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 40) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :descend-limit 3}) {})
          (await (run-ticks s 200))
          (is (finished? s))
          (is (= :no-stone-found (:reason (done-event s))))
          (is (re-find #"dug down 3 blocks through soil, found no stone" (:text (done-event s))))
          (is (zero? (get (inv s) "cobblestone" 0))))))))

(deftest a-stair-that-meets-an-own-zone-ends-own-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [{:name "home" :min [8 40 -3] :max [12 70 3] :owner "Fake"}])
              s (await (zoned {:block "stone" :count 2 :direction "east" :tunnel-length 4}
                              {:blocks (soil-over-stone 63 40) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]} w 200))]
          (is (= :own-zone (:reason (done-event s))))
          (is (= ["home"] (mapv :zone (events-of s :mine.own-zone))) "the warn names the zone")
          (is (every? #(< (first %) 8) (dug-cells s))))))))

(deftest mine-spares-cells-of-the-bodys-own-plan-unless-told-not-to-or-it-is-a-mineshaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[kind extra n] [[nil {} 0] [nil {:settings {:jobs.lib.access/spare-own-builds false}} 1] [:mineshaft {} 1]]]
          (let [own (ew/of-data {"pad" (cond-> (shape/with-author pad-plan "Fake") kind (assoc :kind kind))} {} [])
                s (await (zoned (merge {:block "sand" :count 1 :ways #{:seen}} extra) {:blocks {"6,64,0" "sand"}} own 20))]
            (is (= n (count (dug-cells s))) (pr-str kind extra))))))))

(deftest the-mend-leaves-the-stair-cells-below-the-start-level
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s]} (await (descent-run 300))
              stair-cells (filterv (fn [[x y z]] (and (< y 63) (nil? (some #{(block-at s x y z)} ["stone" "dirt"]))))
                                   (for [x (range -3 13) y (range 55 63) z (range -3 4)] [x y z]))]
          (is (seq (dug-cells s)))
          (is (seq stair-cells) "the stair cells below the start level are still open")
          (is (every? #(#{"air" nil} (block-at s (first %) (second %) (nth % 2))) stair-cells)))))))

;; Linden 158: the descent to the tunnel level cut a stair of its own from the lawn beside the one the body had cut earlier.
(deftest the-descent-walks-down-the-stair-it-cut-earlier-and-goes-on-from-its-foot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (stair-down-east 4) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe
                                :self {:pos [0.5 64 0.5]}}})]
          (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors (vec (for [i (range 1 5)] [i (- 63 i) 0]))})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level 58 :heading "east" :count 1 :tunnel-length 3 :torches false}) {})
          (await (run-ticks s 300))
          (is (every? (fn [[x]] (> x 4)) (dug-cells s)) "nothing is cut from the lawn or along the old stair")
          (is (seq (dug-cells s)) "the new steps go on past its foot")
          (is (<= (apply min (map second (dug-cells s))) 58) "it digs down to the level")
          (is (= [[4 60 0]] (map :own-stair (events-of s :mine.stair-plan))) "the plan is stated once at the start"))))))

(deftest a-given-heading-is-kept-when-the-descent-reuses-a-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (stair-down-east 4) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe
                                :self {:pos [0.5 64 0.5]}}})]
          (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors (vec (for [i (range 1 5)] [i (- 63 i) 0]))})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level 58 :heading "north" :count 1 :tunnel-length 3 :torches false}) {})
          (await (run-ticks s 300))
          (is (some (fn [[_ _ z]] (neg? z)) (dug-cells s)) "the new steps turn north from the old foot"))))))

(deftest a-run-goes-on-from-the-foot-of-its-old-stair
  ;; the walk down the plugged stair is planned over the world as it is (go-to :unknown false by the job-args overlay): the
  ;; case is the new steps' start, not the unseen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (assoc (stair-down-east 4) "2,62,0" "stone" "2,63,0" "stone" "2,64,0" "stone") :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe
                                :self {:pos [0.5 64 0.5]}}})]
          (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors (vec (for [i (range 1 5)] [i (- 63 i) 0]))})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level 58 :heading "east" :count 1 :tunnel-length 3 :torches false}) {})
          (await (settings/with-settings {:engine.settings/job-args {'jobs.movement.go-to {:unknown false}}}
                   #(run-ticks s 300)))
          (is (= 1 (count (events-of s :mine.stair-plan))))
          (is (= [5 61 0] (first (dug-cells s))) "the new steps go on from the foot of the old stair"))))))

(deftest the-stair-it-cuts-gets-a-torch-at-its-mouth-whatever-the-light-now
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 56) :drops cobble :inventory pickaxe}})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :level 58 :heading "east" :count 1 :tunnel-length 3}) {})
          (await (run-ticks s 200))
          (is (seq (for [x (range 0 3) y (range 62 67) z [-1 0 1] :when (#{"torch" "wall_torch"} (block-at s x y z))] [x y z]))
              "a torch stands within two blocks of the mouth"))))))

(deftest a-mouth-torch-round-that-only-crafts-still-hangs-the-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 56) :drops cobble
                                :inventory (into bare-pickaxe [{:name "torch" :count 1} {:name "coal" :count 2} {:name "stick" :count 2}])}})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :level 58 :heading "east" :count 1 :tunnel-length 3}) {})
          (await (run-ticks s 200))
          (is (seq (for [x (range 0 3) y (range 62 67) z [-1 0 1] :when (#{"torch" "wall_torch"} (block-at s x y z))] [x y z]))
              "the round that crafted torches did not use up the mouth torch"))))))

;; Juniper j108: a tunnel run from the foot of a stair back under it would cut its lowest steps' floors.
(deftest a-tunnel-under-the-stair-it-came-down-stops-before-it-cuts-the-way-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (stair-down-east 4) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe
                                :self {:pos [4.5 60 0.5]}}})]
          (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors (vec (for [i (range 1 5)] [i (- 63 i) 0]))})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :heading "west" :tunnel-length 6 :torches false}) {})
          (await (run-ticks s 200))
          (is (finished? s))
          (is (= "stone" (block-at s 3 60 0)) "the floor of the lowest step is not cut")
          (is (= [:undercuts-way] (mapv :reason (tunnel-ends s))) "the tunnel ends saying why")
          (is (= [4 60 0] (feet s))))))))

(deftest lava-behind-the-block-is-unseen-so-no-hazard
  (is (= :ok (classify-in {"0,63,3" "lava"})) "lava behind the block is not known")
  (is (= :no (classify-in {"0,65,3" "lava"})) "lava on top shows as drips: known")
  (is (= :no (classify-in {"0,64,2" "lava"})) "lava in the open face, seen")
  (is (= :buried (classify-in {"1,64,3" "stone" "-1,64,3" "stone" "0,65,3" "stone" "0,63,3" "stone" "0,64,4" "stone" "0,64,2" "stone"}))))
