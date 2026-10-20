(ns engine.mine-b-test
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
            [engine.mine-support :refer [spec as-child floor-block sight-opts seeing start run-ticks scenario events-of done-events done-event finished? inv job-mem calls moved dig-count dug-cells block-at cells floor wide-floor sand-patch set-pos! bare-pickaxe pickaxe cobble stone-floor stone-hill ground-cell? dug-ground farm-zone pad-plan two-in-two-out zoned their-claim rock rock-world feet tunnel-ends open-line east start-x corridor bridge-run no-floor-end? deep-rock far-cave side-cave facing-west-run stay-run side-ore-run shutdown-at-cell! run-until! below-ore-run run-until shaft-plan shafted via-shaft ore-east drop-away! hide-drop-until-beside! walled-ore watched dark-ore-dug long-rock torch-world torch-xs torches8 side-room side-torch-cells branch-torches soil-over-stone soil-all-round descent-run stair-down-east classify-in walk-options spy-go-to home-zone no-shaft-args deep-ore]]))

(deftest no-tool-never-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 2} {:blocks (cells "stone" [3 4] [64] [0]) :inventory [{:name "iron_shovel" :count 1}]} 10))]
          (is (zero? (dig-count s)))
          (is (= "pickaxe" (:tool (done-event s))))
          (is (= :no-tool (:reason (done-event s))))
          (is (nil? (:ground (job-mem s))) "nothing was written")
          (is (finished? s)))))))

(deftest a-last-pickaxe-near-breaking-ends-no-tool-instead-of-breaking-it-deep-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [left [1 3]]
          (let [{:keys [s out]} (await (as-child {:block "stone" :count 2} {:blocks (cells "stone" [3 4] [64] [0]) :inventory [{:name "stone_pickaxe" :count 1 :durability left :max 131}]} nil 10))]
            (is (zero? (dig-count s)))
            (is (= [:condition-failed :no-tool] [(:kind out) (contract/cause-key out)]) (pr-str out))))))))

(deftest a-pickaxe-with-uses-to-spare-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 2} {:blocks (cells "stone" [3 4] [64] [0]) :inventory [{:name "stone_pickaxe" :count 1 :durability 4 :max 131}]} 10))]
          (is (pos? (dig-count s))))))))

(deftest a-block-any-hand-harvests-needs-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "oak_sapling" :count 2} {:blocks (cells "oak_sapling" [3 4] [64] [0]) :inventory [{:name "iron_shovel" :count 1}]} 40))]
          (is (= 2 (dig-count s)))
          (is (= :count (:reason (done-event s)))))))))

(deftest sand-without-a-shovel-is-still-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 2} {:blocks (cells "sand" [3 4] [64] [0])} 40))]
          (is (= 2 (dig-count s)))
          (is (= :count (:reason (done-event s)))))))))

(deftest cells-in-a-zone-are-left-and-the-rest-are-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "sand" :count 4 :ways #{:seen}} {:blocks two-in-two-out}
                              (ew/of-data {} {} [farm-zone]) 40))]
          (is (= #{[6 64 0] [6 64 1]} (set (dug-cells s))))
          (is (= :refused (:reason (done-event s))))
          (is (= 2 (:got (done-event s))))
          (is (= [{:reason :refused :zones ["farm"] :plans []}]
                 (map #(select-keys % [:reason :zones :plans]) (events-of s :mine.declined)))))))))

(deftest a-zone-that-allows-digging-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "sand" :count 4} {:blocks two-in-two-out}
                              (ew/of-data {} {} [(assoc farm-zone :allow #{:dig})]) 40))]
          (is (= 4 (count (dug-cells s))))
          (is (= :count (:reason (done-event s)))))))))

(deftest only-refused-targets-decline-naming-the-zone-or-plan-and-stay-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[w blocks expected] [[(ew/of-data {} {} [farm-zone]) {"3,64,0" "sand"} {:zones ["farm"] :plans []}]
                                     [(ew/of-data {"pad" pad-plan} {} []) {"6,64,0" "sand"} {:zones [] :plans ["pad"]}]
                                     [(ew/of-data {"pad" (shape/with-author pad-plan "Fake")} {} []) {"6,64,0" "sand"} {:zones [] :plans ["pad"]}]]]
          (let [s (await (zoned {:block "sand" :ways #{:seen}} {:blocks blocks :yaw 270} w 5))]
            (is (zero? (count (.-calls (.-world (:p s))))))
            (is (not (finished? s)))
            (is (= (assoc expected :reason :refused)
                   (select-keys (core/waiting (:eng s) "j1") [:reason :zones :plans])))))))))

(deftest a-call-with-no-block-and-no-zone-list-ends-no-block-with-no-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {} {:blocks {"6,64,0" "sand"}} (ew/of-data {} {} nil) 3))]
          (is (empty? (events-of s :mine.declined)))
          (is (= :no-target (:reason (first (events-of s :stopped)))))
          (is (empty? (calls s "dig"))))))))

(deftest no-zone-list-declines-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "sand"} {:blocks {"6,64,0" "sand"}} (ew/of-data {} {} nil) 5))]
          (is (empty? (calls s "dig")))
          (is (not (finished? s)))
          (is (= :no-zones (:reason (core/waiting (:eng s) "j1")))))))))

(deftest a-zone-added-between-the-choice-and-the-dig-stops-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [])
              p (tu/fake-on-floor {:blocks {"6,64,0" "sand"}})
              _ (.override (.-world p) "steer"
                           (fn [token args impl] (ew/set-zones! w [(assoc farm-zone :min [5 60 -2] :max [7 70 2])])
                             (impl token args)))
              s (start {:p p :shared w})]
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :ways #{:seen}}) {})
          (await (run-ticks s 20))
          (is (= 1 (count (moved s))) "one walk, then the zone refuses the dig")
          (is (empty? (calls s "dig")))
          (is (= :refused (:reason (done-event s)))))))))

(deftest a-claim-added-between-the-choice-and-the-dig-is-named-in-the-refusal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [])
              p (tu/fake-on-floor {:blocks {"6,64,0" "sand"}})
              _ (.override (.-world p) "steer"
                           (fn [token args impl] (ew/set-area-claims! w [their-claim])
                             (impl token args)))
              s (start {:p p :shared w})]
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :ways #{:seen}}) {})
          (await (run-ticks s 20))
          (is (empty? (calls s "dig")))
          (is (= [{:reason :claim :claim "c9" :owner "Miles"}]
                 (map #(select-keys % [:reason :claim :owner]) (events-of s :mine.refused)))))))))

(deftest hazards-not-accepted-are-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"3,64,0" "sand" "6,64,0" "sand" "6,64,1" "water"}}
              s (await (scenario {:block "sand" :count 2 :wet true :accept #{} :ways #{:seen}} world 30))]
          (is (= [[3 64 0]] (dug-cells s)))
          (is (= #{:fluid-adjacent :falling-block} (:default (:accept mine/args)))))))))

(deftest ore-the-body-has-not-seen-is-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; buried ore one block past the pocket's wall, and exposed ore in a sealed cave: neither can be seen
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :ways #{:seen}}
                                 (rock-world {"2,64,0" "iron_ore" "-4,64,0" "iron_ore" "-4,65,0" "air"}) 20))]
          (is (empty? (calls s "dig")))
          (is (= :none (:reason (done-event s))))
          (is (pos? (count (calls s "look"))) "it looked around first"))))))

(deftest seen-exposed-ore-is-dug-and-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :ways #{:seen}} (rock-world {"1,64,0" "iron_ore"}) 20))]
          (is (= [[1 64 0]] (dug-cells s)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s)))))))))

(deftest a-drop-seen-but-never-taken-does-not-hold-the-mine
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :ways #{:seen}} (rock-world {"1,64,0" "iron_ore"}) 0))]
          (.override (.-world (:p s)) "collect" (fn [_ _ _] #js {:status "unreachable" :reason "no-path" :gained #js []}))
          (await (run-ticks s 80))
          (is (nil? (get (inv s) "raw_iron")))
          (is (finished? s) "the job ends, the pickup is not asked for ever"))))))

(deftest the-tunnel-exposes-ore-which-is-then-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :trip true}
                                 (rock-world {"3,64,1" "iron_ore"}) 80))
              cut (remove #{[3 64 1]} (dug-cells s))]
          (is (= "air" (block-at s 3 64 1)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s))))
          (is (= [[1 65 0] [1 64 0] [2 65 0] [2 64 0] [3 65 0] [3 64 0]] cut) "a 1x2 run east, no further")
          (is (= {:heading "east" :steps 3} (select-keys (:tunnel (done-event s)) [:heading :steps])))
          (is (= [3 64 0] (feet s)) "stays where the tunnel ended")
          (is (= [3 64 0] (:end (:tunnel (done-event s)))) "where the tunnel ended: the side ore pocket is picked from beside it"))))))

(deftest a-short-run-keeps-the-tunnel-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 2 :direction "east" :tunnel-length 3 :trip true}
                                 (rock-world {"3,64,1" "iron_ore"}) 80))
              text (:text (first (events-of s :stopped)))]
          (is (re-find #"^mine got 1 of 2" text))
          (is (re-find #"tunnel 3 blocks" text)))))))

(deftest the-tunnel-never-digs-a-chest-in-its-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 4}
                                 (rock-world {"2,64,0" "chest"}) 80))]
          (is (= "chest" (block-at s 2 64 0)))
          (is (not-any? #{[2 64 0]} (dug-cells s))))))))

(deftest open-start-runs-over-open-cells-with-a-floor
  (is (= 5 (start-x (open-line (range 0 6) (range 0 6))))))

(deftest open-start-skips-only-cells-the-body-cut
  (let [line (open-line (range 0 6) (range 0 6))
        own-to #(fn [{:keys [x]}] (<= x %))]
    (is (= 3 (:x (mine-tunnel/open-start line {:x 0 :y 64 :z 0} east (own-to 3)))) "the open cells beyond its cut are a cave or another's room")
    (is (= 0 (:x (mine-tunnel/open-start line {:x 0 :y 64 :z 0} east (constantly false)))))))

(deftest open-start-stops-at-an-unseen-cell
  (is (= 2 (start-x (open-line (concat (range 0 3) [4]) (range 0 6))))))

(deftest open-start-stops-at-a-cell-with-no-floor
  (is (= 2 (start-x (open-line (range 0 6) [0 1 2 4 5])))))

(deftest open-start-stops-at-lava-and-water
  (doseq [fluid ["lava" "water"]]
    (is (= 2 (start-x (open-line (range 0 6) (range 0 6) {[3 64 0] fluid}))) fluid)))

(deftest open-start-stops-at-the-skip-cap
  (let [n (mine-tunnel/max-open-skip)]
    (is (= n (start-x (open-line (range 0 (+ n 10)) (range 0 (+ n 10))))))))

(deftest open-start-steps-down-a-stair-but-not-past-the-drop-bound
  (let [descending (fn [{:keys [x y z]}]
                     (when (zero? z)
                       (let [floor (- 63 x)]
                         (cond (= y floor) "stone" (> y floor) "air"))))
        drop (- 64 (:y (mine-tunnel/open-start descending {:x 0 :y 64 :z 0} east any?)))]
    (is (pos? drop) "a stair still counts")
    (is (= (mine-tunnel/open-skip-drop) drop) "the origin sits the drop bound below the start")))

(deftest the-tunnel-skips-its-own-open-corridor-and-never-digs-its-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world (rock-world (assoc corridor "3,65,0" "wall_torch"))})]
          (mem/write! (:store (:eng s)) :tunnel-way {:dim "overworld" :origin [0 64 0] :delta [1 0] :steps 5})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 2}) {})
          (await (run-ticks s 80))
          (is (= [[6 65 0] [6 64 0] [7 65 0] [7 64 0]] (dug-cells s)) "new rock only, no torch")
          (is (= 2 (:length (first (tunnel-ends s))))))))))

(deftest the-tunnel-steps-down-a-stair-to-its-level
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world (rock-world {"1,64,0" "air" "1,65,0" "air" "1,66,0" "air" "1,63,0" "air"})})]
          (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors [[1 62 0]]})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 1}) {})
          (await (run-ticks s 80))
          (is (= [[2 63 0] [2 64 0]] (sort-by second (dug-cells s))) "cut at the lower level")
          (is (empty? (filter #(= :no-floor (:reason %)) (tunnel-ends s)))))))))

(deftest the-tunnel-bridges-a-missing-floor-when-it-carries-a-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (bridge-run (conj pickaxe {:name "cobblestone" :count 5})))]
          (is (not (no-floor-end? s)) (pr-str (map :text (tunnel-ends s))))
          (is (= "cobblestone" (block-at s 1 63 0))))))))

(deftest the-tunnel-stops-at-a-missing-floor-when-it-carries-no-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (bridge-run pickaxe))]
          (is (no-floor-end? s) (pr-str (map :text (tunnel-ends s))))
          (is (= "air" (block-at s 1 63 0))))))))

(deftest the-tunnel-stops-before-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; lava seen over the cell to cut (through the open cells over the start): known before the cut
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 10}
                                 (rock-world {"0,66,0" "air" "1,66,0" "air" "2,66,0" "lava"}) 80))]
          (is (every? #(< (first %) 2) (dug-cells s)) "no cut beside the lava the body knew of")
          (is (= :tunnel-stopped (:reason (done-event s))))
          (is (= [{:reason :lava-adjacent :at [2 66 0] :next [2 64 0]}] (map #(select-keys % [:reason :at :next]) (tunnel-ends s)))
              "the stop names the lava")
          (is (= "lava" (block-at s 2 66 0))))))))

(deftest a-fluid-the-tunnel-stops-for-is-in-seen-memory-and-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[fluid world pos hazard] [["water" {"2,64,1" "water" "2,65,1" "water"} [2 65 1] :fluid-adjacent]
                                   ["lava" {"0,66,0" "air" "1,66,0" "air" "2,66,0" "lava"} [2 66 0] :lava-adjacent]]]
          (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 10}
                                   (rock-world world) 120))
                found (lk/seen-blocks (:p s) {:names [fluid] :radius 8 :live? true})
                end (first (tunnel-ends s))]
            (is (some #(= pos (mapv (:pos %) [:x :y :z])) found) (str fluid " in seen memory: " (pr-str found)))
            (is (= {:reason hazard :at pos} (select-keys end [:reason :at])) (pr-str end))))))))

(deftest a-fluid-in-the-cut-is-in-seen-memory-and-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 10}
                                 (rock-world {"1,64,0" "water"}) 120))
              found (lk/seen-blocks (:p s) {:names ["water"] :radius 8 :live? true})
              end (first (tunnel-ends s))]
          (is (some #(= [1 64 0] (mapv (:pos %) [:x :y :z])) found) (pr-str found))
          (is (= {:reason :fluid-in-cut :fluid :water :at [1 64 0]} (select-keys end [:reason :fluid :at])) (pr-str end)))))))

(deftest the-tunnel-seals-lava-a-cut-lays-open-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; lava behind the rock cannot be seen before the cut: the dig child seals it from the rim (fetching a block)
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 10}
                                 (rock-world {"5,64,0" "lava"}) 80))]
          (is (= 1 (count (events-of s :stair.sealed))))
          (is (not= "lava" (block-at s 5 64 0)))
          (is (= :tunnel-length (:reason (last (done-events s))))))))))

(deftest the-tunnel-length-bound-ends-the-job-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 4} (rock-world {}) 80))]
          (is (finished? s))
          (is (= :tunnel-length (:reason (done-event s))))
          (is (= [{:reason :tunnel-length :length 4}] (map #(select-keys % [:reason :length]) (tunnel-ends s))))
          (is (= (set (for [x [1 2 3 4] y [64 65]] [x y 0])) (set (dug-cells s))))
          (is (= 4 (count (events-of s :stair.step))) "each step is a jobs.access.stair child"))))))

(deftest an-ore-has-a-tunnel-level-and-its-twin-kinds
  (is (= 16 (mine-descent/tunnel-level {:block "iron_ore"})))
  (is (= 16 (mine-descent/tunnel-level {:block "deepslate_iron_ore"})))
  (is (= 30 (mine-descent/tunnel-level {:block "iron_ore" :level 30})))
  (is (= -10 (mine-descent/tunnel-level {:block "sand" :level [5 -10 7]})) "a cell's y")
  (is (= -10 (mine-descent/tunnel-level {:block "sand" :level {:x 5 :y -10 :z 7}})))
  (is (nil? (mine-descent/tunnel-level {:block "iron_ore" :level false})) "false: the tunnel runs where the body stands")
  (is (nil? (mine-descent/tunnel-level {:block "sand"})))
  (is (= #{"iron_ore" "deepslate_iron_ore"} (mine-targets/kinds {:block "iron_ore"})))
  (is (= #{"iron_ore" "deepslate_iron_ore"} (mine-targets/kinds {:block "deepslate_iron_ore"})))
  (is (= #{"sand"} (mine-targets/kinds {:block "sand"})))
  (is (= #{"iron_ore" "deepslate_iron_ore"} (mine-targets/kinds {:item "raw_iron"})) "an item: every block that drops it")
  (is (= #{"iron_ore" "deepslate_iron_ore" "coal_ore" "deepslate_coal_ore"} (mine-targets/kinds {:item ["raw_iron" "coal"]})) "several items")
  (is (= ["raw_iron" "coal"] (mine-targets/items {:item ["raw_iron" "coal"]})))
  (is (= ["raw_iron"] (mine-targets/items {:block "iron_ore"})) "no item: what the block drops")
  (is (= "iron_ore" (mine-targets/main-block {:item "raw_iron" :block "iron_ore"}))))

(deftest a-seen-deepslate-twin-of-the-ore-is-dug-for-its-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :ways #{:seen}}
                                 (assoc-in (rock-world {"1,64,0" "deepslate_iron_ore"}) [:drops "deepslate_iron_ore"] "raw_iron") 20))]
          (is (= [[1 64 0]] (dug-cells s)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s)))))))))

;; ------------------------------------------------------------------ :stay-level
(deftest stay-level-tunnels-at-the-start-height-without-a-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 3 :ways #{:seen :strip-tunnel}}
                                 (deep-rock {}) 120))]
          (is (seq (dug-cells s)))
          (is (every? #(#{64 65} (second %)) (dug-cells s)) "no cell below the start level")
          (is (= 3 (:length (first (tunnel-ends s))))))))))

(deftest stay-level-leaves-ore-seen-across-a-cave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :ways #{:seen :strip-tunnel} :tunnel-length 0}
                                 (rock-world far-cave) 40))]
          (is (empty? (calls s "dig")))
          (is (= :none (:reason (done-event s)))))))))

(deftest stay-level-fills-the-cave-a-cut-opens-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :direction "east" :tunnel-length 6 :ways #{:seen :strip-tunnel}}
                                 (update (rock-world side-cave) :inventory into [{:name "cobblestone" :count 5}]) 120))]
          (is (= :breach-sealed (:reason (first (tunnel-ends s)))) (pr-str (map :text (tunnel-ends s))))
          (is (= "cobblestone" (block-at s 2 64 1)))
          (is (= "cobblestone" (block-at s 2 65 1)))
          (is (= "air" (block-at s 2 64 0)) "the tunnel cell stays open")
          (is (not-any? #(> (first %) 2) (dug-cells s))))))))

(deftest stay-level-turns-from-water-at-its-first-cell-and-lists-the-legs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (facing-west-run {"-1,64,0" "water"} 4 250))
              legs (:legs (done-event s))]
          (is (= {:heading "west" :length 0 :stop :fluid-in-cut} (first legs)))
          (is (= 2 (count legs)) (pr-str legs))
          (is (= 4 (:length (second legs))))
          (is (some #(< (nth % 2) 0) (dug-cells s)) "it went on north"))))))

(deftest a-cave-under-the-tunnel-floor-is-no-stop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world (assoc (rock-world {"-1,62,0" "air"}) :yaw 90)})
              p (:p s)
              orig (aget p "sensedAt")]
          ;; seen before: a cell two under the tunnel floor is out of sight from the tunnel
          (aset p "sensedAt" (fn [pos] (if (= [-1 62 0] [(.-x pos) (.-y pos) (.-z pos)]) (.blockAt p pos) (orig pos))))
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 30 :tunnel-length 4 :level false}) {})
          (await (run-ticks s 250))
          (is (= {:heading "west" :length 4 :stop :tunnel-length} (first (:legs (done-event s)))) (pr-str (:legs (done-event s)))))))))

(deftest stay-level-turns-from-the-last-safe-cell-after-a-sealed-breach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (facing-west-run {"-6,64,1" "air" "-6,65,1" "air"} 9 400))
              legs (:legs (done-event s))]
          (is (= {:heading "west" :stop :breach-sealed} (select-keys (first legs) [:heading :stop])))
          (is (< 1 (count legs)) (pr-str legs))
          (is (not= "west" (:heading (second legs))))
          (is (>= 9 (reduce + (map :length legs))) "the length budget is shared by the legs")
          (is (= "cobblestone" (block-at s -6 64 1)) "the breach stays sealed"))))))

(deftest stay-level-records-what-the-sealed-breach-showed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [air ["air" "cave_air"]]
          (let [s (await (scenario {:block "iron_ore" :direction "east" :tunnel-length 6 :ways #{:seen :strip-tunnel}}
                                   (update (rock-world {"2,64,1" air "2,65,1" air}) :inventory into [{:name "cobblestone" :count 5}]) 120))
                behind {:names [air] :cells [[2 64 1] [2 65 1]]}]
            (is (= behind (:behind (first (tunnel-ends s)))))
            (is (= behind (:behind (first (:legs (done-event s))))))
            (is (= [behind] (map #(select-keys (:data %) [:names :cells]) (mem/entries (mem/view (:store (:eng s))) :breach-behind))))))))))

(deftest remember-behind-keeps-the-sealed-cells-and-the-lava-among-the-cells
  (let [kept (atom nil)
        c {:primitives #js {:self (fn [] #js {:dimension "overworld"})}
           :remember (fn [kind data _] (reset! kept [kind data]))}
        names {{:x 2 :y 64 :z 1} "lava" {:x 2 :y 64 :z -1} "air"}
        b {:names ["air" "lava"] :cells [[2 64 -1] [2 64 1]]}]
    (stay/remember-behind! c {:x 2 :y 64 :z 0} b #(names % "stone") [{:x 2 :y 64 :z 0} {:x 2 :y 65 :z 0}])
    (is (= [:breach-behind (assoc b :dim "overworld" :at {:x 2 :y 64 :z 0} :seals [[2 64 0] [2 65 0]] :lava [[2 64 1]])] @kept))))

(deftest stay-level-takes-the-air-of-its-own-stair-for-no-breach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stay-run side-cave [[:stair-way {:dim "overworld" :floors [[2 63 1]]}]]))]
          (is (= [:tunnel-length] (mapv :reason (tunnel-ends s))) (pr-str (map :text (tunnel-ends s))))
          (is (= "air" (block-at s 2 64 1)) "nothing sealed into the stair"))))))

(deftest stay-level-takes-the-cell-over-a-stair-steps-head-for-its-own-air
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stay-run side-cave [[:stair-way {:dim "overworld" :floors [[2 62 1]]}]]))]
          (is (= [:tunnel-length] (mapv :reason (tunnel-ends s))) (pr-str (map :text (tunnel-ends s)))))))))

(deftest stay-level-takes-the-air-of-its-own-earlier-tunnel-for-no-breach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stay-run (cells "air" [2] [64 65] [1 2 3])
                                 [[:tunnel-way {:dim "overworld" :origin [2 64 1] :delta [0 1] :steps 2}]]))]
          (is (= [:tunnel-length] (mapv :reason (tunnel-ends s))) (pr-str (map :text (tunnel-ends s))))
          (is (= "air" (block-at s 2 64 2))))))))

(deftest a-stay-level-tunnel-is-remembered-as-the-body's-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stay-run {} []))
              e (mem/latest (mem/view (:store (:eng s))) :tunnel-way)]
          (is (= {:dim "overworld" :origin [0 64 0] :delta [1 0] :steps 4} (select-keys (:data e) [:dim :origin :delta :steps]))))))))

(deftest side-cut-names-the-heading-and-steps-to-ore-in-line-with-the-body
  (doseq [[pos reach want] [[{:x 2 :y 64 :z 2} 2 {:heading "south" :steps 1}]
                            [{:x 2 :y 65 :z -2} 2 {:heading "north" :steps 1}]
                            [{:x 2 :y 64 :z 3} 3 {:heading "south" :steps 2}]
                            [{:x 2 :y 64 :z 3} 2 nil]
                            [{:x 2 :y 64 :z 1} 2 nil]
                            [{:x 3 :y 64 :z 2} 2 nil]
                            [{:x 2 :y 66 :z 2} 2 nil]
                            [{:x 2 :y 64 :z 2} 0 nil]]]
    (is (= want (stay/side-cut {:x 2 :y 64 :z 0} [1 0] reach pos)) (pr-str pos reach))))

(deftest the-default-ore-reach-takes-remembered-ore-nine-blocks-into-the-wall
  (is (= {:heading "south" :steps 8} (stay/side-cut {:x 2 :y 64 :z 0} [1 0] (settings/get stay/settings ::stay/ore-reach) {:x 2 :y 64 :z 9}))))

(defn lava-drip!
  "Lava above a dug cell flows into it before the next dig or place (the flow delay), as a source dripping down."
  [p]
  (let [world (.-world p)
        state (.. world -state)
        pending (atom nil)
        flow! (fn [] (when-let [[x y z] @pending]
                       (reset! pending nil)
                       (swap! state assoc-in [:blocks [x y z]] "lava")))]
    (.override world "place" (fn ^:async f [token a impl] (flow!) (await (impl token a))))
    (.override world "dig" (fn ^:async f [token a impl]
                             (flow!)
                             (let [r (await (impl token a))
                                   pos (.-pos a)
                                   cell [(.-x pos) (.-y pos) (.-z pos)]
                                   above (get-in @state [:blocks (update cell 1 inc)])]
                               (when (= "lava" above) (reset! pending cell))
                               r)))))

(deftest lava-over-the-top-cut-cell-is-sealed-at-its-source-and-the-mine-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake-on-floor {:floor-block floor-block :inventory (conj pickaxe {:name "cobblestone" :count 16})
                                   :bodyHitbox true :exact-eye true :self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (merge rock {"4,66,0" "lava" "6,64,0" "iron_ore"}) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"}})
              _ (lava-drip! p)
              s (start {:p p})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 10}) {})
          (await (run-ticks s 120))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (not= "lava" (block-at s 4 66 0)) "the source over the cut is capped"))))))
