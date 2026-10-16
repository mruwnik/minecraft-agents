(ns engine.mine-c-test
  "jobs.gather.mine against the fake world."
  (:require [engine.child-ends :as ce]
            [jobs.gather.mine.drops :as drops]
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
            [jobs.gather.mine :as mine]
            [jobs.gather.mine.descent :as mine-descent]
            [jobs.gather.mine.end :as mine-end]
            [jobs.gather.mine.targets :as mine-targets]
            [jobs.gather.mine.torches :as mine-torches]
            [jobs.gather.mine.tunnel :as mine-tunnel]
            [jobs.gather.mine-stay :as stay]
            [jobs.access.stair :as stair]
            [plan.shape :as shape]
            [engine.mine-support :refer [spec as-child floor-block sight-opts seeing start run-ticks scenario events-of done-events done-event finished? inv job-mem calls moved dig-count dug-cells block-at cells floor wide-floor sand-patch set-pos! bare-pickaxe pickaxe cobble stone-floor stone-hill ground-cell? dug-ground farm-zone pad-plan two-in-two-out zoned their-claim rock rock-world feet tunnel-ends open-line east start-x corridor bridge-run no-floor-end? deep-rock far-cave side-cave facing-west-run stay-run side-ore-run shutdown-at-cell! run-until! below-ore-run run-until shaft-plan shafted via-shaft ore-east drop-away! hide-drop-until-beside! walled-ore watched dark-ore-dug long-rock torch-world torch-xs torches8 side-room side-torch-cells branch-torches soil-over-stone soil-all-round descent-run stair-down-east classify-in walk-options spy-go-to home-go-tos home-zone no-shaft-args deep-ore home-walk-run]]))

(deftest stay-level-takes-ore-seen-two-blocks-into-the-wall-by-a-side-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (side-ore-run 2))]
          (is (= 1 (get (inv s) "raw_iron")) (pr-str (keep :text @(:seen s)) (dug-cells s) (feet s)))
          (is (= "cobblestone" (block-at s 1 64 -1)) "the side cut is shut behind"))))))

(deftest a-side-cut-under-way-goes-on-after-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              s (start {:world (update (rock-world {"0,65,-1" "air" "0,65,-2" "air" "1,65,-2" "iron_ore"}) :inventory into [{:name "cobblestone" :count 5}])
                        :dir dir})]
          (mem/write! (:store (:eng s)) :tunnel-way {:dim "overworld" :origin [0 64 -1] :delta [0 -1] :steps 0})
          (let [cut? (shutdown-at-cell! s #{[1 64 -1] [1 65 -1]})]
            (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4 :stay-level true :ore-reach 2 :mend false}) {})
            (await (run-until! s cut? 500)))
          (let [again (start {:p (:p s) :dir dir})]
            (is (:side-cut (core/job-memory (:eng again) "j1")) "cut in the side cut")
            (await (run-ticks again 500))
            (is (= 1 (get (inv again) "raw_iron")) (pr-str (keep :text @(:seen again))))
            (is (= "cobblestone" (block-at again 1 64 -1)) "the side cut is shut behind")))))))

(deftest a-tunnel-step-under-way-goes-on-after-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [stay [false true]]
          (let [dir (tu/tmp-dir)
                s (start {:world (rock-world {}) :dir dir})]
            (let [cut? (shutdown-at-cell! s #{[2 65 0]})]
              (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 4 :mend false :stay-level stay}) {})
              (await (run-until! s cut? 40)))
            (let [again (start {:p (:p s) :dir dir})]
              (is (:stepping (core/job-memory (:eng again) "j1")) "cut while the stair child dug")
              (await (run-ticks again 150))
              (is (finished? again) (pr-str stay))
              (is (= [{:reason :tunnel-length :length 4}] (map #(select-keys % [:reason :length]) (tunnel-ends again))) (pr-str stay))
              (is (= (set (for [x [1 2 3 4] y [64 65]] [x y 0])) (set (dug-cells again))) (pr-str stay)))))))))

(deftest stay-level-leaves-ore-beyond-its-ore-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (side-ore-run 1))]
          (is (nil? (get (inv s) "raw_iron")))
          (is (= [[1 65 -2]] (:unreached (done-event s))) "the seen ore it left is reported"))))))

(deftest beside-body-takes-the-column-round-the-feet-from-one-below-to-above-the-head
  (doseq [[pos want] [[{:x 1 :y 63 :z -1} true]
                      [{:x 0 :y 63 :z 0} false]
                      [{:x 1 :y 66 :z 1} true]
                      [{:x 1 :y 67 :z 1} false]
                      [{:x 2 :y 64 :z 0} false]
                      [{:x 1 :y 62 :z 0} false]]]
    (is (= want (stay/beside-body? {:x 0 :y 64 :z 0} pos)) (pr-str pos))))

(deftest stay-level-takes-ore-one-below-the-tunnel-and-shuts-the-hole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (below-ore-run {"iron_ore" "raw_iron" "stone" "cobblestone"}))]
          (is (= 1 (get (inv s) "raw_iron")) (pr-str (keep :text @(:seen s))))
          (is (= "cobblestone" (block-at s 0 63 -1)) "the hole is shut after the drop is in the bag"))))))

(deftest stay-level-shuts-the-hole-even-when-the-drop-was-not-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (below-ore-run {"stone" "cobblestone"}))]
          (is (some #{[0 63 -1]} (dug-cells s)) (pr-str (keep :text @(:seen s))))
          (is (= "cobblestone" (block-at s 0 63 -1)) (pr-str (keep :text @(:seen s)))))))))

(deftest stay-level-stops-open-when-it-cannot-seal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :direction "east" :tunnel-length 6 :stay-level true :mend false}
                                 (rock-world side-cave) 120))]
          (is (= :breach-open (:reason (first (tunnel-ends s)))) (pr-str (map :text (tunnel-ends s))))
          (is (not-any? #(> (first %) 2) (dug-cells s))))))))

(deftest with-no-ore-seen-it-stairs-down-to-the-ore-level-then-tunnels-and-comes-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :level 58 :mend false}
                                 (deep-rock {"9,58,1" "iron_ore"}) 400))
              e (done-event s)]
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason e)))
          (is (some (fn [[_ y _]] (< y 60)) (dug-cells s)) "it dug down")
          (is (= 58 (second (:origin (:tunnel e)))) "the tunnel runs at the ore level")
          (is (= [0 64 0] (feet s)) "back on its start cell")
          (is (seq (calls s "place")) "a torch lights the stair or the tunnel"))))))

;; Sorrel #14: a night reflex walked the body home between two steps of the stair down; the next step started a new
;; stair where the body stood, off the heading's line.
(deftest a-descent-interrupted-between-steps-walks-back-to-its-stair-and-goes-on-along-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world (deep-rock {"9,58,1" "iron_ore" "0,64,1" "air" "0,65,1" "air" "0,64,2" "air" "0,65,2" "air"})})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :level 58 :mend false}) {})
          (await (run-until s 400 #(<= 2 (count (events-of % :stair.done)))))
          (swap! (.. (:p s) -world -state) assoc-in [:self :pos] [0.5 64 2.5])
          (await (run-ticks s 400))
          (is (= :count (:reason (done-event s))))
          (is (empty? (remove (fn [[_ y z :as cell]] (or (> y 63) (zero? z) (= [9 58 1] cell))) (dug-cells s))) "every cell below the start but the ore is on the line z 0")
          (is (= 58 (second (:origin (:tunnel (done-event s)))))))))))

;; Sorrel j3047/j3050: a stair step that stopped mid-descent (:floor-unseen) ended the descent, and the tunnel ran 30+
;; blocks above :level. A table in the third step stands in for the stop.
(deftest a-step-blocked-mid-descent-turns-the-stair-and-goes-on-down-to-the-level
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks (merge (dissoc (cells "stone" (range -12 13) (range 50 68) (range -4 13)) "0,64,0" "0,65,0")
                                    {"0,61,3" "crafting_table"})
                     :drops {"stone" "cobblestone"}
                     :inventory (into pickaxe [{:name "torch" :count 8}])}
              s (start {:world world})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :level 58 :tunnel-length 3 :mend false}) {})
          (await (run-ticks s 600))
          (is (= "south" (:from (first (events-of s :mine.turned)))) (pr-str (map :text (events-of s :mine.turned))))
          (is (empty? (events-of s :mine.descent-stopped)) (pr-str (map :text (events-of s :mine.descent-stopped))))
          (is (= "crafting_table" (block-at s 0 61 3)))
          (is (= 58 (second (:origin (:tunnel (done-event s)))))))))))

(deftest a-shaft-in-reach-is-stairred-down-and-a-branch-mined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (shafted via-shaft ore-east [] 400))
              e (done-event s)]
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason e)))
          (is (= [4 60 0] (:origin (:tunnel e))) "the first branch runs on from the shaft's bottom")
          (is (re-find #"via shaft s \(stair 4 steps to y 60\), branch 1 east" (:text e)))
          (is (= [0 64 0] (feet s))))))))

(deftest stone-seen-from-the-shaft-stair-does-not-end-the-descent-before-the-shaft-is-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [soil-over-rock (merge (cells "stone" (range -3 15) (range 50 62) (range -4 5))
                                    (dissoc (cells "dirt" (range -3 15) (range 62 68) (range -4 5)) "0,64,0" "0,65,0"))
              world {:blocks soil-over-rock :drops {"stone" "cobblestone"} :inventory (into pickaxe [{:name "torch" :count 8}])}
              s (await (zoned {:block "stone" :count 3 :mend false} world (ew/of-data {"s" shaft-plan} {} []) 400))
              [shaft] (events-of s :mine.shaft)]
          (is (= "s" (:shaft shaft)) "the stair reached stone, the shaft was still taken")
          (is (re-find #"via shaft s" (:text (done-event s)))))))))

(deftest a-body-inside-a-half-dug-shaft-continues-it-from-where-it-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cut (into {} (for [f [[0 64 0] [1 63 0]] cell (:cut (shape/step-cells f :down :east))] [(clojure.string/join "," cell) "air"]))
              world (assoc (deep-rock (merge cut ore-east)) :self {:pos {:x 2.5 :y 62 :z 0.5}})
              s (await (zoned via-shaft world (ew/of-data {"s" shaft-plan} {} []) 400))
              e (done-event s)]
          (is (re-find #"via shaft s \(stair 2 steps to y 60\)" (:text e)) "the 2 steps left, not the whole shaft walked again from its top")
          (is (= 1 (get (inv s) "raw_iron"))))))))

(deftest the-next-run-takes-another-branch-of-the-shaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (shafted via-shaft (assoc ore-east "7,60,-4" "iron_ore") [] 400))
              _ (core/submit! (:eng s) (spec via-shaft) {})
              _ (await (run-ticks s 400))
              e (second (done-events s))]
          (is (= 2 (get (inv s) "raw_iron")))
          (is (= [[7 60 0] "north"] [(:origin (:tunnel e)) (:heading (:tunnel e))]))
          (is (re-find #"branch 2 north" (:text e))))))))

(deftest a-walk-that-misses-the-next-branch-start-digs-no-branch-from-the-wrong-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (shafted via-shaft (assoc ore-east "7,60,-4" "iron_ore") [] 400))
              _ (doseq [y [60 61]] (fake/set-block! (:p s) [7 y 0] "bedrock"))
              _ (core/submit! (:eng s) (spec via-shaft) {})
              _ (await (run-ticks s 400))
              [no] (events-of s :mine.no-shaft)
              second-done (second (done-events s))]
          (is (= {:shaft "s" :reason :unreachable} (select-keys no [:shaft :reason])))
          (is (not= "east" (:heading (:tunnel second-done))) "the usual descent does not dig the shaft's branch from the stair bottom")
          (is (not (re-find #"branch 2" (str (:text second-done)))))
          (is (= 1 (get-in (last (mem/entries (mem/view (:store (:eng s))) :mine/shafts)) [:data "s" :n])) "the branch that was not dug is not recorded"))))))

(deftest a-shaft-whose-stair-stopped-is-named-and-not-tried-again-by-the-next-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walled (into {} (for [y (range 58 68)] [(str "2," y ",0") "bedrock"]))
              s (await (shafted via-shaft (merge ore-east walled) [] 400))
              _ (core/submit! (:eng s) (spec via-shaft) {})
              _ (await (run-ticks s 400))
              [first-no second-no] (events-of s :mine.no-shaft)]
          (is (= "s" (:shaft first-no)))
          (is (re-find #"shaft s" (:text first-no)) "the text names the shaft")
          (is (re-find #":shaft" (:text first-no)) "and how to override it")
          (is (= {:shaft "s" :reason :failed-lately} (select-keys second-no [:shaft :reason])) "the next run leaves it alone"))))))

(deftest a-shaft-in-another-zone-is-left-alone-unless-zones-are-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [theirs {:name "theirs" :min [-1 63 -1] :max [1 65 1] :owner "Miles"}
              plain (await (shafted via-shaft ore-east [theirs] 400))
              forced (await (shafted (assoc via-shaft :ignore-zones? true) ore-east [theirs] 400))]
          (is (= [{:reason :zone}] (map #(select-keys % [:reason]) (events-of plain :mine.no-shaft))))
          (is (not (re-find #"via shaft" (str (:text (done-event plain))))))
          (is (re-find #"via shaft s" (:text (done-event forced)))))))))

(deftest a-shaft-inside-an-own-zone-is-used-without-walking-out-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [mine-zone {:name "home" :min [-1 59 -1] :max [4 65 1] :owner "Fake"}
              s (await (shafted via-shaft ore-east [mine-zone] 400))]
          (is (empty? (events-of s :mine.left-zone)))
          (is (re-find #"via shaft s" (:text (done-event s))))
          (is (= "stone" (block-at s 0 64 1)) "the zone's other cells are not dug"))))))

(deftest a-plot-with-no-ore-gives-up-naming-the-tunnel-and-its-depth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :level 60 :tunnel-length 3 :mend false}
                                 (deep-rock {}) 400))
              e (done-event s)]
          (is (= :tunnel-length (:reason e)))
          (is (zero? (:got e)))
          (is (= 60 (second (:origin (:tunnel e)))))
          (is (re-find #"tunnel 3 blocks east at y 60" (:text e)))
          (is (= [0 64 0] (feet s))))))))

(deftest the-stair-down-goes-on-under-gravel-and-stops-when-falling-blocks-are-not-accepted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[accept stops?] [[#{:falling-block} false] [#{} true]]]
          (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :level 60 :tunnel-length 3 :mend false :accept accept}
                                   (deep-rock {"1,66,0" "gravel" "0,66,0" "air"}) 60))]
            (is (= stops? (boolean (some #(= :hazard (:stop %)) (events-of s :mine.descent-stopped)))) (pr-str accept))
            (is (= (not stops?) (boolean (some #(< (second %) 64) (dug-cells s)))) "the stair cut below the start")))))))

(deftest a-stair-stopped-far-above-the-level-ends-the-run-instead-of-tunnelling-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :level 40 :tunnel-length 3 :mend false :accept #{}}
                                 (deep-rock {"1,66,0" "gravel" "0,66,0" "air"}) 60))
              e (done-event s)]
          (is (= :descent-stopped (:reason e)))
          (is (nil? (:tunnel e))))))))

(deftest gravel-that-falls-into-the-cut-is-dug-until-the-line-is-clear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world (rock-world {})})
              world (.-world (:p s))
              refills (atom 3)]
          (.override world "dig"
                     (fn [token a impl]
                       (-> (js/Promise.resolve (impl token a))
                           (.then (fn [r]
                                    (let [p (.-pos a)]
                                      (when (and (= [2 65 0] [(.-x p) (.-y p) (.-z p)]) (pos? @refills))
                                        (swap! refills dec)
                                        (swap! (.-state world) fake/put-block [2 65 0] "gravel")))
                                    r)))))
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4 :level false :mend false}) {})
          (await (run-ticks s 120))
          (is (= :tunnel-length (:reason (done-event s))))
          (is (= [{:reason :tunnel-length :length 4}] (map #(select-keys % [:reason :length]) (tunnel-ends s))))
          (is (= "air" (block-at s 2 65 0)) "the fallen gravel is dug")
          (is (= 4 (count (filter #{[2 65 0]} (dug-cells s)))) "the cut, then each of the 3 that fell"))))))

(deftest the-tunnel-runs-the-way-asked-or-the-way-the-body-faces
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args yaw cells-at] [[{:direction "north"} 0 [[0 -1] [0 -2]]]
                                     [{:direction :w} 0 [[-1 0] [-2 0]]]
                                     [{} 180 [[0 -1] [0 -2]]]
                                     [{} 270 [[1 0] [2 0]]]]]
          (let [s (await (scenario (merge {:block "iron_ore" :level false :count 1 :tunnel-length 2} args)
                                   (assoc (rock-world {}) :yaw yaw) 40))]
            (is (= (set (for [[x z] cells-at y [64 65]] [x y z])) (set (dug-cells s))) (pr-str args yaw))))))))

(deftest drops-that-land-away-are-walked-to-and-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})]
          (drop-away! (:p s) [5 64 2])
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (= 1 (get (inv s) "raw_iron")) "counted by what the inventory gained")
          (is (= :count (:reason (done-event s))))
          (is (empty? (events-of s :mine.left-behind))))))))

(deftest a-drop-that-fell-out-of-sight-below-the-dug-cell-is-looked-for-and-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore" "3,62,0" "stone"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})
              p (:p s)]
          ;; the cell under the dug one is known already, so only the job's own look at it shows the drop
          (aset p "sensedAt" (fn [pos] (.blockAt p pos)))
          (.override (.-world p) "dig"
                     (fn [token args impl]
                       (.then (impl token args)
                              (fn [r]
                                (swap! (fake/state p) update :entities
                                       (fn [es] (mapv #(if (= "item" (:kind %)) (assoc % :pos [3 63 0] :visible false) %) es)))
                                r))))
          (.override (.-world p) "look"
                     (fn [token a impl]
                       (when (and (< (js/Math.abs (- 3.5 (.. a -pos -x))) 0.1) (< (js/Math.abs (- 63.5 (.. a -pos -y))) 0.1))
                         (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :visible true) es))))
                       (impl token a)))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 30))
          (is (= 1 (get (inv s) "raw_iron")) "the drop under the dug cell was looked for, seen and collected"))))))

(deftest drops-out-of-reach-are-reported-left-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270
                                :unreachable ["5,64,2"]}})]
          (drop-away! (:p s) [5 64 2])
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (nil? (get (inv s) "raw_iron")))
          (is (= 0 (:got (done-event s))))
          (is (= [[{:pos [5 64 2] :count 1 :reason :unreachable}]] (map :items (events-of s :mine.left-behind))))
          (is (= [{:pos [5 64 2] :count 1}] (:left (done-event s)))))))))

(deftest a-drop-out-of-sight-from-where-it-was-dug-is-walked-to-and-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})]
          (hide-drop-until-beside! (:p s) [3 64 0])
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 30))
          (is (= 1 (get (inv s) "raw_iron")) "the body walked beside the dug cell and picked the drop up")
          (is (= :count (:reason (done-event s))))
          (is (empty? (events-of s :mine.gave-up))))))))

(deftest a-drop-of-ore-dug-over-a-wall-is-cut-to-and-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks walled-ore :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})]
          (hide-drop-until-beside! (:p s) [2 64 0])
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0 :mend false}) {})
          (await (run-ticks s 40))
          (is (= [[2 64 0] [1 64 0]] (dug-cells s)) "dug from behind the wall, then the wall cell between")
          (is (= 1 (get (inv s) "raw_iron")) "the body cut its way beside the dug cell and picked the drop up")
          (is (= :count (:reason (done-event s)))))))))

(deftest a-cut-beside-a-dug-cell-never-opens-a-cell-under-gravel
  (let [cut (fn [blocks] (let [s (start {:world {:blocks blocks :inventory pickaxe :yaw 270}})]
                           (drops/drop-cut {:primitives (:p s)} [2 64 0] 4)))]
    (is (= [[1 64 0]] (cut walled-ore)) "the wall cell between is the way")
    (is (nil? (cut (assoc walled-ore "1,65,0" "gravel"))) "gravel over it would fall on the body walking in")))

(deftest a-mine-that-ends-short-of-its-count-is-stopped-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 5 :tunnel-length 0 :descend-limit 0}
                                 {:blocks (merge (cells "stone" [4 5] [64] [0]) (cells floor-block [4 5] [63] [0])) :drops {"stone" "cobblestone"} :inventory pickaxe} 60))
              stopped (first (events-of s :stopped))]
          (is (= 2 (get (inv s) "cobblestone")))
          (is (= 2 (:got (done-event s))))
          (is (finished? s))
          (is (empty? (events-of s :completed)) "2 of 5 is not completed")
          (is (some? stopped))
          (is (re-find #"mine got 2 of 5: none" (pr-str stopped))))))))

(deftest drops-picked-up-a-moment-later-are-not-left-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270
                                :unreachable ["5,64,2"]}})]
          (drop-away! (:p s) [5 64 2])
          ;; the pickup packet arrives during the wait: the item entity is gone by then
          (.override (.-world (:p s)) "wait"
                     (fn [token args impl]
                       (swap! (fake/state (:p s)) assoc :entities [])
                       (impl token args)))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (empty? (events-of s :mine.left-behind)))
          (is (nil? (:left (done-event s)))))))))

(deftest drops-picked-up-after-the-report-are-not-listed-left-at-the-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (merge wide-floor {"8,64,0" "iron_ore"}) :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270
                                :unreachable ["5,64,2"]}})]
          (drop-away! (:p s) [5 64 2])
          ;; a later steer (the walk home) picks the drop up
          (.override (.-world (:p s)) "steer"
                     (fn [token ^js args impl]
                       (let [decide (.-decide args)
                             wrapped (fn [pose]
                                       (when (<= (.-x pose) 2)
                                         (swap! (fake/state (:p s)) assoc :entities []))
                                       (decide pose))]
                         (impl token (js/Object.assign #js {} args #js {:decide wrapped})))))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 60))
          (is (= 1 (count (events-of s :mine.left-behind))) "reported when it was still lying")
          (is (nil? (:left (done-event s))) "but gone by the end"))))))

(deftest a-run-that-only-dug-targets-still-walks-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})]
          (drop-away! (:p s) [5 64 2])
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 30))
          (is (= :count (:reason (done-event s))))
          (is (= [0 64 0] (feet s))))))))

(deftest a-zone-follows-its-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra dug] [["Fake" {} 4] ["FAKE" {} 4] ["Miles" {} 2] ["Miles" {:ignore-zones? true} 4]]]
          (let [s (await (zoned (merge {:block "sand" :count 4 :mend false :tunnel-length 0} extra) {:blocks two-in-two-out}
                                (ew/of-data {} {} [(assoc farm-zone :owner owner)]) 40))]
            (is (= dug (count (dug-cells s))) (pr-str [owner extra]))))))))

(deftest the-opt-out-needs-no-zone-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "sand" :count 2 :mend false :ignore-zones? true :tunnel-length 0} {:blocks two-in-two-out}
                              (ew/of-data {} {} nil) 40))]
          (is (= 2 (count (dug-cells s)))))))))

(deftest the-mend-follows-the-zone-owner-and-the-opt-out
  ;; the owner mends its own zone only when told to ignore zones: otherwise it walks out before it digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra mended-some declined] [["Fake" {:ignore-zones? true} true []] ["FAKE" {:ignore-zones? true} true []]
                                                   ["Fake" {} false []]
                                                   ["Miles" {} false [{:reason :refused :zones ["farm"]}]]
                                                   ["Miles" {:ignore-zones? true} true []]]]
          (let [zone {:name "farm" :min [-2 60 -2] :max [2 70 2] :owner owner :allow #{:dig}}
                s (await (zoned (merge {:block "dirt" :count 4} extra) {:blocks floor} (ew/of-data {} {} [zone]) 80))]
            (is (= mended-some (pos? (count (calls s "place")))) (pr-str [owner extra]))
            (is (= declined (mapv #(select-keys % [:reason :zones]) (events-of s :mine.declined))) (pr-str [owner extra]))))))))

(deftest drops-beyond-the-radius-of-the-body-but-near-the-dug-block-are-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"5,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})]
          (drop-away! (:p s) [8 64 0])
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0 :collect-radius 3}) {})
          (await (run-ticks s 30))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (empty? (events-of s :mine.left-behind))))))))

(deftest drops-beyond-the-radius-of-the-body-and-out-of-reach-are-listed-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"5,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270
                                :unreachable ["8,62,0"]}})]
          (drop-away! (:p s) [8 62 0])
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0 :collect-radius 3}) {})
          (await (run-ticks s 30))
          (is (nil? (get (inv s) "raw_iron")))
          (is (= [{:pos [8 62 0] :count 1}] (:left (done-event s)))))))))
