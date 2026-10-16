(ns engine.mine-a-test
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

;; ------------------------------------------------------------------ the job
(deftest mines-a-patch-and-leaves-the-ground-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 6} {:blocks (merge floor sand-patch)} 60))]
          (is (>= (get (inv s) "sand") 6))
          (is (= :count (:reason (done-event s))))
          (is (>= (:got (done-event s)) 6))
          (is (every? #(= "dirt" %) (for [x (range -2 3) y [62 63] z (range -2 3)] (block-at s x y z))))
          (is (finished? s)))))))

(deftest a-round-has-no-step-cap-of-its-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 6} {:blocks (merge floor sand-patch)} 2))]
          (is (every? #(not= :continue (:kind %)) (done-events s)))
          (is (= :count (:reason (done-event s)))))))))

(deftest every-dig-goes-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 6} {:blocks (merge floor sand-patch)} 60))]
          (is (pos? (dig-count s)))
          (is (= (dig-count s) (count (ce/ends s :dug)))))))))

;; sand on a 5-high stone pillar (in sight, out of every stand's reach) is nearer in a line than sand on the floor (off
;; the pillar's line of sight)
(deftest the-reachable-target-is-walked-to-before-a-nearer-one-out-of-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pillar (merge (cells "stone" [5] (range 63 68) [0]) {"5,68,0" "sand"})
              s (await (scenario {:block "sand" :count 1 :tunnel-length 0} {:blocks (merge floor pillar {"12,64,4" "sand"})} 30))]
          (is (= [12 64 4] (first (dug-cells s))))
          (is (= {:x 12 :y 64 :z 4} (:target (first (moved s)))) "the first walk is to the floor sand")
          (is (not-any? #(= {:x 5 :y 68 :z 0} (:target %)) (moved s)) "no walk toward the pillar")
          (is (finished? s)))))))

(deftest dirt-is-gathered-from-the-top-layer-only-never-a-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "dirt" :count 40 :mend false :tunnel-length 0 :descend-limit 0} {:blocks floor} 120))]
          (is (seq (dug-cells s)))
          (is (every? #(>= (second %) 63) (dug-cells s)) "no dig below the ground the body started on")
          (is (finished? s)))))))

(deftest digging-the-floor-it-stands-on-is-mended
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "dirt" :count 4} {:blocks floor} 80))]
          (is (= :spent-on-mend (:reason (done-event s))) "the dirt is the only filler, so the mend eats the count")
          (is (= :count (:dig-reason (done-event s))))
          (is (pos? (:mended (done-event s))))
          (is (= 4 (:mended (done-event s))))
          (is (every? #(= "dirt" %) (for [x (range -2 3) y [62 63] z (range -2 3)] (block-at s x y z))))
          (is (finished? s)))))))

(deftest the-ground-is-written-before-the-first-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks floor}})]
          (tu/shutdown-at! s "dig" 1)
          (core/submit! (:eng s) (spec {:block "dirt" :count 4}) {})
          (await (run-ticks s 1))
          (let [m (job-mem s)]
            (is (= {:x 0 :y 64 :z 0} (:start m)))
            (is (= 25 (count (:ground m)))) ; the 5x5 layer under the start in view; the one below is learnt as it is dug
            (is (= :dig (:phase m)))
            (is (= 4 (:goal m)))
            (is (= 1 (dig-count s)) "the snapshot is in memory when the first dig is cut")))))))

(deftest a-restart-still-mends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              s (start {:world {:blocks floor} :dir dir})]
          (tu/shutdown-at! s "dig" 3)
          (core/submit! (:eng s) (spec {:block "dirt" :count 4}) {})
          (await (run-ticks s 1))
          (is (pos? (dig-count s)))
          (is (not (finished? s)))
          (let [again (start {:p (:p s) :dir dir})]
            (is (<= 25 (count (:ground (core/job-memory (:eng again) "j1")))) "the seen layer, and cells booked as they were dug")
            (await (run-ticks again 80))
            (is (finished? again))
            (is (every? #(= "dirt" %) (for [x (range -2 3) y [62 63] z (range -2 3)] (block-at s x y z))))))))))

(deftest a-call-with-no-block-named-is-impossible-and-touches-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world {:blocks sand-patch}})]
          (core/submit! eng (spec {}) {})
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))))
          (is (zero? (count (.-calls (.-world p))))))))))

(deftest with-no-tunnel-and-nothing-seen-it-looks-around-and-ends-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args world note]
                [[{:block "sand" :tunnel-length 0} {} "nothing at all"]
                 [{:block "sand" :tunnel-length 0} {:blocks {"2,64,0" "dirt"}} "other blocks"]
                 [{:block "sand" :tunnel-length 0 :radius 5} {:blocks {"9,64,0" "sand"}} "outside the radius"]]]
          (let [s (await (scenario args world 6))]
            (is (= :none (:reason (done-event s))) note)
            (is (empty? (calls s "dig")) note)
            (is (= 12 (count (calls s "look"))) note)))))))

(deftest wet-targets-are-skipped-unless-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"3,64,0" "sand" "6,64,0" "sand" "6,64,1" "water"}}
              dry (await (scenario {:block "sand" :count 2 :tunnel-length 0} world 30))
              wet (await (scenario {:block "sand" :count 2 :wet true} world 30))]
          (is (= :wet (:reason (done-event dry))))
          (is (= 1 (:wet-skipped (done-event dry))))
          (is (re-find #"1 .*water.*:wet" (:text (done-event dry))))
          (is (= 1 (dig-count dry)))
          (is (= :count (:reason (done-event wet))))
          (is (= 2 (dig-count wet))))))))

(deftest wet-stone-seen-is-reported-not-descended-past
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"3,64,0" "stone" "3,64,1" "water"} :inventory [{:name "iron_pickaxe" :count 1}]}
              s (await (scenario {:block "stone" :count 1 :tunnel-length 0} world 30))]
          (is (= :wet (:reason (done-event s))))
          (is (= 1 (:wet-skipped (done-event s))))
          (is (zero? (dig-count s))))))))

(deftest wet-stone-seen-with-a-tunnel-still-ends-wet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"3,64,0" "stone" "3,64,1" "water"} :inventory [{:name "iron_pickaxe" :count 1}]}
              s (await (scenario {:block "stone" :count 1 :tunnel-length 4} world 80))]
          (is (= :wet (:reason (done-event s))))
          (is (<= 1 (:wet-skipped (done-event s))))
          (is (re-find #"water.*:wet" (:text (done-event s)))))))))

(deftest lava-next-to-a-block-always-skips-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 2 :wet true :tunnel-length 0}
                                 {:blocks {"3,64,0" "sand" "6,64,0" "sand" "6,64,1" "lava"}} 30))]
          (is (= 1 (dig-count s)))
          (is (= :none (:reason (done-event s)))))))))

(deftest bedrock-is-skipped-and-not-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (cells "sand" [3 4 5 6 7 8] [64] [0])}})]
          (.override (.-world (:p s)) "dig" (fn ^:async f [_ _ _] #js {:status "cannot"}))
          (core/submit! (:eng s) (spec {:block "sand" :max-failures 2 :tunnel-length 0}) {})
          (await (run-ticks s 30))
          (is (= :none (:reason (done-event s))))
          (is (zero? (:failures (job-mem s) 0)))
          (is (empty? (events-of s :mine.gave-up)))
          (is (= 6 (dig-count s)) "each cell once")
          (is (finished? s)))))))

;; Juniper j747: ore a go-to judges has no stand beside it (a permanent verdict) is skipped; three of them were three failures.
(deftest unstandable-ore-is-skipped-without-counting-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sand (cells "sand" [6] [64] [-1 0 1 2])
              s (await (scenario {:block "sand" :count 4}
                                 {:blocks (merge floor sand) :unreachable (vec (keys sand))} 30))]
          (is (empty? (events-of s :mine.gave-up)))
          (is (= 4 (count (events-of s :mine.unreachable))))
          (is (zero? (dig-count s)))
          (is (finished? s)))))))

(deftest a-short-first-walk-is-partial-and-the-job-ends-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"15,64,0" "sand"}}})]
          (tu/short-walks! (:p s) 7)
          (core/submit! (:eng s) (spec {:block "sand" :tunnel-length 0}) {})
          (await (run-ticks s 1))
          (await (run-ticks s 20))
          (is (= "partial" (:status (first (moved s)))) "the first walk is cut short")
          (is (zero? (dig-count s)) "one cut-short walk is no walker fault to escalate")
          (is (= :none (:reason (done-event s))))
          (is (finished? s)))))))

(deftest the-cheapest-tool-is-equipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[block carried expected]
                [["sand" ["wooden_shovel" "iron_shovel"] "wooden_shovel"]
                 ["stone" ["wooden_pickaxe" "stone_pickaxe"] "wooden_pickaxe"]
                 ["oak_log" ["iron_axe" "diamond_pickaxe"] "iron_axe"]]]
          (let [s (start {:world {:blocks {"3,64,0" block} :inventory (mapv #(hash-map :name % :count 1) carried)}})]
            (core/submit! (:eng s) (spec {:block block :count 1}) {})
            (await (run-ticks s 10))
            (is (= expected (.-held (.self (:p s)))) block)
            (is (= 1 (count (calls s "equip"))) block)))))))

(deftest nothing-to-mend-with-is-reported-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "dirt" :item "rotten_flesh" :count 2}
                                 {:blocks floor :drops {"dirt" "rotten_flesh"}} 60))
              short-warns (events-of s :mine.mend-short)]
          (is (= 1 (count short-warns)))
          (is (= :warn (:level (first short-warns))))
          (is (some? (done-event s)))
          (is (zero? (count (calls s "place"))))
          (is (finished? s)))))))

(deftest the-result-is-handed-to-a-parent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s0 (start {:world {:blocks (merge wide-floor sand-patch)}})
              {:keys [eng clock]} s0
              out (atom nil)
              parent (tu/recording-parent 'jobs.gather.mine {:block "sand" :count 2} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'mining-parent parent))]
          (core/submit! eng '(mining-parent) {})
          (await (tu/run-ticks eng clock 120 700))
          (is (= :count (:reason @out)))
          (is (>= (:got @out) 2)))))))

(deftest the-drop-item-table-decides-what-is-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 2}
                                 {:blocks (cells "stone" [3 4] [64] [0]) :drops {"stone" "cobblestone"} :inventory [{:name "iron_pickaxe" :count 1}]} 30))]
          (is (>= (get (inv s) "cobblestone") 2))
          (is (nil? (get (inv s) "stone")))
          (is (= :count (:reason (done-event s)))))))))

(deftest only-the-feet-cell-owed-is-raised-with-jump-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; digging the cell under the start drops the item there; collecting it moves the body into the pit
        (let [s (await (scenario {:block "dirt" :count 1} {:blocks {"0,63,0" "dirt" "0,62,0" "dirt"}} 40))]
          (is (= 1 (count (calls s "jumpPlace"))))
          (is (zero? (count (calls s "place"))))
          (is (= "dirt" (.-item (.-args (first (calls s "jumpPlace"))))))
          (is (= 1 (:mended (done-event s))))
          (is (= "dirt" (block-at s 0 63 0)))
          (is (finished? s)))))))

(deftest six-failed-fills-warn-mend-failed-and-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks floor}})]
          (.override (.-world (:p s)) "place" (fn [_ _ _] (js/Promise.resolve #js {:status "no-support"})))
          ;; the body ends the dig beside the pit holding the item, so the owed cell is a place target, not the feet
          (.override (.-world (:p s)) "dig"
                     (fn [token args impl]
                       (.then (impl token args)
                              (fn [r]
                                (fake/add-item! (:p s) "dirt" 1)
                                (swap! (fake/state (:p s)) assoc :entities []) ; nothing left to collect
                                (set-pos! s 3 64 0)
                                r))))
          (core/submit! (:eng s) (spec {:block "dirt" :count 1}) {})
          (await (run-ticks s 80))
          (is (= 6 (count (calls s "place"))) "bounded at max-mend-failures")
          (is (= 1 (count (events-of s :mine.mend-failed))))
          (is (= :warn (:level (first (events-of s :mine.mend-failed)))))
          (is (empty? (events-of s :mine.mend-short)))
          (is (some? (done-event s)))
          (is (finished? s)))))))

(deftest mend-false-finishes-straight-from-the-dig-phase
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks floor}})]
          (tu/shutdown-at! s "dig" 1)
          (core/submit! (:eng s) (spec {:block "dirt" :count 2 :mend false}) {})
          (await (run-ticks s 1))
          (is (= [] (:ground (job-mem s))) "no ground is recorded")
          (await (run-ticks s 60))
          (is (= :count (:reason (done-event s))) "the dig reached its count; the walk home ends beside the open floor, not on it")
          (is (empty? (events-of s :mine.not-home)))
          (is (zero? (:mended (done-event s))))
          (is (zero? (count (calls s "place"))))
          (is (zero? (count (calls s "jumpPlace"))))
          (is (finished? s)))))))

(deftest a-mend-target-beyond-four-blocks-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks floor}})
              p (:p s)]
          ;; the dig takes the cell under the start and the body is left 10 blocks away holding the item
          (.override (.-world p) "dig"
                     (fn [token args impl]
                       (.then (impl token args)
                              (fn [r]
                                (fake/add-item! (:p s) "dirt" 1)
                                ;; the drop is gone too (the body was carried off), so the walk back is the mend's
                                (swap! (fake/state p) assoc :entities [])
                                (set-pos! s 10 64 0)
                                r))))
          ;; how many walks to the owed cell (y 63) were made when the first place came
          (let [walks-at-place (atom nil)]
            (.override (.-world p) "place"
                       (fn [token args impl]
                         (swap! walks-at-place #(or % (count (filter (fn [t] (= 63 (:y t))) (tu/walked-to (:eng s))))))
                         (impl token args)))
            (core/submit! (:eng s) (spec {:block "dirt" :count 1}) {})
            (await (run-ticks s 40))
            (is (pos? @walks-at-place) "walked to the owed cell before placing"))
          (let [names (mapv #(.-name %) (.-calls (.-world p)))
                first-place (.indexOf (clj->js names) "place")]
            (is (<= 0 first-place))
            (is (= 1 (:mended (done-event s))))
            (is (= "dirt" (block-at s 0 63 0)))))))))

;; ------------------------------------------------------------------ dry digs
(deftest digs-that-bring-nothing-end-no-drops-and-still-mend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 4}
                                 {:blocks (merge floor (cells "stone" [4 5 6 7 8 9] [64] [0])) :drops {"stone" nil} :inventory [{:name "iron_pickaxe" :count 1}]} 60))
              warns (events-of s :mine.gave-up)]
          (is (= 3 (dig-count s)))
          (is (= :no-drops (:reason (done-event s))))
          (is (= [:no-drops] (mapv :reason warns)))
          (is (= :warn (:level (first warns))))
          (is (= [[4 64 0] [5 64 0] [6 64 0]] (mapv :cell (events-of s :mine.drop-lost))) "each dig that brought nothing is told")
          (is (= [[4 64 0] [5 64 0] [6 64 0]] (:lost (done-event s))))
          (is (= "dirt" (block-at s 0 63 0)) "the ground is intact")
          (is (finished? s)))))))

(deftest no-drops-with-dug-ground-under-the-start-mends-it-from-the-carried-filler
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "dirt" :count 4}
                                 {:blocks floor :drops {"dirt" nil} :inventory [{:name "dirt" :count 10}]} 80))
              dug-ground (for [x (range -2 3) y [62 63] z (range -2 3)] (block-at s x y z))]
          (is (= :no-drops (:reason (done-event s))))
          (is (pos? (dig-count s)) "ground cells were dug")
          (is (pos? (:mended (done-event s))))
          (is (pos? (+ (count (calls s "place")) (count (calls s "jumpPlace")))) "the mend placed")
          (is (every? #(= "dirt" %) dug-ground) "the ground is solid again")
          (is (finished? s)))))))

(deftest a-custom-dry-digs-bound-is-honoured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 4 :dry-digs 2}
                                 {:blocks (cells "stone" [4 5 6 7 8 9] [64] [0]) :drops {"stone" nil} :inventory [{:name "iron_pickaxe" :count 1}]} 60))]
          (is (= 2 (dig-count s)))
          (is (= :no-drops (:reason (done-event s)))))))))

(deftest digs-that-bring-the-item-are-not-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 6 :dry-digs 1}
                                 {:blocks (merge (cells "stone" [4 5 6 7 8 9] [64] [0]) (cells floor-block [4 5 6 7 8 9] [63] [0])) :drops {"stone" "cobblestone"} :inventory [{:name "iron_pickaxe" :count 1}]} 80))]
          (is (= :count (:reason (done-event s))))
          (is (empty? (events-of s :mine.gave-up))))))))

(deftest nearer-intact-block-is-dug-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [r [0 0.5 0.999]]
          (let [s (await (scenario {:block "stone" :count 6 :dry-digs 1}
                                   {:blocks (merge (cells "stone" [4 5 6 7 8 9] [64] [0]) (cells floor-block [4 5 6 7 8 9] [63] [0])) :drops {"stone" "cobblestone"} :inventory [{:name "iron_pickaxe" :count 1}]} 80
                                   (constantly r)))
                order (mapv first (dug-cells s))]
            (is (= [4 5] (take 2 order)) (str "random " r))
            (is (= :count (:reason (done-event s))) (str "random " r))
            (is (empty? (events-of s :mine.gave-up)))))))))

(deftest drop-in-line-is-blocked-by-an-intact-block-between
  (let [p (tu/fake-on-floor {:blocks (cells "stone" [4 5] [64] [0]) :floor-block floor-block})
        c {:primitives p}]
    (is (true? (mine-targets/drop-in-line? c {:x 4 :y 64 :z 0})))
    (is (false? (mine-targets/drop-in-line? c {:x 5 :y 64 :z 0})))
    (is (true? (mine-targets/drop-in-line? c {:x 5 :y 64 :z 3})))))

(deftest exposed-ground-under-the-start-is-dug-last
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 6}
                                 {:blocks (merge stone-floor stone-hill) :drops cobble :inventory pickaxe} 80))]
          (is (= 6 (get (inv s) "cobblestone")))
          (is (empty? (dug-ground s)) "the floor under the start is never dug")
          (is (= :count (:reason (done-event s))))
          (is (= 6 (:got (done-event s)))))))))

(deftest the-mend-fills-with-other-filler-before-the-mined-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 2}
                                 {:blocks stone-floor :drops cobble
                                  :inventory (conj pickaxe {:name "dirt" :count 10})} 80))
              items (mapv #(.-item (.-args %)) (concat (calls s "place") (calls s "jumpPlace")))]
          (is (= :count (:reason (done-event s))))
          (is (pos? (count items)) "the mend placed")
          (is (every? #{"dirt"} items) "never the cobblestone")
          (is (= 2 (get (inv s) "cobblestone"))))))))

(deftest a-mend-that-spends-the-count-ends-spent-on-mend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 6}
                                 {:blocks stone-floor :drops cobble :inventory pickaxe} 120))
              done-ev (done-event s)]
          (is (= :spent-on-mend (:reason done-ev)))
          (is (= :count (:dig-reason done-ev)))
          (is (= (:got done-ev) (get (inv s) "cobblestone" 0)) "got is what is left")
          (is (every? #{"stone" "cobblestone"} (for [x (range -2 3) y [62 63] z (range -2 3)] (block-at s x y z))) "the floor is solid")
          (is (finished? s)))))))

(deftest a-mend-that-spends-the-count-looks-around-and-resumes-for-a-seen-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the only target at first is the ground; the mend spends the one cobblestone, and while it does a stone
        ;; turns up beside the start, which the look around after the mend sees
        (let [s (start {:world {:blocks stone-floor :drops cobble :inventory pickaxe}})
              p (:p s)]
          (doseq [act ["place" "jumpPlace"]]
            (.override (.-world p) act
                       (fn [token args impl]
                         (fake/set-block! p [3 64 0] "stone")
                         (impl token args))))
          (core/submit! (:eng s) (spec {:block "stone" :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 80))
          (is (= :count (:reason (done-event s))))
          (is (= 1 (:got (done-event s))))
          (is (= 1 (:resumes (done-event s))))
          (is (= [3 64 0] (last (dug-cells s))))
          (is (= 2 (dig-count s)) "a ground cell, then the stone that turned up")
          (is (finished? s)))))))

;; ------------------------------------------------------------------ no tool, no dig
(deftest stone-without-a-pickaxe-is-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (start {:world {:blocks (cells "stone" [3 4] [64] [0])}})
              out (atom nil)
              parent (tu/recording-parent 'jobs.gather.mine {:block "stone" :count 2} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'mining-parent parent))]
          (core/submit! eng '(mining-parent) {})
          (await (tu/run-ticks eng clock 10 700))
          (is (= {:status :stopped :got 0 :reason :no-tool :tool "pickaxe"} (select-keys @out [:status :got :reason :tool]))))))))
