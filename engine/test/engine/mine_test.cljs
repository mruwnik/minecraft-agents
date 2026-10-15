(ns engine.mine-test
  "jobs.gather.mine against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.blocks :as lb]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.tools :as tools]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
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
            [plan.shape :as shape]))

(defn spec [args] (list 'jobs.gather.mine args))

(declare as-child)

(def floor-block
  "The ground under the walks: a block no test here mines, so the floor never counts as a target (stone would)."
  "andesite")

(def sight-opts
  "The perception the fake body runs: a shorter radius and coarser rays than the body's, the same rules."
  {:radius 16 :ray-deg 2})

(defn seeing
  "Fake primitives p with the body's perception over them (engine.perception/wrap), as the body runs; p as it is
  when it already has one."
  [p & [rand]]
  (if (aget p "perception") p (perception/wrap p (perception/create (fake-raw/create p) (cond-> sight-opts rand (assoc :rand rand))))))

(defn start
  "An engine over primitives p (made from world, on a floor-block floor, when not given) on dir, seeing through
  perception; one sight pass first (what the body sees where it faces before the job; the job's own looks do the
  rest)."
  [{:keys [world p dir clock shared jobs rand]}]
  (let [clock (or clock (atom 1000000))
        [seen sink] (tu/legacy-capture-sink)
        p (seeing (or p (tu/fake-on-floor (-> world (assoc :floor-block floor-block) (update :inventory #(or % [{:name "torch" :count 10}]))))) rand)
        eng (core/create {:primitives p :jobs (or jobs registry/jobs) :triggers triggers/all :dir (or dir (tu/tmp-dir))
                          :now #(deref clock) :world shared
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (perception/pass! (aget p "perception"))
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-ticks
  [{:keys [eng clock]} n]
  (dotimes [_ n]
    (swap! clock + 700)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks; the setup map."
  [args world n & [rand]]
  (let [s (start {:world world :rand rand})]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :mine.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn job-mem [{:keys [eng]}] (core/job-memory eng "j1"))
(defn calls [{:keys [p]} name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn moved [{:keys [eng]}] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))
(defn dig-count [s] (count (calls s "dig")))
(defn dug-cells [s] (mapv #(let [p (.-pos (.-args %))] [(.-x p) (.-y p) (.-z p)]) (calls s "dig")))
(defn block-at [{:keys [p]} x y z] (some-> (.blockAt p (tu/pos x y z)) .-name))

(defn cells
  "{\"x,y,z\" block} for x in xs, y in ys, z in zs."
  [block xs ys zs]
  (into {} (for [x xs y ys z zs] [(str x "," y "," z) block])))

(def floor
  "Two layers of dirt under the start, on stone (the ground goes on below what the job digs)."
  (merge (cells "stone" (range -2 3) [61] (range -2 3)) (cells "dirt" (range -2 3) [62 63] (range -2 3))))
(def wide-floor
  "The floor out to x 12, for walks home from the far end of a patch."
  (cells "dirt" (range -2 13) [62 63] (range -3 4)))
(def sand-patch (cells "sand" (range 4 7) [64] (range -1 2)))

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

(deftest every-dig-goes-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 6} {:blocks (merge floor sand-patch)} 60))]
          (is (pos? (dig-count s)))
          (is (= (dig-count s) (count (events-of s :blocks.dig.done)))))))))

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
            (is (= 8 (count (calls s "look"))) note)))))))

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
          (is (zero? (dig-count s)))
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
              parent {:check (constantly true)
                      :round (fn ^:async mining-parent [c]
                               (let [r (await (ctx/call-child c :kid 'jobs.gather.mine {:block "sand" :count 2}))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'mining-parent parent))]
          (core/submit! eng '(mining-parent) {})
          (dotimes [_ 120]
            (swap! clock + 700)
            (await (core/tick! eng)))
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

(deftest the-drop-table-and-tools
  (is (= "cobblestone" (lb/drop-item "stone")))
  (is (= "raw_iron" (lb/drop-item "deepslate_iron_ore")))
  (is (= "shovel" (kind/tool-kind "sand")))
  (is (= "axe" (kind/tool-kind "oak_log")))
  (is (= "pickaxe" (kind/tool-kind "stone"))))

;; ------------------------------------------------------------------ mend branches

(defn set-pos! [{:keys [p]} x y z] (fake/swap-self! p assoc :pos [x y z]))

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

;; ------------------------------------------------------------------ the mend must not eat the count

(def bare-pickaxe [{:name "iron_pickaxe" :count 1}])
(def pickaxe
  "The pickaxe and enough torches that the job does not stock up first (jobs.lib.trip/torches-ready!)."
  (conj bare-pickaxe {:name "torch" :count 10}))
(def cobble {"stone" "cobblestone"})
(def stone-floor
  "A stone floor under the start (y 62, 63) on a dirt layer, so a body in a pit has support."
  (merge (cells "dirt" (range -2 3) [61] (range -2 3)) (cells "stone" (range -2 3) [62 63] (range -2 3))))
(def stone-hill (cells "stone" [5 6] [64 65] [0 1 2]))

(defn ground-cell? [{:keys [x y z]}] (and (<= -2 x 2) (<= -2 z 2) (#{62 63} y)))
(defn dug-ground [s] (filterv #(ground-cell? (js->clj (.-pos (.-args %)) :keywordize-keys true)) (calls s "dig")))

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
              parent {:check (constantly true)
                      :round (fn ^:async mining-parent [c]
                               (let [r (await (ctx/call-child c :kid 'jobs.gather.mine {:block "stone" :count 2}))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'mining-parent parent))]
          (core/submit! eng '(mining-parent) {})
          (dotimes [_ 10]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (= {:status :stopped :got 0 :reason :no-tool :tool "pickaxe"} (select-keys @out [:status :got :reason :tool]))))))))

(deftest no-tool-warns-and-never-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 2} {:blocks (cells "stone" [3 4] [64] [0]) :inventory [{:name "iron_shovel" :count 1}]} 10))
              warns (events-of s :mine.no-tool)]
          (is (zero? (dig-count s)))
          (is (= 1 (count warns)))
          (is (= ["pickaxe"] (mapv :tool warns)))
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
            (is (= :no-tool (:reason (done-event s))))
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
          (is (empty? (events-of s :mine.no-tool)))
          (is (= 2 (dig-count s)))
          (is (= :count (:reason (done-event s)))))))))

(deftest sand-without-a-shovel-is-still-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 2} {:blocks (cells "sand" [3 4] [64] [0])} 40))]
          (is (empty? (events-of s :mine.no-tool)))
          (is (= 2 (dig-count s)))
          (is (= :count (:reason (done-event s)))))))))

;; ------------------------------------------------------------------ zones and footprints

(def farm-zone {:name "farm" :min [2 60 -2] :max [4 70 2] :owner "Miles"})
(def pad-plan {:id "pad" :parts [{:id "p" :box [[6 64 0] [6 64 0]] :want "sand"}]})
(def two-in-two-out {"3,64,0" "sand" "3,64,1" "sand" "6,64,0" "sand" "6,64,1" "sand"})

(defn ^:async zoned
  "Submit the job with args in a fake world, sharing world w; run n ticks; the setup map."
  [args world w n]
  (let [s (start {:world world :shared w})]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n))
    s))


(deftest cells-in-a-zone-are-left-and-the-rest-are-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "sand" :count 4 :mend false :tunnel-length 0} {:blocks two-in-two-out}
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
        (let [s (await (zoned {:block "sand" :count 4 :mend false} {:blocks two-in-two-out}
                              (ew/of-data {} {} [(assoc farm-zone :allow #{:dig})]) 40))]
          (is (= 4 (count (dug-cells s))))
          (is (= :count (:reason (done-event s)))))))))

(deftest only-refused-targets-decline-once-naming-the-zone-or-plan-and-stay-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[w blocks expected] [[(ew/of-data {} {} [farm-zone]) {"3,64,0" "sand"} {:zones ["farm"] :plans []}]
                                     [(ew/of-data {"pad" pad-plan} {} []) {"6,64,0" "sand"} {:zones [] :plans ["pad"]}]
                                     [(ew/of-data {"pad" (shape/with-author pad-plan "Fake")} {} []) {"6,64,0" "sand"} {:zones [] :plans ["pad"]}]]]
          (let [s (await (zoned {:block "sand" :tunnel-length 0} {:blocks blocks :yaw 270} w 5))]
            (is (zero? (count (.-calls (.-world (:p s))))))
            (is (not (finished? s)))
            (is (= [(assoc expected :reason :refused)]
                   (map #(select-keys % [:reason :zones :plans]) (events-of s :mine.declined))))))))))

(deftest no-zone-list-declines-with-one-warn-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "sand"} {:blocks {"6,64,0" "sand"}} (ew/of-data {} {} nil) 5))]
          (is (empty? (calls s "dig")))
          (is (not (finished? s)))
          (is (= [:no-zones] (map :reason (events-of s :mine.declined)))))))))

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
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :mend false :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (= 2 (count (moved s))) "one walk, then the zone refuses the dig, then the walk home")
          (is (empty? (calls s "dig")))
          (is (= :refused (:reason (done-event s)))))))))

(def their-claim {:id "c9" :owner "Miles" :status :active :until 9999999999999 :min [5 60 -2] :max [7 70 2]})

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
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :mend false :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (empty? (calls s "dig")))
          (is (= [{:reason :claim :claim "c9" :owner "Miles"}]
                 (map #(select-keys % [:reason :claim :owner]) (events-of s :mine.refused)))))))))

(deftest hazards-not-accepted-are-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"3,64,0" "sand" "6,64,0" "sand" "6,64,1" "water"}}
              s (await (scenario {:block "sand" :count 2 :wet true :accept #{} :tunnel-length 0} world 30))]
          (is (= [[3 64 0]] (dug-cells s)))
          (is (= #{:fluid-adjacent :falling-block} (:default (:accept mine/args)))))))))

;; ------------------------------------------------------------------ seen targets and the strip tunnel

(def rock
  "Stone over x -9..9, y 62..67, z -9..9 with the body's cell (0,64,0) and the one over it air: a body in a pocket."
  (dissoc (cells "stone" (range -9 10) (range 62 68) (range -9 10)) "0,64,0" "0,65,0"))

(defn rock-world [extra]
  {:blocks (merge rock extra) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe})

(defn feet [{:keys [p]}] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))
(defn tunnel-ends [s] (events-of s :mine.tunnel-end))

(deftest ore-the-body-has-not-seen-is-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; buried ore one block past the pocket's wall, and exposed ore in a sealed cave: neither can be seen
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :tunnel-length 0}
                                 (rock-world {"2,64,0" "iron_ore" "-4,64,0" "iron_ore" "-4,65,0" "air"}) 20))]
          (is (empty? (calls s "dig")))
          (is (= :none (:reason (done-event s))))
          (is (pos? (count (calls s "look"))) "it looked around first"))))))

(deftest seen-exposed-ore-is-dug-and-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :tunnel-length 0} (rock-world {"1,64,0" "iron_ore"}) 20))]
          (is (= [[1 64 0]] (dug-cells s)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s)))))))))

(deftest a-drop-seen-but-never-taken-does-not-hold-the-mine
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :tunnel-length 0} (rock-world {"1,64,0" "iron_ore"}) 0))]
          (.override (.-world (:p s)) "collect" (fn [_ _ _] #js {:status "unreachable" :reason "no-path" :gained #js []}))
          (await (run-ticks s 80))
          (is (nil? (get (inv s) "raw_iron")))
          (is (finished? s) "the job ends, the pickup is not asked for ever"))))))

(deftest the-tunnel-exposes-ore-which-is-then-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :mend false}
                                 (rock-world {"3,64,1" "iron_ore"}) 80))
              cut (remove #{[3 64 1]} (dug-cells s))]
          (is (= "air" (block-at s 3 64 1)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s))))
          (is (= [[1 65 0] [1 64 0] [2 65 0] [2 64 0] [3 65 0] [3 64 0]] cut) "a 1x2 run east, no further")
          (is (= {:heading "east" :steps 3} (select-keys (:tunnel (done-event s)) [:heading :steps])))
          (is (= [0 64 0] (feet s)) "back where it started")
          (is (= [3 64 0] (:end (:tunnel (done-event s)))) "where the tunnel ended, before the walk back: the side ore pocket is picked from beside it")
          (is (true? (:walked-back? (:tunnel (done-event s)))))
          (is (= [0 64 0] (:back-at (:tunnel (done-event s)))))
          (is (re-find #"walked back to \[?0,64,0" (:text (done-event s)))))))))

(deftest the-tunnel-never-digs-a-chest-in-its-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 4 :mend false}
                                 (rock-world {"2,64,0" "chest"}) 80))]
          (is (= "chest" (block-at s 2 64 0)))
          (is (not-any? #{[2 64 0]} (dug-cells s))))))))

;; ------------------------------------------------------------------ open-start: where the strip tunnel starts over open cells

(defn open-line
  "name-at of an east-running corridor from x 0 at y 64: air at feet and head for xs, stone floor under floor-xs, rest unseen (nil)."
  [xs floor-xs & [extra]]
  (let [m (merge (into {} (for [x xs y [64 65]] [[x y 0] "air"]))
                 (into {} (for [x floor-xs] [[x 63 0] "stone"]))
                 extra)]
    (fn [{:keys [x y z]}] (get m [x y z]))))

(def east [1 0])
(defn start-x [name-at] (:x (mine-tunnel/open-start name-at {:x 0 :y 64 :z 0} east)))

(deftest open-start-runs-over-open-cells-with-a-floor
  (is (= 5 (start-x (open-line (range 0 6) (range 0 6))))))

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
        drop (- 64 (:y (mine-tunnel/open-start descending {:x 0 :y 64 :z 0} east)))]
    (is (pos? drop) "a stair still counts")
    (is (= (mine-tunnel/open-skip-drop) drop) "the origin sits the drop bound below the start")))

(def corridor (into {} (for [x (range 1 6) y [64 65]] [(str x "," y ",0") "air"])))

(deftest the-tunnel-skips-its-own-open-corridor-and-never-digs-its-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 2 :mend false}
                                 (rock-world (assoc corridor "3,65,0" "wall_torch")) 80))]
          (is (= [[6 65 0] [6 64 0] [7 65 0] [7 64 0]] (dug-cells s)) "new rock only, no torch")
          (is (= 2 (:length (first (tunnel-ends s))))))))))

(deftest the-tunnel-steps-down-a-stair-to-its-level
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 1 :mend false}
                                 (rock-world {"1,64,0" "air" "1,65,0" "air" "1,66,0" "air" "1,63,0" "air"}) 80))]
          (is (= [[2 63 0] [2 64 0]] (sort-by second (dug-cells s))) "cut at the lower level")
          (is (empty? (filter #(= :no-floor (:reason %)) (tunnel-ends s)))))))))

(defn ^:async bridge-run
  "A 3-long strip tunnel east over a gap in the floor at [1 63 0], carrying inv."
  [inv]
  (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 3 :mend false}
                   (assoc (rock-world {"1,63,0" "air" "2,63,0" "air"}) :inventory inv) 120)))

(defn no-floor-end? [s] (boolean (some #(= :no-floor (:reason %)) (tunnel-ends s))))

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
          (is (= [{:reason :lava :at [2 66 0] :next [2 64 0]}] (map #(select-keys % [:reason :at :next]) (tunnel-ends s)))
              "the stop names the lava")
          (is (= "lava" (block-at s 2 66 0))))))))

(deftest the-tunnel-seals-lava-a-cut-lays-open-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; lava behind the rock cannot be seen before the cut: the dig child seals it from the rim (fetching a block)
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 10}
                                 (rock-world {"5,64,0" "lava"}) 80))]
          (is (= 1 (count (events-of s :stair.sealed))))
          (is (not= "lava" (block-at s 5 64 0)))
          (is (= :tunnel-length (:reason (last (events-of s :mine.done))))))))))

(deftest the-tunnel-length-bound-ends-the-job-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 4} (rock-world {}) 80))]
          (is (finished? s))
          (is (= :tunnel-length (:reason (done-event s))))
          (is (= [{:reason :tunnel-length :length 4}] (map #(select-keys % [:reason :length]) (tunnel-ends s))))
          (is (= (set (for [x [1 2 3 4] y [64 65]] [x y 0])) (set (dug-cells s))))
          (is (= 4 (count (events-of s :stair.step))) "each step is a jobs.access.stair child")
          (is (= [0 64 0] (feet s))))))))

;; ------------------------------------------------------------------ ore depth and variants

(defn deep-rock
  "Stone over x -3..14, y 50..67, z -4..4 with the body's cell (0,64,0) and the one over it air, plus extra."
  [extra]
  {:blocks (merge (dissoc (cells "stone" (range -3 15) (range 50 68) (range -4 5)) "0,64,0" "0,65,0") extra)
   :drops {"iron_ore" "raw_iron" "deepslate_iron_ore" "raw_iron" "stone" "cobblestone" "gravel" "gravel"}
   :inventory (into pickaxe [{:name "torch" :count 8}])})

(deftest an-ore-has-a-tunnel-level-and-its-twin-kinds
  (is (= 16 (mine-descent/tunnel-level {:block "iron_ore"})))
  (is (= 16 (mine-descent/tunnel-level {:block "deepslate_iron_ore"})))
  (is (= 30 (mine-descent/tunnel-level {:block "iron_ore" :level 30})))
  (is (= -10 (mine-descent/tunnel-level {:block "sand" :level [5 -10 7]})) "a cell's y")
  (is (= -10 (mine-descent/tunnel-level {:block "sand" :level {:x 5 :y -10 :z 7}})))
  (is (nil? (mine-descent/tunnel-level {:block "iron_ore" :level false})) "false: the tunnel runs where the body stands")
  (is (nil? (mine-descent/tunnel-level {:block "sand"})))
  (is (= #{"iron_ore" "deepslate_iron_ore"} (mine-targets/kinds {:block "iron_ore" :variants true})))
  (is (= #{"iron_ore" "deepslate_iron_ore"} (mine-targets/kinds {:block "deepslate_iron_ore" :variants true})))
  (is (= #{"iron_ore"} (mine-targets/kinds {:block "iron_ore" :variants false})))
  (is (= #{"sand"} (mine-targets/kinds {:block "sand" :variants true}))))

(deftest a-seen-deepslate-twin-of-the-ore-is-dug-for-its-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :tunnel-length 0}
                                 (assoc-in (rock-world {"1,64,0" "deepslate_iron_ore"}) [:drops "deepslate_iron_ore"] "raw_iron") 20))]
          (is (= [[1 64 0]] (dug-cells s)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s)))))))))

;; ------------------------------------------------------------------ :stay-level

(deftest stay-level-tunnels-at-the-start-height-without-a-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 3 :stay-level true :mend false}
                                 (deep-rock {}) 120))]
          (is (seq (dug-cells s)))
          (is (every? #(#{64 65} (second %)) (dug-cells s)) "no cell below the start level")
          (is (= 3 (:length (first (tunnel-ends s))))))))))

(def far-cave
  "A cave corridor east of the body (x 1..6, y 64 and 65) with ore at its far end."
  (merge (cells "air" (range 1 7) [64 65] [0]) {"6,64,0" "iron_ore"}))

(deftest stay-level-leaves-ore-seen-across-a-cave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :tunnel-length 0 :stay-level true}
                                 (rock-world far-cave) 40))]
          (is (empty? (calls s "dig")))
          (is (= :none (:reason (done-event s)))))))))

(def side-cave "A cave pocket behind the wall beside the tunnel's second cell." {"2,64,1" "air" "2,65,1" "air"})

(deftest stay-level-seals-a-cut-that-opens-into-a-cave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :direction "east" :tunnel-length 6 :stay-level true :mend false}
                                 (update (rock-world side-cave) :inventory into [{:name "cobblestone" :count 5}]) 120))]
          (is (= :breach-sealed (:reason (first (tunnel-ends s)))) (pr-str (map :text (tunnel-ends s))))
          (is (= "cobblestone" (block-at s 2 64 0)))
          (is (= "cobblestone" (block-at s 2 65 0)))
          (is (not-any? #(> (first %) 2) (dug-cells s)))
          (is (not-any? #(= 2 (if (map? %) (:x %) (first %))) (tu/walked-to (:eng s))) "sealed from the line before stepping in"))))))

(defn ^:async facing-west-run
  "A stay-level mine with no heading argument, the body facing west, over rock-world extra with cobblestone carried."
  [extra length ticks]
  (let [s (start {:world (-> (rock-world extra) (assoc :yaw 90) (update :inventory into [{:name "cobblestone" :count 5}]))})]
    (core/submit! (:eng s) (spec {:block "iron_ore" :count 30 :tunnel-length length :stay-level true :mend false}) {})
    (await (run-ticks s ticks))
    s))

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
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 30 :tunnel-length 4 :level false :mend false}) {})
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
          (is (= "cobblestone" (block-at s -6 64 0)) "the breach stays sealed"))))))

(deftest stay-level-records-what-the-sealed-breach-showed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [air ["air" "cave_air"]]
          (let [s (await (scenario {:block "iron_ore" :direction "east" :tunnel-length 6 :stay-level true :mend false}
                                   (update (rock-world {"2,64,1" air "2,65,1" air}) :inventory into [{:name "cobblestone" :count 5}]) 120))
                behind {:names [air] :cells [[2 64 1] [2 65 1]]}]
            (is (= behind (:behind (first (tunnel-ends s)))))
            (is (= behind (:behind (first (:legs (done-event s))))))
            (is (= [behind] (map #(select-keys (:data %) [:names :cells]) (mem/entries (mem/view (:store (:eng s))) :breach-behind))))))))))

(defn ^:async stay-run
  "A stay-level mine east over rock-world extra with body memory (kind data pairs) written first; the setup."
  [extra memory]
  (let [s (start {:world (update (rock-world extra) :inventory into [{:name "cobblestone" :count 5}])})]
    (doseq [[kind data] memory] (mem/write! (:store (:eng s)) kind data))
    (core/submit! (:eng s) (spec {:block "iron_ore" :direction "east" :tunnel-length 4 :stay-level true :mend false}) {})
    (await (run-ticks s 120))
    s))

(deftest stay-level-takes-the-air-of-its-own-stair-for-no-breach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stay-run side-cave [[:stair-way {:dim "overworld" :floors [[2 63 1]]}]]))]
          (is (= [:tunnel-length] (mapv :reason (tunnel-ends s))) (pr-str (map :text (tunnel-ends s))))
          (is (= "air" (block-at s 2 64 1)) "nothing sealed into the stair"))))))

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

(defn ^:async side-ore-run
  "A stay-level mine east with :ore-reach reach in rock with ore at (1 65 -2) beside a pocket of air that the body sees through a one-cell window of its own earlier tunnel (no breach)."
  [reach]
  (let [s (start {:world (update (rock-world {"0,65,-1" "air" "0,65,-2" "air" "1,65,-2" "iron_ore"}) :inventory into [{:name "cobblestone" :count 5}])})]
    (mem/write! (:store (:eng s)) :tunnel-way {:dim "overworld" :origin [0 64 -1] :delta [0 -1] :steps 0})
    (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4 :stay-level true :ore-reach reach :mend false}) {})
    (await (run-ticks s 500))
    s))

(deftest stay-level-takes-ore-seen-two-blocks-into-the-wall-by-a-side-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (side-ore-run 2))]
          (is (= 1 (get (inv s) "raw_iron")) (pr-str (keep :text @(:seen s)) (dug-cells s) (feet s)))
          (is (= "cobblestone" (block-at s 1 64 -1)) "the side cut is shut behind"))))))

(defn shutdown-at-cell!
  "On the first dig of one of cells, the engine shuts down (the round is cut), as tu/shutdown-at!; the atom that says so."
  [{:keys [p eng]} cells]
  (let [done? (atom false)]
    (.override (.-world p) "dig"
               (fn [token a impl]
                 (let [pos (.-pos a)]
                   (if (and (not @done?) (contains? cells [(.-x pos) (.-y pos) (.-z pos)]))
                     (do (reset! done? true)
                         (core/shutdown! eng)
                         (js/Promise.reject (core/cut-error)))
                     (impl token a)))))
    done?))

(defn ^:async run-until!
  "run-ticks one at a time until the atom cut? is true, at most n ticks."
  [s cut? n]
  (loop [i 0]
    (when (and (< i n) (not @cut?))
      (await (run-ticks s 1))
      (recur (inc i)))))

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

(defn ^:async below-ore-run
  "A stay-level mine east, ore at (0 63 -1) one below a cell of the body's own earlier tunnel beside the start; drops as given."
  [drops]
  (let [s (start {:world (-> (rock-world {"0,64,-1" "air" "0,65,-1" "air" "0,63,-1" "iron_ore" "-1,64,0" "air" "-1,65,0" "air"})
                             (assoc :drops drops)
                             (update :inventory into [{:name "cobblestone" :count 5}]))})]
    (mem/write! (:store (:eng s)) :tunnel-way {:dim "overworld" :origin [0 64 -1] :delta [0 -1] :steps 0})
    (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 3 :stay-level true :mend false}) {})
    (await (run-ticks s 400))
    s))

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

(defn ^:async run-until
  "Run ticks until (done? s) or n ticks."
  [s n done?]
  (loop [i 0]
    (when (and (< i n) (not (done? s)))
      (await (run-ticks s 1))
      (recur (inc i)))))

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

(def shaft-plan
  "A mineshaft by Miles: its top is the body's start cell, a stair east down to y 60."
  (-> {:id "s" :kind :mineshaft :parts [{:id "stair" :stair {:from [0 64 0] :heading :east :to-y 60} :want :clear}]}
      (shape/with-author "Miles")))

(defn shafted
  "Submit args in a deep-rock world sharing plans and zones; run n ticks; the setup map."
  [args extra zones n]
  (zoned args (deep-rock extra) (ew/of-data {"s" shaft-plan} {} zones) n))

(def via-shaft {:block "iron_ore" :count 1 :level 60 :mend false})
(def ore-east {"10,60,0" "iron_ore"})

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
              e (second (events-of s :mine.done))]
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
              second-done (second (events-of s :mine.done))]
          (is (= {:shaft "s" :reason :unreachable} (select-keys no [:shaft :reason])))
          (is (not= [4 60 0] (:origin (:tunnel second-done))) "no tunnel dug from the stair bottom as if it were the branch start")
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

(defn drop-away!
  "The fake's digs throw their drops to cell at (as a drop that bounced off), instead of the dug cell (in sight)."
  [p at]
  (.override (.-world p) "dig"
             (fn [token args impl]
               (.then (impl token args)
                      (fn [r]
                        (swap! (fake/state p) update :entities
                               (fn [es] (mapv #(if (= "item" (:kind %)) (assoc % :pos at :visible true) %) es)))
                        r)))))

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

(defn hide-drop-until-beside!
  "The fake's dug drops lie out of sight (in a pocket) until the body stands within 1 of cell [x y z]."
  [p [cx cy cz]]
  (let [beside? (fn [w] (let [[x y z] (fake/body-pos w)
                              d (map - [(js/Math.floor x) y (js/Math.floor z)] [cx cy cz])]
                          (<= (reduce + (map * d d)) 1)))]
    (.override (.-world p) "dig"
               (fn [token args impl]
                 (.then (impl token args)
                        (fn [r]
                          (swap! (fake/state p) update :entities (fn [es] (mapv #(if (= "item" (:kind %)) (assoc % :visible false) %) es)))
                          r))))
    (doseq [walk ["moveTo" "steer"]]
      (.override (.-world p) walk
                 (fn [token args impl]
                   (.then (impl token args)
                          (fn [r]
                            (when (beside? @(fake/state p))
                              (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :visible true) es))))
                            r)))))))

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

(def walled-ore
  "Iron ore two cells east of the start behind a rock cell at feet height, its top seen over that cell; rock over the
  start leaves no room to step up, so no walk gets beside the ore."
  (-> (merge (cells floor-block [0 1 2 3] [63] [-1 0 1])
             (cells "stone" [1 2 3] [64 65] [-1 0 1])
             (cells "stone" [0] [66] [-1 0 1]))
      (dissoc "1,65,0" "2,65,0")
      (assoc "2,64,0" "iron_ore")))

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

;; ------------------------------------------------------------------ looking round while digging (card 943cac28)

(defn watched [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))

(deftest a-strip-tunnel-looks-round-between-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :mend false}
                                 (rock-world {"3,64,1" "iron_ore"}) 80))]
          (is (seq (watched s)) "the tunnel watched, lit or not: torches do not stop a creeper walking up it"))))))

(deftest a-vein-dig-in-a-lit-place-does-not-look-round-and-in-the-dark-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (await (scenario {:block "iron_ore" :level false :count 1 :tunnel-length 0} (rock-world {"1,64,0" "iron_ore"}) 20))
              p (tu/fake-on-floor (assoc (rock-world {"1,64,0" "iron_ore"}) :floor-block floor-block))
              _ (swap! (fake/state p) assoc :light-default [0 0])
              dark (start {:p p})]
          (core/submit! (:eng dark) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
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
              s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :mend false}
                                 (rock-world (merge open trench {"6,64,0" "iron_ore"})) 60))]
          (is (some #{[6 64 0]} (dug-cells s)) (pr-str (dug-cells s))))))))

(defn ^:async dark-ore-dug
  "A dark open corridor with ore 6 blocks east of the body (no tunnel), the self map merged in: the cells dug."
  [self]
  (let [world (rock-world (merge (cells "air" (range 1 6) [64 65] [0]) {"6,64,0" "iron_ore"}))
        p (tu/fake-on-floor (assoc world :floor-block floor-block))
        _ (swap! (fake/state p) update :self merge self)
        _ (swap! (fake/state p) assoc :light-default [0 0])
        s (start {:p p})]
    (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 0 :mend false}) {})
    (await (run-ticks s 120))
    (dug-cells s)))

(deftest a-torch-in-either-hand-lets-a-dark-ore-six-away-be-seen-and-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (some #{[6 64 0]} (await (dark-ore-dug {:offhand "torch"}))) "off hand")
        (is (some #{[6 64 0]} (await (dark-ore-dug {:held "torch"}))) "main hand")
        (is (not-any? #{[6 64 0]} (await (dark-ore-dug {}))) "no torch")))))

;; ------------------------------------------------------------------ torches

(def long-rock
  "Stone over x -9..40, y 62..67, z -9..9 with the body's pocket at the origin."
  (dissoc (cells "stone" (range -9 41) (range 62 68) (range -9 10)) "0,64,0" "0,65,0"))

(defn torch-world [inventory]
  {:blocks long-rock :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory inventory})

(defn torch-xs
  "The sorted x of the torches standing in the tunnel (z -1..1, y 64..65, x 0..40)."
  [s]
  (vec (sort (for [x (range 0 41) y [64 65] z [-1 0 1]
                   :when (#{"torch" "wall_torch"} (block-at s x y z))]
               x))))

(def torches8 [{:name "iron_pickaxe" :count 1} {:name "torch" :count 8}])

(deftest the-tunnel-hangs-a-torch-every-interval
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 22 :mend false}
                                 (torch-world torches8) 200))]
          (is (= [0 10 20] (torch-xs s)) "the entry, then every 10")
          (is (= 5 (get (inv s) "torch")))
          (is (empty? (events-of s :mine.no-torches)))
          (is (= :tunnel-length (:reason (done-event s)))))))))

;; the glance at the support before a torch is for support not yet seen: all is seen here, so hanging adds no looks
(deftest a-torch-on-seen-support-is-hung-without-a-glance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn [inv] (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 22 :mend false}
                                      (torch-world inv) 200))
              lit (await (run torches8))
              dark (await (run pickaxe))]
          (is (= [0 10 20] (torch-xs lit)))
          (is (= (count (calls dark "look")) (count (calls lit "look")))))))))

(deftest torches-held-in-the-off-hand-are-hung-and-none-crafted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 3} {:name "stick" :count 3}])
                           :equipment {"offHand" {:name "torch" :count 8}})
              s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 22 :mend false} world 200))]
          (is (= [0 10 20] (torch-xs s)) "hung from the off-hand stack")
          (is (empty? (events-of s :mine.no-torches)))
          (is (= 3 (get (inv s) "coal")) "nothing crafted"))))))

(deftest the-torch-interval-is-an-arg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 9 :torch-interval 4 :mend false}
                                 (torch-world torches8) 200))]
          (is (= [0 4 8] (torch-xs s))))))))

(deftest the-end-of-the-cut-gets-a-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 6 :mend false}
                                 (torch-world torches8) 120))]
          (is (= [0 5] (torch-xs s)) "the entry, and the cut's end 5 blocks on"))))))

(deftest coal-and-sticks-are-crafted-into-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :mend false}
                                 (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "stick" :count 1}]) 300))]
          (is (= [0 10] (torch-xs s)))
          (is (= 2 (get (inv s) "torch")) "4 crafted, 2 hung")
          (is (nil? (get (inv s) "coal")))
          (is (empty? (events-of s :mine.no-torches))))))))

(deftest coal-and-a-log-are-crafted-into-sticks-and-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :mend false}
                                 (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "oak_log" :count 1}]) 300))]
          (is (= [0 10] (torch-xs s)) "the first one hung")
          (is (pos? (get (inv s) "torch" 0)))
          (is (empty? (events-of s :mine.no-torches))))))))

(deftest fetch-false-with-coal-and-a-log-but-no-stick-leaves-the-tunnel-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :mend false :fetch false}
                                 (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "oak_log" :count 1}]) 300))]
          (is (= [] (torch-xs s)))
          (is (= 1 (count (events-of s :mine.no-torches)))))))))

(deftest no-torches-and-no-materials-keeps-mining-with-one-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 22 :mend false}
                                 (torch-world bare-pickaxe) 200))]
          (is (= [] (torch-xs s)))
          (is (= 1 (count (events-of s :mine.no-torches))))
          (is (= 22 (:steps (:tunnel (done-event s)))) "the tunnel went on")
          (is (= :tunnel-length (:reason (done-event s)))))))))

;; an open room beside the tunnel's 5th..10th cells, ore on its far wall: unseen until the tunnel reaches it
(def side-room (merge (cells "air" (range 4 12) [64 65] (range 1 10)) {"10,64,8" "iron_ore"}))

(defn side-torch-cells
  "The cells of torches standing off the tunnel line (z 3..9, y 64..65, x 4..11)."
  [s]
  (vec (for [x (range 4 12) y [64 65] z (range 3 10)
             :when (#{"torch" "wall_torch"} (block-at s x y z))]
         [x y z])))

(deftest a-dig-of-seen-ore-4-or-more-off-the-tunnel-hangs-a-torch-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :mend false}
                                 (assoc (torch-world torches8) :blocks (merge long-rock side-room)) 200))]
          (is (some #{[10 64 8]} (dug-cells s)) "the ore was dug")
          (is (seq (side-torch-cells s)) "a torch hangs on the branch"))))))

(deftest a-branch-dig-with-no-torch-leaves-the-branch-dark-and-still-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :mend false}
                                 (assoc (torch-world bare-pickaxe) :blocks (merge long-rock side-room)) 200))]
          (is (some #{[10 64 8]} (dug-cells s)))
          (is (empty? (side-torch-cells s))))))))

(defn ^:async branch-torches
  "The torch cells off the tunnel line (z 2..) after a run digging iron ore at the cells in a side room (air, z 1..the
  farthest ore, x 4..15), with the run's args."
  [ores args]
  (let [zmax (apply max (map last ores))
        room (merge (cells "air" (range 4 16) [64 65] (range 1 (inc zmax)))
                    (into {} (for [[x y z] ores] [(str x "," y "," z) "iron_ore"])))
        s (await (scenario (merge {:block "iron_ore" :level false :count (count ores) :direction "east" :tunnel-length 16 :mend false} args)
                           (assoc (torch-world torches8) :blocks (merge long-rock room)) 300))]
    {:dug (dug-cells s)
     :torches (vec (for [x (range 4 16) y [64 65] z (range 2 (inc zmax))
                         :when (#{"torch" "wall_torch"} (block-at s x y z))]
                     [x y z]))}))

(deftest a-dig-3-off-the-tunnel-line-gets-no-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [dug torches]} (await (branch-torches [[5 64 3]] {}))]
          (is (some #{[5 64 3]} dug) "the ore was dug")
          (is (empty? torches)))))))

(deftest a-second-branch-dig-within-4-of-the-first-torch-gets-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [dug torches]} (await (branch-torches [[10 64 8] [9 64 8]] {}))]
          (is (every? (set dug) [[10 64 8] [9 64 8]]))
          (is (= 1 (count torches))))))))

(deftest a-branch-dig-4-or-more-from-the-first-branch-torch-gets-its-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [dug torches]} (await (branch-torches [[4 64 8] [15 64 8]] {}))]
          (is (every? (set dug) [[4 64 8] [15 64 8]]))
          (is (= 2 (count torches))))))))

(deftest torch-interval-0-hangs-no-branch-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [dug torches]} (await (branch-torches [[10 64 8]] {:torch-interval 0}))]
          (is (some #{[10 64 8]} dug))
          (is (empty? torches)))))))

(deftest the-branch-torch-site-is-none-straight-above-or-below-the-tunnel
  (let [tunnel {:origin {:x 0 :y 64 :z 0} :heading "east" :steps 8}]
    (is (nil? (mine-torches/branch-site tunnel {:x 5 :y 70 :z 0})) "straight above: no side to hang on")
    (is (nil? (mine-torches/branch-site tunnel {:x 5 :y 58 :z 0})) "straight below")
    (is (= {:dir [0 1] :site {:x 5 :y 64 :z 3}} (mine-torches/branch-site tunnel {:x 5 :y 64 :z 4})) "off to the side: the cell behind")))

(deftest the-no-torch-text-names-a-branch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :mend false}
                                 (assoc (torch-world bare-pickaxe) :blocks (merge long-rock side-room)) 200))]
          (is (= [false true] (mapv #(boolean (re-find #"branch stays dark" (:text %))) (events-of s :mine.no-torches)))
              (pr-str (mapv :text (events-of s :mine.no-torches)))))))))

;; ------------------------------------------------------------------ honest ends, soil, fetching the pickaxe

(defn ^:async as-child
  "Run the job with args under a contract parent (ctx/call) for n ticks; {:s setup :out the child's ending :fetch the get-tool calls}.
  get-tool is a stub that records its args and, when give is set, hands over that pickaxe (else stops :no-source)."
  [args world give n & [{:keys [move-to before jobs]}]]
  (let [s (start {:world world})
        _ (when before (before s))
        out (atom nil)
        fetched (atom [])
        stub {:ways [:chest :craft :gather] :check (constantly true)
              :round (fn ^:async get-tool [c]
                       (swap! fetched conj (:args c))
                       (if give
                         (do (fake/add-item! (:p s) give 1)
                             (when move-to (swap! (fake/state (:p s)) assoc-in [:self :pos] move-to))
                             (ctx/result! c {:status :done :tool give}))
                         (ctx/result! c {:status :stopped :reason :no-source}))
                       :done)
              :args {:block {:default nil} :item {:default nil} :kind {:default nil} :how {:default nil} :depth {:default nil}
                     :minutes {:default nil} :fail-minutes {:default nil} :chain {:default []} :explore {:default true}}}
        parent {:check (constantly true)
                :round (fn ^:async mining-parent [c]
                         (let [e (await (ctx/call c :kid 'jobs.gather.mine args))]
                           (if (= :continue e) :continue (do (reset! out e) :done))))}
        eng (assoc (:eng s) :jobs (merge (assoc (:jobs (:eng s)) 'mining-parent parent 'jobs.items.get-tool stub) jobs))]
    (core/submit! eng '(mining-parent) {})
    (dotimes [_ n]
      (swap! (:clock s) + 700)
      (await (core/tick! eng)))
    {:s s :out @out :fetch @fetched}))

(deftest getting-none-of-the-block-is-stopped-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (as-child {:block "iron_ore" :level false :count 2 :tunnel-length 0} (rock-world {}) nil 30))]
          (is (= [:condition-failed :none 0] [(:kind out) (contract/cause-key out) (:got (:data out))])))))))

(deftest a-run-that-meets-the-least-of-its-call-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[least kind] [[1 :done] [nil :condition-failed]]]
          (let [{:keys [out]} (await (as-child (cond-> {:block "iron_ore" :level false :count 2 :tunnel-length 0 :mend false} least (assoc :least least))
                                               (rock-world {"1,64,0" "air" "1,65,0" "air" "2,64,0" "iron_ore"}) nil 60))]
            (is (= kind (:kind out)) (pr-str least out))
            (is (= 1 (:got (:data out))) (pr-str least))))))))

(deftest a-tunnel-used-up-with-nothing-got-ends-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 2 :mend false} (rock-world {}) nil 120))]
          (is (= [:nothing-possible :tunnel-length 0] [(:kind out) (contract/cause-key out) (:got (:data out))]) (pr-str out)))))))

(deftest a-missing-pickaxe-is-fetched-then-the-stone-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1} {:blocks {"3,64,0" "stone"} :drops cobble} "stone_pickaxe" 40))]
          (is (= ["stone"] (mapv :block fetch)))
          (is (= [:done :count] [(:kind out) (:reason (:data out))]))
          (is (= 1 (get (inv s) "cobblestone"))))))))

(deftest the-tunnel-starts-where-the-body-stood-before-the-pickaxe-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [aisle (-> rock (dissoc "1,64,0" "2,64,0" "1,65,0" "2,65,0"))
              {:keys [s]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "west" :tunnel-length 3 :mend false}
                                           {:blocks aisle :drops {"iron_ore" "raw_iron" "stone" "cobblestone"}}
                                           "stone_pickaxe" 120 {:move-to [2.5 64 0.5]}))]
          (is (= [0 64 0] (:origin (:tunnel (done-event s)))) "the origin is the cell the job began on, not the fetch spot"))))))

(deftest a-pickaxe-that-cannot-be-fetched-ends-stopped-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1} {:blocks {"3,64,0" "stone"} :drops cobble} nil 20))]
          (is (= 1 (count fetch)))
          (is (= [:condition-failed :no-tool "pickaxe" 0] [(:kind out) (contract/cause-key out) (:tool (:cause out)) (:got (:data out))]) (pr-str out))
          (is (zero? (dig-count s))))))))

(deftest a-failed-pickaxe-fetch-is-the-because-of-the-no-tool-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (as-child {:block "stone" :count 1} {:blocks {"3,64,0" "stone"} :drops cobble} nil 20))]
          (is (= :no-tool (contract/cause-key out)))
          (is (= :no-source (contract/cause-key (:because out))) (pr-str out)))))))

(deftest a-bad-fetch-arg-is-refused-at-submit-not-treated-as-true
  (is (re-find #"jobs.gather.mine :fetch must be true, false"
               (expr/problem registry/jobs '(jobs.gather.mine {:block "stone" :count 1 :fetch "please"})))))

(deftest a-bad-direction-is-refused-at-submit
  (is (re-find #"jobs.gather.mine :direction must be"
               (expr/problem registry/jobs '(jobs.gather.mine {:block "stone" :count 1 :direction "up"}))))
  (is (nil? (expr/problem registry/jobs '(jobs.gather.mine {:block "stone" :count 1 :direction :w})))))

(deftest fetch-false-ends-no-tool-without-asking-get-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1 :fetch false} {:blocks {"3,64,0" "stone"} :drops cobble} "stone_pickaxe" 20))]
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
          (core/submit! (:eng s) (spec {:block "stone" :count 1 :direction "east" :tunnel-length 3 :descend-limit 0 :mend false}) {})
          (await (run-ticks s 60))
          (is (pos? (dig-count s)) "it did dig the soil")
          (is (nil? (.-held (.self (:p s)))) "by hand")
          (is (= 1 (count (calls s "unequip")))))))))

(defn soil-over-stone
  "Dirt from y top-down to bottom+1 over stone below, in a block wide enough for a stair east."
  [top bottom]
  (merge (cells "stone" (range -3 13) (range (- bottom 4) (inc bottom)) (range -3 4))
         (cells "dirt" (range -3 13) (range (inc bottom) (inc top)) (range -3 4))))

(deftest stone-under-soil-is-reached-by-a-stair-down-then-mined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 59) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :mend false}) {})
          (await (run-ticks s 200))
          (is (<= 2 (get (inv s) "cobblestone" 0)) "the ore is kept")
          (is (= :count (:reason (done-event s))))
          (is (= [0 64 0] (feet s)) "the body is back on its start cell")
          (is (empty? (events-of s :mine.not-home)))
          (let [names (mapv #(.-name %) (.-calls (.-world (:p s))))
                soil-digs (filter #(< 59 (nth % 1)) (dug-cells s))]
            (is (seq soil-digs) "it dug soil")
            (is (< -1 (.indexOf names "unequip") (.indexOf names "equip")) "the hand was emptied before the pickaxe came out")))))))

;; soil over stone wide enough for a stair in any of the four headings
(def soil-all-round (merge (cells "stone" (range -8 9) (range 55 60) (range -8 9)) (cells "dirt" (range -8 9) (range 60 64) (range -8 9))))

;; Sorrel j23: the crafting table the kit just placed stood in the first stair step of the faced heading.
(deftest a-table-in-the-first-step-of-the-faced-heading-turns-the-stair-to-another
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (merge soil-all-round {"-1,64,0" "crafting_table"}) :drops cobble :yaw 90
                                :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :tunnel-length 4 :mend false}) {})
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
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :tunnel-length 4 :mend false}) {})
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
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :heading "west" :tunnel-length 4 :mend false}) {})
          (await (run-ticks s 300))
          (is (empty? (events-of s :mine.turned)))
          (is (= :no-stone-found (:reason (done-event s)))))))))

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
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :mend false}) {})
          (await (run-ticks s 300))
          (is (empty? (events-of s :mine.restair)) "the stair walks back onto its line itself")
          (is (<= 2 (get (inv s) "cobblestone" 0)))
          (is (= :count (:reason (done-event s)))))))))

;; Rowan j119: mining stone from the pit at the foot of a short stair; the body must get home (escalation may dig out).
(deftest mining-at-the-foot-of-its-stair-still-gets-the-body-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 61) :drops cobble :inventory pickaxe}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 20 :direction "east" :tunnel-length 0 :mend false}) {})
          (await (run-ticks s 400))
          (is (finished? s))
          (is (empty? (events-of s :mine.not-home)) "the walk home arrives")
          (is (= [0 64 0] (feet s)) "the body is back on its start cell"))))))

;; Rowan j119: mine dug the step-up cells and floors round its pit until the body stood on a one-cell column.
(deftest mine-leaves-the-block-that-is-the-bodys-last-way-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"1,63,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe}})]
          (doseq [c [[-1 63 0] [0 63 1] [0 63 -1]]] (fake/set-block! (:p s) c "air"))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0 :mend false}) {})
          (await (run-ticks s 40))
          (is (= "iron_ore" (block-at s 1 63 0)) "the only floor to step onto is left")
          (is (seq (events-of s :mine.spared-exit))))))))

(deftest soil-deeper-than-the-descent-limit-ends-no-stone-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (soil-over-stone 63 40) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (core/submit! (:eng s) (spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :descend-limit 3 :mend false}) {})
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
              s (await (zoned {:block "stone" :count 2 :direction "east" :tunnel-length 4 :mend false}
                              {:blocks (soil-over-stone 63 40) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]} w 200))]
          (is (= :own-zone (:reason (done-event s))))
          (is (= ["home"] (mapv :zone (events-of s :mine.own-zone))) "the warn names the zone")
          (is (every? #(< (first %) 8) (dug-cells s))))))))

(deftest mine-spares-cells-of-the-bodys-own-plan-unless-told-not-to-or-it-is-a-mineshaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[kind extra n] [[nil {} 0] [nil {:spare-own-builds false} 1] [:mineshaft {} 1]]]
          (let [own (ew/of-data {"pad" (cond-> (shape/with-author pad-plan "Fake") kind (assoc :kind kind))} {} [])
                s (await (zoned (merge {:block "sand" :count 1 :mend false :tunnel-length 0} extra) {:blocks {"6,64,0" "sand"}} own 20))]
            (is (= n (count (dug-cells s))) (pr-str kind extra))))))))

(defn ^:async descent-run
  "A stair-down mine (mend on) of soil over stone; {:s setup :go-to the args of each go-to call}."
  [n]
  (let [s (start {:world {:blocks (soil-over-stone 63 59) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})
        go-tos (atom [])
        real (get-in (:eng s) [:jobs 'jobs.movement.go-to])
        spy (assoc real :round (fn [c] (swap! go-tos conj (:args c)) ((:round real) c)))
        eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'jobs.movement.go-to spy))]
    (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
    (core/submit! eng (spec {:block "stone" :count 1 :direction "east" :tunnel-length 4}) {})
    (dotimes [_ n]
      (swap! (:clock s) + 700)
      (await (core/tick! eng)))
    {:s s :go-to @go-tos}))

(deftest after-a-descent-the-walk-home-is-a-go-to-and-ends-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s go-to]} (await (descent-run 300))]
          (is (finished? s))
          (is (= :count (:reason (done-event s))))
          (is (seq go-to) "home by a go-to child")
          (is (= [16] (distinct (map :y-margin (filter #(= 64 (:y (:pos %))) go-to)))) "the walk home plans no deep way round")
          (is (= [0 64 0] (feet s)) "the body is back on its start cell")
          (is (empty? (events-of s :mine.not-home))))))))

(deftest the-walk-home-bounds-its-search-only-near-under-the-start
  (is (= {:y-margin 16} (mine-end/home-margin 6)) "a start near above: no way round through deep caves")
  (is (nil? (mine-end/home-margin 16)) "from far below go-to's own climb rules hold, which an explicit :y-margin switches off"))

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

;; ------------------------------------------------------------------ a way out

(defn stair-down-east
  "Stone over x -6..12, y 50..63, z -3..3 with a stair cut east from the surface cell (0,64,0): step i stands at (i, 64-i, 0)."
  [steps]
  (apply dissoc (cells "stone" (range -6 13) (range 50 64) (range -3 4))
         (for [i (range 1 (inc steps)) dy [0 1 2]] (str i "," (+ (- 64 i) dy) ",0"))))

;; Juniper j108: a tunnel run from the foot of a stair back under it would cut its lowest steps' floors.
(deftest a-tunnel-under-the-stair-it-came-down-stops-before-it-cuts-the-way-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (stair-down-east 4) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe
                                :self {:pos [4.5 60 0.5]}}})]
          (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors (vec (for [i (range 1 5)] [i (- 63 i) 0]))})
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :heading "west" :tunnel-length 6 :mend false :torch-interval 0}) {})
          (await (run-ticks s 200))
          (is (finished? s))
          (is (= "stone" (block-at s 3 60 0)) "the floor of the lowest step is not cut")
          (is (= [:undercuts-way] (mapv :reason (tunnel-ends s))) "the tunnel ends saying why")
          (is (= [4 60 0] (feet s))))))))

;; ------------------------------------------------------------------ unseen faces are no hazard

(defn classify-in
  "mine-targets/classify of the sand at [0 64 3] (in view) with extra blocks, after the start's sight pass."
  [extra]
  (let [s (start {:world {:blocks (merge {"0,64,3" "sand"} extra)}})]
    (mine-targets/classify {:primitives (:p s)} false {:x 0 :y 64 :z 3})))

(deftest lava-behind-the-block-is-unseen-so-no-hazard
  (is (= :ok (classify-in {"0,63,3" "lava"})) "lava behind the block is not known")
  (is (= :no (classify-in {"0,65,3" "lava"})) "lava on top shows as drips: known")
  (is (= :no (classify-in {"0,64,2" "lava"})) "lava in the open face, seen")
  (is (= :buried (classify-in {"1,64,3" "stone" "-1,64,3" "stone" "0,65,3" "stone" "0,63,3" "stone" "0,64,4" "stone" "0,64,2" "stone"}))))

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
            (fn ^:async b [] (core/submit! (:eng s) (spec {:block "sand" :tunnel-length 0}) {})
            (await (run-ticks s 6)))))
          (is (seq @opts))
          (is (every? :zone-tolls @opts))
          (is (zero? (:failures (job-mem s) 0)) "a waiting walk is no failure")
          (is (zero? (dig-count s))))))))

(defn ^:async walk-options
  "The options of the walks a sand mine makes in six ticks, with the stair memory written first when stair-way is given."
  [stair-way]
  (let [s (start {:world {:blocks {"15,64,0" "sand"}}})
        opts (atom [])]
    (some->> stair-way (mem/write! (:store (:eng s)) :stair-way))
    (await (tu/with-near-stub (fn ^:async f [_ _ _ o] (swap! opts conj o) :partial)
             (fn ^:async b [] (core/submit! (:eng s) (spec {:block "sand" :tunnel-length 0}) {})
               (await (run-ticks s 6)))))
    @opts))

(deftest a-mine-started-on-its-own-stair-walks-on-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts (await (walk-options {:dim "overworld" :floors (vec (for [y (range 58 68)] [0 y 0]))}))]
          (is (seq opts))
          (is (= [:go] (distinct (map :at-night opts)))))))))

(deftest a-mine-started-off-its-stair-keeps-the-default-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts (await (walk-options nil))]
          (is (seq opts))
          (is (= [nil] (distinct (map :at-night opts)))))))))

(deftest no-tool-names-the-kind-the-block-needs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "cobweb" :count 2} {:blocks (cells "cobweb" [3 4] [64] [0]) :inventory []} 10))]
          (is (zero? (dig-count s)))
          (is (= ["sword"] (mapv :tool (events-of s :mine.no-tool)))))))))

(deftest never-digs-the-cell-under-the-feet-onto-an-unseen-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"0,63,0" "sand" "0,62,0" "lava"}
                                :inventory [{:name "cobblestone" :count 2}]}})
              ys (atom [])]
          (add-watch (fake/state (:p s)) ::ys (fn [_ _ _ w] (swap! ys conj (second (get-in w [:self :pos])))))
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :tunnel-length 0}) {})
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
        (let [s (await (scenario {:block "sand" :count 1 :tunnel-length 0 :max-failures 1 :mend false}
                                 {:floor [0 0 0 0] :blocks {"0,63,0" "sand" "0,62,0" "stone"}} 20))]
          (is (empty? (dug-cells s)) "a pillar top is never dug from on top")
          (is (= [{:x 0 :y 63 :z 0}] (mapv :pos (events-of s :mine.refused))))
          (is (not= :failures (:reason (done-event s))) "no dig failure is booked"))))))

(deftest the-walk-home-does-not-aim-at-a-start-cell-whose-floor-the-mend-left-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"0,63,0" "sand" "0,62,0" "lava"}}})
              ys (atom [])]
          (add-watch (fake/state (:p s)) ::ys (fn [_ _ _ w] (swap! ys conj (second (get-in w [:self :pos])))))
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 30))
          (is (every? #(>= % 63) @ys) "the body never goes down onto the open hole")
          (is (not-any? #(= {:x 0.5 :y 64 :z 0.5} (select-keys (:target %) [:x :y :z])) (moved s))
              "no raw move onto the start cell")
          (is (empty? (events-of s :mine.not-home)) "ending beside the hole is the normal end")
          (is (let [[x y z] (feet s)] (and (<= (abs x) 1) (<= (abs z) 1) (>= y 64))) "the body ends on a standable cell beside the start"))))))

(defn spy-go-to
  "s with its engine's go-to job spied: [s' atom-of-the-args-of-each-go-to-round]."
  [s]
  (let [go-tos (atom [])
        real (get-in (:eng s) [:jobs 'jobs.movement.go-to])
        spy (assoc real :round (fn [c] (swap! go-tos conj (:args c)) ((:round real) c)))]
    [(assoc s :eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'jobs.movement.go-to spy))) go-tos]))

(defn home-go-tos "The go-to calls that aim at the start cell or a cell next to it: the walk home."
  [go-tos]
  (filterv #(let [{:keys [x y z]} (:pos %)] (and (= 64 y) (<= (+ (abs (- x 0.5)) (abs (- z 0.5))) 1))) go-tos))

(deftest the-walk-home-from-a-patch-is-one-go-to-onto-the-start-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[s go-tos] (spy-go-to (start {:world {:blocks (merge wide-floor sand-patch)}}))]
          (core/submit! (:eng s) (spec {:block "sand" :count 6 :mend false}) {})
          (await (run-ticks s 120))
          (is (seq (home-go-tos @go-tos)) "home by a go-to child, not a raw moveTo")
          (is (= #{0} (set (map :range (home-go-tos @go-tos)))) "onto the start cell: range 0 on every round of the walk")
          (is (= #{16} (set (map :y-margin (home-go-tos @go-tos)))) "a start near above: no way round through deep caves")
          (is (= [0 64 0] (feet s)))
          (is (finished? s)))))))

(deftest the-go-to-args-of-the-walk-home-stay-the-same-while-the-walk-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[s go-tos] (spy-go-to (start {:world {:blocks {"0,63,0" "sand" "0,62,0" "lava"}}}))]
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 30))
          (is (seq (home-go-tos @go-tos)))
          (is (= 1 (count (set (home-go-tos @go-tos)))) "one set of args for the whole walk home"))))))

(deftest beside-counts-only-a-standable-cell-next-to-the-start-like-go-to-range-1
  (let [start {:x 0 :y 64 :z 0}]
    (is (mine-end/beside? {:x 1 :y 64 :z 0} start))
    (is (mine-end/beside? {:x 0 :y 64 :z -1} start))
    (is (mine-end/beside? {:x 0 :y 65 :z 0} start) "above the start is standable")
    (is (not (mine-end/beside? {:x 0 :y 63 :z 0} start)) "inside the dug hole is not home")
    (is (not (mine-end/beside? {:x 1 :y 63 :z 0} start)))
    (is (not (mine-end/beside? {:x 1 :y 64 :z 1} start)) "a diagonal is out of go-to's range 1")
    (is (not (mine-end/beside? {:x 2 :y 64 :z 0} start)))))

(deftest a-start-floor-felt-at-the-start-decides-when-it-is-not-seen-now
  (is (mine-end/solid-floor? nil "dirt") "unseen now: the felt floor counts")
  (is (not (mine-end/solid-floor? nil nil)) "never known: no place to aim at")
  (is (not (mine-end/solid-floor? nil "air")))
  (is (not (mine-end/solid-floor? "air" "dirt")) "seen open now wins over the felt floor")
  (is (mine-end/solid-floor? "dirt" "air")))

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
          (is (= {:cell "air" :floor "stone"} (select-keys (:walk e) [:cell :floor])))
          (is (re-find #"walk stopped unreachable, cell air, floor stone" (:text e))))))))

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
          (is (= 0.5 (:health (:cause (first waits)))))
          (is (re-find #"heal" (str (:text (:cause (first waits))))))
          (swap! (.. world -state) assoc-in [:self :health] 20)
          (await (run-ticks s 60))
          (is (>= (get (inv s) "sand") 6) "healed, it mines"))))))

(deftest a-started-mine-at-low-health-holds-still-till-it-heals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 30} {:blocks (merge wide-floor (cells "sand" (range 4 10) [64 65] (range -2 3)))} 0))
              world (.-world (:p s))
              _ (loop [n 0] (when (and (< n 40) (zero? (dig-count s))) (await (run-ticks s 1)) (recur (inc n))))
              dug (dig-count s)]
          (is (< 0 dug 30) "the job is under way")
          (swap! (.. world -state) assoc-in [:self :health] 2)
          (await (run-ticks s 10))
          (is (= dug (dig-count s)) "no dig after the body got hurt")
          (is (= [:low-health] (distinct (map :reason (events-of s :holding)))))
          (is (not (finished? s)) "the run waits, it does not end"))))))

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
          (let [stub {:check (constantly true)
                      :round (fn [c] (ctx/result! c {:status :stopped :arrived false :reason :unreachable :why why}) :done)}
                s (start {:world {:blocks (cells "sand" [15] [64] [-1 0 1 2])} :jobs (assoc registry/jobs 'jobs.movement.go-to stub)})]
            (core/submit! (:eng s) (spec {:block "sand" :count 4 :tunnel-length 0 :mend false}) {})
            (await (run-ticks s 60))
            (is (empty? (events-of s :mine.gave-up)) (str why))
            (is (= 4 (count (events-of s :mine.unreachable))) (str why))))))))


;; Juniper j754: the walk to an ore waited for day for ever (go-to at night); mine gave the round back for 12 minutes.
(deftest a-walk-that-keeps-waiting-is-skipped-after-the-stall-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [waiting {:check (constantly true) :round (fn ^:async w [_] :continue)}
              s (start {:world {:blocks {"15,64,0" "sand"}} :jobs (assoc registry/jobs 'jobs.movement.go-to waiting)})]
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :tunnel-length 0 :mend false}) {})
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
              s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 4 :mend false} w 80))
              held (mapv #(.-item (.-args %)) (calls s "equip"))]
          (is (seq held))
          (is (every? #{"stone_pickaxe"} held) (pr-str held)))))))

(deftest a-pickaxe-that-breaks-in-the-tunnel-says-tool-broke
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (rock-world {}) :inventory [{:name "stone_pickaxe" :count 1 :durability 4 :max 131}])
              s (start {:world w})
              world (.-world (:p s))
              n (atom 0)]
          (.override world "dig"
                     (fn ^:async f [token a impl]
                       (let [r (await (impl token a))]
                         (when (= 3 (swap! n inc))
                           (swap! (.. world -state) assoc :inventory []))
                         r)))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :mend false}) {})
          (await (run-ticks s 120))
          (is (= 1 (count (events-of s :tool.broke))))
          (is (= 1 (count (events-of s :tool.none)))))))))

;; ------------------------------------------------------------------ an own zone is left before the first dig

(def home-zone {:name "home" :min [-3 60 -3] :max [3 70 3] :owner "Fake"})

(deftest the-body-walks-out-of-its-own-zone-before-it-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [home-zone])
              s (await (zoned {:block "sand" :count 2 :mend false :tunnel-length 0} {:floor [-30 -30 40 30] :blocks {"6,64,11" "sand" "7,64,11" "sand"}} w 60))
              left (first (events-of s :mine.left-zone))
              first-dig (first (dug-cells s))]
          (is (= "home" (:zone left)))
          (is (= 2 (count (dug-cells s))))
          (is (every? (fn [[_ _ z]] (> z 9)) (dug-cells s)))
          (is (some? first-dig)))))))

(deftest ignoring-zones-digs-from-inside-the-own-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [home-zone])
              s (await (zoned {:block "sand" :count 1 :mend false :tunnel-length 0 :ignore-zones? true} {:blocks {"2,64,0" "sand"}} w 20))]
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
          (is (= [:own-zone] (map :reason (events-of s :mine.tunnel-end)))))))))

(def no-shaft-args {:block "iron_ore" :count 1 :direction "east" :level 58 :mend false})
(def deep-ore (deep-rock {"9,58,1" "iron_ore"}))

(deftest a-fresh-stair-with-no-shaft-suggests-declaring-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario no-shaft-args deep-ore 400))
              e (done-event s)
              {:keys [at heading to-y] :as suggest} (:shaft-suggest e)]
          (is (= {:at [0 64 0] :heading :east} (select-keys suggest [:at :heading])))
          (is (< to-y 64) "the bottom of the stair")
          (is (re-find (re-pattern (str "declare it with plans.mjs mineshaft <id> --at=0,64,0 --heading east --to-y " to-y)) (:text e)))
          (is (empty? (shape/plan-errors {:id "x" :kind :mineshaft :parts [{:id "stair" :stair {:from at :heading heading :to-y to-y} :want :clear}]} "x"))
              "the suggestion is a valid shaft"))))))

(deftest a-shaft-in-use-or-shafts-off-suggest-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [used (await (shafted via-shaft ore-east [] 400))
              off (await (scenario (assoc no-shaft-args :shaft false) deep-ore 400))]
          (is (nil? (:shaft-suggest (done-event used))))
          (is (nil? (:shaft-suggest (done-event off))))
          (is (empty? (events-of off :mine.no-shaft)) "false never looks a shaft up"))))))

(deftest shaft-required-with-none-usable-stops-before-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario (assoc no-shaft-args :shaft :required) deep-ore 400))
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
        (let [s (await (shafted (assoc via-shaft :shaft :required) ore-east [] 400))]
          (is (= :count (:reason (done-event s)))))))))

(deftest mine-digs-an-exposed-ore-within-reach-of-its-stand-and-reports-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks (merge floor sand-patch {"-1,64,1" "coal_ore"}) :inventory [{:name "wooden_pickaxe" :count 1}]}
              on (await (scenario {:block "sand" :count 6} world 60))
              off (await (scenario {:block "sand" :count 6 :pick-up-ores false} world 60))]
          (is (= "air" (block-at on -1 64 1)))
          (is (= [[-1 64 1]] (mapv :cell (:ores (done-event on)))))
          (is (= "coal_ore" (block-at off -1 64 1)) "opted out"))))))

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
    (is (= [:block :heading] (:target def)))
    (is (seq (:conditions def)))
    (is (nil? (:check def)))
    (is (nil? (contract/quote-problem q)) (pr-str q))
    (is (= [0 3 3] (get-in q [:yields :items "sand"])))
    (is (fn? (:done? def)))))

(deftest hunger-ends-limit-reached-on-the-food-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (as-child {:block "sand" :count 6} {:blocks (merge floor sand-patch) :self {:food 2}} nil 5
                                             {:before #(mem/write! (:store (:eng %)) :hungry {:food 2} {:cap 10 :ttl 3600000})}))]
          (is (= [:limit-reached :food :hungry] [(:kind out) (:line (:cause out)) (contract/cause-key out)]) (pr-str out)))))))

(deftest a-run-that-got-its-count-but-not-home-ends-nothing-possible-not-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [setup (atom nil)
              real (get-in (start {:world {}}) [:eng :jobs 'jobs.movement.go-to])
              no-walk (assoc real :round (fn ^:async go-to [c]
                                   (if (< (:x (:pos (:args c))) 2)
                                     (do (swap! (fake/state (:p @setup)) assoc-in [:self :pos] [6.5 64 0.5])
                                         (ctx/end c :nothing-possible {:reason :unreachable} {}))
                                     (await ((:round real) c)))))
              {:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :tunnel-length 0}
                                             {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270} nil 300
                                             {:jobs {'jobs.movement.go-to no-walk} :before #(reset! setup %)}))]
          (is (= [:nothing-possible :not-home 1] [(:kind out) (contract/cause-key out) (:got (:data out))]) (pr-str out)))))))

(deftest a-pickaxe-that-breaks-in-the-dig-phase-with-none-to-fetch-ends-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (rock-world {}) :inventory [{:name "stone_pickaxe" :count 1 :durability 4 :max 131}])
              n (atom 0)
              {:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :mend false} w nil 120
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
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4 :stay-level true :ore-reach 2 :mend false :from [-6 64 0]}) {})
          (await (run-ticks s 500))
          (is (= 1 (get (inv s) "raw_iron")) (pr-str (keep :text @(:seen s)))))))))
