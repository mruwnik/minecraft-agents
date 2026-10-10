(ns engine.mine-test
  "jobs.gather.mine against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.blocks :as lb]
            [jobs.lib.tools :as tools]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.expr :as expr]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.gather.mine :as mine]
            [plan.shape :as shape]))

(defn spec [args] (list 'jobs.gather.mine args))

(def floor-block
  "The ground under the walks: a block no test here mines, so the floor never counts as a target (stone would)."
  "andesite")

(def sight-opts
  "The perception the fake body runs: a shorter radius and coarser rays than the body's, the same rules."
  {:radius 16 :ray-deg 2})

(defn seeing
  "Fake primitives p with the body's perception over them (engine.perception/wrap), as the body runs; p as it is
  when it already has one."
  [p]
  (if (aget p "perception") p (perception/wrap p (perception/create (fake-raw/create p) sight-opts))))

(defn start
  "An engine over primitives p (made from world, on a floor-block floor, when not given) on dir, seeing through
  perception; one sight pass first (what the body sees where it faces before the job; the job's own looks do the
  rest)."
  [{:keys [world p dir clock shared]}]
  (let [clock (or clock (atom 1000000))
        [seen sink] (tu/legacy-capture-sink)
        p (seeing (or p (tu/fake-on-floor (assoc world :floor-block floor-block))))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (or dir (tu/tmp-dir))
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
  [args world n]
  (let [s (start {:world world})]
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

(deftest the-check-declines-with-no-block-named
  (let [{:keys [eng p]} (start {:world {:blocks sand-patch}})]
    (core/submit! eng (spec {}) {})
    (is (nil? (core/tick! eng)))
    (is (zero? (count (.-calls (.-world p)))))))

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

(deftest blocked-walks-give-up-and-still-mend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sand (cells "sand" [6] [64] [-1 0 1 2])
              s (await (scenario {:block "sand" :count 4}
                                 {:blocks (merge floor sand) :unreachable (vec (keys sand))} 30))
              warns (events-of s :mine.gave-up)]
          (is (= :gave-up (:reason (done-event s))))
          (is (= 1 (count warns)))
          (is (= :warn (:level (first warns))))
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
  (is (= "shovel" (tools/tool-kind "sand")))
  (is (= "axe" (tools/tool-kind "oak_log")))
  (is (= "pickaxe" (tools/tool-kind "stone"))))

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

(defn with-random
  "Run async f with js/Math.random pinned to r, restored after."
  [r f]
  (let [orig js/Math.random]
    (set! js/Math.random (fn [] r))
    (-> (f) (.finally (fn [] (set! js/Math.random orig))))))

(deftest nearer-intact-block-is-dug-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [r [0 0.5 0.999]]
          (let [s (await (with-random r
                           #(scenario {:block "stone" :count 6 :dry-digs 1}
                                      {:blocks (merge (cells "stone" [4 5 6 7 8 9] [64] [0]) (cells floor-block [4 5 6 7 8 9] [63] [0])) :drops {"stone" "cobblestone"} :inventory [{:name "iron_pickaxe" :count 1}]} 80)))
                order (mapv first (dug-cells s))]
            (is (= [4 5] (take 2 order)) (str "random " r))
            (is (= :count (:reason (done-event s))) (str "random " r))
            (is (empty? (events-of s :mine.gave-up)))))))))

(deftest drop-in-line-is-blocked-by-an-intact-block-between
  (let [p (tu/fake-on-floor {:blocks (cells "stone" [4 5] [64] [0]) :floor-block floor-block})
        c {:primitives p}]
    (is (true? (mine/drop-in-line? c {:x 4 :y 64 :z 0})))
    (is (false? (mine/drop-in-line? c {:x 5 :y 64 :z 0})))
    (is (true? (mine/drop-in-line? c {:x 5 :y 64 :z 3})))))

;; ------------------------------------------------------------------ the mend must not eat the count

(def pickaxe [{:name "iron_pickaxe" :count 1}])
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
          (is (= {:status :stopped :got 0 :reason :no-tool :tool "pickaxe"} @out)))))))

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
(defn start-x [name-at] (:x (mine/open-start name-at {:x 0 :y 64 :z 0} east)))

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
  (is (= mine/max-open-skip (start-x (open-line (range 0 (+ mine/max-open-skip 10)) (range 0 (+ mine/max-open-skip 10)))))))

(deftest open-start-steps-down-a-stair-but-not-past-the-drop-bound
  (let [descending (fn [{:keys [x y z]}]
                     (when (zero? z)
                       (let [floor (- 63 x)]
                         (cond (= y floor) "stone" (> y floor) "air"))))
        drop (- 64 (:y (mine/open-start descending {:x 0 :y 64 :z 0} east)))]
    (is (pos? drop) "a stair still counts")
    (is (= (mine/open-skip-drop) drop) "the origin sits the drop bound below the start")))

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
          (is (= 1 (count (events-of s :blocks.dig.sealed))))
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
          (is (= [0 64 0] (feet s))))))))

;; ------------------------------------------------------------------ ore depth and variants

(defn deep-rock
  "Stone over x -3..14, y 50..67, z -4..4 with the body's cell (0,64,0) and the one over it air, plus extra."
  [extra]
  {:blocks (merge (dissoc (cells "stone" (range -3 15) (range 50 68) (range -4 5)) "0,64,0" "0,65,0") extra)
   :drops {"iron_ore" "raw_iron" "deepslate_iron_ore" "raw_iron" "stone" "cobblestone" "gravel" "gravel"}
   :inventory (into pickaxe [{:name "torch" :count 8}])})

(deftest an-ore-has-a-tunnel-level-and-its-twin-kinds
  (is (= 16 (mine/tunnel-level {:block "iron_ore"})))
  (is (= 16 (mine/tunnel-level {:block "deepslate_iron_ore"})))
  (is (= 30 (mine/tunnel-level {:block "iron_ore" :level 30})))
  (is (= -10 (mine/tunnel-level {:block "sand" :level [5 -10 7]})) "a cell's y")
  (is (= -10 (mine/tunnel-level {:block "sand" :level {:x 5 :y -10 :z 7}})))
  (is (nil? (mine/tunnel-level {:block "iron_ore" :level false})) "false: the tunnel runs where the body stands")
  (is (nil? (mine/tunnel-level {:block "sand"})))
  (is (= #{"iron_ore" "deepslate_iron_ore"} (mine/kinds {:block "iron_ore" :variants true})))
  (is (= #{"iron_ore" "deepslate_iron_ore"} (mine/kinds {:block "deepslate_iron_ore" :variants true})))
  (is (= #{"iron_ore"} (mine/kinds {:block "iron_ore" :variants false})))
  (is (= #{"sand"} (mine/kinds {:block "sand" :variants true}))))

(deftest a-seen-deepslate-twin-of-the-ore-is-dug-for-its-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :tunnel-length 0}
                                 (assoc-in (rock-world {"1,64,0" "deepslate_iron_ore"}) [:drops "deepslate_iron_ore"] "raw_iron") 20))]
          (is (= [[1 64 0]] (dug-cells s)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s)))))))))

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

(deftest what-a-cut-leaves-tells-an-unseen-cell-from-a-fallen-block
  (let [world {[1 65 0] "air" [1 64 0] "cave_air" [2 65 0] "gravel"}
        cut [{:x 1 :y 65 :z 0} {:x 1 :y 64 :z 0}]
        after #(mine/after-cut (fn [{:keys [x y z]}] (get % [x y z])) cut)]
    (is (= :clear (after world)))
    (is (= :refilled (after (assoc world [1 64 0] "gravel"))))
    (is (= :unseen (after (dissoc world [1 64 0]))) "not yet seen: look again, do not cut")
    (is (= :unseen (after (-> world (dissoc [1 64 0]) (assoc [1 65 0] "sand")))))))

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
          (is (= [[{:pos [5 64 2] :count 1}]] (map :items (events-of s :mine.left-behind))))
          (is (= [{:pos [5 64 2] :count 1}] (:left (done-event s)))))))))

(deftest a-drop-out-of-sight-from-where-it-was-dug-is-walked-to-and-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})
              p (:p s)
              near-dug? (fn [w] (let [[x y z] (fake/body-pos w)] (<= (+ (* (- (js/Math.floor x) 3) (- (js/Math.floor x) 3)) (* (- y 64) (- y 64)) (* (js/Math.floor z) (js/Math.floor z))) 1)))]
          ;; dug from where it stands (within reach), the drop lies out of sight in a pocket; beside the dug cell it is in sight
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
                                  (when (near-dug? @(fake/state p))
                                    (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :visible true) es))))
                                  r)))))
          (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 30))
          (is (= 1 (get (inv s) "raw_iron")) "the body walked beside the dug cell and picked the drop up")
          (is (= :count (:reason (done-event s))))
          (is (empty? (events-of s :mine.gave-up))))))))

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
                                 (torch-world pickaxe) 200))]
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
                                 (assoc (torch-world pickaxe) :blocks (merge long-rock side-room)) 200))]
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
    (is (nil? (mine/branch-site tunnel {:x 5 :y 70 :z 0})) "straight above: no side to hang on")
    (is (nil? (mine/branch-site tunnel {:x 5 :y 58 :z 0})) "straight below")
    (is (= {:dir [0 1] :site {:x 5 :y 64 :z 3}} (mine/branch-site tunnel {:x 5 :y 64 :z 4})) "off to the side: the cell behind")))

(deftest the-no-torch-text-names-a-branch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :mend false}
                                 (assoc (torch-world pickaxe) :blocks (merge long-rock side-room)) 200))]
          (is (= [false true] (mapv #(boolean (re-find #"branch stays dark" (:text %))) (events-of s :mine.no-torches)))
              (pr-str (mapv :text (events-of s :mine.no-torches)))))))))

;; ------------------------------------------------------------------ honest ends, soil, fetching the pickaxe

(defn ^:async as-child
  "Run the job with args under a parent for n ticks; {:s setup :out the child's result :fetch the get-tool calls}.
  get-tool is a stub that records its args and, when give is set, hands over that pickaxe (else stops :no-source)."
  [args world give n & [{:keys [move-to]}]]
  (let [s (start {:world world})
        out (atom nil)
        fetched (atom [])
        stub {:check (constantly true)
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
                         (let [r (await (ctx/call-child c :kid 'jobs.gather.mine args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'mining-parent parent 'jobs.items.get-tool stub))]
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
          (is (= {:status :stopped :got 0 :reason :none} (dissoc out :text))))))))

(deftest a-missing-pickaxe-is-fetched-then-the-stone-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1} {:blocks {"3,64,0" "stone"} :drops cobble} "stone_pickaxe" 40))]
          (is (= ["stone"] (mapv :block fetch)))
          (is (= :count (:reason out)))
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
          (is (= {:status :stopped :got 0 :reason :no-tool :tool "pickaxe"} out))
          (is (zero? (dig-count s))))))))

(deftest a-failed-pickaxe-fetch-is-booked-through-the-fetch-lib
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out]} (await (as-child {:block "stone" :count 1} {:blocks {"3,64,0" "stone"} :drops cobble} nil 20))]
          (is (= :no-tool (:reason out)))
          (is (= 1 (count (events-of s :fetch.failed)))))))))

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
          (is (= {:status :stopped :got 0 :reason :no-tool :tool "pickaxe"} out))
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
          (is (every? #(< (first %) 8) (dug-cells s))))))))

(deftest mine-spares-cells-of-the-bodys-own-plan-unless-told-not-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [own (ew/of-data {"pad" (shape/with-author pad-plan "Fake")} {} [])]
          (doseq [[extra n] [[{} 0] [{:spare-own-builds false} 1]]]
            (let [s (await (zoned (merge {:block "sand" :count 1 :mend false :tunnel-length 0} extra) {:blocks {"6,64,0" "sand"}} own 20))]
              (is (= n (count (dug-cells s))) (pr-str extra)))))))))

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
          (is (= [0 64 0] (feet s)) "the body is back on its start cell")
          (is (empty? (events-of s :mine.not-home))))))))

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

;; ------------------------------------------------------------------ unseen faces are no hazard

(defn classify-in
  "mine/classify of the sand at [0 64 3] (in view) with extra blocks, after the start's sight pass."
  [extra]
  (let [s (start {:world {:blocks (merge {"0,64,3" "sand"} extra)}})]
    (mine/classify {:primitives (:p s)} false {:x 0 :y 64 :z 3})))

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
    (is (mine/beside? {:x 1 :y 64 :z 0} start))
    (is (mine/beside? {:x 0 :y 64 :z -1} start))
    (is (mine/beside? {:x 0 :y 65 :z 0} start) "above the start is standable")
    (is (not (mine/beside? {:x 0 :y 63 :z 0} start)) "inside the dug hole is not home")
    (is (not (mine/beside? {:x 1 :y 63 :z 0} start)))
    (is (not (mine/beside? {:x 1 :y 64 :z 1} start)) "a diagonal is out of go-to's range 1")
    (is (not (mine/beside? {:x 2 :y 64 :z 0} start)))))

(deftest a-start-floor-felt-at-the-start-decides-when-it-is-not-seen-now
  (is (mine/solid-floor? nil "dirt") "unseen now: the felt floor counts")
  (is (not (mine/solid-floor? nil nil)) "never known: no place to aim at")
  (is (not (mine/solid-floor? nil "air")))
  (is (not (mine/solid-floor? "air" "dirt")) "seen open now wins over the felt floor")
  (is (mine/solid-floor? "dirt" "air")))

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
          (is (= :walk-failed (:reason e)))
          (is (= [2 64 0] (:at e)))
          (is (= {:status "blocked" :reason "noPath"} (select-keys (:walk e) [:status :reason])))
          (is (= {:cell "air" :floor "stone"} (select-keys (:walk e) [:cell :floor])))
          (is (re-find #"blocked noPath" (:text e))))))))

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

(deftest a-cut-mine-resumed-at-low-health-waits
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
          (is (= [:low-health] (distinct (map :reason (events-of s :waiting))))))))))

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
