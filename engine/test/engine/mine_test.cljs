(ns engine.mine-test
  "jobs.gather.mine against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.tools :as tools]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.gather.mine :as mine]))

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

(def floor (cells "dirt" (range -2 3) [62 63] (range -2 3)))
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
          (core/submit! (:eng s) (spec {:block "dirt" :count 4}) {})
          (await (run-ticks s 1))
          (let [m (job-mem s)]
            (is (= {:x 0 :y 64 :z 0} (:start m)))
            (is (= 50 (count (:ground m))))
            (is (= :dig (:phase m)))
            (is (= 4 (:goal m)))
            (is (= 0 (dig-count s)) "the snapshot comes first, in its own round")))))))

(deftest a-restart-still-mends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              s (start {:world {:blocks floor} :dir dir})]
          (core/submit! (:eng s) (spec {:block "dirt" :count 4}) {})
          (await (run-ticks s 6))
          (is (pos? (dig-count s)))
          (is (not (finished? s)))
          (let [again (start {:p (:p s) :dir dir})]
            (is (= 50 (count (:ground (core/job-memory (:eng again) "j1")))))
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
          (is (= 1 (dig-count dry)))
          (is (= :count (:reason (done-event wet))))
          (is (= 2 (dig-count wet))))))))

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
          (is (nil? (:level (first warns))))
          (is (zero? (dig-count s)))
          (is (finished? s)))))))

(deftest endless-partial-walks-skip-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"15,64,0" "sand"}}})]
          (tu/short-walks! (:p s) 7)
          (core/submit! (:eng s) (spec {:block "sand" :tunnel-length 0}) {})
          (await (run-ticks s 1))
          (is (zero? (:failures (job-mem s))) "failures start at 0")
          (await (run-ticks s 20))
          (is (= ["partial" "partial" "partial" "arrived"] (mapv :status (moved s))) "three walks, each cut short, then the walk home")
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
          (is (nil? (:level (first short-warns))))
          (is (some? (done-event s)))
          (is (zero? (count (calls s "place"))))
          (is (finished? s)))))))

(deftest the-result-is-handed-to-a-parent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (start {:world {:blocks sand-patch}})
              out (atom nil)
              parent {:check (constantly true)
                      :round (fn ^:async mining-parent [c]
                               (let [r (await (ctx/call-child c :kid 'jobs.gather.mine {:block "sand" :count 2}))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'mining-parent parent))]
          (core/submit! eng '(mining-parent) {})
          (dotimes [_ 40]
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
  (is (= "cobblestone" (mine/drop-item "stone")))
  (is (= "raw_iron" (mine/drop-item "deepslate_iron_ore")))
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
          (is (nil? (:level (first (events-of s :mine.mend-failed)))))
          (is (empty? (events-of s :mine.mend-short)))
          (is (some? (done-event s)))
          (is (finished? s)))))))

(deftest mend-false-finishes-straight-from-the-dig-phase
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks floor}})]
          (core/submit! (:eng s) (spec {:block "dirt" :count 2 :mend false}) {})
          (await (run-ticks s 1))
          (is (= [] (:ground (job-mem s))) "no ground is recorded")
          (await (run-ticks s 60))
          (is (= :count (:reason (done-event s))))
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
          (is (nil? (:level (first warns))))
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
          (is (= {:got 0 :reason :no-tool :tool "pickaxe"} @out)))))))

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
                                     [(ew/of-data {"pad" pad-plan} {} []) {"6,64,0" "sand"} {:zones [] :plans ["pad"]}]]]
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
          (is (= #{:fluid-adjacent :falling-block :under-feet} (:default (:accept mine/args)))))))))

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
        (let [s (await (scenario {:block "iron_ore" :count 1 :tunnel-length 0}
                                 (rock-world {"2,64,0" "iron_ore" "-4,64,0" "iron_ore" "-4,65,0" "air"}) 20))]
          (is (empty? (calls s "dig")))
          (is (= :none (:reason (done-event s))))
          (is (pos? (count (calls s "look"))) "it looked around first"))))))

(deftest seen-exposed-ore-is-dug-and-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :tunnel-length 0} (rock-world {"1,64,0" "iron_ore"}) 20))]
          (is (= [[1 64 0]] (dug-cells s)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s)))))))))

(deftest the-tunnel-exposes-ore-which-is-then-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :mend false}
                                 (rock-world {"3,64,1" "iron_ore"}) 80))
              cut (remove #{[3 64 1]} (dug-cells s))]
          (is (= "air" (block-at s 3 64 1)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= :count (:reason (done-event s))))
          (is (= [[1 65 0] [1 64 0] [2 65 0] [2 64 0] [3 65 0] [3 64 0]] cut) "a 1x2 run east, no further")
          (is (= {:heading "east" :steps 3} (select-keys (:tunnel (done-event s)) [:heading :steps])))
          (is (= [0 64 0] (feet s)) "back where it started")
          (is (= [3 64 1] (:end (:tunnel (done-event s)))) "where the tunnel ended, before the walk back")
          (is (true? (:walked-back? (:tunnel (done-event s)))))
          (is (= [0 64 0] (:back-at (:tunnel (done-event s)))))
          (is (re-find #"walked back to \[?0,64,0" (:text (done-event s)))))))))

(deftest the-tunnel-stops-before-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 10}
                                 (rock-world {"4,64,1" "lava"}) 80))]
          (is (every? #(< (first %) 4) (dug-cells s)) "no cut beside the lava")
          (is (= :tunnel-stopped (:reason (done-event s))))
          (is (= [{:reason :lava :at [4 64 1] :next [4 64 0]}] (map #(select-keys % [:reason :at :next]) (tunnel-ends s))))
          (is (= "lava" (block-at s 4 64 1))))))))

(deftest the-tunnel-length-bound-ends-the-job-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4} (rock-world {}) 80))]
          (is (finished? s))
          (is (= :tunnel-length (:reason (done-event s))))
          (is (= [{:reason :tunnel-length :length 4}] (map #(select-keys % [:reason :length]) (tunnel-ends s))))
          (is (= (set (for [x [1 2 3 4] y [64 65]] [x y 0])) (set (dug-cells s))))
          (is (= [0 64 0] (feet s))))))))

(deftest the-tunnel-runs-the-way-asked-or-the-way-the-body-faces
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args yaw cells-at] [[{:direction "north"} 0 [[0 -1] [0 -2]]]
                                     [{:direction :w} 0 [[-1 0] [-2 0]]]
                                     [{} 180 [[0 -1] [0 -2]]]
                                     [{} 270 [[1 0] [2 0]]]]]
          (let [s (await (scenario (merge {:block "iron_ore" :count 1 :tunnel-length 2} args)
                                   (assoc (rock-world {}) :yaw yaw) 40))]
            (is (= (set (for [[x z] cells-at y [64 65]] [x y z])) (set (dug-cells s))) (pr-str args yaw))))))))

(defn drop-away!
  "The fake's digs throw their drops to cell at (as a drop that bounced off), instead of the dug cell."
  [p at]
  (.override (.-world p) "dig"
             (fn [token args impl]
               (.then (impl token args)
                      (fn [r]
                        (swap! (fake/state p) update :entities
                               (fn [es] (mapv #(if (= "item" (:kind %)) (assoc % :pos at) %) es)))
                        r)))))

(deftest drops-that-land-away-are-walked-to-and-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})]
          (drop-away! (:p s) [5 64 2])
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (= 1 (get (inv s) "raw_iron")) "counted by what the inventory gained")
          (is (= :count (:reason (done-event s))))
          (is (empty? (events-of s :mine.left-behind))))))))

(deftest drops-out-of-reach-are-reported-left-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270
                                :unreachable ["5,64,2"]}})]
          (drop-away! (:p s) [5 64 2])
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (nil? (get (inv s) "raw_iron")))
          (is (= 0 (:got (done-event s))))
          (is (= [[{:pos [5 64 2] :count 1}]] (map :items (events-of s :mine.left-behind))))
          (is (= [{:pos [5 64 2] :count 1}] (:left (done-event s)))))))))

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
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (empty? (events-of s :mine.left-behind)))
          (is (nil? (:left (done-event s)))))))))

(deftest drops-picked-up-after-the-report-are-not-listed-left-at-the-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"8,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270
                                :unreachable ["5,64,2"]}})]
          (drop-away! (:p s) [5 64 2])
          ;; a later steer (the walk home) picks the drop up
          (.override (.-world (:p s)) "moveTo"
                     (fn [token args impl]
                       (when (= 0.5 (.-x (.-pos args)))
                         (swap! (fake/state (:p s)) assoc :entities []))
                       (impl token args)))
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :tunnel-length 0}) {})
          (await (run-ticks s 20))
          (is (= 1 (count (events-of s :mine.left-behind))) "reported when it was still lying")
          (is (nil? (:left (done-event s))) "but gone by the end"))))))

(deftest a-run-that-only-dug-targets-still-walks-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270}})]
          (drop-away! (:p s) [5 64 2])
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :tunnel-length 0}) {})
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
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra mended-some declined] [["Fake" {} true []] ["FAKE" {} true []]
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
          (drop-away! (:p s) [8 62 0])
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :tunnel-length 0 :collect-radius 3}) {})
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
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :tunnel-length 0 :collect-radius 3}) {})
          (await (run-ticks s 30))
          (is (nil? (get (inv s) "raw_iron")))
          (is (= [{:pos [8 62 0] :count 1}] (:left (done-event s)))))))))

;; ------------------------------------------------------------------ looking round while digging (card 943cac28)

(defn watched [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))

(deftest a-strip-tunnel-looks-round-between-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :mend false}
                                 (rock-world {"3,64,1" "iron_ore"}) 80))]
          (is (seq (watched s)) "the tunnel watched, lit or not: torches do not stop a creeper walking up it"))))))

(deftest a-vein-dig-in-a-lit-place-does-not-look-round-and-in-the-dark-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (await (scenario {:block "iron_ore" :count 1 :tunnel-length 0} (rock-world {"1,64,0" "iron_ore"}) 20))
              p (tu/fake-on-floor (assoc (rock-world {"1,64,0" "iron_ore"}) :floor-block floor-block))
              _ (swap! (fake/state p) assoc :light-default [0 0])
              dark (start {:p p})]
          (core/submit! (:eng dark) (spec {:block "iron_ore" :count 1 :tunnel-length 0}) {})
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
              s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 8 :mend false}
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
    (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 0 :mend false}) {})
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
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 22 :mend false}
                                 (torch-world torches8) 200))]
          (is (= [0 10 20] (torch-xs s)) "the entry, then every 10")
          (is (= 5 (get (inv s) "torch")))
          (is (empty? (events-of s :mine.no-torches)))
          (is (= :tunnel-length (:reason (done-event s)))))))))

(deftest the-torch-interval-is-an-arg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 9 :torch-interval 4 :mend false}
                                 (torch-world torches8) 200))]
          (is (= [0 4 8] (torch-xs s))))))))

(deftest the-end-of-the-cut-gets-a-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 6 :mend false}
                                 (torch-world torches8) 120))]
          (is (= [0 5] (torch-xs s)) "the entry, and the cut's end 5 blocks on"))))))

(deftest coal-and-sticks-are-crafted-into-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 12 :mend false}
                                 (torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "stick" :count 1}]) 300))]
          (is (= [0 10] (torch-xs s)))
          (is (= 2 (get (inv s) "torch")) "4 crafted, 2 hung")
          (is (nil? (get (inv s) "coal")))
          (is (empty? (events-of s :mine.no-torches))))))))

(deftest no-torches-and-no-materials-keeps-mining-with-one-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 22 :mend false}
                                 (torch-world pickaxe) 200))]
          (is (= [] (torch-xs s)))
          (is (= 1 (count (events-of s :mine.no-torches))))
          (is (= 22 (:steps (:tunnel (done-event s)))) "the tunnel went on")
          (is (= :tunnel-length (:reason (done-event s)))))))))
