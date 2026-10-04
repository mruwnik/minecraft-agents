(ns engine.mine-test
  "jobs.gather.mine against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.jobs.tools :as tools]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as ew]
            [jobs.gather.mine :as mine]))

(defn spec [args] (list 'jobs.gather.mine args))

(defn start
  "An engine over primitives p (made from world when not given) on dir."
  [{:keys [world p dir clock shared]}]
  (let [clock (or clock (atom 1000000))
        [seen sink] (tu/legacy-capture-sink)
        p (or p (tu/fake world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (or dir (tu/tmp-dir))
                          :now #(deref clock) :world shared
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
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
(defn dig-count [s] (count (calls s "dig")))
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

(deftest the-check-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args world note]
                [[{} {:blocks sand-patch} "no block named"]
                 [{:block "sand"} {} "nothing at all"]
                 [{:block "sand"} {:blocks {"2,64,0" "dirt"}} "other blocks"]
                 [{:block "sand" :radius 5} {:blocks {"9,64,0" "sand"}} "outside the radius"]
                 [{:block "stone" :buried false} {:blocks (merge (into {} (for [[dx dy dz] [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
                                                              [(str (+ 4 dx) "," (+ 64 dy) "," dz) "dirt"]))
                                                   {"4,64,0" "stone"})}
                  "buried"]]]
          (let [{:keys [eng p]} (start {:world world})]
            (core/submit! eng (spec args) {})
            (is (nil? (core/tick! eng)) note)
            (is (zero? (count (.-calls (.-world p)))) note)))))))

(deftest wet-targets-are-skipped-unless-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"3,64,0" "sand" "6,64,0" "sand" "6,64,1" "water"}}
              dry (await (scenario {:block "sand" :count 2} world 30))
              wet (await (scenario {:block "sand" :count 2 :wet true} world 30))]
          (is (= :wet (:reason (done-event dry))))
          (is (= 1 (dig-count dry)))
          (is (= :count (:reason (done-event wet))))
          (is (= 2 (dig-count wet))))))))

(deftest lava-next-to-a-block-always-skips-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "sand" :count 2 :wet true}
                                 {:blocks {"3,64,0" "sand" "6,64,0" "sand" "6,64,1" "lava"}} 30))]
          (is (= 1 (dig-count s)))
          (is (= :none (:reason (done-event s)))))))))

(deftest bedrock-is-skipped-and-not-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:world {:blocks (cells "sand" [3 4 5 6 7 8] [64] [0])}})]
          (.override (.-world (:p s)) "dig" (fn ^:async f [_ _ _] #js {:status "cannot"}))
          (core/submit! (:eng s) (spec {:block "sand" :max-failures 2}) {})
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
        (let [sand (cells "sand" [6 7 8 9] [64] [0])
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
        (let [s (start {:world {:blocks {"9,64,0" "sand"}}})]
          (.override (.-world (:p s)) "moveTo" (fn ^:async f [_ _ _] #js {:status "partial"}))
          (core/submit! (:eng s) (spec {:block "sand"}) {})
          (await (run-ticks s 1))
          (is (zero? (:failures (job-mem s))) "failures start at 0")
          (await (run-ticks s 20))
          (is (= 3 (count (calls s "moveTo"))))
          (is (zero? (dig-count s)))
          (is (= :none (:reason (done-event s))))
          (is (finished? s)))))))

(deftest the-best-tool-is-equipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[block carried expected]
                [["sand" ["wooden_shovel" "iron_shovel"] "iron_shovel"]
                 ["stone" ["wooden_pickaxe" "stone_pickaxe"] "stone_pickaxe"]
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

(defn world-state [{:keys [p]}] (.-state (.-world p)))
(defn set-pos! [s x y z] (set! (.-pos (.-self (world-state s))) #js {:x x :y y :z z}))

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
                                (.push (.-inventory (world-state s)) #js {:name "dirt" :count 1})
                                (.splice (.-entities (world-state s)) 0) ; nothing left to collect
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
                                (.push (.-inventory (world-state s)) #js {:name "dirt" :count 1})
                                (set-pos! s 10 64 0)
                                r))))
          ;; a walk to a cell leaves the body two blocks beside it
          (.override (.-world p) "moveTo"
                     (fn [_ args _]
                       (set-pos! s (+ 2 (.-x (.-pos args))) 64 (.-z (.-pos args)))
                       (js/Promise.resolve #js {:status "arrived"})))
          (core/submit! (:eng s) (spec {:block "dirt" :count 1}) {})
          (await (run-ticks s 40))
          (let [names (mapv #(.-name %) (.-calls (.-world p)))
                first-place (.indexOf (clj->js names) "place")
                walk-to-ground (first (keep-indexed (fn [i call] (when (and (= "moveTo" (.-name call)) (= 63 (.-y (.-pos (.-args call))))) i))
                                                    (.-calls (.-world p))))]
            (is (<= 0 first-place))
            (is (some? walk-to-ground) "walked to the owed cell")
            (is (< walk-to-ground first-place) "before placing")
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
                                 {:blocks (cells "stone" [4 5 6 7 8 9] [64] [0]) :drops {"stone" "cobblestone"} :inventory [{:name "iron_pickaxe" :count 1}]} 80))]
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

(def stone-tunnel
  "A dirt block east of the start with a stone tunnel through it: (2,63,0) is ground under the start, 3 and 4 lie beyond it and are buried until it is dug."
  (merge (cells "dirt" (range 2 6) [62 63 64] [-1 0 1])
         (cells "stone" [2 3 4] [63] [0])))

(deftest a-mend-that-spends-the-count-resumes-when-more-is-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "stone" :count 2} {:blocks stone-tunnel :drops cobble :inventory pickaxe} 200))]
          (is (= :count (:reason (done-event s))))
          (is (= 2 (:got (done-event s))))
          (is (= 3 (dig-count s)) "the ground cell, then the two beyond it")
          (is (= 1 (:resumes (done-event s))))
          (is (<= 2 (get (inv s) "cobblestone" 0)))
          (is (= "cobblestone" (block-at s 2 63 0)) "the ground cell is mended")
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
(def pad-plan {:id "pad" :status :active :parts [{:id "p" :box [[6 64 0] [6 64 0]] :want "sand"}]})
(def two-in-two-out {"3,64,0" "sand" "3,64,1" "sand" "6,64,0" "sand" "6,64,1" "sand"})

(defn ^:async zoned
  "Submit the job with args in a fake world, sharing world w; run n ticks; the setup map."
  [args world w n]
  (let [s (start {:world world :shared w})]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n))
    s))

(defn dug-cells [s] (mapv #(let [p (.-pos (.-args %))] [(.-x p) (.-y p) (.-z p)]) (calls s "dig")))

(deftest cells-in-a-zone-are-left-and-the-rest-are-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "sand" :count 4 :mend false} {:blocks two-in-two-out}
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
          (let [s (await (zoned {:block "sand"} {:blocks blocks} w 5))]
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
              p (tu/fake {:blocks {"6,64,0" "sand"}})
              _ (.override (.-world p) "moveTo"
                           (fn [token args impl] (ew/set-zones! w [(assoc farm-zone :min [5 60 -2] :max [7 70 2])])
                             (impl token args)))
              s (start {:p p :shared w})]
          (core/submit! (:eng s) (spec {:block "sand" :count 1 :mend false}) {})
          (await (run-ticks s 20))
          (is (= 1 (count (calls s "moveTo"))))
          (is (empty? (calls s "dig")))
          (is (= :refused (:reason (done-event s)))))))))

(deftest hazards-not-accepted-are-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"3,64,0" "sand" "6,64,0" "sand" "6,64,1" "water"}}
              s (await (scenario {:block "sand" :count 2 :wet true :accept #{}} world 30))]
          (is (= [[3 64 0]] (dug-cells s)))
          (is (= #{:fluid-adjacent :falling-block :under-feet} (:default (:accept mine/args)))))))))

;; ------------------------------------------------------------------ buried targets

(def stone-slab
  "Stone over x -12..12, y 50..64, z -3..3 with iron ore at 0,60,0 under four blocks of stone."
  (assoc (cells "stone" (range -12 13) (range 50 65) (range -3 4)) "0,60,0" "iron_ore"))

(def buried-world {:blocks stone-slab :self {:pos {:x 0 :y 65 :z 0}}
                   :drops {"iron_ore" "raw_iron" "stone" "cobblestone"}
                   :inventory [{:name "iron_pickaxe" :count 1}]})

(defn buried-fake
  "The fake over world whose collect leaves the body where it stands, as the real one does for an item in pickup
  reach (the fake's walks onto the item's cell, here the 1-high cell the ore left)."
  [world]
  (let [p (tu/fake world)
        w (.-world p)]
    (.override w "collect" (fn ^:async f [token a impl]
                             (let [at (.. w -state -self -pos)
                                   r (await (impl token a))]
                               (set! (.. w -state -self -pos) at)
                               r)))
    p))

(defn ^:async buried-scenario [args world n]
  (let [s (start {:p (buried-fake world)})]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n))
    s))

(defn feet [{:keys [p]}] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))

(deftest a-buried-ore-is-tunnelled-to-mined-and-the-body-walks-back-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (buried-scenario {:block "iron_ore" :count 1 :buried true :mend false} buried-world 300))]
          (is (finished? s))
          (is (= :count (:reason (done-event s))))
          (is (= 1 (:got (done-event s))))
          (is (= "air" (block-at s 0 60 0)))
          (is (= [0 65 0] (feet s)) "back where it started")
          (is (= 1 (count (events-of s :mine.tunnel))))
          (is (= 1 (count (:tunnels (done-event s))))))))))

(deftest buried-targets-wait-for-the-buried-arg-turned-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world buried-world})]
          (core/submit! eng (spec {:block "iron_ore" :count 1 :buried false}) {})
          (is (nil? (core/tick! eng)))
          (is (zero? (count (.-calls (.-world p))))))))))

(deftest a-buried-target-the-tunnel-declines-is-skipped-and-nothing-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:block "iron_ore" :count 1 :buried true :mend false}
                                 (update buried-world :blocks assoc "0,61,0" "water") 60))]
          (is (finished? s))
          (is (empty? (calls s "dig")))
          (is (= [0 65 0] (feet s)))
          (is (= 1 (count (events-of s :tunnel.stopped)))))))))

(deftest a-buried-target-in-a-zone-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (zoned {:block "iron_ore" :count 1 :buried true} buried-world
                              (ew/of-data {} {} [{:name "vault" :min [0 60 0] :max [0 60 0]}]) 5))]
          (is (empty? (calls s "dig")))
          (is (= [{:reason :refused :zones ["vault"] :plans []}]
                 (map #(select-keys % [:reason :zones :plans]) (events-of s :mine.declined)))))))))

(deftest a-restart-underground-finishes-and-walks-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              s (start {:p (buried-fake buried-world) :dir dir})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :buried true :mend false}) {})
          (await (run-ticks s 14))
          (is (< (second (feet s)) 65) "underground when stopped")
          (let [again (start {:p (:p s) :dir dir})]
            (await (run-ticks again 300))
            (is (finished? again))
            (is (= :count (:reason (done-event again))))
            (is (= [0 65 0] (feet again)))))))))

(deftest a-tunnel-never-entered-is-not-walked-out-of
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (buried-fake (update buried-world :blocks assoc "40,99,40" "stone"))
              w (.-world p)
              _ (.override w "steer" (fn ^:async f [token a impl]
                                       (let [r (await (impl token a))]
                                         (set! (.. w -state -self -pos) #js {:x 40.5 :y 100 :z 40.5})
                                         r)))
              s (start {:p p})]
          (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :buried true :mend false}) {})
          (await (run-ticks s 80))
          (is (finished? s))
          (is (= [:walk-in-failed] (map :reason (events-of s :tunnel.stopped))))
          (is (empty? (events-of s :mine.trapped)) "the body never went in")
          (is (empty? (calls s "dig"))))))))

(deftest buried-is-on-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (buried-scenario {:block "iron_ore" :count 1 :mend false} buried-world 300))]
          (is (finished? s))
          (is (= 1 (:got (done-event s))))
          (is (= "air" (block-at s 0 60 0)))
          (is (= 1 (count (events-of s :mine.tunnel))))
          (is (= [0 65 0] (feet s))))))))

(def lit-world
  (assoc buried-world :inventory [{:name "iron_pickaxe" :count 1} {:name "torch" :count 8} {:name "cobblestone" :count 10}]
         :drops {"iron_ore" "raw_iron" "stone" "cobblestone" "wall_torch" "torch"}))

(deftest a-buried-visit-takes-its-torches-back-and-seals-the-mouth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (buried-scenario {:block "iron_ore" :count 1 :mend false} lit-world 600))
              ground (for [x (range -12 13) z (range -3 4)] (block-at s x 64 z))]
          (is (finished? s))
          (is (= :count (:reason (done-event s))))
          (is (= 1 (:got (done-event s))))
          (is (= 8 (get (inv s) "torch")) "every torch is back")
          (is (not-any? #{"air"} ground) "the mouth of the tunnel is closed")
          (is (= 1 (count (events-of s :leave-tunnel.done))))
          (is (empty? (events-of s :mine.trapped)))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (= [0 65 0] (feet s))))))))
