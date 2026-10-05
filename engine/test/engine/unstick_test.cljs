(ns engine.unstick-test
  "The :moved memory entry written by act, the stuck trigger and the unstick job."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.access.ledger :as ledger]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.triggers.stuck :as stuck]
            [jobs.maintenance.unstick :as unstick]))

(def t0 1000000)

(defn setup
  ([world] (setup world (tu/tmp-dir) t0))
  ([world dir start]
   (let [clock (atom start)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/fake world)
         eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir
                           :now #(deref clock) :backoff false
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
      {:eng eng :p p :seen seen :clock clock :dir dir})))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [p name]
  (mapv #(js->clj (.-args %) :keywordize-keys true) (calls p name)))

(defn dig-positions [p] (mapv :pos (call-args p "dig")))

(defn block-moveTo!
  "Make every moveTo of the fake fail without moving."
  [p]
  (.override (.-world p) "moveTo"
             (fn ^:async f [_ _ _] #js {:status "blocked" :pos (.-pos (.self p)) :distance 5})))

(def at5 {:x 5 :y 64 :z 0})
(def goal {:x 12 :y 64 :z 0})

(defn bad-move [] {:from at5 :to at5 :status "blocked" :target goal})
(defn good-move [] {:from at5 :to {:x 8 :y 64 :z 0} :status "arrived" :target {:x 8 :y 64 :z 0}})

(defn seed-moved! [eng datas]
  (doseq [d datas] (mem/write! (:store eng) :moved d {:cap 20 :ttl 600000})))

(defn moved [eng] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))

(defn stuck-now?
  ([eng] (stuck-now? eng {}))
  ([eng args] ((:when (get triggers/all :stuck)) nil (mem/view (:store eng)) args)))

;; ---------------------------------------------------------------- act

(deftest act-records-a-moved-entry-for-move-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (core/submit! eng '(jobs.movement.pace {:a {:x 5 :y 64 :z 0} :b {:x 5 :y 64 :z 0} :laps 1 :rounds 1}) {})
          (await (core/tick! eng))
          (is (= [{:from {:x 0 :y 64 :z 0} :to at5 :status "arrived" :target at5}
                  {:from at5 :to at5 :status "arrived" :target at5}]
                 (moved eng)))
          (is (= {:cap 20 :ttl 600000} (mem/policy (mem/view (:store eng)) :moved))))))))

(deftest act-records-a-moved-entry-even-when-the-body-does-not-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:unreachable ["5,64,0"]})]
          (core/submit! eng '(jobs.movement.pace {:a {:x 5 :y 64 :z 0} :b {:x 5 :y 64 :z 0} :laps 1 :rounds 1}) {})
          (await (core/tick! eng))
          (is (= [{:from {:x 0 :y 64 :z 0} :to {:x 0 :y 64 :z 0} :status "blocked" :target at5}]
                 (moved eng))))))))

(deftest act-writes-no-moved-entry-for-other-primitives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:time 1000})]
          (core/submit! eng '(jobs.movement.look-around) {})
          (await (core/tick! eng))
          (is (= [] (moved eng))))))))

;; ---------------------------------------------------------------- trigger

(deftest stuck-trigger-needs-n-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (is (false? (stuck-now? eng)) "no entries")
          (seed-moved! eng (repeat 3 (bad-move)))
          (is (false? (stuck-now? eng)) "three of four")
          (seed-moved! eng [(bad-move)])
          (is (true? (stuck-now? eng)) "four bad moves")
          (is (false? (stuck-now? eng {:n 5})) ":n is an argument"))))))

(deftest stuck-trigger-is-false-after-one-good-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (seed-moved! eng [(bad-move) (bad-move) (bad-move) (bad-move) (good-move)])
          (is (false? (stuck-now? eng))))))))

(deftest stuck-trigger-counts-small-displacements-and-ignores-no-op-arrivals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [slow {:from at5 :to {:x 6 :y 64 :z 0} :status "partial" :target goal}
              noop {:from at5 :to at5 :status "arrived" :target at5}]
          (let [{:keys [eng]} (setup {})]
            (seed-moved! eng (repeat 4 slow))
            (is (true? (stuck-now? eng)) "partial but under 1.5 blocks each"))
          (let [{:keys [eng]} (setup {})]
            (seed-moved! eng [(bad-move) (bad-move) (bad-move) noop])
            (is (false? (stuck-now? eng)) "already there is not stuck"))
          (let [{:keys [eng]} (setup {})]
            (seed-moved! eng (repeat 4 slow))
            (is (false? (stuck-now? eng {:min-move 0.5})) ":min-move is an argument")))))))

(deftest stuck-trigger-needs-the-entries-inside-the-window
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {})]
          (seed-moved! eng (repeat 4 (bad-move)))
          (reset! clock (+ t0 59000))
          (is (true? (stuck-now? eng)))
          (reset! clock (+ t0 61000))
          (is (false? (stuck-now? eng)) "old entries do not count")
          (is (true? (stuck-now? eng {:window-ms 120000}))))))))

(defn seed-spaced!
  "Four bad moves, 20 s apart starting at t0, as four blocked 20 s moveTos write them."
  [eng clock]
  (doseq [i (range 4)]
    (reset! clock (+ t0 (* i 20000)))
    (seed-moved! eng [(bad-move)])))

(deftest stuck-trigger-counts-the-window-from-the-newest-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {})]
          (seed-spaced! eng clock)
          (reset! clock (+ t0 61000))
          (is (true? (stuck-now? eng)) "four bad moves spanning 60 s, the newest 1 s old")
          (reset! clock (+ t0 60000 60000 1))
          (is (false? (stuck-now? eng)) "newest move older than the window"))))))

(deftest stuck-is-registered-and-valid-in-a-scenario
  (is (some? (get triggers/all :stuck)))
  (is (= [] (scenario/problems registry/jobs triggers/all (scenario/parse "{:register [{:trigger :stuck}]}")))))

;; ---------------------------------------------------------------- job

(def pit
  "Solid blocks on all four sides of (5,64,0), at feet and head height."
  (into {} (for [[x z] [[4 0] [6 0] [5 1] [5 -1]] y [64 65]] [(str x "," y "," z) "stone"])))

(defn failed-event [seen] (first (filter #(= :unstick.failed (:kind %)) @seen)))

(deftest unstick-does-nothing-when-not-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (is (nil? (core/tick! eng))))))))

(deftest unstick-step-back-and-a-good-move-ends-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5}})]
          (seed-moved! eng (repeat 4 (bad-move)))
          (is (true? (stuck-now? eng)))
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (await (core/tick! eng))
          (is (= [{:pos {:x 4 :y 64 :z 0} :range 0} {:pos goal :range 1 :maxDistance 3}] (call-args p "moveTo"))
              "one step back, away from the goal, then a capped moveTo toward the goal itself")
          (is (= [] (:list (core/state eng))) "done after a good move")
          (is (= [] (calls p "dig")))
          (is (false? (stuck-now? eng)) "the good move is in the window, so no re-trigger"))))))

(deftest unstick-digs-when-roofed-then-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5}
                                           :blocks (merge pit {"5,66,0" "stone" "5,63,0" "stone"})
                                           :inventory [{:name "cobblestone" :count 3}]})]
          (block-moveTo! p)
          (seed-moved! eng (repeat 4 (bad-move)))
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (await (core/tick! eng))
          (is (= [] (calls p "jumpPlace") (calls p "place")) "the roof leaves no headroom, so no pillar")
          (is (= [{:x 6 :y 64 :z 0} {:x 6 :y 65 :z 0}] (dig-positions p)) "attempt 1 in a pit digs the door, no step-back")
          (is (= [{:pos goal :range 1 :maxDistance 3} {:pos goal :range 1 :timeoutS 6}] (call-args p "moveTo"))
              "only the capped hop and the uncapped retry")
          (is (= ["j1"] (:list (core/state eng))))
          (doseq [_ (range 6)] (await (core/tick! eng))) ; round 2 pillars out of the dug roof: a rise, not counted
          (is (not-any? #{"job.unstick.failed"} (tu/kinds seen)))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "gave up")
          (let [ev (failed-event seen)]
            (is (nil? (:level ev)))
            (is (= {:x 5 :z 0} (select-keys (:pos ev) [:x :z]))))
          (is (= [{:x 5 :z 0}] (mapv #(select-keys (:pos (:data %)) [:x :z]) (mem/entries (mem/view (:store eng)) :stuck))))
          (is (= {:cap 10 :ttl 3600000} (mem/policy (mem/view (:store eng)) :stuck)))
          (is (false? (stuck-now? eng)) "the failed attempts do not re-fire the trigger"))))))

(deftest unstick-gives-up-after-max-attempts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5}})]
          (block-moveTo! p)
          (seed-moved! eng (repeat 4 (bad-move)))
          (core/submit! eng '(jobs.maintenance.unstick {:max-attempts 1}) {})
          (await (core/tick! eng))
          (is (= ["j1"] (:list (core/state eng))))
          (is (empty? (filter #(= :unstick.failed (:kind %)) @seen)))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :unstick.failed (:kind %)) @seen))))
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :stuck)))))))))

;; ---------------------------------------------------------------- quiet period

(def unloaded-around
  "Every cell in a box around the body, as the fake's \"x,y,z\" keys."
  (vec (for [x (range 0 14) y (range 62 68) z (range -3 4)] (str x "," y "," z))))

(deftest unstick-survives-unloaded-cells-around-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:pos at5} :unloaded unloaded-around})]
          (seed-moved! eng (repeat 4 (bad-move)))
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (await (core/tick! eng))
          (is (not-any? #(= :failed (:kind %)) @seen) "a null blockAt is not a job failure"))))))

(deftest stuck-trigger-is-quiet-after-a-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {})]
          (mem/write! (:store eng) :stuck {:pos at5} {:cap 10 :ttl 3600000})
          (reset! clock (+ t0 1000))
          (seed-moved! eng (repeat 4 (bad-move)))
          (reset! clock (+ t0 60000))
          (is (false? (stuck-now? eng)) "fresh bad moves, but the give-up was a minute ago")
          (reset! clock (+ t0 300001))
          (seed-moved! eng (repeat 4 (bad-move)))
          (is (true? (stuck-now? eng)) "quiet period over")
          (is (false? (stuck-now? eng {:quiet-ms 600000})) ":quiet-ms is an argument"))))))

(defn restart! [eng clock at]
  (reset! clock at)
  (mem/write! (:store eng) :restart {}))

(deftest stuck-trigger-ignores-moves-from-before-the-latest-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {})]
          (reset! clock (+ t0 1000))
          (seed-moved! eng (repeat 4 (bad-move)))
          (restart! eng clock (+ t0 2000))
          (reset! clock (+ t0 3000))
          (is (false? (stuck-now? eng)) "four bad moves, all before the restart")
          (seed-moved! eng (repeat 4 (bad-move)))
          (is (true? (stuck-now? eng)) "four bad moves after it"))))))

(deftest stuck-trigger-needs-n-moves-after-the-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {})]
          (reset! clock (+ t0 1000))
          (seed-moved! eng (repeat 2 (bad-move)))
          (restart! eng clock (+ t0 2000))
          (reset! clock (+ t0 3000))
          (seed-moved! eng (repeat 2 (bad-move)))
          (is (false? (stuck-now? eng)) "two before and two after"))))))

(deftest stuck-trigger-quiet-check-ignores-the-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {})]
          (mem/write! (:store eng) :stuck {:pos at5} {:cap 10 :ttl 3600000})
          (restart! eng clock (+ t0 1000))
          (reset! clock (+ t0 300001))
          (seed-moved! eng (repeat 4 (bad-move)))
          (is (true? (stuck-now? eng)) "give-up over quiet-ms old, restart newer")
          (is (false? (stuck-now? eng {:quiet-ms 400000})) "measured from the :stuck, not the restart"))))))

(deftest engine-over-a-memory-with-recent-bad-moves-does-not-fire-stuck-on-its-first-tick
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock dir]} (setup {})]
          (reset! clock (+ t0 1000))
          (seed-moved! eng (repeat 4 (bad-move)))
          (core/save-memory! eng)
          (let [{again :eng seen :seen} (setup {} dir (+ t0 2000))]
            (core/register-reflex! again {:trigger :stuck})
            (is (= 4 (count (moved again))) "the moves were persisted")
            (is (false? (stuck-now? again)))
            (await (core/tick! again))
            (is (not-any? #(= :stuck (:trigger %)) @seen))
            (is (= [] (:list (core/state again))) "no unstick job was started")))))))

(deftest unstick-has-a-quiet-ms-arg
  (is (= (:quiet-ms stuck/defaults) (get-in unstick/args [:quiet-ms :default])))
  (is (= 300000 (:quiet-ms stuck/defaults))))

(defn call-names [p] (mapv #(.-name %) (.-calls (.-world p))))

(defn dig-setup [inventory]
  (setup {:self {:pos at5} :blocks (merge pit {"5,66,0" "stone"}) :inventory inventory}))

(defn ^:async run-to-dig! [eng p]
  (block-moveTo! p)
  (seed-moved! eng (repeat 4 (bad-move)))
  (core/submit! eng '(jobs.maintenance.unstick) {})
  (await (core/tick! eng)))

(deftest unstick-equips-the-best-pickaxe-before-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (dig-setup [{:name "stone_pickaxe" :count 1} {:name "iron_pickaxe" :count 1}
                                          {:name "wooden_pickaxe" :count 1}])]
          (await (run-to-dig! eng p))
          (is (= [{:item "iron_pickaxe" :dest "hand"}] (call-args p "equip")))
          (is (= 2 (count (calls p "dig"))) "door: front at feet and head height")
          (let [names (call-names p)]
            (is (< (.indexOf names "equip") (.indexOf names "dig")) "equip precedes the digs")))))))

(deftest unstick-digs-with-no-pickaxe-and-nothing-to-equip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (dig-setup [{:name "bucket" :count 1}])]
          (await (run-to-dig! eng p))
          (is (= [] (calls p "equip")))
          (is (= 2 (count (calls p "dig")))))))))

;; ---------------------------------------------------------------- pillar

(def ground {"5,63,0" "stone"})

(def deep-pit
  "A 3-deep 1x1 pit: feet at y 64, walls at y 64, 65, 66, ground under the feet."
  (into ground (for [[x z] [[4 0] [6 0] [5 1] [5 -1]] y [64 65 66]] [(str x "," y "," z) "stone"])))

(def low-walls
  "Walls 2 high around the body on flat ground."
  (into ground (for [[x z] [[4 0] [6 0] [5 1] [5 -1]] y [64 65]] [(str x "," y "," z) "stone"])))

(def roofed-pit (merge deep-pit {"5,66,0" "stone"}))

(defn block-first-moveTo!
  "Make the first two moveTos of the fake (attempt 1's capped hop and its uncapped retry) fail, later ones real."
  [p]
  (let [n (atom 0)]
    (.override (.-world p) "moveTo"
               (fn ^:async f [token args impl]
                 (if (<= (swap! n inc) 2)
                   #js {:status "blocked" :pos (.-pos (.self p)) :distance 5}
                   (await (impl token args)))))))

(defn ^:async run-attempts!
  "Seed a stuck body, block moveTo (all of them when hop? is false, only the first two when true, none when :free)
  and tick n times."
  [eng p n hop?]
  (case hop?
    true (block-first-moveTo! p)
    false (block-moveTo! p)
    nil)
  (seed-moved! eng (repeat 4 (bad-move)))
  (core/submit! eng '(jobs.maintenance.unstick) {})
  (loop [i 0]
    (when (< i n)
      (await (core/tick! eng))
      (recur (inc i)))))

(deftest unstick-pillars-out-of-a-deep-pit-with-jump-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks deep-pit :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 1 :free))
          (is (= [{:item "dirt" :count 3}] (call-args p "jumpPlace")) "attempt 1 pillars: dug blocks are carried, so dig only when it cannot")
          (is (= [] (calls p "dig")))
          (is (= [] (calls p "place")))
          (is (= [] (:list (core/state eng))) "the hop succeeded: done"))))))

(defn ledger-of [eng] (ledger/open-entries (mem/view (:store eng))))

(deftest unstick-pillar-blocks-are-written-to-the-scaffold-ledger-for-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks low-walls :inventory [{:name "cobblestone" :count 4}]})]
          (await (run-attempts! eng p 1 :free))
          (is (= [{:cell [5 64 0] :item "cobblestone" :before "air" :purpose :unstick-pillar :state :placed}
                  {:cell [5 65 0] :item "cobblestone" :before "air" :purpose :unstick-pillar :state :placed}]
                 (->> (ledger-of eng) (sort-by :cell) (mapv #(dissoc % :job))))
              "each cell jumpPlace filled is an open entry, so jobs.access.cleanup takes it back once the body is out")
          (is (= #{"j1"} (set (map :job (ledger-of eng)))) "owned by the unstick job"))))))

(deftest unstick-pillar-that-places-nothing-leaves-no-ledger-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks low-walls :inventory [{:name "cobblestone" :count 4}]})]
          (.override (.-world p) "jumpPlace" (fn ^:async f [_ _ _] #js {:status "failed" :placed 0 :reason "no-support"}))
          (await (run-attempts! eng p 1 true))
          (is (= [] (ledger-of eng))))))))

(deftest unstick-pillars-with-the-largest-stack-and-cobblestone-counts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks deep-pit
                                      :inventory [{:name "cobblestone" :count 8} {:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 1 true))
          (is (= [{:item "cobblestone" :count 3}] (call-args p "jumpPlace"))))))))

(deftest unstick-pillar-count-is-the-wall-height
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks low-walls :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 1 true))
          (is (= [{:item "dirt" :count 2}] (call-args p "jumpPlace"))))))))

(deftest unstick-does-not-pillar-with-no-block-and-says-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5} :blocks deep-pit})]
          (await (run-attempts! eng p 7 false))
          (is (= [] (calls p "jumpPlace")))
          (let [ev (failed-event seen)]
            (is (some? ev))
            (is (seq (:reasons ev)))
            (is (re-find #"no block in inventory" (:text ev)))
            (is (re-find #"^still stuck after 6 attempts: " (:text ev)))))))))

(deftest unstick-roofed-pit-reports-no-headroom-and-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5} :blocks roofed-pit :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 8 false))
          (is (re-find #"no headroom" (:text (failed-event seen))))
          (is (= [] (:list (core/state eng))) "gave up after max-attempts, no livelock"))))))

(deftest unstick-does-not-pillar-on-open-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks ground :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 7 false))
          (is (= [] (calls p "jumpPlace"))))))))

;; ---------------------------------------------------------------- hop judged by displacement

(def far-goal {:x 11 :y 64 :z 0})

(defn far-bad-move [] {:from at5 :to at5 :status "blocked" :target far-goal})

(defn walled-in-setup []
  (setup {:self {:pos at5} :blocks (merge pit {"5,66,0" "stone"})}))

(defn moved-to!
  "Make every moveTo of the fake move the body to x, then report blocked."
  [p x]
  (.override (.-world p) "moveTo"
             (fn ^:async f [token args impl]
               (await (impl token #js {:pos #js {:x x :y 64 :z 0} :range 0}))
               #js {:status "blocked" :pos (.-pos (.self p)) :distance 5})))

(defn ^:async start-spell! [eng]
  (seed-moved! eng (repeat 4 (far-bad-move)))
  (core/submit! eng '(jobs.maintenance.unstick) {})
  (await (core/tick! eng)))

(deftest unstick-retries-uncapped-when-the-capped-point-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (walled-in-setup)]
          (.override (.-world p) "moveTo"
                     (fn ^:async f [token args impl]
                       (cond
                         (.-maxDistance args) #js {:status "blocked" :pos (.-pos (.self p)) :distance 6}
                         (seq (calls p "dig")) (await (impl token #js {:pos (clj->js far-goal) :range 1}))
                         :else #js {:status "blocked" :pos (.-pos (.self p)) :distance 6})))
          (await (start-spell! eng))
          (is (= [] (:list (core/state eng))) "attempt 1 dug the door, the capped moveTo stayed blocked, the uncapped one walked out")
          (is (= [{:pos far-goal :range 1 :maxDistance 3} {:pos far-goal :range 1 :timeoutS 6}]
                 (call-args p "moveTo")))
          (is (= [] (calls p "jumpPlace")))
          (is (not-any? #(= :unstick.failed (:kind %)) @seen)))))))

(deftest unstick-counts-displacement-not-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (walled-in-setup)]
          (moved-to! p 7)
          (await (start-spell! eng))
          (is (= [] (:list (core/state eng))) "blocked but 2 blocks from the stuck spot: done"))))))

(deftest unstick-does-not-count-a-short-move-as-success
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (walled-in-setup)]
          (moved-to! p 6)
          (await (start-spell! eng))
          (is (= ["j1"] (:list (core/state eng))) "1 block from the stuck spot is under :min-move"))))))

;; ---------------------------------------------------------------- dig step: door and stair

(defn dirt-box
  "Solid dirt for y 60..66, x 0..9, z -3..3 (surface feet at y 67), minus the carved cells."
  [carved]
  (apply dissoc
         (into {} (for [x (range 0 10) y (range 60 67) z (range -3 4)] [(str x "," y "," z) "dirt"]))
         carved))

(def dirt-pit
  "A 3-deep 1x1 pit in solid dirt: feet at (5,64,0), open cells up to y 66."
  (dirt-box ["5,64,0" "5,65,0" "5,66,0"]))

(def dirt-self {:pos {:x 5 :y 64 :z 0}})

(deftest unstick-in-a-dirt-pit-digs-a-stair-step-toward-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self dirt-self :blocks dirt-pit})]
          (await (run-attempts! eng p 1 false))
          (is (= [{:x 6 :y 65 :z 0} {:x 6 :y 66 :z 0}] (dig-positions p))
              "feet and head cells of the next step; the headroom cell is already open, and front at feet height is the step")
          (is (= {:pos {:x 6 :y 65 :z 0} :range 0} (first (call-args p "moveTo"))) "then one step up")
          (is (= [] (calls p "jumpPlace"))))))))

(deftest unstick-stair-digs-the-headroom-cell-when-solid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self dirt-self :blocks (dirt-box ["5,64,0" "5,65,0"])})]
          (await (run-attempts! eng p 1 false))
          (is (= [{:x 5 :y 66 :z 0} {:x 6 :y 65 :z 0} {:x 6 :y 66 :z 0}] (dig-positions p))))))))

(deftest unstick-chooses-cells-from-the-landed-position
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos {:x 5 :y 65 :z 0} :onGround false} :blocks dirt-pit})
              state (.-state (.-world p))]
          (.override (.-world p) "wait"
                     (fn ^:async f [_ _ _]
                       (swap! state update :self assoc :onGround true :pos [5 64 0])
                       #js {:status "ok"}))
          (await (run-attempts! eng p 1 :free))
          (is (pos? (count (calls p "wait"))) "waits for the body to land first")
          (is (= [{:x 6 :y 65 :z 0} {:x 6 :y 66 :z 0}] (dig-positions p)) "cells of the landed feet cell, not the airborne one"))))))

(deftest unstick-uses-the-cell-anyway-when-it-never-lands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos {:x 5 :y 64 :z 0} :onGround false} :blocks dirt-pit})]
          (await (run-attempts! eng p 1 false))
          (is (<= 1 (count (calls p "wait")) 20) "bounded polling")
          (is (= [{:x 6 :y 65 :z 0} {:x 6 :y 66 :z 0}] (dig-positions p))))))))

(deftest unstick-digs-a-door-through-a-one-block-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks low-walls})]
          (await (run-attempts! eng p 1 false))
          (is (= [{:x 6 :y 64 :z 0} {:x 6 :y 65 :z 0}] (dig-positions p)) "front at feet and head height")
          (is (= [{:pos goal :range 1 :maxDistance 3} {:pos goal :range 1 :timeoutS 6}] (call-args p "moveTo"))
              "no stair step"))))))

(def bedrock-pit
  (into {} (for [x (range 4 9) y (range 63 68) z (range -2 3)
                 :when (not (and (= [5 0] [x z]) (<= 64 y 66)))]
             [(str x "," y "," z) "bedrock"])))

(deftest unstick-bedrock-pit-gives-up-with-cannot-and-no-step-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self dirt-self :blocks bedrock-pit})]
          (await (run-attempts! eng p 7 false))
          (let [ev (failed-event seen)]
            (is (= 6 (:attempts ev)))
            (is (= ["pillar: no block in inventory (x6)" "dig: cannot (x6)"] (:reasons ev)))
            (is (= "still stuck after 6 attempts: pillar: no block in inventory (x6); dig: cannot (x6)" (:text ev))))
          (is (= [] (filter #(zero? (:range %)) (call-args p "moveTo"))) "no step-back, no stair move"))))))

(deftest stuck-reflex-in-a-bedrock-pit-ends-through-its-own-give-up-not-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom t0)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self dirt-self :blocks bedrock-pit})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (block-moveTo! p)
          (core/register-reflex! eng {:trigger :stuck})
          (seed-moved! eng (repeat 4 (bad-move)))
          (loop [i 0]
            (when (< i 10)
              (await (core/tick! eng))
              (recur (inc i))))
          (is (some? (failed-event seen)) "gave up with unstick.failed")
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :stuck))) "the :stuck entry quiets the trigger")
          (is (not-any? #(= :backoff (:kind %)) @seen) "no job.backoff")
          (is (empty? (core/backoff-entries eng)) "no backoff counted"))))))

(deftest unstick-stair-refuses-a-cell-under-gravel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self dirt-self :blocks (assoc dirt-pit "6,67,0" "gravel")})]
          (await (run-attempts! eng p 7 false))
          (is (not-any? #{{:x 6 :y 66 :z 0}} (dig-positions p)) "the cell under the gravel is not dug")
          (is (some #(re-find #"^dig: gravel above \(6 66 0\)" %) (:reasons (failed-event seen)))))))))

(deftest unstick-stair-refuses-a-cell-next-to-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self dirt-self :blocks (assoc dirt-pit "7,65,0" "lava")})]
          (await (run-attempts! eng p 7 false))
          (is (not-any? #{{:x 6 :y 65 :z 0}} (dig-positions p)))
          (is (some #(re-find #"^dig: lava next to \(6 65 0\)" %) (:reasons (failed-event seen)))))))))

(deftest unstick-roofed-pit-with-blocks-digs-the-roof-instead-of-pillaring
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self dirt-self :blocks (dirt-box ["5,64,0" "5,65,0"])
                                      :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 1 false))
          (is (= [] (calls p "jumpPlace")) "no headroom, so no pillar")
          (is (some #{{:x 5 :y 66 :z 0}} (dig-positions p)) "the stair digs the roof first"))))))

(deftest unstick-bedrock-roofed-pit-with-blocks-gives-up-without-pillaring
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self dirt-self :blocks (assoc bedrock-pit "5,66,0" "bedrock")
                                           :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 7 false))
          (is (= [] (calls p "jumpPlace")))
          (is (some #(re-find #"^dig: cannot" %) (:reasons (failed-event seen)))))))))

(deftest summarize-reasons-counts-repeats-once-each-in-first-seen-order
  (is (= ["pillar: no block in inventory (x6)" "dig: cannot (x6)"]
         (unstick/summarize-reasons (take 12 (cycle ["pillar: no block in inventory" "dig: cannot"]))))))

(deftest summarize-reasons-omits-suffix-for-a-single-occurrence
  (is (= ["dig: cannot"] (unstick/summarize-reasons ["dig: cannot"]))))

(deftest summarize-reasons-keeps-first-seen-order
  (is (= ["b (x2)" "a" "c"] (unstick/summarize-reasons ["b" "a" "b" "c"]))))

(deftest summarize-reasons-of-nothing-is-empty
  (is (= [] (unstick/summarize-reasons []))))

;; ---------------------------------------------------------------- stair result and progress

(defn arrive-without-moving!
  "Make every moveTo of the fake report arrived and leave the body where it is."
  [p]
  (.override (.-world p) "moveTo"
             (fn ^:async f [_ _ _] #js {:status "arrived" :pos (.-pos (.self p)) :distance 0})))

(deftest unstick-stair-step-that-arrives-records-no-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self dirt-self :blocks dirt-pit})]
          (arrive-without-moving! p)
          (await (run-attempts! eng p 7 :free))
          (is (= ["pillar: no block in inventory (x6)"] (:reasons (failed-event seen)))
              "no \"[object Object]\" and no stair reason when the moveTo arrived"))))))

(deftest unstick-stair-step-that-is-blocked-records-the-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self dirt-self :blocks dirt-pit})]
          (await (run-attempts! eng p 7 false))
          (is (= ["pillar: no block in inventory (x6)" "stair: moveTo blocked (x6)"] (:reasons (failed-event seen)))))))))

(defn climb-box
  "Solid dirt for x 0..24, y 52..90, z -3..3, minus the carved cells."
  [carved]
  (apply dissoc
         (into {} (for [x (range 0 25) y (range 52 91) z (range -3 4)] [(str x "," y "," z) "dirt"]))
         carved))

(defn lifting-moveTo!
  "Every stair moveTo (range 0) puts the body on its target. A hop (anything else) stays blocked until the
  body's feet reach y top, then walks it 25 blocks away."
  [p top]
  (let [state (.-state (.-world p))]
    (.override (.-world p) "moveTo"
               (fn ^:async f [_ args _]
                 (let [pos (.-pos args)
                       y (.-y (.-pos (.self p)))
                       lift? (zero? (.-range args))
                       out? (>= y top)]
                   (cond
                     lift? (swap! state assoc-in [:self :pos] [(.-x pos) (.-y pos) (.-z pos)])
                     out? (swap! state assoc-in [:self :pos] [30 y 0]))
                   #js {:status (if (or lift? out?) "arrived" "blocked") :pos (.-pos (.self p)) :distance 5})))))

(def climb-pit (climb-box (for [y (range 60 68)] (str "5," y ",0"))))

(deftest unstick-climbs-an-eight-deep-pit-in-one-spell-since-rising-is-not-an-attempt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos {:x 5 :y 60 :z 0}} :blocks climb-pit})]
          (lifting-moveTo! p 68)
          (await (run-attempts! eng p 9 :free))
          (is (= [] (:list (core/state eng))) "out of the pit, the spell is over")
          (is (nil? (failed-event seen)) "no give-up although 8 stair steps exceed :max-attempts 6")
          (is (= 8 (count (filter #(zero? (:range %)) (call-args p "moveTo")))) "one stair step per round"))))))

(def wide-hollow
  "A 2-deep 5x5 hollow in solid dirt (x 3..7, z -2..2, open at y 65 and 66, floor y 64, surface feet at y 67):
  no cell inside has three walls, and from the middle no side at feet height is solid."
  (dirt-box (for [x (range 3 8) z (range -2 3) y [65 66]] (str x "," y "," z))))

(deftest unstick-walks-to-a-wall-and-stairs-out-of-a-wide-hollow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos {:x 5 :y 65 :z 0}} :blocks wide-hollow
                                           :inventory [{:name "dirt" :count 4}]})]
          (lifting-moveTo! p 67)
          (await (run-attempts! eng p 8 :free))
          (is (nil? (failed-event seen)) "no give-up: not a 1x1 pit, but still a way out")
          (is (= [] (:list (core/state eng))) "out of the hollow, the spell is over")
          (is (seq (dig-positions p)) "it dug stair steps into a wall")
          (is (= 67 (.-y (.-pos (.self p)))) "feet back at the surface"))))))

(deftest unstick-bedrock-pit-gives-up-after-max-attempts-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self dirt-self :blocks bedrock-pit})]
          (await (run-attempts! eng p 7 false))
          (is (= {:attempts 6 :rounds 6} (select-keys (failed-event seen) [:attempts :rounds])) "no rise, every round counts"))))))

(deftest unstick-gives-up-at-the-round-cap-when-it-rises-every-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos {:x 5 :y 60 :z 0}} :blocks climb-pit})]
          (lifting-moveTo! p 1000)
          (await (run-attempts! eng p 15 :free))
          (is (= [] (:list (core/state eng))) "gave up")
          (let [ev (failed-event seen)]
            (is (= {:attempts 0 :rounds 14} (select-keys ev [:attempts :rounds])))
            (is (re-find #"^still stuck after 14 attempts: " (:text ev)))))))))
