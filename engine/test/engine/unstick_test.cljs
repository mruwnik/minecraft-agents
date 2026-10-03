(ns engine.unstick-test
  "The :moved memory entry written by act, the stuck trigger and the unstick job."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.triggers.stuck :as stuck]
            [jobs.maintenance.unstick :as unstick]))

(def t0 1000000)

(defn setup [world]
  (let [clock (atom t0)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [p name]
  (mapv #(js->clj (.-args %) :keywordize-keys true) (calls p name)))

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
          (core/submit! eng '(jobs.movement.go-to {:pos {:x 5 :y 64 :z 0}}) {})
          (await (core/tick! eng))
          (is (= [{:from {:x 0 :y 64 :z 0} :to at5 :status "arrived" :target at5}] (moved eng)))
          (is (= {:cap 20 :ttl 600000} (mem/policy (mem/view (:store eng)) :moved))))))))

(deftest act-records-a-moved-entry-even-when-the-body-does-not-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:unreachable ["5,64,0"]})]
          (core/submit! eng '(jobs.movement.go-to {:pos {:x 5 :y 64 :z 0}}) {})
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

(deftest unstick-escalates-pillar-then-dig-then-fails
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
          (is (= [] (calls p "dig") (calls p "place")) "attempt 1 only moves")
          (is (= [{:pos goal :range 1 :maxDistance 3}] (call-args p "moveTo"))
              "no free cell behind in a pit, so only the capped moveTo toward the goal")
          (is (= ["j1"] (:list (core/state eng))))
          (await (core/tick! eng))
          (is (= [{:item "cobblestone" :count 2}] (call-args p "jumpPlace")) "attempt 2 pillars with jumpPlace")
          (is (= [] (calls p "dig") (calls p "place")) "the roof stops it, and no old place-into-own-cell pillar")
          (await (core/tick! eng))
          (is (= [{:pos {:x 6 :y 64 :z 0}} {:pos {:x 6 :y 65 :z 0}} {:pos {:x 5 :y 66 :z 0}}]
                 (call-args p "dig"))
              "attempt 3 digs the block in front at feet and head height, and the one above the head")
          (is (= ["j1"] (:list (core/state eng))))
          (await (core/tick! eng))
          (is (= ["j1"] (:list (core/state eng))) "attempt 4 still tries")
          (is (not-any? #{"job.unstick.failed"} (tu/kinds seen)))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "gave up")
          (let [ev (first (filter #(= :unstick.failed (:kind %)) @seen))]
            (is (= :warn (:level ev)))
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
  (await (core/tick! eng))
  (await (core/tick! eng)))

(deftest unstick-equips-the-best-pickaxe-before-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (dig-setup [{:name "stone_pickaxe" :count 1} {:name "iron_pickaxe" :count 1}
                                          {:name "wooden_pickaxe" :count 1}])]
          (await (run-to-dig! eng p))
          (is (= [{:item "iron_pickaxe" :dest "hand"}] (call-args p "equip")))
          (is (= 3 (count (calls p "dig"))))
          (let [names (call-names p)]
            (is (< (.indexOf names "equip") (.indexOf names "dig")) "equip precedes the digs")))))))

(deftest unstick-digs-as-before-with-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (dig-setup [{:name "cobblestone" :count 3}])]
          (await (run-to-dig! eng p))
          (await (core/tick! eng))
          (is (= [] (calls p "equip")) "attempt 2 pillared (and failed), attempt 3 digs")
          (is (= 3 (count (calls p "dig")))))))))

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
  "Make the first moveTo of the fake (attempt 1's hop) fail, later ones real."
  [p]
  (let [n (atom 0)]
    (.override (.-world p) "moveTo"
               (fn ^:async f [token args impl]
                 (if (<= (swap! n inc) 1)
                   #js {:status "blocked" :pos (.-pos (.self p)) :distance 5}
                   (await (impl token args)))))))

(defn ^:async run-attempts!
  "Seed a stuck body, block moveTo (all of them, or only the first when hop?) and tick n times."
  [eng p n hop?]
  (if hop? (block-first-moveTo! p) (block-moveTo! p))
  (seed-moved! eng (repeat 4 (bad-move)))
  (core/submit! eng '(jobs.maintenance.unstick) {})
  (loop [i 0]
    (when (< i n)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn failed-event [seen] (first (filter #(= :unstick.failed (:kind %)) @seen)))

(deftest unstick-pillars-out-of-a-deep-pit-with-jump-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks deep-pit :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 1 true))
          (is (= [] (calls p "jumpPlace")) "attempt 1 is the step-back")
          (is (= ["j1"] (:list (core/state eng))))
          (await (core/tick! eng))
          (is (= [{:item "dirt" :count 3}] (call-args p "jumpPlace")) "attempt 2 pillars")
          (is (= [] (calls p "dig")))
          (is (= [] (calls p "place")))
          (is (= [] (:list (core/state eng))) "the hop succeeded: done"))))))

(deftest unstick-pillars-with-the-largest-stack-and-cobblestone-counts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks deep-pit
                                      :inventory [{:name "cobblestone" :count 8} {:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 2 true))
          (is (= [{:item "cobblestone" :count 3}] (call-args p "jumpPlace"))))))))

(deftest unstick-pillar-count-is-the-wall-height
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks low-walls :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 2 true))
          (is (= [{:item "dirt" :count 2}] (call-args p "jumpPlace"))))))))

(deftest unstick-does-not-pillar-with-no-block-and-says-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5} :blocks deep-pit})]
          (await (run-attempts! eng p 5 false))
          (is (= [] (calls p "jumpPlace")))
          (let [ev (failed-event seen)]
            (is (some? ev))
            (is (seq (:reasons ev)))
            (is (re-find #"no block in inventory" (:text ev)))
            (is (re-find #"^still stuck after 4 attempts: " (:text ev)))))))))

(deftest unstick-roofed-pit-reports-no-headroom-and-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5} :blocks roofed-pit :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 5 false))
          (is (pos? (count (calls p "jumpPlace"))))
          (is (re-find #"no-headroom" (:text (failed-event seen))))
          (is (= [] (:list (core/state eng))) "gave up after max-attempts, no livelock"))))))

(deftest unstick-does-not-pillar-on-open-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos at5} :blocks ground :inventory [{:name "dirt" :count 4}]})]
          (await (run-attempts! eng p 5 false))
          (is (= [] (calls p "jumpPlace"))))))))
