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
            [engine.jobs.reach :as reach]
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
        (let [{:keys [eng]} (setup {:floor [-2 -2 8 2]})]
          (core/submit! eng '(jobs.movement.pace {:a {:x 5 :y 64 :z 0} :b {:x 5 :y 64 :z 0} :laps 1 :rounds 1}) {})
          (await (core/tick! eng))
          (is (= [{:from {:x 0 :y 64 :z 0} :to {:x 4 :y 64 :z 0} :status "arrived" :target at5}]
                 (moved eng)) "the walker arrives within range of the target")
          (is (= {:cap 20 :ttl 600000} (mem/policy (mem/view (:store eng)) :moved))))))))

(deftest act-records-a-moved-entry-even-when-the-body-does-not-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:unreachable ["5,64,0"]})]
          (core/submit! eng '(jobs.movement.pace {:a {:x 5 :y 64 :z 0} :b {:x 5 :y 64 :z 0} :laps 1 :rounds 1}) {})
          (await (core/tick! eng))
          (is (= [{:from {:x 0 :y 64 :z 0} :to {:x 0 :y 64 :z 0} :status "blocked" :target at5 :no-path true}]
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

(defn failed-event [seen] (first (filter #(= :unstick.failed (:kind %)) @seen)))

(deftest unstick-does-nothing-when-not-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (is (nil? (core/tick! eng))))))))

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


;; ---------------------------------------------------------------- the job (go-to with escalation)

(def ground {"5,63,0" "stone"})

(def deep-pit
  "A 3-deep 1x1 pit: feet at y 64, walls at y 64, 65, 66, ground under the feet."
  (into ground (for [[x z] [[4 0] [6 0] [5 1] [5 -1]] y [64 65 66]] [(str x "," y "," z) "stone"])))

(defn bad-move-at
  "A bad move that held the body at pos (a cell map) on its way to target."
  [pos target]
  {:from pos :to pos :status "blocked" :target target})

(defn ^:async run-unstick!
  "Seed n bad moves at the body's cell toward target, submit unstick and tick until the list is empty (at most 200)."
  [eng pos target]
  (seed-moved! eng (repeat 4 (bad-move-at pos target)))
  (core/submit! eng '(jobs.maintenance.unstick) {})
  (loop [i 0]
    (when (and (< i 200) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))

(defn escalations [seen] (mapv :step (filter #(= :go-to.escalated (:kind %)) @seen)))

(deftest unstick-walks-to-the-stuck-job-s-goal-with-go-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5} :blocks (tu/floor -3 -3 20 3)})]
          (await (run-unstick! eng at5 goal))
          (is (= [] (:list (core/state eng))))
          (is (nil? (failed-event seen)))
          (is (<= 11 (first (feet p)) 13) "next to the goal")
          (is (empty? (escalations seen)) "open ground: nothing built or dug"))))))

(def pit
  "A 3-deep 1x1 pit (feet at (5,61,0)) in dirt x 3..7, y 60..63, z -2..2; stone ground east of it, top at the rim."
  (apply dissoc (merge (tu/box 8 60 -3 20 63 3 "stone") (tu/box 3 60 -2 7 63 2 "dirt"))
         ["5,61,0" "5,62,0" "5,63,0"]))

(def in-pit {:x 5 :y 61 :z 0})

(deftest unstick-pillars-out-of-a-pit-through-go-to-escalation
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos in-pit} :blocks pit :inventory [{:name "dirt" :count 4}]})]
          (await (run-unstick! eng in-pit goal))
          (is (= [] (:list (core/state eng))))
          (is (nil? (failed-event seen)))
          (is (= [:pillar] (escalations seen)))
          (is (= 3 (count (calls p "jumpPlace"))))
          (is (<= 11 (first (feet p)) 13)))))))

(deftest unstick-stairs-out-of-a-pit-with-nothing-to-pillar-with
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos in-pit} :blocks pit})]
          (await (run-unstick! eng in-pit goal))
          (is (nil? (failed-event seen)))
          (is (= [:stair] (escalations seen)))
          (is (<= 11 (first (feet p)) 13)))))))

(def bedrock-pit
  (into {} (for [x (range 4 9) y (range 63 68) z (range -2 3)
                 :when (not (and (= [5 0] [x z]) (<= 64 y 66)))]
             [(str x "," y "," z) "bedrock"])))

(deftest unstick-gives-up-in-a-bedrock-pit-and-quiets-the-trigger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos at5} :blocks (merge bedrock-pit (tu/box 9 63 -2 15 63 2 "stone"))
                                           :inventory [{:name "iron_pickaxe" :count 1}]})]
          (await (run-unstick! eng at5 goal))
          (let [ev (failed-event seen)]
            (is (some? ev))
            (is (= {:step :none :why :no-dig} (:escalation ev)) "bedrock: no pillar block, no door, no stair")
            (is (= {:x 5 :z 0} (select-keys (:pos ev) [:x :z]))))
          (is (= [] (calls p "dig")))
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :stuck))))
          (is (= {:cap 10 :ttl 3600000} (mem/policy (mem/view (:store eng)) :stuck)))
          (is (false? (stuck-now? eng)) "the :stuck entry quiets the trigger"))))))

(deftest unstick-with-no-goal-gives-up-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:pos at5} :blocks (tu/floor -3 -3 20 3)})]
          (seed-moved! eng (repeat 4 {:from at5 :to at5 :status "blocked"}))
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= :no-goal (:why (failed-event seen)))))))))

(deftest stuck-reflex-in-a-bedrock-pit-ends-through-its-own-give-up-not-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom t0)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:pos at5} :blocks bedrock-pit})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/register-reflex! eng {:trigger :stuck})
          (seed-moved! eng (repeat 4 (bad-move)))
          (loop [i 0]
            (when (< i 40)
              (await (core/tick! eng))
              (recur (inc i))))
          (is (some? (failed-event seen)) "gave up with unstick.failed")
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :stuck))) "the :stuck entry quiets the trigger")
          (is (not-any? #(= :backoff (:kind %)) @seen) "no job.backoff")
          (is (empty? (core/backoff-entries eng)) "no backoff counted"))))))

(def huge-hollow
  "A 2-deep 20x20 hollow in solid dirt (x 3..22, z -10..9, open at y 65 and 66, floor y 64, surface feet at y 67): about
  400 standable cells, the nearest wall 10 blocks from the middle."
  (apply dissoc
         (into {} (for [x (range 0 26) y (range 60 67) z (range -13 13)] [(str x "," y "," z) "dirt"]))
         (for [x (range 3 23) z (range -10 10) y [65 66]] (str x "," y "," z))))

(deftest a-body-in-the-middle-of-a-huge-hollow-is-enclosed
  (is (true? (reach/enclosed? (tu/fake {:self {:pos {:x 12 :y 65 :z 0}} :blocks huge-hollow})))))

(deftest unstick-gets-out-of-a-huge-hollow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [here {:x 12 :y 65 :z 0}
              {:keys [eng p seen]} (setup {:self {:pos here} :blocks huge-hollow})]
          (await (run-unstick! eng here {:x 24 :y 67 :z 0}))
          (is (nil? (failed-event seen)) "the wall is 10 blocks away, but there is a way out")
          (is (= [] (:list (core/state eng))))
          (is (= 67 (second (feet p))) "feet back at the surface"))))))
