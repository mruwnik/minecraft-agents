(ns engine.go-to-sensed-test
  "go-to's adjacent reads (escalation, escape, clear-path, pass, near, reach, retreat, vehicle) answer from what the
  body feels, sees or remembers, never from the raw blockAt: behind stone a cell is unknown, a dig lays it open. The
  primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [jobs.lib.escape :as escape]
            [jobs.lib.pass :as pass]
            [jobs.lib.reach :as reach]
            [jobs.lib.vehicle :as vehicle]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.survival.retreat :as retreat]
            [jobs.survival.retreat-walk :as walk]))

(defn sensed-only
  "Wrapped fake primitives for spec, lit, whose raw blockAt throws. rawWorld stays, as on the body: go-to's planner
  snapshot (reach/lookup) reads it."
  [spec]
  (let [p (tu/fake spec)
        raw (fake-raw/create p)
        per (perception/create raw {:now (constantly 1000000)})
        w (perception/wrap p per)]
    (aset w "rawWorld" raw)
    (aset w "blockAt" (fn [_] (throw (js/Error. "raw blockAt"))))
    {:p p :w w}))

(def ground (box -4 63 -4 8 63 4 "stone"))

(defn wall-east
  "A stone wall at x 1 .. x (thick), y 64..65, z -1..1, on stone ground; the body at the origin facing east, looking
  30 degrees down. behind: the
  blocks at x thick+1 (the far side), default air."
  ([thick] (wall-east thick {}))
  ([thick behind]
   {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 30
    :blocks (merge ground (box 1 64 -1 thick 65 1 "stone") behind)}))

(deftest escape-reads-a-hidden-cell-as-rock-and-a-seen-one-as-it-is
  (let [{:keys [w]} (sensed-only (wall-east 1 {"2,64,0" "oak_planks"}))
        block-at (escape/block-at-of w)]
    (is (= "stone" (block-at [1 65 0])) "the wall face is in view")
    (is (= "stone" (block-at [2 64 0])) "behind it: not seen, taken for rock")
    (is (true? ((escape/unseen-of w) [2 64 0])))
    (is (false? ((escape/unseen-of w) [1 65 0])))))

(deftest escape-door-guesses-an-unseen-far-side-and-a-dig-shows-it
  (let [{:keys [p w]} (sensed-only (wall-east 1))]
    (is (= {:cells [[1 65 0] [1 64 0]] :through [2 64 0] :unseen true}
           (escape/door (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0] 3)))
    (fake/remove-block! p [1 64 0])
    (fake/remove-block! p [1 65 0])
    (is (= {:cells [] :through [2 64 0]}
           (escape/door-from (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0] 3 2))
        "row one dug: the far side is seen open")))

(deftest escape-door-from-goes-on-through-a-row-seen-solid
  (let [{:keys [p w]} (sensed-only (wall-east 2))]
    (fake/remove-block! p [1 64 0])
    (fake/remove-block! p [1 65 0])
    (is (= {:cells [[2 65 0] [2 64 0]] :through [3 64 0] :unseen true}
           (escape/door-from (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0] 3 2)))))

(deftest escape-door-over-seen-cells-is-as-before
  (let [{:keys [w]} (sensed-only (assoc (wall-east 1) :blocks ground))]
    (is (nil? (escape/door (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0] 3)) "open ground seen ahead: no door")))

(defn ^:async run-clear-path
  "Run clear-path :east over the sensed-only world spec as a child; its result."
  [spec]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        {:keys [w]} (sensed-only spec)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid 'jobs.access.clear-path {:heading :east :max-thick 3}))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives w :jobs (assoc registry/jobs 'recording-parent parent) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 80) (seq (:list (core/state eng))))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(deftest clear-path-digs-on-into-a-wall-thicker-than-it-could-see
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-clear-path (assoc (wall-east 2) :inventory [{:name "wooden_pickaxe" :count 1}])))]
          (is (= [:done [3 64 0]] [(:status r) (:through r)]) (pr-str r))
          (is (= 4 (count (:dug r))) "both rows"))))))

(deftest clear-path-in-solid-rock-gives-up-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-clear-path (assoc (wall-east 8) :inventory [{:name "wooden_pickaxe" :count 1}])))]
          (is (= [:stopped :no-door] [(:status r) (:reason r)]) (pr-str r))
          (is (<= (count (:dug r)) 6) "at most max-thick rows of two")
          (is (string? (:why r))))))))

(deftest a-floor-the-body-has-not-seen-may-be-a-hazard
  (let [{:keys [w]} (sensed-only (wall-east 1 {"3,63,0" "magma_block"}))
        kind-at (fn [_ y _] (if (= y 63) :solid :open))]
    (is (false? (reach/standable-cell? w {:x 3 :y 64 :z 0} kind-at)) "the floor behind the wall is unseen")
    (is (true? (reach/standable-cell? w {:x 0 :y 64 :z 0} kind-at)) "a seen plain floor")))

(deftest pass-reads-a-door-in-view-and-nothing-behind-stone
  (let [{:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 30
                                  :blocks (merge ground {"1,64,0" "oak_door" "2,64,0" "stone" "2,65,0" "stone"
                                                         "3,64,0" "oak_door"})})
        c {:primitives w}]
    (is (= "oak_door" (some-> (pass/block-at c {:x 1 :y 64 :z 0}) .-name)))
    (is (nil? (pass/block-at c {:x 3 :y 64 :z 0})) "behind stone: not known")))

(deftest reach-reads-the-door-the-body-stands-in-and-the-floor-it-sees
  (let [{:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 64 :z 0.3}} :yaw 0 :pitch 40
                                  :blocks (merge ground {"0,64,0" "iron_door" "0,65,0" "iron_door"})})]
    (is (= [0 1] (reach/panel-step w (constantly :solid) {:x 0 :y 64 :z 0})) "a shut door facing north (its default state)")
    (is (true? (reach/standable-cell? w {:x 0 :y 64 :z 2} (fn [_ y _] (if (= y 63) :solid :open))))
        "kind-at decides feet and head; the floor read only for hazards")))

(deftest retreat-reads-an-unseen-cell-as-rock
  (let [{:keys [w]} (sensed-only (wall-east 1 {"2,64,0" "stone"}))
        block-at (retreat/block-at-fn w)]
    (is (= "stone" (block-at {:x 1 :y 65 :z 0})))
    (is (= "stone" (block-at {:x 2 :y 64 :z 0})) "behind the wall: unseen is taken for rock, never a free way")
    (is (zero? (walk/open-cells block-at {:x 0 :y 64 :z 0} [-1 0] 3)) "behind the body: open ground it never saw is no way")))

(deftest retreat-reads-an-unseen-head-cell-as-rock-over-a-seen-open-feet-cell
  (let [{:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 60
                                  :blocks (merge ground {"2,65,0" "stone"})})
        block-at (retreat/block-at-fn w)]
    (is (= "air" (block-at {:x 2 :y 64 :z 0})) "the feet cell is in view")
    (is (= "stone" (block-at {:x 2 :y 65 :z 0})) "the head cell over it is not: a 1-high gap is no way")))

(deftest vehicle-dry-cell-reads-what-the-body-sees
  (let [{:keys [w]} (sensed-only (wall-east 1))]
    (is (true? (vehicle/dry-cell? w {:x 0 :y 64 :z 0})) "the feet cell is felt")
    (is (false? (vehicle/dry-cell? w {:x 2 :y 64 :z 0})) "behind the wall: unknown is not land")))

(deftest escalation-hole-changed-only-when-seen-filled
  (let [{:keys [p w]} (sensed-only (wall-east 1 {"3,64,0" "stone"}))]
    (fake/remove-block! p [1 64 0])
    (is (false? (esc/changed? w [1 64 0])) "seen air")
    (is (false? (esc/changed? w [3 64 0])) "unknown: not known to be filled")
    (is (true? (esc/changed? w [1 65 0])) "seen stone")))

(def room
  "A sealed 5x5 dirt room (inside x -2..2, z -2..2, feet y 64; walls one thick at x/z +-3, y 64..65; roof y 66) on a
  stone floor x -6..12, z -6..6."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 66 3 "dirt") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(def pit
  "A 3-deep 1x1 pit (feet at y 61) in dirt x -2..2, y 60..63; stone ground east of it (x 3..14) topped at the rim."
  (apply dissoc (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "dirt")) #{"0,61,0" "0,62,0" "0,63,0"}))

(defn ^:async run-go-to
  "Run go-to with args as a child over the sensed-only world spec; {:out :seen}."
  [spec args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        {:keys [w]} (sensed-only spec)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid 'jobs.movement.go-to args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives w :jobs (assoc registry/jobs 'recording-parent parent) :triggers {}
                          :dir (tu/tmp-dir) :now #(deref clock) :backoff false
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 1000) (seq (:list (core/state eng))))
        (swap! clock + 100)
        (await (core/tick! eng))
        (recur (inc i))))
    {:out @out :seen @seen}))

(defn steps [seen] (mapv :step (filter #(= :go-to.escalated (:kind %)) seen)))

(deftest go-to-clears-a-door-out-of-a-sealed-room-it-sees-from-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x 0 :y 64 :z 0}} :blocks room
                                                    :inventory [{:name "dirt" :count 2}]}
                                                   {:pos [8 64 0] :range 1}))]
          (is (= {:arrived true} out) (pr-str (last seen)))
          (is (= [:clear-path] (steps seen))))))))

(deftest go-to-stairs-out-of-a-pit-it-sees-from-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x 0 :y 61 :z 0}} :blocks pit
                                                    :inventory [{:name "dirt" :count 2}]}
                                                   {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} out) (pr-str (last seen)))
          (is (= [:stair] (steps seen))))))))

(def deep-pit
  "A 5-deep 1x1 pit (feet at y 61) in dirt x -2..2, y 60..65; stone ground east of it (x 3..14) topped at the rim."
  (apply dissoc (merge (box 3 60 -3 14 65 3 "stone") (box -2 60 -2 2 65 2 "dirt"))
         (for [y (range 61 66)] (str "0," y ",0"))))

(deftest go-to-pillars-out-of-a-deep-pit-with-just-enough-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x 0 :y 61 :z 0}} :blocks deep-pit
                                                    :inventory [{:name "dirt" :count 5}]}
                                                   {:pos [10 66 0] :range 1}))]
          (is (= {:arrived true} out) (pr-str (last seen)))
          (is (= [:pillar] (steps seen)) (pr-str (filter #(= :go-to.escalated (:kind %)) seen))))))))
