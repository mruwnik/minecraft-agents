(ns engine.go-to-sensed-test
  "go-to's adjacent reads (escalation, escape, clear-path, pass, near, reach, retreat, vehicle) answer from what the
  body feels, sees or remembers, never from the raw blockAt: behind stone a cell is unknown, a dig lays it open. The
  primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-support :as gts]
            [engine.go-to-test :as gt]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.go-to-doors-test :as gd]
            [engine.memory :as mem]
            [jobs.lib.util :as u]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.lib.world-files :as world-files]
            [jobs.lib.pass :as pass]
            [jobs.lib.reach :as reach]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.vehicle :as vehicle]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.health :as gth]
            [jobs.movement.go-to.restore :as restore]
            [jobs.movement.go-to.torch :as torch]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.survival.retreat-walk :as walk]))

(defn sensed-only
  "Wrapped fake primitives for spec, lit, whose raw blockAt throws. rawWorld stays, as on the body: go-to's planner
  snapshot (reach/lookup) reads it. A spec's :jitter fixes the sight passes' ray jitter (perception's :rand)."
  [spec]
  (let [p (tu/fake (dissoc spec :jitter))
        raw (fake-raw/create p)
        per (perception/create raw (cond-> {:now (constantly 1000000)}
                                     (:jitter spec) (assoc :rand (constantly (:jitter spec)))))
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

(deftest escape-door-from-takes-a-row-seen-open-above-and-unseen-below-for-the-far-side
  (let [block-at #(if (= % [3 65 0]) "air" "stone")]
    (is (= {:cells [[2 65 0] [2 64 0]] :through [3 64 0] :unseen true}
           (escape/door-from block-at #{[3 64 0]} [0 64 0] [1 0] 3 2)))))

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
        parent (tu/recording-parent 'jobs.access.clear-path {:heading :east} out)
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
        block-at (cornered/block-at-fn w)]
    (is (= "stone" (block-at {:x 1 :y 65 :z 0})))
    (is (= "stone" (block-at {:x 2 :y 64 :z 0})) "behind the wall: unseen is taken for rock, never a free way")
    (is (zero? (walk/open-cells block-at {:x 0 :y 64 :z 0} [-1 0] 3)) "behind the body: open ground it never saw is no way")))

(deftest retreat-reads-an-unseen-head-cell-as-rock-over-a-seen-open-feet-cell
  (let [{:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 60
                                  :blocks (merge ground {"2,65,0" "stone"})})
        block-at (cornered/block-at-fn w)]
    (is (= "air" (block-at {:x 2 :y 64 :z 0})) "the feet cell is in view")
    (is (= "stone" (block-at {:x 2 :y 65 :z 0})) "the head cell over it is not: a 1-high gap is no way")))

(deftest vehicle-dry-cell-reads-what-the-body-sees
  (let [{:keys [w]} (sensed-only (wall-east 1))]
    (is (true? (vehicle/dry-cell? w {:x 0 :y 64 :z 0})) "the feet cell is felt")
    (is (false? (vehicle/dry-cell? w {:x 2 :y 64 :z 0})) "behind the wall: unknown is not land")))

(deftest escalation-hole-changed-only-when-seen-filled
  (let [{:keys [p w]} (sensed-only (wall-east 1 {"3,64,0" "stone"}))]
    (fake/remove-block! p [1 64 0])
    (is (false? (restore/changed? w [1 64 0])) "seen air")
    (is (false? (restore/changed? w [3 64 0])) "unknown: not known to be filled")
    (is (true? (restore/changed? w [1 65 0])) "seen stone")))

(def room
  "A sealed 5x5 dirt room (inside x -2..2, z -2..2, feet y 64; walls one thick at x/z +-3, y 64..65; roof y 66) on a
  stone floor x -6..12, z -6..6."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 66 3 "dirt") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(def pit
  "A 3-deep 1x1 pit (feet at y 61) in dirt x -2..2, y 60..63; stone ground east of it (x 3..14) topped at the rim."
  (apply dissoc (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "dirt")) #{"0,61,0" "0,62,0" "0,63,0"}))

(defn ^:async run-job-unpinned
  "run-job with the sight jitter left random unless the spec has :jitter. A spec's :zones (a zone list) is the body's world's."
  [job spec args & [setup before]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        {:keys [w p] :as world} (sensed-only spec)
        _ (when setup (setup world))
        out (atom :not-done)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (gts/result-of e)) (ctx/end c :done nil)))))}
        eng (core/create (cond-> {:primitives w :jobs (assoc registry/jobs 'recording-parent parent) :triggers {}
                                  :dir (tu/tmp-dir) :now #(deref clock) :backoff false
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           (contains? spec :zones) (assoc :world (world-files/of-data {} {} (:zones spec)))))]
    (when before (before eng))
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 1000) (seq (:list (core/state eng))))
        (swap! clock + 100)
        (await (core/tick! eng))
        (recur (inc i))))
    {:out @out :seen @seen :p p}))

(defn ^:async run-job
  "Run job (a symbol) with args as a child over the sensed-only world spec; {:out :seen :p}. setup, when given, is called
  with the sensed-only {:p :w} first. The sight jitter is fixed at 0.5 unless the spec has :jitter: some jitters
  (0.54 to 0.59 in the dark tunnel) see one more cell, which changes the walk."
  [job spec args & [setup before]]
  (run-job-unpinned job (merge {:jitter 0.5} spec) args setup before))

(defn run-go-to [spec args & [setup before]] (run-job 'jobs.movement.go-to spec args setup before))

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

(defn ^:async pit-escape
  "The result and escalation steps of go-to out of the 3-deep pit carrying n dirt."
  [n]
  (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x 0 :y 61 :z 0}} :blocks pit
                                              :inventory [{:name "dirt" :count n}]}
                                             {:pos [10 64 0] :range 1}))]
    [out (steps seen) (pr-str (last seen))]))

(deftest go-to-stairs-out-of-a-pit-it-sees-from-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[out st shown] (await (pit-escape 1))]
          (is (= {:arrived true} out) shown)
          (is (= [:stair] st)))))))

(deftest go-to-pillars-out-of-a-pit-whose-rim-it-sees-with-the-blocks-for-all-but-the-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[out st shown] (await (pit-escape 2))]
          (is (= {:arrived true} out) shown)
          (is (= [:pillar] st)))))))

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

(defn ^:async run-step-off
  "Run step-off! from the origin cell over the sensed-only world spec as a child of a parent job; [its result, world calls]."
  [spec opts]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        {:keys [w]} (sensed-only spec)
        out (atom :not-done)
        parent {:round (fn ^:async recording-round [c]
                         (let [r (await (step-off/step-off! c {:x 0 :y 64 :z 0} opts))]
                           (when-not (= :continue r) (reset! out r))
                           (if (= :continue r) :continue (ctx/end c :done nil))))}
        eng (core/create {:primitives w :jobs (assoc registry/jobs 'recording-parent parent) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 40) (seq (:list (core/state eng))))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(deftest step-off-looks-at-floors-it-has-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-step-off {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 0 :pitch 0 :blocks ground} {:reach 1}))]
          (is (not= {:unreachable :no-cell} r) "a body at pitch 0 that remembers nothing glances down before it judges")
          (is (= :arrived r)))))))

;; ---- go-to :unknown: a hidden tunnel down a dark shaft

(def tunnel
  "Rock (stone x -2..10, y 59..66, z -2..2) with a room (x -1..1, y 64..65, z -1..1, the body's), a 3-deep shaft at x 1
  (feet cells y 63..61 over a floor at 60) and a tunnel east from its foot (x 2..9, y 61..62, z 0)."
  (apply dissoc (box -2 59 -2 10 66 2 "stone")
         (concat (for [x (range -1 2) y [64 65] z (range -1 2)] (str x "," y "," z))
                 (for [y [61 62 63]] (str "1," y ",0"))
                 (for [x (range 2 10) y [61 62]] (str x "," y ",0")))))

(defn dark-and-looked
  "The world dark, and a sight pass to each side from where the body stands, looking down 30 degrees: the room is known,
  the shaft's floor is not (deeper than near, 4)."
  [{:keys [p w]}]
  (swap! (fake/state p) assoc :light-default [0 0] :pitch 30)
  (doseq [yaw [0 90 180 270]]
    (swap! (fake/state p) assoc :yaw yaw)
    (perception/pass! (aget w "perception"))))

(def in-tunnel
  "The body in the room one block west of the shaft's rim (a walk to the rim comes before the drop)."
  {:self {:pos {:x -0.5 :y 64 :z 0.5}} :blocks tunnel})

(deftest go-to-without-unknown-walks-the-tunnel-it-never-saw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to in-tunnel {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal}} dark-and-looked))]
          (is (= {:arrived true} out) (pr-str (last seen))))))))

(deftest go-to-with-unknown-does-not-drop-into-a-dark-shaft-it-cannot-see
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (run-go-to in-tunnel {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} dark-and-looked))]
          (is (= [false :unseen-floor-dark] [(:arrived out) (:why out)]) (pr-str out))
          (is (re-find #":place-torches" (str (:text out))) "the words say what would help")
          (is (= 64 (js/Math.floor (second (:pos (fake/self p))))) "never dropped")
          (is (empty? (filter #(= :go-to.off-hand-torch (:kind %)) seen)) "no torch carried"))))))

(deftest go-to-with-unknown-holds-a-carried-torch-in-the-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [held (atom false)
              watch! (fn [{:keys [p] :as world}]
                       (dark-and-looked world)
                       (add-watch (fake/state p) :off-hand
                                  (fn [_ _ _ w] (when (= "torch" (get-in w [:equipment "offHand" :name])) (reset! held true)))))
              {:keys [out seen]} (await (run-go-to (assoc in-tunnel :inventory [{:name "torch" :count 4}])
                                                   {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} watch!))]
          (is (true? @held) "the off hand held the torch during the walk")
          (is (= 1 (count (filter #(= :go-to.off-hand-torch (:kind %)) seen))) "once per call")
          (is (= [:unseen-floor-dark [0 64 0] [1 61 0]] [(:why out) (:at out) (vals (:dark-best out))])
              (str "unseen land counts as open: the body walks to the takeoff before the dark drop is refused; a floor shows by any face point, so the best unseen one is a floor deeper " (pr-str out))))))))

(deftest go-to-with-unknown-holds-a-carried-soul-torch-in-the-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [held (atom nil)
              watch! (fn [{:keys [p] :as world}]
                       (dark-and-looked world)
                       (add-watch (fake/state p) :off-hand
                                  (fn [_ _ _ w] (when-let [n (get-in w [:equipment "offHand" :name])] (reset! held n)))))
              {:keys [out p]} (await (run-go-to (assoc in-tunnel :inventory [{:name "soul_torch" :count 4}])
                                                {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} watch!))
              w @(fake/state p)]
          (is (= "soul_torch" @held) "the off hand held the soul torch during the walk")
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (nil? (get-in w [:equipment "offHand"])) "put back")
          (is (= 4 (reduce + (map :count (filter #(= "soul_torch" (:name %)) (:inventory w))))) (pr-str (:inventory w))))))))

(deftest torch-aid-reads-nothing-without-unknown
  (let [p (js/Proxy. #js {} #js {:get (fn [_ k] (throw (js/Error. (str "read " (str k)))))})]
    (is (false? (torch/torch-aid? {:primitives p} {:dark-best {:x 1 :y 62 :z 0}})))))

(deftest go-to-with-unknown-puts-the-off-hand-torch-back-when-the-call-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen p]} (await (run-go-to (assoc in-tunnel :inventory [{:name "torch" :count 4}])
                                                 {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} dark-and-looked))
              w @(fake/state p)]
          (is (= 1 (count (filter #(= :go-to.off-hand-torch (:kind %)) seen))) "it was held")
          (is (nil? (get-in w [:equipment "offHand"])) "off hand empty again")
          (is (= 4 (reduce + (map :count (filter #(= "torch" (:name %)) (:inventory w))))) "all four torches back"))))))

(deftest go-to-with-unknown-leaves-a-full-off-hand-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (run-go-to (assoc in-tunnel :inventory [{:name "torch" :count 4}]
                                                            :equipment {"offHand" {:name "shield" :count 1}})
                                                     {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} dark-and-looked))]
          (is (empty? (filter #(= :go-to.off-hand-torch (:kind %)) seen)))
          (is (= "shield" (get-in @(fake/state p) [:equipment "offHand" :name])))
          (is (= :unseen-floor-dark (:why out)) (pr-str out)))))))

(deftest go-to-with-unknown-names-the-dark-drop-after-walking-to-its-takeoff
  ;; at ray jitter 0.55 the sight passes see one more cell: the first plan walks to the shaft's rim before the drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job-unpinned 'jobs.movement.go-to
                                                          (assoc in-tunnel :jitter 0.55 :inventory [{:name "torch" :count 4}]
                                                                 :equipment {"offHand" {:name "shield" :count 1}})
                                                          {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} dark-and-looked))]
          (is (empty? (filter #(= :walker-fault (:kind %)) seen)) (pr-str (filter #(= :walker-fault (:kind %)) seen)))
          (is (= :unseen-floor-dark (:why out)) (pr-str out)))))))

(deftest go-to-with-unknown-returns-a-torch-a-cut-call-left-in-the-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen p]} (await (run-go-to (assoc in-tunnel :equipment {"offHand" {:name "torch" :count 4}})
                                                 {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} dark-and-looked
                                                 #(mem/write! (:store %) torch/kind {:held true} {:cap 1 :ttl :forever})))
              w @(fake/state p)]
          (is (nil? (get-in w [:equipment "offHand"])) "off hand empty")
          (is (= 4 (reduce + (map :count (filter #(= "torch" (:name %)) (:inventory w))))) (pr-str (:inventory w)))
          (is (= 1 (count (filter #(= :go-to.off-hand-torch (:kind %)) seen))) "returned at the start, so held again for the dark walk")
          (is (empty? (filter #(= :go-to.off-hand-kept (:kind %)) seen))))))))

(deftest go-to-with-unknown-puts-the-torch-back-when-a-round-throws
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [throw-held! (fn [{:keys [p w] :as world}]
                            (dark-and-looked world)
                            (let [pw (aget w "pathWorld")]
                              (aset w "pathWorld" (fn [& xs]
                                                    (when (= "torch" (get-in @(fake/state p) [:equipment "offHand" :name]))
                                                      (throw (js/Error. "pathWorld broke")))
                                                    (apply pw xs)))))
              {:keys [seen p]} (await (run-go-to (assoc in-tunnel :inventory [{:name "torch" :count 4}])
                                                 {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} throw-held!))
              w @(fake/state p)]
          (is (= 1 (count (filter #(= :go-to.off-hand-torch (:kind %)) seen))) "it was held")
          (is (nil? (get-in w [:equipment "offHand"])) "off hand empty again")
          (is (= 4 (reduce + (map :count (filter #(= "torch" (:name %)) (:inventory w))))) (pr-str (:inventory w))))))))

(deftest go-to-refused-returns-a-torch-a-cut-call-left-in-the-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-go-to (assoc in-tunnel :equipment {"offHand" {:name "torch" :count 4}})
                                                {:place "nowhere-known" :unknown true} dark-and-looked
                                                #(mem/write! (:store %) torch/kind {:held true} {:cap 1 :ttl :forever})))
              w @(fake/state p)]
          (is (= :unknown-place (:reason out)) (pr-str out))
          (is (nil? (get-in w [:equipment "offHand"])) "off hand empty")
          (is (= 4 (reduce + (map :count (filter #(= "torch" (:name %)) (:inventory w))))) (pr-str (:inventory w))))))))

(deftest go-to-with-unknown-leaves-a-torch-it-never-held
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-go-to (assoc in-tunnel :equipment {"offHand" {:name "torch" :count 4}})
                                            {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true} dark-and-looked))]
          (is (= "torch" (get-in @(fake/state p) [:equipment "offHand" :name]))))))))

(defn ^:async quote-route
  "go-to's quote :route for args over the sensed-only world spec, after setup (as run-job's)."
  [spec args setup]
  (let [{:keys [w] :as world} (sensed-only (assoc spec :jitter 0.5))
        _ (setup world)
        eng (core/create {:primitives w :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now (constantly 1000000) :backoff false
                          :events (events/make {:body "Fake" :sinks [] :now (constantly 1000000)})})]
    (get-in (await (event-api/quote-view eng "jobs.movement.go-to" (pr-str args))) [:quote :route])))

(deftest go-to-quote-with-unknown-shows-the-dark-drop-go-to-would-refuse
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [preview (fn [args] (quote-route in-tunnel (merge {:pos [9 61 0] :range 1} args) dark-and-looked))
              seen-all (await (preview {}))
              masked (await (preview {:unknown true}))]
          (is (true? (:found seen-all)) (pr-str seen-all))
          (is (= [false :unseen-floor-dark] [(:found masked) (:reason masked)]) (pr-str masked))
          (is (= {:x 1 :y 62 :z 0} (select-keys (:dark-best masked) [:x :y :z])) (pr-str masked)))))))

(deftest go-to-escalation-child-sees-the-torches-in-the-inventory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [torches (atom :none)
              child {:round (fn [c] (reset! torches (reduce + (map :count (filter #(= "torch" (:name %)) (u/inventory (:primitives c))))))
                              (ctx/end c :done nil))}
              parent {:round (fn ^:async r [c]
                               (let [c (assoc c :unknown true)]
                                 (await (torch/hold-torch! c))
                                 (ctx/update-mem! c assoc :escalation {:step :clear-path :heading [1 0]})
                                 (await (esc/escalation-round! c {:x 5 :y 64 :z 0}))
                                 (ctx/end c :done nil)))}
              w (sensed-only {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks ground :inventory [{:name "torch" :count 4}]})
              eng (core/create {:primitives (:w w) :jobs (assoc registry/jobs 'jobs.access.clear-path child 'torch-parent parent)
                                :triggers {} :dir (tu/tmp-dir) :now (constantly 1000000) :backoff false})]
          (core/submit! eng '(torch-parent) {})
          (loop [i 0] (when (and (< i 20) (seq (:list (core/state eng)))) (await (core/tick! eng)) (recur (inc i))))
          (is (= 4 @torches) "all four torches are in the inventory while the child runs"))))))

;; the button sits on the wall north of the line, out of the body's view as it walks east to the door: it is looked at
;; before the press, and the press reads the button back as the body sees it
(deftest go-to-looks-at-a-button-it-has-not-seen-before-it-presses-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-go-to (gd/button-world :wires {gd/button-cell ["5,64,0"]}) {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} out))
          (is (= 1 (gd/clicks p)) "one click, on the button"))))))

(def iron-hut
  "A stone hut (hollow x -2..2, z -2..2, y 63..66; inside x -1..1, z -1..1, feet 64) on a stone floor x -8..10, z -8..10,
  a shut iron door in its south wall at [0 64 2] (facing north: the panel fills the cell's south side)."
  (merge (box -8 62 -8 10 62 10 "stone") (box -8 63 -8 10 63 10 "stone")
         (apply dissoc (box -2 64 -2 2 66 2 "stone") (for [x (range -1 2) z (range -1 2) y [64 65]] (str x "," y "," z)))
         {"0,64,2" "iron_door" "0,65,2" "iron_door"}))

(deftest go-to-from-the-free-part-of-an-iron-door-cell-it-sees-digs-out-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x 0.5 :y 64 :z 2.4}} :blocks iron-hut
                                                    :inventory [{:name "stone_pickaxe" :count 1}]}
                                                   {:pos [0 64 6] :range 1}))]
          (is (= {:arrived true} out) (pr-str out))
          (is (seq (steps seen))))))))

(deftest go-to-out-of-an-iron-door-hut-it-sees-digs-out-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x 0.5 :y 64 :z -0.5}} :blocks iron-hut
                                                    :inventory [{:name "stone_pickaxe" :count 1}]}
                                                   {:pos [0 64 6] :range 1}))]
          (is (= {:arrived true} out) (pr-str out))
          (is (seq (steps seen))))))))


(defn ^:async await-open-from
  "A job that runs pass/await-open! on the cell [5 64 0] of a sensed-only world with the shut-looking stone wall east
  of the floor, the body at the origin facing yaw; the result."
  [yaw]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        {:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw yaw :pitch 0
                                  :blocks (merge ground (box 5 64 0 5 65 0 "iron_door"))
                                  :states {"5,64,0" {:open true :half "lower" :facing "east"}
                                           "5,65,0" {:open true :half "upper" :facing "east"}}})
        out (atom nil)
        job {:round (fn ^:async door-round [c]
                      (reset! out (await (pass/await-open! c {:x 5 :y 64 :z 0})))
                      (ctx/end c :done nil))}
        eng (core/create {:primitives w :jobs (assoc registry/jobs 'door-poll job) :triggers {} :dir (tu/tmp-dir)
                          :now #(deref clock) :backoff false
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(door-poll) {})
    (loop [i 0]
      (when (and (< i 200) (seq (:list (core/state eng))))
        (swap! clock + 100)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(deftest an-open-door-out-of-view-is-read-open-only-after-the-body-turns-to-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:result :was-open} (await (await-open-from 90))) "facing away (west): turned to the door, then read open")))))

(def long-gap-tunnel
  "A 2-high tunnel (x 0..20, y 64..65, z 0) in stone, its floor at y 63 broken at x 14 down to y 62: a gap too far off to see in the dark."
  (apply dissoc (box -2 60 -2 22 67 2 "stone")
         "14,63,0" "14,62,0"
         (for [x (range 0 21) y [64 65]] (str x "," y ",0"))))

(deftest go-to-fills-a-gap-under-a-low-ceiling-it-has-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x 1.5 :y 64 :z 0.5}} :blocks long-gap-tunnel :inventory [{:name "dirt" :count 5}]}
                                                     {:pos [19 64 0] :range 0}
                                                     (fn [{:keys [p]}] (swap! (fake/state p) assoc :light-default [0 0]))))]
          (is (= {:arrived true} out) (pr-str (last seen)))
          (is (= [:fill-gap] (steps seen))))))))

(defn slime-pit
  "Stone x -12..6, y 54..59, z -4..4 with a 3x3 pit at x 0..2 (floor y 55 of pad, the cells over it open), the body on the stone at its west edge (feet y 60): a 4-block drop whose pad shows only from the lip. pad: the pit's floor blocks, a map of [x z] -> block."
  [pad]
  (let [pit (for [x (range 0 3) z (range -1 2)] [x z])]
    (-> (box -12 54 -4 6 59 4 "stone")
        (as-> w (apply dissoc w (for [[x z] pit y (range 55 60)] (str x "," y "," z))))
        (as-> w (reduce (fn [w [x z]] (assoc w (str x ",55," z) (get pad [x z] "stone"))) w pit)))))

(def slime-floor (into {} (for [x (range 0 3) z (range -1 2)] [[x z] "slime_block"])))

(deftest go-to-takes-a-slime-pad-drop-it-looks-at-from-the-lip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (slime-pit slime-floor)}
                                                   {:pos [1 56 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}}))]
          (is (= {:arrived true} out) (pr-str (last seen))))))))

(defn record-moves!
  "Keeps in atom a every raw moveTo and every steer tick's controls of the world {:p p} (steer ticks as [:steer controls x]); on-steer, when given, is called with p at every steer."
  [a & [on-steer]]
  (fn [{:keys [p]}]
    (let [w (.-world p)]
      (.override w "moveTo" (fn [token args impl] (swap! a conj [:move-to]) (impl token args)))
      (.override w "steer"
                 (fn [token args impl]
                   (when on-steer (on-steer p))
                   (let [d (.-decide args)
                         args' (js/Object.defineProperty (js/Object.assign #js {} args) "decide"
                                                         #js {:value (fn [pose] (let [r (d pose)] (swap! a conj [:steer (js->clj (.-controls r)) (.-x pose)]) r))})]
                     (impl token args')))))))

(deftest the-look-over-a-drop-edge-keeps-the-box-on-the-floor-by-sneaking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [acts (atom [])
              {:keys [out]} (await (run-go-to {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (slime-pit slime-floor)}
                                              {:pos [1 56 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}}
                                              (record-moves! acts)))
              steers (filter (comp #{:steer} first) @acts)]
          (is (= {:arrived true} out))
          (is (not-any? #{[:move-to]} @acts) "no raw moveTo to the lip: it overhangs the drop")
          (is (some #(get-in % [1 "sneak"]) steers) "the step to the lip sneaks"))))))

(def look-from-afar
  "A job that looks over the drop from [-1 60 0] to the pit floor [1 55 0] with the body far from it:."
  {:round (fn ^:async look-round [c]
            (ctx/update-mem! c assoc :probe-drops [{:from [-1 60 0] :floor [1 55 0]}])
            (when-let [cell (:look (:args c))] (await (dig-look/look-at! c cell)))
            (ctx/end c :done {:looked (boolean (await (gth/look-at-drops! c)))}))})

(deftest the-lean-to-the-lip-starts-beside-the-drop-after-a-planned-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [acts (atom [])
              _ (await (run-job look-from-afar {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (slime-pit slime-floor)}
                                            {} (record-moves! acts)))
              sneaks (filter #(get-in % [1 "sneak"]) @acts)]
          (is (seq sneaks))
          (is (every? #(>= (nth % 2) -1.1) sneaks) "no blind sneak across the stone: the lean begins beside the drop"))))))

(def probe-masked
  "A job that probes the way to the pit floor under go-to's :unknown mask (the spec go-to puts on its context)."
  {:round (fn ^:async probe-round [c]
            (let [c (assoc c :unknown (wworld/unknown-spec nil))]
              (ctx/end c :done {:cost (await (gth/probe-plan! c {:x -6.5 :y 60 :z 0.5} 0))})))})

(deftest the-damage-probe-plans-over-the-mask-of-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [masks (atom [])
              orig wplan/plan-within!]
          (set! wplan/plan-within! (fn [c pw to range weight policy]
                                     (swap! masks conj (some? (wworld/unknown-of pw)))
                                     (orig c pw to range weight policy)))
          (try (await (run-job probe-masked {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (slime-pit slime-floor)}
                               {:pos [1 56 0] :range 0}))
               (finally (set! wplan/plan-within! orig)))
          (is (= [true] @masks) "the unseen pad is not read from the raw chunks"))))))

(def pit-job {:pos [1 56 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}})

(defn pit-sneaks
  "The sneaking steer ticks of go-to over the slime pit with the body's spec extras and blocks."
  [extra blocks]
  (let [acts (atom [])]
    (-> (run-go-to (merge {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks blocks} extra) pit-job (record-moves! acts))
        (.then (fn [_] (filter #(get-in % [1 "sneak"]) @acts))))))

(deftest a-hostile-near-holds-back-the-look-over-a-drop-edge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (empty? (await (pit-sneaks {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x -9 :y 60 :z 0}}]} (slime-pit slime-floor)))))))))

(deftest a-hostile-beyond-the-danger-radius-does-not-hold-back-the-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (seq (await (pit-sneaks {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x -16 :y 60 :z 0}}]} (slime-pit slime-floor)))))))))

(deftest lava-seen-in-the-drop-holds-back-the-look-over-the-edge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [acts (atom [])]
          (await (run-job look-from-afar {:self {:pos {:x -2.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true
                                          :blocks (assoc (slime-pit slime-floor) "1,59,0" "lava")}
                          {:look [1 59 0]} (record-moves! acts)))
          (is (empty? (filter #(get-in % [1 "sneak"]) @acts))))))))

(deftest lava-seen-beside-the-landing-holds-back-the-look-over-the-edge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [acts (atom [])]
          (await (run-job look-from-afar {:self {:pos {:x -2.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true
                                          :blocks (assoc (slime-pit slime-floor) "2,59,0" "lava")}
                          {:look [2 59 0]} (record-moves! acts)))
          (is (empty? (filter #(get-in % [1 "sneak"]) @acts))))))))

(deftest a-hostile-that-comes-during-the-walk-to-the-takeoff-holds-back-the-lean
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [acts (atom [])
              zombie (fake/entity-in {:id 1 :name "zombie" :kind "hostile" :pos [-3 60 0]})
              walked (atom false)
              come (fn [p] (when-not @walked (reset! walked true) (swap! (fake/state p) update :entities conj zombie)))]
          (await (run-job look-from-afar {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (slime-pit slime-floor)}
                          {} (record-moves! acts come)))
          (is (seq @acts) "the walk to the takeoff steered")
          (is (empty? (filter #(get-in % [1 "sneak"]) @acts))))))))

(defn look-args
  "A job that looks over the drop from afar and puts the args its takeoff walk was given in atom walk."
  [walk]
  {:round (fn ^:async look-args-round [c]
            (ctx/update-mem! c assoc :probe-drops [{:from [-1 60 0] :floor [1 55 0]}])
            (await (gth/look-at-drops! c))
            (reset! walk (get-in (ctx/mem c) [:child-args :lip]))
            (ctx/end c :done nil))})

(deftest the-walk-to-the-takeoff-cell-spends-no-health-so-it-never-looks-at-a-drop-itself
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walk (atom nil)]
          (await (run-job (look-args walk) {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (slime-pit slime-floor)} {}))
          (is (= {:health 0} (:spend @walk)) (pr-str @walk)))))))

(deftest go-to-refuses-a-drop-onto-one-slime-block-ringed-by-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (run-go-to {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true
                                               :blocks (slime-pit {[1 0] "slime_block"})}
                                              {:pos [1 56 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}}))]
          (is (= [:needs-health :unreachable] ((juxt :why :reason) out)) (pr-str out)))))))

(def glass-shaft
  "slime-pit with one slime block at [1 0] and every other pit cell glass from the floor up to the surface: a 1x1 shaft whose walls the body sees through the glass."
  (let [pit (for [x (range 0 3) z (range -1 2) :when (not= [1 0] [x z])] [x z])]
    (reduce (fn [w [x z]] (reduce #(assoc %1 (str x "," %2 "," z) "glass") w (range 56 60)))
            (slime-pit {[1 0] "slime_block"}) pit)))

(deftest go-to-takes-a-drop-onto-one-slime-block-walled-with-glass-it-sees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks glass-shaft}
                                                   {:pos [1 56 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}}))]
          (is (= {:arrived true} out) (pr-str (last seen))))))))

(deftest go-to-refuses-a-drop-onto-a-stone-pit-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (run-go-to {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true
                                               :blocks (slime-pit {})}
                                              {:pos [1 56 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}}))]
          (is (= [:needs-health :unreachable] ((juxt :why :reason) out)) (pr-str out)))))))
