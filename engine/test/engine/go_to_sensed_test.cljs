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
            [engine.go-to-column-test :as gcol]
            [engine.go-to-doors-test :as gd]
            [engine.memory :as mem]
            [jobs.lib.util :as u]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.memory-age :as memory-age]
            [jobs.lib.world-files :as world-files]
            [jobs.lib.pass :as pass]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.tools :as tools]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.vehicle :as vehicle]
            [jobs.lib.walk.drive :as drive]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.cut :as gcut]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.health :as gth]
            [jobs.movement.go-to.restore :as restore]
            [jobs.movement.go-to.torch :as torch]
            [jobs.movement.go-to.walk :as gwalk]
            [jobs.movement.reach :as reach-job]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.lib.flee-walk :as walk]))

(defn sensed-only
  "Wrapped fake primitives for spec, lit, whose raw blockAt throws. rawWorld stays, as on the body: go-to's planner
  snapshot (reach/lookup) reads it. A spec's :jitter fixes the sight passes' ray jitter (perception's :rand), its :radius
  how far they see."
  [spec]
  (let [p (tu/fake (dissoc spec :jitter :radius :now))
        raw (fake-raw/create p)
        per (perception/create raw (cond-> {:now (or (:now spec) (constantly 1000000))}
                                     (:jitter spec) (assoc :rand (constantly (:jitter spec)))
                                     (:radius spec) (assoc :radius (:radius spec))))
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
           (escape/door (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0])))
    (fake/remove-block! p [1 64 0])
    (fake/remove-block! p [1 65 0])
    (is (= {:cells [] :through [2 64 0]}
           (escape/door-from (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0] 2))
        "row one dug: the far side is seen open")))

(deftest escape-door-from-goes-on-through-a-row-seen-solid
  (let [{:keys [p w]} (sensed-only (wall-east 2))]
    (fake/remove-block! p [1 64 0])
    (fake/remove-block! p [1 65 0])
    (is (= {:cells [[2 65 0] [2 64 0]] :through [3 64 0] :unseen true}
           (escape/door-from (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0] 2)))))

(deftest escape-door-from-takes-a-row-seen-open-above-and-unseen-below-for-the-far-side
  (let [block-at #(if (= % [3 65 0]) "air" "stone")]
    (is (= {:cells [[2 65 0] [2 64 0]] :through [3 64 0] :unseen true}
           (escape/door-from block-at #{[3 64 0]} [0 64 0] [1 0] 2)))))

(deftest escape-door-over-seen-cells-is-as-before
  (let [{:keys [w]} (sensed-only (assoc (wall-east 1) :blocks ground))]
    (is (nil? (escape/door (escape/block-at-of w) (escape/unseen-of w) [0 64 0] [1 0])) "open ground seen ahead: no door")))

(defn ^:async run-clear-path
  "Run clear-path :east (with args) over the sensed-only world spec as a child; its result."
  [spec & [args]]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        {:keys [w]} (sensed-only spec)
        out (atom :not-done)
        parent (tu/recording-parent 'jobs.access.clear-path (merge {:heading :east} args) out)
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

(deftest clear-path-takes-an-opening-beside-a-dug-cell-for-the-far-side
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec (-> (wall-east 3 (box 4 64 -1 4 65 1 "stone"))
                       (assoc :inventory [{:name "wooden_pickaxe" :count 1}])
                       (update :blocks dissoc "1,64,1" "1,65,1"))
              r (await (run-clear-path spec))]
          (is (= [:done [1 64 1]] [(:status r) (:through r)]) (pr-str r)))))))

(deftest clear-path-in-solid-rock-stops-at-what-it-may-spend-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec (assoc (wall-east 8) :inventory [{:name "wooden_pickaxe" :count 1}])
              cell-s (cost/dig-cells-s [(tools/clear-ms (tu/fake spec) "stone")])
              r (await (run-clear-path spec {:max-dig-s (* 7 cell-s)}))]
          (is (= [:stopped :too-dear] [(:status r) (:reason r)]) (pr-str r))
          (is (<= (count (:dug r)) 6) "no more rows than the seconds it may spend")
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
    (is (= [0 1] (reach-room/panel-step w (constantly :solid) {:x 0 :y 64 :z 0})) "a shut door facing north (its default state)")
    (is (true? (reach/standable-cell? w {:x 0 :y 64 :z 2} (fn [_ y _] (if (= y 63) :solid :open))))
        "kind-at decides feet and head; the floor read only for hazards")))

(deftest retreat-reads-an-unseen-cell-as-rock
  (let [{:keys [w]} (sensed-only (wall-east 1 {"2,64,0" "stone"}))
        block-at (respond.goal/block-at-fn w)]
    (is (= "stone" (block-at {:x 1 :y 65 :z 0})))
    (is (= "stone" (block-at {:x 2 :y 64 :z 0})) "behind the wall: unseen is taken for rock, never a free way")
    (is (zero? (walk/open-cells block-at {:x 0 :y 64 :z 0} [-1 0] 3)) "behind the body: open ground it never saw is no way")))

(deftest retreat-reads-an-unseen-head-cell-as-rock-over-a-seen-open-feet-cell
  (let [{:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 60
                                  :blocks (merge ground {"2,65,0" "stone"})})
        block-at (respond.goal/block-at-fn w)]
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

(defn search-clock
  "dig-route/search whose runs move clock on by the time their nodes take a body (about 0.2 ms a node): a search the
  engine goes on with in one tick stands the body still as long as it would in the world."
  [clock]
  (let [search dig-route/search]
    (fn [& args]
      (let [run (apply search args)]
        (fn [n] (let [[_ used :as r] (run n)] (swap! clock + (* 0.2 used)) r))))))

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
    (with-redefs [dig-route/search (search-clock clock)]
      (loop [i 0]
        (when (and (< i 1000) (seq (:list (core/state eng))))
          (swap! clock + 100)
          (await (core/tick! eng))
          (recur (inc i)))))
    {:out @out :seen @seen :p p}))

(defn ^:async run-job
  "Run job (a symbol) with args as a child over the sensed-only world spec; {:out :seen :p}. setup, when given, is called
  with the sensed-only {:p :w} first. The sight jitter is fixed at 0.5 unless the spec has :jitter: some jitters
  (0.54 to 0.59 in the dark tunnel) see one more cell, which changes the walk."
  [job spec args & [setup before]]
  (run-job-unpinned job (merge {:jitter 0.5} spec) args setup before))

(defn run-go-to
  "go-to over the sensed-only world with :unknown false unless args say otherwise: these cases test the escalations and the walk, not the unseen."
  [spec args & [setup before]]
  (run-job 'jobs.movement.go-to spec (merge {:unknown false} args) setup before))

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
          (is (re-find #":torches" (str (:text out))) "the words say what would help")
          (is (= 64 (js/Math.floor (second (:pos (fake/self p))))) "never dropped")
          (is (empty? (filter #(= :go-to.off-hand-torch (:kind %)) seen)) "no torch carried"))))))

(def shelter-over-shaft
  "Rock (stone x -2..12, y 52..66, z -2..2) with a room (x -1..1, y 64..65, z -1..1, the body's own shelter at the origin)
  over a 6-deep shaft at x 1 (feet cells y 63..58, floor 57) and a tunnel east from its foot (x 2..9, y 58..59, z 0)."
  (apply dissoc (box -2 52 -2 12 66 2 "stone")
         (concat (for [x (range -1 2) y [64 65] z (range -1 2)] (str x "," y "," z))
                 (for [y (range 58 64)] (str "1," y ",0"))
                 (for [x (range 2 10) y [58 59]] (str x "," y ",0")))))

(defn shelter-entry! [eng]
  (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built} {:cap 10 :ttl 86400000}))

(deftest go-to-shut-in-its-own-shelter-by-day-leaves-it-instead-of-giving-up-on-a-dark-shaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen out]} (await (run-go-to {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks shelter-over-shaft :time 1000
                                                :inventory [{:name "stone_pickaxe" :count 1}]}
                                               {:pos [9 58 0] :range 1 :unknown true}
                                               dark-and-looked shelter-entry!))]
          (is (= :leave-shelter (first (mapv :step (filter #(= :go-to.escalated (:kind %)) seen)))) (pr-str out)))))))

;; ---- go-to :unknown under ground, lit: a tunnel whose way on turns a corner out of sight

(defn round-the-corner
  "Rock (stone x -14..66, y 62..67, z -2..12), lit by torches (no sky): a tunnel through the body's cell (x -12..64, y 64..65,
  z 0) and one to the goal (z 10, x -12..64), joined (with join) by a passage south at x 64."
  [join]
  (apply dissoc (box -14 62 -2 66 67 12 "stone")
         (concat (for [x (range -12 65) y [64 65] z [0 10]] (str x "," y "," z))
                 (when join (for [z (range 1 10) y [64 65]] (str "64," y "," z))))))

(defn looking
  "The world lit as light says ({:light-default [sky block] :light {[x y z] [sky block]}}), and a sight pass to each side
  from every cell the body comes to (a body's passes run on a timer as it walks)."
  [light]
  (fn [{:keys [p w]}]
    (let [look! (fn [] (let [yaw (:yaw @(fake/state p))]
                         (doseq [y [0 90 180 270]]
                           (swap! (fake/state p) assoc :yaw y)
                           (perception/pass! (aget w "perception")))
                         (swap! (fake/state p) assoc :yaw yaw)))
          cell #(mapv js/Math.floor (:pos (:self %)))]
      (swap! (fake/state p) merge light {:pitch 0})
      (look!)
      (add-watch (fake/state p) :sight (fn [_ _ a b] (when (not= (cell a) (cell b)) (look!)))))))

(def lit-and-looking "The world lit by block light alone, looking as it walks." (looking {:light-default [0 15]}))

(defn corner-world
  "round-the-corner with the body at x 40, seeing 16 blocks, its loaded land 1 chunk round it: the goal (x -12) is
  unloaded at the start and the passage (x 64) is past sight; from the dead end by the goal the seen rim is unloaded."
  [join]
  {:self {:pos {:x 40.5 :y 64 :z 0.5}} :blocks (round-the-corner join) :viewChunks 1 :radius 16})

;; live twin: walkway-stair-out-of-view/stair-100-east-arrives. The rock guess (cells 4 past all seen open read stone)
;; closes the unseen passage, so every search ends exhausted; the body must tour to the rim of what it saw, not give up
(deftest go-to-under-ground-tours-past-the-rock-it-guessed-round-a-corner
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-go-to (corner-world true)
                                                {:pos [-12 64 10] :range 1 :ways #{:walk :clear-seal} :unknown true} lit-and-looking))]
          (is (= {:arrived true} (select-keys out [:arrived])) (str (pr-str out) " at " (:pos (fake/self p)))))))))

(deftest go-to-under-ground-gives-up-a-tunnel-with-no-way-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (run-go-to (corner-world false)
                                              {:pos [-12 64 10] :range 1 :ways #{:walk :clear-seal} :unknown true} lit-and-looking))]
          (is (= {:arrived false :reason :unreachable} (select-keys out [:arrived :reason])) (pr-str out)))))))

(deftest rounds-from-one-cell-with-no-gain-are-a-loop
  (let [rounds (fn [bests] (reduce (fn [[starts _] best] (gwalk/note-start starts [13 85 -237] best)) [{} false] bests))]
    (is (false? (second (rounds [20 20 20]))) "three rounds are fruitless tries, not yet a loop")
    (is (true? (second (rounds [20 20 20 20]))))
    (is (true? (second (rounds [20 19.5 20 19.5]))) "under a block of gain")
    (is (false? (second (rounds [20 20 18 18]))) "a block nearer is progress")))

;; live twin: walkway-stair-out-of-view/stair-100-east-arrives (b3c99e64). Down the stair and west on the ground, a search
;; cut by its budget got nearest at the walkway's dead end over the goal, where the body had stood 13 off: the walk swung
;; back up the stair to it, and off it again
(deftest a-budget-cut-search-that-gets-no-nearer-than-the-body-has-been-tours-the-edge-instead
  (let [mem* (atom {})
        c (gcol/stub-ctx (tu/fake {}) mem* {:unknown true})
        check (gwalk/known-end-check c {:x 60 :y 0 :z 16} nil)
        plan (fn [reason end] {:status "partial" :r #js {:reason reason} :steps [{:x 179 :y 0 :z 16} end]})
        dead-end {:x 61 :y 11 :z 8}]
    (swap! gcut/approaches assoc (:id c) {:closest 13.2})
    (is (= {:known-end true} (check (plan "budget" dead-end) false)))
    (is (nil? (check (plan "budget" {:x 64 :y 0 :z 16}) false)) "an end nearer than the body has been")
    (is (nil? (check (plan "exhausted" dead-end) false)) "an ended search")
    (swap! mem* assoc :edge-mode true)
    (is (nil? (check (plan "budget" dead-end) true)) "already touring the edge")
    (swap! gcut/approaches dissoc (:id c))))

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
                                                   {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} watch!))]
          (is (true? @held) "the off hand held the torch during the walk")
          (is (= 1 (count (filter #(= :go-to.off-hand-torch (:kind %)) seen))) "once per call")
          (is (true? (:arrived out)) (str "the torch lights the shaft; looking over its rim shows the floor, so the body goes on " (pr-str out))))))))

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
                                                {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} watch!))
              w @(fake/state p)]
          (is (= "soul_torch" @held) "the off hand held the soul torch during the walk")
          (is (true? (:arrived out)) (pr-str out))
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
                                                 {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} dark-and-looked))
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
                                                     {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} dark-and-looked))]
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
                                                          {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} dark-and-looked))]
          (is (empty? (filter #(= :walker-fault (:kind %)) seen)) (pr-str (filter #(= :walker-fault (:kind %)) seen)))
          (is (= :unseen-floor-dark (:why out)) (pr-str out)))))))

(deftest go-to-with-unknown-returns-a-torch-a-cut-call-left-in-the-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen p]} (await (run-go-to (assoc in-tunnel :equipment {"offHand" {:name "torch" :count 4}})
                                                 {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} dark-and-looked
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
                                                 {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} throw-held!))
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
                                            {:pos [9 61 0] :range 1 :ways #{:walk :clear-seal} :unknown true :torches :never} dark-and-looked))]
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
              seen-all (await (preview {:unknown false}))
              masked (await (preview {:unknown true}))]
          (is (true? (:found seen-all)) (pr-str seen-all))
          (is (= [false :unseen-floor-dark] [(:found masked) (:reason masked)]) (pr-str masked))
          (is (= {:x 1 :y 62 :z 0} (select-keys (:dark-best masked) [:x :y :z])) (pr-str masked)))))))

(deftest go-to-escalation-child-sees-the-torches-in-the-inventory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [torches (atom :none)
              child {:ways [:carried :chest :craft :gather] :round (fn [c] (reset! torches (reduce + (map :count (filter #(= "torch" (:name %)) (u/inventory (:primitives c))))))
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

(def corridor
  "A lit 2-high corridor (x 0..20, y 64..65, z 0) in stone, the body at x 10."
  {:self {:pos {:x 10.5 :y 64 :z 0.5}} :pitch 0
   :blocks (apply dissoc (box -2 63 -1 22 66 1 "stone") (for [x (range 0 21) y [64 65]] (str x "," y ",0")))})

(defn walled-behind
  "The body looks west down the corridor, turns east, and the corridor is walled at x 5 out of its sight."
  [{:keys [p w]}]
  (swap! (fake/state p) assoc :yaw 90)
  (perception/pass! (aget w "perception"))
  (swap! (fake/state p) assoc :yaw 270)
  (swap! (fake/state p) update :blocks assoc [5 64 0] "stone" [5 65 0] "stone"))

(deftest a-wall-built-out-of-sight-is-planned-through-as-remembered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [w] :as world} (sensed-only corridor)
              _ (walled-behind world)
              snapshot (.-snapshot (wworld/path-world w (wworld/unknown-spec)))
              route (await (quote-route corridor {:pos [2 64 0] :range 0 :unknown true} walled-behind))]
          (is (= (.stateAt snapshot 6 64 0) (.stateAt snapshot 5 64 0) (.stateAt snapshot 5 65 0)) "remembered air")
          (is (true? (:found route)) (pr-str route))
          (is (= 8 (:length route)) (pr-str route)))))))

(deftest the-masked-planner-keeps-a-remembered-door-gate-trapdoor-fire-or-water-and-prices-the-chance-it-changed
  (doseq [[block kind] [["oak_door" memory-age/DOOR] ["oak_fence_gate" memory-age/DOOR] ["oak_trapdoor" memory-age/DOOR]
                        ["fire" memory-age/FIRE] ["water" memory-age/FLUID]]]
    (let [clock (atom 1000000)
          {:keys [p w]} (sensed-only (assoc-in (assoc corridor :yaw 90 :now #(deref clock)) [:blocks "5,64,0"] block))
          _ (perception/pass! (aget w "perception"))
          _ (swap! (fake/state p) assoc :yaw 270) ; looking away: memory, not sight
          read (fn [] (let [pw (wworld/path-world w (wworld/unknown-spec))
                            changed (or (.-changed (wworld/unknown-of pw)) (constantly 0))]
                        {:cell (.stateAt (.-snapshot pw) 5 64 0) :changed (changed 5 64 0) :stone (changed 5 63 0)}))
          fresh (read)
          scan (wworld/way-scan w)
          _ (swap! clock + 1000 (memory-age/half-age-ms kind))
          aged (read)]
      (is (= (:cell fresh) (:cell aged)) (str block ": the remembered state however old"))
      (is (zero? (:changed fresh)) (str block " just seen: no chance worth pricing"))
      (is (= 0.5 (:changed aged)) (str block " past its half age: priced at one half"))
      (is (zero? (:stone aged)) "stone does not age")
      (is (not (identical? scan (wworld/way-scan w))) (str block ": way-scan reads again once the chance moved")))))

(deftest the-air-beside-remembered-water-ages-as-the-water-does
  (let [clock (atom 1000000)
        {:keys [p w]} (sensed-only (assoc-in (assoc corridor :yaw 90 :now #(deref clock)) [:blocks "5,64,0"] "water"))]
    (perception/pass! (aget w "perception"))
    (swap! (fake/state p) assoc :yaw 270)
    (swap! clock + 1000 (memory-age/half-age-ms memory-age/FLUID))
    (let [changed (.-changed (wworld/unknown-of (wworld/path-world w (wworld/unknown-spec))))]
      (is (= [0.5 0.5 0.5 0] (mapv #(changed % 64 0) [4 5 6 8])) "the water at 5 and the air at 4 and 6; 8 is not beside it"))))

(deftest go-to-walks-into-a-wall-built-out-of-sight-sees-it-and-digs-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (run-go-to (assoc corridor :inventory [{:name "stone_pickaxe" :count 1}])
                                                     {:pos [2 64 0] :range 0 :unknown true} walled-behind))]
          (is (= {:arrived true} out) (pr-str (last seen)))
          (is (= [2 64 0] (get-in @(fake/state p) [:self :pos])))
          (is (= [:dig-route] (steps seen)) "a corridor in rock: the rock guess holds, so the dig leg goes first"))))))

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
            (let [c (assoc c :unknown (wworld/unknown-spec))]
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

(def probed (atom nil))

(def probe-drops-job
  "A job that leans over the lip to the pit floor under the landing [0 55 0], then probes the way to the pit and keeps the drops left to look at in probed."
  {:round (fn ^:async probe-drops-round [c]
            (ctx/update-mem! c assoc :probe-drops [{:from [-1 60 0] :floor [0 55 0]}])
            (await (gth/look-at-drops! c))
            (await (gth/probe-plan! c {:x 1 :y 56 :z 0} 0))
            (reset! probed (:probe-drops (ctx/mem c)))
            (ctx/end c :done nil))})

(defn ^:async probed-drops [pad]
  (await (run-job probe-drops-job {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (slime-pit pad)} {}))
  @probed)

(deftest a-drop-onto-a-floor-in-view-that-does-not-bounce-is-not-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [] (await (probed-drops {}))))))))

(deftest a-drop-onto-a-seen-slime-floor-is-still-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= 1 (count (await (probed-drops slime-floor)))))))))

(defn cliff-foot
  "Stone x -12..-1 to y 59 (the body on it, feet y 60), then open ground x 0..8 at y 53 (a 6 drop at the cliff face), a 3x3 slime pad at x 0..2 laid in that ground. face: the block of the cliff's last column x -1."
  [face]
  (-> (merge (box -12 50 -4 -2 59 4 "stone") (box -1 50 -4 -1 59 4 face) (box 0 50 -4 8 53 4 "stone"))
      (into (for [x (range 0 3) z (range -1 2)] [(str x ",53," z) "slime_block"]))))

(deftest go-to-refuses-a-slime-pad-at-a-cliff-foot-whose-face-it-cannot-see
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (run-go-to {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (cliff-foot "stone")}
                                              {:pos [1 54 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}}))]
          (is (= [:needs-health :unreachable] ((juxt :why :reason) out)) (pr-str out)))))))

(deftest go-to-takes-a-slime-pad-drop-at-a-cliff-foot-whose-face-is-glass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-go-to {:self {:pos {:x -6.5 :y 60 :z 0.5}} :exact-eye true :bodyHitbox true :blocks (cliff-foot "glass")}
                                                   {:pos [1 54 0] :range 0 :ways #{:walk :clear-seal} :floor {:health 20}}))]
          (is (= {:arrived true} out) (pr-str (last seen))))))))

(deftest a-body-shut-in-on-a-plateau-far-from-a-sealed-pocket-gets-off-at-the-edge-nearest-it
  ;; world case go-to-far-enclosed-goal: a plateau 6 high on a stone mass, the pocket 70 across and 9 down too far for one
  ;; dig route; no stair, pillar or wall leads off the top, so it walks to the edge nearest the pocket, looks over it and
  ;; gets down to the plain (the old approaches round the top, each no nearer, gave up after 3); a dig route over cells it
  ;; guessed is tried from the top, and the ways chosen again once a guess shows open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box 0 50 0 99 54 63 "stone") (box 0 55 0 20 60 20 "stone") {"80,51,56" "air" "80,52,56" "air"})
              {:keys [out seen p]} (await (run-go-to {:self {:pos {:x 10.5 :y 61 :z 10.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]}
                                                     {:pos [80 51 56] :range 0}))]
          (is (true? (:arrived out)) (pr-str (select-keys out [:why :text :escalation]) (map (juxt :step :at) (filter #(= :go-to.escalated (:kind %)) seen))))
          (is (<= (count (filter #(and (= :approach (:step %)) (= 61 (second (:at %)))) (filter #(= :go-to.escalated (:kind %)) seen))) 2) "no tour round the top")
          (is (some #(and (= :dig-route (:step %)) (= 61 (second (:at %)))) (filter #(= :go-to.escalated (:kind %)) seen)) "a dig route over guessed rock is tried from the top")
          (is (= [80 51 56] (mapv js/Math.floor ((juxt :x :y :z) (u/pos-of (.-pos (.self p))))))))))))

(deftest a-buried-bodys-shut-in-verdict-reads-the-unseen-rock-as-its-plans-do
  ;; a dark 1x2 pocket in a stone mass, its walls mostly unseen: the plans read unseen cells past rock-beyond as stone and
  ;; find no way, so the verdict must too (read as open ground, the body was free: no escalation on :start-enclosed,
  ;; :goal-unloaded or a loop, a pickaxe fetched, a give-up without :shut-in)
  (let [{:keys [p w]} (sensed-only {:self {:pos {:x 0.5 :y 55 :z 0.5}} :blocks (dissoc (box -8 50 -8 8 60 8 "stone") "0,55,0" "0,56,0")})
        shut? (fn [spec] (escape/walk-enclosed? {:primitives w :args {} :unknown spec}))
        looked (wworld/unknown-spec)
        no-guess #(settings/with-settings {:jobs.lib.walk.world/rock-beyond false} %)]
    (swap! (fake/state p) assoc :light-default [0 0])
    (doseq [[x y z] [[1 55 0] [-1 55 0] [0 55 1] [0 55 -1] [1 56 0] [-1 56 0] [0 56 1] [0 56 -1] [0 54 0] [0 57 0]]]
      (.add ^js (.-blocked looked) (str x "," y "," z)))
    (is (true? (shut? (wworld/unknown-spec))) "buried: the unseen rock is rock")
    (is (false? (no-guess #(shut? (wworld/unknown-spec)))) "no rock guess: unseen cells are open, as the plans read them")
    (is (true? (no-guess #(shut? looked))) "walls looked at and not seen read stone, as the plans read them")))

(deftest a-walk-spec-spares-the-cells-round-the-goal-from-the-rock-guess
  (let [{:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 55 :z 0.5}} :blocks (dissoc (box -8 50 -8 8 60 8 "stone") "0,55,0" "0,56,0")})
        c {:primitives w :view (constantly {:data {} :now 0})}
        kind-at-goal (fn [goal] ((wworld/unseen-kind w (escape/walk-spec c goal)) 6 55 0))]
    (swap! (fake/state w) assoc :light-default [0 0])
    (is (= [:solid :ground] [(kind-at-goal nil) (kind-at-goal [6 55 0])]))))

(deftest the-reach-job-asks-the-shut-in-verdict-over-a-spec-seeded-with-its-goal
  (let [{:keys [w]} (sensed-only {:self {:pos {:x 0.5 :y 55 :z 0.5}} :blocks (box -8 50 -8 8 60 8 "stone")})
        goals (atom [])
        walk-spec escape/walk-spec
        c {:primitives w :args {:pos {:x 6 :y 55 :z 0} :walk {:doors :never}} :view (constantly {:data {} :now 0})}]
    (with-redefs [escape/walk-spec (fn [c goal] (swap! goals conj goal) (walk-spec c goal))]
      (reach-job/fetch-tool? c))
    (is (= [[6 55 0]] @goals))))

;; ---- go-to :unknown in a lit room: a walled walkway whose way down is a stair at its east end

(defn walkway
  "A lit stone room (inside x -2..len+24, y 64..87, z -4..10; floor y 63): a walkway (floor y 84, z 0, x 0..len) walled at
  z -1 and 1 (y 85..86) and shut at x -1, and a 3-wide stair (z -1..1) down east of it, x len+1 (top y 83) to x len+21 (y 63).
  The walls hide the air beside the walkway, each step the air under the step beside it."
  [len]
  (merge (apply dissoc (box -3 63 -5 (+ len 25) 88 11 "stone")
                (for [x (range -2 (+ len 25)) y (range 64 88) z (range -4 11)] (str x "," y "," z)))
         (box 0 84 0 len 84 0 "stone") (box -1 85 -1 len 86 -1 "stone") (box -1 85 1 len 86 1 "stone") (box -1 85 0 -1 86 0 "stone")
         (into {} (for [i (range 21) z [-1 0 1]] [(str (+ len 1 i) "," (- 83 i) "," z) "stone"]))))

(def walkway-room (walkway 20))

;; live twin: walkway-stair-out-of-view/stair-100-east-arrives
(deftest go-to-down-a-stair-past-unseen-air-beside-it-in-a-lit-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (run-go-to {:self {:pos {:x 15.5 :y 85 :z 0.5}} :blocks walkway-room :radius 16 :exact-eye true :bodyHitbox true}
                                                     {:pos [5 64 8] :range 1 :ways #{:walk :clear-seal} :unknown true} lit-and-looking))
              replans (filter #(= :replan (:kind %)) seen)]
          (is (= {:arrived true} (select-keys out [:arrived])) (str (pr-str out) " at " (:pos (fake/self p))))
          (is (< (count replans) (wwatch/max-learned)) "each floor found missing shows the stair's side, not one cell of it"))))))

;; the way on from the walkway's far end runs through air no look has reached, guessed a dark cave (Q16): the walk goes to
;; the cut before the first guessed step, looks there and is priced again (reprice/safe-prefix), not held :risky at the
;; stair top (the tour budget is lifted: the twin's walk back under the walkway is long for its goal)
(deftest go-to-down-a-stair-at-the-far-end-of-a-long-lit-walkway-is-not-held-risky
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (settings/with-settings {:jobs.lib.cost.value/tour-worth 1000}
                                       #(run-go-to {:self {:pos {:x 15.5 :y 85 :z 0.5}} :blocks (walkway 60) :radius 16 :exact-eye true :bodyHitbox true}
                                                   {:pos [5 64 8] :range 1 :ways #{:walk :clear-seal} :unknown true} lit-and-looking)))]
          (is (= {:arrived true} (select-keys out [:arrived])) (str (pr-str out) " at " (:pos (fake/self p)))))))))

(def tunnel-into-cave
  "A buried stone tunnel (floor y 63, feet and head y 64..65) lit (block light 3) from x -2 to 20 along z 0 and on to z 2
  at x 20, where it opens north into a dark cave (x 14..26, y 64..68, z 3..34) the tunnel's rock hides until the turn; the
  light spills 2 blocks into it at x 20."
  {:blocks (apply dissoc (box -3 63 -1 27 69 35 "stone")
                  (concat (for [x (range -2 21) y [64 65]] (str x "," y ",0"))
                          (for [z [1 2] y [64 65]] (str "20," y "," z))
                          (for [x (range 14 27) y (range 64 69) z (range 3 35)] (str x "," y "," z))))
   :light (into {} (concat (for [[x z] (concat (for [x (range -2 21)] [x 0]) [[20 1] [20 2]]) y [64 65]] [[x y z] [0 3]])
                           (for [y [64 65] [z l] [[3 2] [4 1]]] [[20 y z] [0 l]])
                           (for [[x y] [[19 64] [21 64] [19 65] [21 65] [20 66]]] [[x y 3] [0 1]])))})

(def tunnel-with-locked-gate
  "tunnel-into-cave with a locked fence gate across the tunnel at x 17 and a lit way round it, 6 blocks longer, along z -3."
  (let [round (for [[x z] (concat (for [z (range -3 0)] [16 z]) (for [z (range -3 0)] [18 z]) [[17 -3]]) y [64 65]] (str x "," y "," z))]
    (-> tunnel-into-cave
        (update :blocks #(-> (apply dissoc (merge (box -3 63 -8 27 69 -2 "stone") %) (concat round ["17,64,0"])) (assoc "17,64,0" "oak_fence_gate")))
        (update :light into (for [k round] [(mapv js/parseInt (.split k ",")) [0 3]]))
        (assoc :states {"17,64,0" {:open false :facing "east" :locked true}}))))

;; the plan runs on from the lit tunnel into the cave, unseen when planned: its walk stops at the cut before the first step
;; guessed a dark cave, looks, and is priced by the dark it then sees, on every way the walk could go on past the cut
;; unpriced: the look-ahead finding the cells open, a refresh refused (the old plan kept), the plan walked unwatched
(defn ^:async walk-into-cave
  "go-to from the tunnel's west end to the cave, a hurt, fed body that may risk 0.2 hp and heals while it waits: {:out :cave :turn} (cave the cells of the cave past the spilled light it stood in, turn those at the tunnel's turn, x 20 z 1..2)."
  [& [tunnel]]
  (let [tunnel (or tunnel tunnel-into-cave)
        cells (atom #{})
        look! (looking {:light-default [0 0] :light (:light tunnel)})
        track! (fn [{:keys [p] :as s}]
                 (look! s)
                 (add-watch (fake/state p) :track (fn [_ _ _ b] (swap! cells conj (mapv js/Math.floor (:pos (:self b)))))))
        {:keys [out]} (await (settings/with-settings {:jobs.lib.cost.risk/risk-share 0.05}
                               #(run-go-to (cond-> {:self {:pos {:x 0.5 :y 64 :z 0.5} :health 4 :food 20} :blocks (:blocks tunnel) :radius 16 :exact-eye true :bodyHitbox true}
                                             (:states tunnel) (assoc :states (:states tunnel)))
                                           {:pos [20 64 32] :range 1 :ways #{:walk} :unknown true} track!)))]
    {:out out
     :cave (sort (filter #(>= (nth % 2) 5) @cells))
     :turn (sort (filter #(and (= 20 (first %)) (<= 1 (nth % 2) 2)) @cells))}))

(deftest go-to-from-a-lit-tunnel-into-a-dark-cave-prices-the-cave-before-it-walks-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out cave turn]} (await (walk-into-cave))]
          (is (= :risky (:reason out)) (pr-str out))
          (is (seq turn) "the walk goes on up to the turn")
          (is (empty? cave) "the look-ahead finds the cells open"))
        (let [{:keys [out cave turn]} (with-redefs [wwatch/take-refresh? (constantly false)] (await (walk-into-cave)))]
          (is (= :risky (:reason out)) (pr-str out))
          (is (seq turn))
          (is (empty? cave) "a refresh refused keeps the old plan"))
        (let [{:keys [out cave turn]} (with-redefs [wwatch/max-watch-replans 0] (await (walk-into-cave)))]
          (is (= :risky (:reason out)) (pr-str out))
          (is (seq turn))
          (is (empty? cave) "the plan walked unwatched"))))))

(deftest go-to-from-a-lit-tunnel-past-a-locked-gate-prices-the-cave-before-it-walks-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out cave turn]} (with-redefs [wwatch/max-watch-replans 0] (await (walk-into-cave tunnel-with-locked-gate)))]
          (is (= :risky (:reason out)) (pr-str out))
          (is (seq turn) "the way round the gate is walked up to the turn")
          (is (empty? cave) "the re-plan after the gate is cut and priced like the first"))))))

(def stair-body {:self {:pos {:x 15.5 :y 85 :z 0.5}} :blocks walkway-room :radius 16 :exact-eye true :bodyHitbox true})
(def stair-job {:pos [5 64 8] :range 1 :ways #{:walk :clear-seal} :unknown true})

(def look-over-walkway-end
  "A job that looks over the walkway's east end (the body on [20 84 0]) toward the missing floor [21 83 0]."
  {:round (fn ^:async look-round [c]
            (await (drive/look-over-edge! c [21 83 0]))
            (ctx/end c :done {}))})

(defn walkway-end-acts
  "{:sneaks the sneaking steer ticks, :looks the look acts} of look-over-edge! at the walkway end, with the extra spec."
  [extra]
  (let [acts (atom [])
        looks (atom 0)]
    (-> (run-job look-over-walkway-end (merge {:self {:pos {:x 20.5 :y 85 :z 0.5}} :blocks walkway-room :radius 16 :exact-eye true :bodyHitbox true} extra)
                 {} (fn [s] ((record-moves! acts) s)
                      (.override (.-world (:p s)) "look" (fn [token a impl] (swap! looks inc) (impl token a)))))
        (.then (fn [_] {:sneaks (filter #(get-in % [1 "sneak"]) @acts) :looks @looks})))))

(deftest a-hostile-near-holds-back-the-lean-to-a-lip-the-walk-learned-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (seq (:sneaks (await (walkway-end-acts {})))) "control: with no hostile the body leans")
        (let [{:keys [sneaks looks]} (await (walkway-end-acts {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 24 :y 80 :z 0}}]}))]
          (is (empty? sneaks))
          (is (pos? looks) "a look moves nothing, so it still happens"))))))

(defn ^:async dark-stair-replans []
  (let [{:keys [out seen]} (await (run-go-to stair-body stair-job
                                             (fn [{:keys [p]}] (swap! (fake/state p) assoc :light-default [0 0] :pitch 30))))]
    {:out out :replans (filter #(= :replan (:kind %)) seen)}))

;; how many cells the dark walk learns follows the prices of the unseen ways it tries, so the count is not pinned
(deftest go-to-down-a-stair-in-a-dark-room-ends-with-bounded-replans
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out replans]} (await (dark-stair-replans))
              learned (mapcat :learned replans)]
          (is (= {:arrived true} (select-keys out [:arrived])) (str (pr-str out) ": the drop off the stair's foot is a short climb back, so it is taken"))
          (is (every? :learned replans) "each replan learns an unseen cell, none is a price changing its mind")
          (is (apply distinct? learned) "no cell is learned twice")
          (is (= (count replans) (count (:replans (await (dark-stair-replans))))) "the same count every run: the walk clock is stopped"))))))

;; the walls hide the room: past what sight has reached the planner reads the next wall cell as open, so the walk learns
;; a wall cell a sight's reach on, again and again, while it walks on; none of that is a stretch to give up
(deftest go-to-along-a-long-walkway-learns-its-wall-without-spending-the-learned-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [follow! drive/follow!
              spent (atom [])
              {:keys [out p]} (with-redefs [drive/follow! (fn ^:async f [c plan opts]
                                                                 (let [r (await (follow! c plan opts))]
                                                                   (when (:learn-spent (:done r)) (swap! spent conj (:at (:done r))))
                                                                   r))]
                                     (await (run-go-to {:self {:pos {:x 0.5 :y 85 :z 0.5}} :blocks (walkway 140) :radius 16 :exact-eye true :bodyHitbox true}
                                                       {:pos [140 64 8] :range 1 :ways #{:walk :clear-seal} :unknown true} lit-and-looking)))]
          (is (= {:arrived true} (select-keys out [:arrived])) (str (pr-str out) " at " (:pos (fake/self p))))
          (is (= [] @spent) "no walk ends learn-spent"))))))
