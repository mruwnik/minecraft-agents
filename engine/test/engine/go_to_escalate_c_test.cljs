(ns engine.go-to-escalate-c-test
  "jobs.movement.go-to escalation (pillar, stair, clear-path, a walk to a wall) and jobs.lib.escape, against the fake
  world."
  (:require [engine.contract :as contract]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-test :as gt]
            [engine.go-to-support :refer [hiding]]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [plan.shape :as shape]
            [jobs.movement.go-to.escalation :as esc]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [triggers.access.scaffold-left :as scaffold-left]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [engine.go-to-escalate-support :refer [setup recording-parent tick-out! run-job! feet block events-of calls pit-cells pit stone-pit in-pit room in-room hollow ba ledge-room on-ledge wide-pit plank-yard holes dug-cells foreign trespass deep-pit stone-room run-in-shelter! ledger restores-after-the-last-stair? deep-shaft own-pit-wall cobble-room room-and-deck pocket free-by-pocket high-pocket moat door-cell dark-at gap-tunnel long-loop loop-tolls stair-below-surface cave below-pocket dig-route-run!]]))

(deftest escape-choose-walks-along-the-wall-when-the-wall-in-line-is-an-iron-door
  (let [iron (-> room (assoc "3,64,0" "iron_door" "3,65,0" "iron_door"))
        c (escape/choose (tu/fake (merge in-room {:blocks iron :self {:pos {:x 2 :y 64 :z 0}}})) [2 64 0] [8 64 0])]
    (is (= :approach (:step c)))
    (is (not= [2 64 0] (:pos c)))))

(deftest escape-choose-searches-for-a-door-spot-once
  (let [iron (-> room (assoc "3,64,0" "iron_door" "3,65,0" "iron_door"))
        calls (atom 0)
        real escape/door-spot]
    (with-redefs [escape/door-spot (fn [& args] (swap! calls inc) (apply real args))]
      (escape/choose (tu/fake (merge in-room {:blocks iron :self {:pos {:x 2 :y 64 :z 0}}})) [2 64 0] [8 64 0]))
    (is (= 1 @calls))))

(deftest go-to-escalates-on-a-door-that-doors-never-refuses
  (let [ok? (fn [result doors] (esc/escalate-reason? result doors))]
    (is (true? (ok? {:reason :exhausted} :shut)))
    (is (true? (ok? {:reason :start-enclosed} :shut)) "a pen sealed in the loaded world, goal far and unloaded (card 5a822a99)")
    (is (true? (ok? {:reason :abilities :kind :open} :never)) "the planner's way through a door, refused as a wall")
    (is (false? (ok? {:reason :abilities :kind :open} :shut)))
    (is (false? (ok? {:reason :abilities :kind :break} :never)))
    (is (false? (ok? {:reason :stuck} :never)))))

(deftest escape-enclosed-counts-doors-as-walls-under-doors-never
  (let [wide (merge room (box -20 63 -20 30 63 20 "stone"))
        p (tu/fake (merge in-room {:blocks (assoc wide "3,64,0" "oak_door" "3,65,0" "oak_door")}))]
    (is (false? (escape/enclosed? p :shut)) "a wooden door is a way out")
    (is (true? (escape/enclosed? p :never)) "doors :never: the room is shut")))

;; ------------------------------------------------------------------ other headings (card 4ad29ed7)
(deftest escape-choose-skips-only-the-refused-stair-heading
  (let [p (tu/fake (merge in-pit {:blocks pit}))
        first-heading (:heading (escape/choose p [0 61 0] [10 64 0]))
        again (escape/choose p [0 61 0] [10 64 0] (constantly true) {:skip #{[:stair first-heading]}})]
    (is (= :east first-heading))
    (is (= :stair (:step again)))
    (is (not= first-heading (:heading again)))))

(deftest escape-enclosed-counts-an-unseen-door-as-a-wall-under-doors-never
  ;; live go-to-escalate/oak-door-with-doors-never-digs: in a dark room the door was never seen
  (let [wide (merge room (box -20 63 -20 30 63 20 "stone"))
        p (tu/fake (merge in-room {:blocks (assoc wide "3,64,0" "oak_door" "3,65,0" "oak_door")}))
        door? #{[3 64 0] [3 65 0]}]
    (aset p "sensedAt" (fn [pos] (if (door? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (is (true? (escape/enclosed? p :never)))))

(deftest go-to-climbs-out-of-a-pit-by-another-heading-when-the-first-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lava-pit (assoc pit "2,63,1" "lava")
              {:keys [out seen]} (await (run-job! (merge in-pit {:blocks lava-pit}) {:pos [10 64 0] :range 1}))]
          (is (true? (:arrived @out)))
          (is (< 1 (count (events-of seen :go-to.escalated))) "the first heading was refused, another was tried"))))))

(deftest go-to-escalates-from-the-free-part-of-a-shut-iron-door-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [iron (assoc room "3,64,0" "iron_door" "3,65,0" "iron_door")
              states {"3,64,0" {:open false :half "lower" :facing "west"} "3,65,0" {:open false :half "upper" :facing "west"}}
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 3.3 :y 64 :z 0.5}} :blocks iron :states states
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [8 64 0] :range 1}))]
          (is (seq (events-of seen :go-to.escalated)) "a body shut in by the panel escalates")
          (is (= {:arrived true} @out))
          (is (< 6 (first (feet p)))))))))

;; ------------------------------------------------------------------ rounds follow-ups (card be8f7a88)
(deftest a-put-back-whose-place-fails-its-condition-is-skipped-and-stays-in-the-ledger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [place (get registry/jobs 'jobs.blocks.place)
              flaky (assoc place :can-start? (constantly (contract/later :flaky "flaky")))
              {:keys [out p seen eng]} (with-redefs [registry/jobs (assoc registry/jobs 'jobs.blocks.place flaky)]
                                         (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                          'jobs.movement.go-to (merge {:pos [10 64 0] :range 1 :ways #{:walk :stair}} trespass) foreign)))
              skipped (:cells (first (events-of seen :go-to.restore-skipped)))]
          (is (= {:arrived true} @out))
          (is (= #{:flaky} (set (map :why skipped))) "a skip with the place's cause")
          (is (= [[1 62 0] [1 63 0] [2 63 0]] (sort (mapv :cell skipped))) "every hole is left for restore-broken")
          (is (= "air" (block p [1 62 0])) "nothing was put back"))))))

(deftest go-to-escalates-on-a-goal-cut-off-as-on-a-walled-in-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge {:self {:pos {:x 5 :y 64 :z 0}}} {:blocks room-and-deck})
              {:keys [out seen]} (await (run-job! world {:pos [8 71 0] :range 0}))
              {off :out} (await (run-job! world {:pos [8 71 0] :range 0 :ways #{:walk :clear-seal} :spend {:health 0}}))]
          (is (= :goal-cut-off (:why @off)) "without escalation the planner's verdict is cut-off")
          (is (seq (events-of seen :go-to.escalated)) "shut in: it escalates")
          (is (= :unreachable (:reason @out))))))))

(deftest a-free-body-digs-into-a-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! free-by-pocket {:pos [8 64 0] :range 0}))]
          (is (true? (:arrived @out)) "not shut in, but the goal is: it digs to it")
          (is (= [8 64 0] (feet p)))
          (is (some #{:clear-path} (map :step (events-of seen :go-to.escalated)))))))))

(deftest a-free-body-digs-down-to-a-walled-in-goal-below-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [below (apply dissoc (box -20 55 -20 20 63 20 "stone") ["3,60,0" "3,61,0"])
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks below
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [3 60 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [3 60 0] (feet p)))
          (is (= [:dig-route] (map :step (events-of seen :go-to.escalated))) "no door or stair leads down: the fewest cells are dug"))))))

(deftest a-wall-nearby-does-not-lead-into-a-goal-below
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (assoc below-pocket "5,64,3" "stone")
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [3 60 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:dig-route] (map :step (events-of seen :go-to.escalated))) "a walk to a wall cuts no way down"))))))

(deftest a-body-on-a-plateau-it-cannot-walk-off-digs-down-to-a-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; 29x29 top 6 above a floor: fewer cells than reach/room-cells, so the body counts as shut in
        (let [blocks (apply dissoc (merge (box -40 57 -40 40 57 40 "stone") (box -14 58 -14 14 63 14 "stone")) ["3,60,0" "3,61,0"])
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [3 60 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [3 60 0] (feet p)))
          (is (= [:dig-route] (map :step (events-of seen :go-to.escalated))) "the goal is a dig away: no wall is walked to")
          (is (empty? (events-of seen :go-to.wall-walk)) "no wall is walked to"))))))

(deftest a-walled-in-goal-is-not-dug-to-in-another-s-zone-or-under-walk-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[what args zones] [[:zone {} [{:name "vault" :owner "Miles" :min [6 60 -2] :max [10 70 2]}]]
                                   [:walk-only {:ways #{:walk}} nil]]]
          (let [{:keys [out p seen]} (await (run-job! free-by-pocket 'jobs.movement.go-to (merge {:pos [8 64 0] :range 0} args) zones))]
            (is (false? (:arrived @out)) what)
            (is (= :goal-enclosed (:why @out)) what)
            (is (re-find #"the goal is walled in" (:text @out)) what)
            (is (empty? (calls p "dig")) what)
            (is (empty? (events-of seen :go-to.escalated)) what)))))))

(deftest a-walk-only-call-walks-to-the-wall-of-a-walled-in-goal-when-its-budget-allows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spend from at] [[nil {:x 0 :z 0} [4 64 0]] [{:wall-time 0.5} {:x 0 :z 0} [0 64 0]] [nil {:x 3 :z 2} [4 64 2]]]]
          (let [{:keys [out p]} (await (run-job! (update-in free-by-pocket [:self :pos] merge from) (cond-> {:pos [8 64 0] :range 0 :ways #{:walk}} spend (assoc :spend spend))))]
            (is (= :goal-enclosed (:why @out)) (pr-str spend @out))
            (is (= at (feet p)) (str spend ": the nearest cell to the goal, when the walk there fits the budget"))
            (is (empty? (calls p "dig")))))))))

(deftest a-wall-walk-takes-the-short-walk-among-near-equal-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; west face one block thicker than the north face: the cell north of the goal is 1 nearer, but 8 blocks of walking farther
        (let [blocks (apply dissoc (merge (box -20 63 -20 20 63 20 "stone") (box 5 64 -2 10 66 2 "stone")) ["8,64,0" "8,65,0"])
              {:keys [out p]} (await (run-job! {:self {:pos {:x 0 :y 64 :z 0}} :blocks blocks}
                                               {:pos [8 64 0] :range 0 :ways #{:walk}}))]
          (is (= :goal-enclosed (:why @out)) (pr-str @out))
          (is (= [4 64 0] (feet p))))))))

(deftest a-free-body-far-from-a-walled-in-goal-walks-to-the-nearest-cell-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (assoc-in free-by-pocket [:self :pos] {:x -15 :y 64 :z 0}) {:pos [8 64 0] :range 0}))]
          (is (true? (:arrived @out)))
          (is (= [8 64 0] (feet p)))
          (is (= [:approach :clear-path] (take 2 (map :step (events-of seen :go-to.escalated)))))
          (is (every? #(<= 5 (first %) 8) (dug-cells p)) "nothing dug on the open land of the walk there"))))))

(deftest a-free-body-stairs-or-pillars-up-into-a-higher-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:pos {:x 0 :y 64 :z 0}} :blocks high-pocket
                     :inventory [{:name "stone_pickaxe" :count 1} {:name "cobblestone" :count 16}]}
              {:keys [out p seen]} (await (run-job! world {:pos [8 67 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text])))
          (is (= [8 67 0] (feet p)))
          (is (some #{:stair :pillar} (map :step (events-of seen :go-to.escalated)))))))))

(deftest a-goal-behind-lava-is-not-dug-toward-and-one-behind-water-is-swum-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lava (await (run-job! (assoc free-by-pocket :blocks (moat "lava")) {:pos [8 64 0] :range 0}))
              water (await (run-job! (assoc free-by-pocket :blocks (moat "water")) {:pos [8 64 0] :range 0}))]
          (is (false? (:arrived @(:out lava))))
          (is (not= :goal-enclosed (:why @(:out lava))) (pr-str (select-keys @(:out lava) [:why :text])))
          (is (empty? (events-of (:seen lava) :go-to.escalated)) "lava: no way in is made")
          (is (empty? (calls (:p lava) "dig")) "lava: nothing dug")
          (is (true? (:arrived @(:out water))) (pr-str (select-keys @(:out water) [:why :text]))))))))

(deftest go-to-the-cut-off-deck-with-the-default-budget-is-still-cut-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (run-job! {:self {:pos {:x 5 :y 64 :z 0}} :blocks room-and-deck}
                                             {:pos [8 71 0] :range 0 :ways #{:walk :clear-seal}}))]
          (is (= {:why :goal-cut-off :reason :unreachable :arrived false} (select-keys @out [:why :reason :arrived]))
              "the default budget lets drops of 10 in, and still no way leads up onto the deck"))))))

(deftest door-back-is-a-seen-standable-cell-or-one-not-seen-yet
  (let [back (escape/door-back (tu/fake door-cell) [0 64 0])
        [bx by bz] back
        walled (update door-cell :blocks assoc (str bx "," by "," bz) "stone")
        seen-wall (tu/fake walled)
        dark-wall (tu/fake walled)
        _ (dark-at dark-wall [back])]
    (is (some? back))
    (is (nil? (escape/door-back seen-wall [0 64 0])) "a wall it sees")
    (is (= back (escape/door-back dark-wall [0 64 0])) "a wall it has not seen: go and look, not an x-ray read")))

(deftest go-to-fills-a-one-cell-gap-under-a-low-ceiling-with-a-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 1.5 :y 64 :z 0.5}} :blocks gap-tunnel :inventory [{:name "dirt" :count 5}]}
                                                    {:pos [9 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [:fill-gap] (mapv :step (events-of seen :go-to.escalated))))
          (is (= "dirt" (block p [5 63 0])) "the block went into the gap")
          (is (empty? (calls p "dig"))))))))

(deftest go-to-gives-up-at-a-gap-under-a-low-ceiling-with-nothing-to-fill-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-job! {:self {:pos {:x 1.5 :y 64 :z 0.5}} :blocks gap-tunnel}
                                               {:pos [9 64 0] :range 0}))]
          (is (false? (:arrived @out)))
          (is (= "air" (block p [5 63 0]))))))))

(deftest go-to-digs-a-stair-up-instead-of-a-long-priced-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 50 :z 0}} :blocks long-loop :inventory [{:name "iron_pickaxe" :count 1}]}
                                              {:pos [10 60 2] :range 1 :tolls loop-tolls}))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated)))))))))

(deftest go-to-walks-the-long-loop-when-escalating-is-off-or-the-goal-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:pos {:x 0 :y 50 :z 0}} :blocks long-loop :inventory [{:name "iron_pickaxe" :count 1}]}
              off (await (run-job! world {:pos [10 60 2] :range 1 :tolls loop-tolls :ways #{:walk :clear-seal}}))
              near (await (run-job! world {:pos [24 52 0] :range 1 :tolls loop-tolls}))]
          (is (true? (:arrived @(:out off))))
          (is (empty? (events-of (:seen off) :go-to.escalated)))
          (is (true? (:arrived @(:out near))))
          (is (empty? (events-of (:seen near) :go-to.escalated))))))))

(deftest go-to-digs-the-last-steps-of-its-stair-out-instead-of-calling-the-way-one-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x -8.5 :y 19 :z 0.5}} :blocks stair-below-surface
                                                  :inventory [{:name "iron_pickaxe" :count 1}]}
                                                 {:pos [-40 41 0] :range 1}))]
          (is (= [:stair] (take 1 (mapv :step (events-of seen :go-to.escalated)))))
          (is (= {:arrived true} @out)))))))

(deftest a-go-to-sealed-in-a-cave-without-escalate-says-why-each-time-it-plans-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 3.5 :y 64 :z 0.5}} :blocks cave
                                                   :inventory [{:name "iron_pickaxe" :count 1}]}
                                                  {:pos [30 64 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= [false :exhausted] ((juxt :arrived :why) @out)) (pr-str @out))
          (is (= [[:exhausted 1] [:exhausted 2]] (mapv (juxt :reason :try) (events-of seen :go-to.retrying)))
              (pr-str (events-of seen :go-to.retrying))))))))

(deftest an-approach-walk-may-take-back-its-own-seal
  (is (= #{:walk :clear-seal} (:ways (second (esc/escalation-job {:step :approach :pos [1 64 0]} nil false false true false))))))


(deftest walker-faults-past-the-retries-feed-the-escalation-ladder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (merge (box -20 63 -40 20 63 40 "stone") (box 3 64 -12 3 65 12 "dirt"))
              {:keys [eng p seen]} (setup {:self {:pos {:x 2.5 :y 64 :z 0.5}} :blocks wall})
              steers (atom 0)
              stuck (gt/steer-through (fn [_] (gt/pose 2.5 0.5)))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 0})))]
          (gt/override-steer! p (fn [token a impl] (if (<= (swap! steers inc) 3) (stuck token a impl) (impl token a))))
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))) "three stuck walks round the wall, then a door through it"))))))

(deftest one-stuck-walk-under-retry-false-gives-up-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (merge (box -20 63 -40 20 63 40 "stone") (box 3 64 -12 3 65 12 "dirt"))
              {:keys [eng p seen]} (setup {:self {:pos {:x 2.5 :y 64 :z 0.5}} :blocks wall})
              steers (atom 0)
              stuck (gt/steer-through (fn [_] (gt/pose 2.5 0.5)))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 0 :retry false})))]
          (gt/override-steer! p (fn [token a impl] (if (<= (swap! steers inc) 1) (stuck token a impl) (impl token a))))
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (false? (:arrived @out)) (pr-str @out))
          (is (empty? (events-of seen :go-to.escalated)) "a caller that re-aims itself gets the stuck walk back, no door dug")
          (is (empty? (calls p "dig"))))))))

(deftest a-dig-route-never-digs-a-block-built-there-after-it-was-planned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [below (apply dissoc (box -20 55 -20 20 63 20 "stone") ["3,60,0" "3,61,0"])
              around [[2 60 0] [4 60 0] [3 60 1] [3 60 -1] [3 59 0] [2 61 0] [4 61 0] [3 61 1] [3 61 -1] [3 62 0]]
              {:keys [eng p seen]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks below
                                           :inventory [{:name "stone_pickaxe" :count 1}]})
              walled? (atom false)
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [3 60 0] :range 0})))]
          (.override (.-world p) "dig"
                     (fn [token a impl]
                       (when-not @walled?
                         (reset! walled? true)
                         (swap! (fake/state p) update :blocks merge (zipmap around (repeat "bricks"))))
                       (impl token a)))
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= (repeat (count around) "bricks") (map #(block p %) around))
              "a brick wall put round the goal after the route was planned is someone's build")
          (is (some #(= [:dig-route :not-diggable] ((juxt :step :reason) %)) (events-of seen :go-to.escalation-stopped))
              (pr-str (events-of seen :go-to.escalation-stopped))))))))

(deftest a-free-body-with-one-pickaxe-or-none-lets-its-stair-fetch-a-tool
  (let [flat {:blocks (box -20 63 -20 20 63 20 "stone") :self {:pos {:x 0 :y 64 :z 0}}}
        fetch? #(esc/fetch-tool? {:primitives (tu/fake %) :args {}})
        picks #(vec (repeat % {:name "stone_pickaxe" :count 1}))]
    (is (= [true true false] (map #(fetch? (assoc flat :inventory (picks %))) [0 1 2])))
    (is (false? (fetch? (merge {:blocks room :inventory (picks 1)} in-room))) "shut in: no walk to a tool")
    (is (= [true false] (map #(:fetch (second (esc/escalation-job {:step :stair :heading :east :steps 2} nil false false true %))) [true false])))))

(deftest a-dig-route-s-digs-fetch-a-pickaxe-as-fetch-tool-says
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pick {:name "stone_pickaxe" :count 1}
              one (await (dig-route-run! below-pocket [pick] nil))
              two (await (dig-route-run! below-pocket [pick pick] nil))]
          (is (= [true true] (map (comp :arrived deref :out) [one two])))
          (is (= #{true} (set (map :fetch @(:digs one)))) "one pickaxe: its breaking is no stop")
          (is (= #{false} (set (map :fetch @(:digs two)))) "a spare pickaxe: no fetch"))))))

(deftest a-dig-route-cell-a-look-leaves-unseen-is-dug-and-the-lava-behind-it-sealed
  ;; [3 62 0] stays unseen until dug whatever the body looks at (occluded, too old); lava at [4 62 0] beside it shows only
  ;; once that cell is dug: a player mining digs into unknown rock, the dig child seals what it opens
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (assoc below-pocket "4,62,0" "lava")
              sense (fn [p]
                      (let [sensed (.-sensedAt p)]
                        (aset p "sensedAt" (fn [q] (let [cell [(.-x q) (.-y q) (.-z q)]]
                                                     (if (and (#{[3 62 0] [4 62 0]} cell) (not= "air" (block p [3 62 0])))
                                                       #js {:unknown true}
                                                       (sensed q)))))))
              {:keys [out p seen]} (await (dig-route-run! blocks [{:name "stone_pickaxe" :count 1} {:name "cobblestone" :count 4}] sense))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [3 60 0] (feet p)))
          (is (= "air" (block p [3 62 0])) "dug as guessed rock")
          (is (= "cobblestone" (block p [4 62 0])) "the lava it laid open is sealed")
          (is (seq (events-of seen :blocks.dig.sealed))))))))

(deftest a-dig-route-does-not-step-on-a-floor-the-body-has-not-seen-under-seen-air
  (let [world (apply dissoc (box 0 60 -1 6 70 1 "stone") (for [x (range 3) y [64 65]] (str x "," y ",0")))
        p (hiding world #(= [2 63 0] %))
        cells (esc/route-cells p [0 64 0] [3 64 0] (constantly true) (constantly false))]
    (is (some #(= 1 (nth (:cell %) 2)) cells) "it digs round the seen-open cell whose floor is unseen")))
