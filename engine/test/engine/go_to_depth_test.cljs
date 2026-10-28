(ns engine.go-to-depth-test
  "go-to's :y-margin: blocks past the lowest and highest of body and goal that a walk's search may reach, so a route that
  dips far below both ends (through deep caves) is refused instead of walked; absent: the walks' wide box."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.cost :as cost]
            [jobs.movement.go-to.search-bound :as search-bound]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.go-to-test :as gt]
            [engine.memory :as mem]
            [engine.go-to-quote-test :as pt]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box floor]]
            [jobs.lib.near :as near]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.detour :as detour]
            [jobs.movement.go-to :as go-to]
            [jobs.lib.pace :as pace]))

;; level ground x 0..6 (feet 64), a valley stepping down one block a column to feet 52 and up again, level ground to x 40
(def valley
  (let [heights (concat (repeat 7 63) (range 62 50 -1) (range 52 64) (repeat 17 63))]
    (apply merge (map-indexed (fn [x y] (floor y x -3 x 3)) heights))))

(def goal [40 64 1])

(deftest a-narrow-margin-refuses-the-route-through-the-valley
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (:found (:out (await (pt/preview valley {:pos goal :settings {:jobs.lib.walk.world/y-margin 5}}))))))))))

(deftest a-margin-that-holds-the-valley-or-none-walks-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:found (:out (await (pt/preview valley {:pos goal :settings {:jobs.lib.walk.world/y-margin 14}}))))))
        (is (true? (:found (:out (await (pt/preview valley {:pos goal}))))))))))

;; A body whose goal is far above and whose only way dives first: feet 40 at x 0, a ramp down one block a column to feet
;; 10 (x 30), 8 columns level, then up one a column to the goal at feet 60 (x 88). No sky light anywhere: under ground.
(def dive-heights (concat [39] (range 38 8 -1) (repeat 8 9) (range 10 60)))

(def dive
  (apply merge (map-indexed (fn [x y] (floor y x 0 x 0)) dive-heights)))

(def top [88 60 0])

;; lit by block light: a 100-block walk in the dark is priced over the body's risk allowance (go-to stops :risky), and these
;; cases are about the depth bounds, not the risk
;; a roof 3 over the feet all the way: a block-lit column seen open overhead reads as lit open land, not under ground
(def buried {:blocks (apply merge dive (map-indexed (fn [x y] (floor (+ y 4) x 0 x 0)) dive-heights)) :seen :all :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 15]})

(deftest a-buried-body-does-not-dive-to-reach-a-goal-far-above
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! buried {:pos top}))]
          (is (= {:arrived false :why :way-dives :dip 24} (select-keys @out [:arrived :why :dip])))
          (is (<= 20 (:rise @out)))
          (is (re-find #"dives more than 24 blocks" (str (:text @out))))
          (is (<= 16 (second (gt/at p))) "never below the floor of the call: 24 under where it began"))))))

(deftest the-dive-is-walked-under-the-sky-with-a-y-margin-or-a-deeper-dip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:arrived @(:out (await (gt/go! (assoc buried :light [15 0]) {:pos top}))))) "sky light on the body")
        (is (true? (:arrived @(:out (await (gt/go! buried {:pos top :settings {:jobs.lib.walk.world/y-margin 96}}))))))
        (is (true? (:arrived @(:out (await (settings/with-settings {:jobs.movement.go-to.tuning/climb-dip 30}
                                             #(gt/go! buried {:pos top}))))))
            "setting ::climb-dip")
        (is (true? (:arrived @(:out (await (settings/with-settings {:jobs.movement.go-to.tuning/climb-rise 50}
                                             #(gt/go! buried {:pos top}))))))
            "setting ::climb-rise")))))

;; The same dive cut 3 high through a dirt mass (x -30..40, y 6..59, z -1..1): feet 40 at x 0, down one block a column to
;; feet 10 (x 30), out onto a wide cave floor (more cells than a shut-in body has), and from its far side a ramp beside
;; the mass up to its top. The goal stands on the mass, 20 above the body. No sky light.
(def mine
  (let [heights (concat [39] (range 38 8 -1) (repeat 10 9))
        cave (for [[x y] (map-indexed vector heights) dy [1 2 3]] (str x "," (+ y dy) ",0"))
        ramp (for [i (range 51)] [(str (- 70 i) "," (+ 9 i) ",2") "stone"])]
    (merge (apply dissoc (box -30 6 -1 40 59 1 "dirt") cave)
           (floor 9 41 -17 75 17)
           (into {} ramp))))

(def on-top [20 60 0])

(def in-mine {:blocks mine :seen :all :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 15]})

(defn ^:async climb!
  "go! with ticks enough for a stair, a second of fake time each (the stair waits out a lava delay): {:out :p :seen
  :track}, track every position the body had. A world key :stair-way (floors) is written to memory as a stair the body
  cut, :own-ways ([kind data] each) as more of its ways, :known (cells) as cells the body remembers as they are. A world key :on-move (fn [p pos]) is called with every position the body has; :ticks bounds the run (default 1000)."
  [world args]
  (let [clock (atom 1000000)
        {:keys [eng p] :as s} (gt/setup (assoc (dissoc world :stair-way :own-ways :known :on-move :ticks) :clock clock))
        out (atom :not-done)
        track (atom [])
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out args)))]
    (add-watch (fake/state p) :track (fn [_ _ _ w] (swap! track conj (:pos (:self w)))))
    (when-let [f (:on-move world)]
      (add-watch (fake/state p) :on-move (fn [_ _ _ w] (f p (:pos (:self w))))))
    (run! #(perception/touch! (.-perception p) %) (:known world))
    (some->> (:stair-way world) (hash-map :dim "overworld" :floors) (mem/write! (:store eng) :stair-way))
    (doseq [[kind data] (:own-ways world)] (mem/write! (:store eng) kind (assoc data :dim "overworld")))
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i (:ticks world 1000)) (seq (:list (core/state eng))))
        (await (core/tick! eng))
        (swap! clock + 1000)
        (recur (inc i))))
    (assoc s :eng eng :out out :track @track)))

(defn lowest-walk [s] (apply min (map #(:y (:to %)) (gt/moved (:eng s)))))

(deftest a-buried-body-whose-way-dives-cuts-a-stair-up-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (climb! in-mine {:pos on-top}))]
          (is (true? (:arrived @out)))
          (is (= 60 (second (gt/at p))))
          (is (= #{:stair} (set (map :step (gt/events-of s :go-to.escalated)))))
          (is (= #{:way-dives} (set (map :why (gt/events-of s :go-to.escalated)))))
          (is (<= 16 (lowest-walk s)) "never below the floor of the call")
          (is (seq (mem/entries (mem/view (:store (:eng s))) :stair-way)) "the stair is remembered")
          (is (empty? (concat (gt/events-of s :go-to.restored) (gt/events-of s :go-to.restore-skipped)))
              "the stair that got the body away from the dive is not filled again"))))))

;; The mine with water as the floor of one step of the climb's westward stair ([-8 47 0]: never bridged, and a missing floor
;; would now get filler fetched), and
;; more dirt south of there (x -9..0, y 44..59, z 2..14) for a stair on another heading.
(def holed-mine (merge (assoc mine "-8,47,0" "water") (box -9 44 2 0 59 14 "dirt")))

(deftest a-climb-whose-stair-stops-part-way-goes-on-along-another-heading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (climb! (assoc in-mine :blocks holed-mine) {:pos on-top}))]
          (is (true? (:arrived @out)))
          (is (= [:hazard] (map :reason (gt/events-of s :go-to.escalation-stopped))))
          (is (<= 16 (lowest-walk s)) "it did not walk back down toward the dive"))))))

(deftest with-the-climb-stair-setting-off-the-dive-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (settings/with-settings {:jobs.movement.go-to.tuning/climb-stair false}
                                           #(climb! in-mine {:pos on-top})))]
          (is (= {:arrived false :why :way-dives} (select-keys @out [:arrived :why])))
          (is (empty? (gt/events-of s :go-to.escalated))))))))

;; A dirt mass (x -30..20, y 29..59, z -1..1) with a tunnel cut 3 high: feet 40 at x 0, down one block a column to feet
;; 33 (x 7), level to the mass's east face, out onto a cave floor, and from its far side a ramp beside the mass up to its
;; top. The goal stands on the mass, 20 above the body: the way there dips 7, less than ::climb-dip. No sky light.
(def shallow
  (let [heights (concat [39] (range 38 31 -1) (repeat 13 32))
        cut (for [[x y] (map-indexed vector heights) dy [1 2 3]] (str x "," (+ y dy) ",0"))
        ramp (for [i (range 28)] [(str (- 47 i) "," (+ 32 i) ",2") "stone"])]
    (merge (apply dissoc (box -30 29 -1 20 59 1 "dirt") cut)
           (floor 32 21 -17 53 17)
           (into {} ramp))))

(def in-shallow {:blocks shallow :seen :all :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 15]})

(def mass-top [10 60 0])

(defn lowest [s] (apply min (map second (:track s))))

(defn farthest-east [s] (apply max (map first (:track s))))

(deftest a-buried-body-cuts-a-stair-up-rather-than-walk-down-into-a-cave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! in-shallow {:pos mass-top}))]
          (is (true? (:arrived @out)))
          (is (= #{:stair} (set (map :step (gt/events-of s :go-to.escalated)))))
          (is (= #{:way-dives} (set (map :why (gt/events-of s :go-to.escalated)))))
          (is (<= 37 (lowest s)) "never more than ::cave-dip below where it began"))))))

;; The shallow world plus a stair tunnel west (floors [-i 39+i 0], i 1..20) up to the mass's top: the first plan climbs it
;; and dips nowhere. Once the body is past x -3 the tunnel is walled at x -10, so the re-plan can only go down to the cave.
(def walled-stair
  (let [carve (for [i (range 1 21) y (range (+ 40 i) (+ 44 i))] (str (- i) "," y ",0"))]
    (apply dissoc shallow carve)))

(defn wall-stair! []
  (let [walled (atom false)]
    (fn [p pos]
      (when (and (< (first pos) -3) (not @walled))
        (reset! walled true)
        (doseq [y (range 50 54)] (fake/set-block! p [-10 y 0] "dirt"))))))

(deftest a-re-plan-after-a-partial-leg-that-dips-into-the-cave-cuts-a-stair-up-instead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! (assoc in-shallow :blocks walled-stair :on-move (wall-stair!)) {:pos mass-top}))]
          (is (true? (:arrived @out)))
          (is (= #{:way-dives} (set (map :why (gt/events-of s :go-to.escalated)))))
          (is (<= 37 (lowest s)) "never more than ::cave-dip below where the call began"))))))

;; The stair tunnel left open, a zombie standing in it at x -10 once the body is past x -3.
(defn zombie-on-stair! []
  (let [there (atom false)]
    (fn [p pos]
      (when (and (< (first pos) -3) (not @there))
        (reset! there true)
        (fake/add-entity! p {:id 77 :name "zombie" :kind "hostile" :pos [-9.5 50 0.5]})))))

(deftest a-re-plan-round-a-new-danger-that-dips-into-the-cave-does-not-walk-the-old-way-on-past-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! (assoc in-shallow :blocks walled-stair :on-move (zombie-on-stair!)) {:pos mass-top}))]
          (is (= [{:why :danger :kept false}] (map #(select-keys % [:why :kept]) (gt/events-of s :replan))))
          (is (= #{:way-dives} (set (map :why (gt/events-of s :go-to.escalated)))))
          (is (<= 37 (lowest s)) "never more than ::cave-dip below where the call began"))))))

(deftest the-cave-is-walked-with-a-y-margin-or-a-deeper-cave-dip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [margin (await (climb! in-shallow {:pos mass-top :settings {:jobs.lib.walk.world/y-margin 96}}))
              dip (await (settings/with-settings {:jobs.movement.go-to.climb/cave-dip 8}
                           #(climb! in-shallow {:pos mass-top})))]
          (is (= [true true] (map (comp :arrived deref :out) [margin dip])))
          (is (= [33 33] (map lowest [margin dip])))
          (is (= [[] []] (map #(vec (gt/events-of % :go-to.escalated)) [margin dip]))))))))

;; A dirt mass (x -40..20, y 29..59, z -1..1) with a level tunnel (feet 40) from x -6 to its east face, where a cave floor
;; and a ramp beside the mass lead up to its top: the short way to the goal on top. From the tunnel's west end the body's
;; own stair climbs west to the top (floors [-7 40 0] .. [-26 59 0]): the longer way.
(def stair-floors (vec (for [i (range 1 21)] [(- -6 i) (+ 39 i) 0])))

(def forked
  (let [tunnel (for [x (range -6 21) y [40 41 42]] (str x "," y ",0"))
        stair (for [[x y _] stair-floors dy [1 2 3]] (str x "," (+ y dy) ",0"))
        ramp (for [i (range 21)] [(str (- 40 i) "," (+ 39 i) ",2") "stone"])]
    (merge (apply dissoc (box -40 29 -1 20 59 1 "dirt") (concat tunnel stair))
           (floor 39 21 -17 46 17)
           (into {} ramp))))

(def in-forked {:blocks forked :seen :all :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 15] :stair-way stair-floors})

(def fork-top [18 60 0])

(deftest a-buried-body-climbs-its-own-stair-before-a-shorter-way-through-a-cave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! in-forked {:pos fork-top}))]
          (is (true? (:arrived @out)))
          (is (> 21 (farthest-east s)) "never out of the tunnel's east end into the cave")
          (is (empty? (gt/events-of s :go-to.escalated))))))))

(deftest walking-a-stair-of-its-own-makes-it-the-most-recently-used-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [other [[100 5 100]]
              s (await (climb! (assoc in-forked :own-ways [[:stair-way {:floors other}]]) {:pos fork-top}))
              ways (map (comp :floors :data) (mem/entries (mem/view (:store (:eng s))) :stair-way))]
          (is (= [other stair-floors] (map vec ways)) "the stair, written first and then walked, is the newest"))))))

(deftest a-goal-on-a-step-of-its-own-stair-is-walked-to-with-nothing-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! in-forked {:pos [-24 58 0] :range 0}))]
          (is (true? (:arrived @out)))
          (is (> 21 (farthest-east s)) "never out of the tunnel's east end into the cave")
          (is (empty? (gt/events-of s :go-to.escalated))))))))

(deftest a-stair-of-its-own-not-in-memory-or-with-the-setting-off-is-not-preferred
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [unknown (await (climb! (dissoc in-forked :stair-way) {:pos fork-top}))
              off (await (settings/with-settings {:jobs.movement.go-to.climb/own-way false}
                           #(climb! in-forked {:pos fork-top})))]
          (is (= [true true] (map (comp :arrived deref :out) [unknown off])))
          (is (= [true true] (map #(< 21 (farthest-east %)) [unknown off]))))))))

;; The forked mass with two steps of the stair filled in again (the cuts over [-14 47 0] and [-15 48 0]).
(def refilled
  (merge forked (box -15 48 0 -14 51 0 "dirt")))

(deftest a-blocked-stair-of-its-own-is-cut-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! (assoc in-forked :blocks refilled) {:pos fork-top}))]
          (is (true? (:arrived @out)))
          (is (> 21 (farthest-east s)) "never out of the tunnel's east end into the cave")
          (is (= [[:stair :own-way-blocked]] (map (juxt :step :why) (gt/events-of s :go-to.escalated))))
          (is (= "air" (get-in @(fake/state (:p s)) [:blocks [-14 49 0]] "air")) "the stair is left open"))))))

;; The body at the foot of its open stair, a goal walled in dirt above: nothing blocks the stair, so it is not cut again.
(deftest an-open-stair-of-its-own-is-not-cut-again-for-a-goal-it-does-not-lead-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! (assoc in-forked :self {:pos {:x -5.5 :y 40 :z 0.5}} :inventory [{:name "stone_pickaxe" :count 1}])
                                                 {:pos [10 50 0] :range 0}))]
          (is (not-any? #(= :own-way-blocked (:why %)) (gt/events-of s :go-to.escalated))))))))

;; The forked mass with the block of the stair's top floor gone: its feet cell [-26 60 0] has a hole under it.
(def topless (dissoc forked "-26,59,0"))

;; f's call may take n steps of go-to's loop (its pace timer between them); the step after fails the job.
(defn ^:async within-steps [n f]
  (let [pace pace/timer
        left (atom n)]
    (set! pace/timer #(if (neg? (swap! left dec)) (js/Promise.reject (js/Error. "step budget spent")) (pace)))
    (try (await (f))
         (finally (set! pace/timer pace)))))

(deftest an-own-stair-whose-top-floor-is-gone-does-not-hold-the-call-for-ever
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (within-steps 200 #(climb! (assoc in-forked :blocks topless :ticks 50) {:pos fork-top})))]
          (is (true? (:arrived @out)) "the call ends within 200 steps, at the goal")
          (is (> 21 (farthest-east s)) "never out of the tunnel's east end into the cave"))))))

(deftest a-body-moved-off-every-search-up-its-own-stair-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walk near/walk-round!
              _ (set! near/walk-round! (fn [& _] (js/Promise.resolve {:status "searching" :result {:status :searching :fresh true}})))
              {:keys [out]} (await (-> (within-steps 300 #(climb! (assoc in-forked :ticks 50) {:pos fork-top}))
                                       (.finally #(set! near/walk-round! walk))))]
          (is (= {:arrived false :why :moved-while-searching} (select-keys @out [:arrived :why]))))))))

;; A dirt mass (x -90..10, y 29..59, z -1..1) with the body's own level tunnel (feet 40) from x 0 west to x -60, its own
;; stair from there west up to the top (floors [-61 40 0] .. [-80 59 0]), and beside the body a stair of its own that
;; ends in rock after 5 steps east (floors [1 40 0] .. [5 44 0]).
(def far-floors (vec (for [i (range 1 21)] [(- -60 i) (+ 39 i) 0])))
(def stub-floors (vec (for [i (range 1 6)] [i (+ 39 i) 0])))

(def branched
  (let [tunnel (for [x (range -60 1) y [40 41 42]] (str x "," y ",0"))
        stairs (for [[x y _] (concat far-floors stub-floors) dy [1 2 3]] (str x "," (+ y dy) ",0"))]
    (apply dissoc (box -90 29 -1 10 59 1 "dirt") (concat tunnel stairs))))

(def in-branched {:blocks branched :seen :all :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 15]
                  :own-ways [[:stair-way {:floors far-floors}] [:stair-way {:floors stub-floors}]
                             [:tunnel-way {:origin [0 40 0] :delta [-1 0] :steps 60}]]})

(deftest a-buried-body-walks-its-tunnel-to-its-stair-that-reaches-the-top-not-up-a-dead-end-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! in-branched {:pos [-84 60 0]}))]
          (is (true? (:arrived @out)))
          (is (> 2 (farthest-east s)) "never up the dead end")
          (is (empty? (gt/events-of s :go-to.escalated)) "nothing new is cut")
          (is (= [[true [-79 59 0]]] (map (juxt :take :to) (gt/events-of s :go-to.own-way)))))))))

;; A dirt mass with a shut tunnel (feet 40, x 0..6) and the body's own stair from its west end up to feet 46 at x -6
;; (floors [-1 40 0] .. [-6 45 0]), shut by the body with two cobblestone in the head space of its second step (a pickaxe carried).
(def short-floors (vec (for [i (range 1 7)] [(- i) (+ 39 i) 0])))
(def cap [[-2 43 0] [-2 44 0]])

(def capped
  (let [tunnel (for [x (range 0 7) y [40 41 42]] (str x "," y ",0"))
        stair (for [[x y _] short-floors dy [1 2 3]] (str x "," (+ y dy) ",0"))]
    (merge (apply dissoc (box -30 29 -1 20 59 1 "dirt") (concat tunnel stair))
           (into {} (for [[x y z] cap] [(str x "," y "," z) "cobblestone"])))))

(deftest a-body-shut-in-by-a-cap-in-its-own-stair-digs-the-cap-and-climbs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (climb! {:blocks capped :seen :all :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 15] :stair-way short-floors
                                                    :inventory [{:name "stone_pickaxe" :count 1}]}
                                                   {:pos [-6 46 0]}))
              blocks (:blocks @(fake/state p))]
          (is (true? (:arrived @out)))
          (is (= ["air" "air"] (map #(get blocks % "air") cap)) "the cap is dug")
          (is (= (repeat 6 "dirt") (map #(get blocks %) short-floors)) "no floor of the stair is dug")
          (is (= [[:stair :own-way-blocked]] (map (juxt :step :why) (gt/events-of s :go-to.escalated)))))))))

;; The forked mass with the stair's top steps shut by the body: cobblestone in the head of its 18th step and the feet of
;; its 19th, so the 17th stands in a pit under the open sky, 3 below the top (a pickaxe carried). The body remembers the
;; stair's top (mouth), and walks its way from the stair's foot in hops of 3, so that no hop goes straight to a step
;; above the cap.
(def mouth-cap [[-24 59 0] [-25 59 0]])

(def mouth (for [x (range -27 -20) y (range 55 63)] [x y 0]))

(def mouth-capped (merge forked (into {} (for [[x y z] mouth-cap] [(str x "," y "," z) "cobblestone"]))))

(deftest a-stair-of-its-own-capped-under-its-mouth-is-cut-again-not-walked-back-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (settings/with-settings {:jobs.movement.go-to.climb/own-way-reach 3}
                                             #(climb! (assoc in-forked :blocks mouth-capped :known mouth :self {:pos {:x -5.5 :y 40 :z 0.5}}
                                                             :inventory [{:name "stone_pickaxe" :count 1}])
                                                      {:pos fork-top})))
              blocks (:blocks @(fake/state p))]
          (is (true? (:arrived @out)))
          (is (> 21 (farthest-east s)) "never back down and out of the tunnel's east end")
          (is (= ["air" "air"] (map #(get blocks % "air") mouth-cap)) "the cap is dug")
          (is (= [[:stair :own-way-blocked]] (map (juxt :step :why) (gt/events-of s :go-to.escalated)))))))))

;; The forked mass with a stair of 17 steps only: its top stands in a pit under the open sky, 3 below the top.
(def pit-topped (merge forked {"-24,58,0" "dirt" "-24,59,0" "dirt" "-25,59,0" "dirt"}))

(deftest a-body-at-the-top-of-its-own-stair-in-a-pit-cuts-on-up-not-back-down-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! (assoc in-forked :blocks pit-topped :known mouth :stair-way (subvec stair-floors 0 17)) {:pos fork-top}))]
          (is (true? (:arrived @out)))
          (is (> 21 (farthest-east s)) "never back down and out of the tunnel's east end")
          (is (= [:stair] (map :step (gt/events-of s :go-to.escalated)))))))))

(deftest the-rock-between-the-feet-and-a-stand-of-its-own-is-what-a-straight-walk-digs
  (let [kind-at (fn [open] (fn [x y z] (if (contains? open [x y z]) :open :solid)))
        ends (set (for [x [0 3] y [64 65]] [x y 0]))]
    (are [open n] (= n (climb/rock-between (kind-at open) [0 64 0] [3 64 0]))
      (into ends (for [x [1 2] y [64 65]] [x y 0])) 0
      (into ends (for [y [64 65]] [1 y 0])) 2
      ends 4)))

(deftest a-walk-up-its-own-way-slower-than-a-new-stair-is-not-taken
  (let [cut-s (* 16 3)
        route (fn [blocks] (vec (range (inc blocks))))]
    (is (= [true false] (map #(:take (climb/weigh (route %) 16 3))
                             [(js/Math.floor (* cut-s cost/walk-speed)) (js/Math.ceil (* (inc cut-s) cost/walk-speed))])))))

;; (These cases are about the planner's refusal of a long way over cells never seen, so they pass :unknown false.)
;; Two corridors 6 apart (z 0 and z 6, x 0..40) joined only at their far end: the goal is 6 blocks from the body across
;; a gap it cannot jump, and 84 by the only way. No sky light: under ground, the far cells never seen.
(def loop-way (merge (floor 63 0 0 40 0) (floor 63 40 0 40 6) (floor 63 0 6 40 6)))

(def across [0 64 6])

(def in-loop {:blocks loop-way :self {:pos {:x 0.5 :y 64 :z 0.5}} :light [0 15]})

(deftest a-buried-body-does-not-walk-a-long-unseen-way-to-a-near-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! in-loop {:pos across :unknown false}))]
          (is (= {:arrived false :why :detour :route 84 :straight 6 :allowed 48}
                 (select-keys @out [:arrived :why :route :straight :allowed])))
          (is (re-find #"84 blocks.*6 blocks away.*detour/floor" (str (:text @out))))
          (is (= [0.5 64 0.5] (gt/at p)) "it did not set out"))))))

;; The loop with stone between its corridors (x 0..39, z 1..5): 5 blocks of rock part the body from its goal.
(def rock-loop (merge loop-way (box 0 63 1 39 66 5 "stone") (box 0 66 0 40 66 6 "stone")))

(deftest a-buried-body-digs-through-to-a-near-goal-rather-than-walk-a-long-unseen-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (climb! (assoc in-loop :blocks rock-loop :inventory [{:name "stone_pickaxe" :count 1}]) {:pos across :range 0 :unknown false}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:dig-route] (map :step (gt/events-of s :go-to.escalated))))
          (is (empty? (gt/events-of s :go-to.escalation-stopped)) "the goal's cell, guessed rock and seen open, leaves nothing to dig: no failed route")
          (is (= [0 64 6] (mapv js/Math.floor (gt/at p)))))))))

;; The loop's corridors cut in solid rock, with bedrock two high at z 3 in the line between them: the dig route is planned
;; through the bedrock as guessed rock and planned again round it once a look shows it.
(def bedrock-loop
  (apply dissoc (merge (box -3 60 -3 43 67 9 "stone") {"0,64,3" "bedrock" "0,65,3" "bedrock"})
         (for [[x z] (concat (for [x (range 41) z [0 6]] [x z]) (for [z (range 1 6)] [40 z])) y [64 65]] (str x "," y "," z))))

(deftest a-dig-route-cell-a-look-shows-otherwise-plans-the-route-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc in-loop :blocks bedrock-loop :inventory [{:name "stone_pickaxe" :count 1}])
              {:keys [out p] :as s} (await (climb! world {:pos across :range 0 :unknown false}))
              capped (with-redefs [search-bound/fallback-s (constantly 0)]
                       (await (climb! world {:pos across :range 0 :unknown false})))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [0 64 6] (mapv js/Math.floor (gt/at p))))
          (is (= ["bedrock" "bedrock"] (map #(.-name (.blockAt p (clj->js %))) [{:x 0 :y 64 :z 3} {:x 0 :y 65 :z 3}])))
          (is (= [:dig-route] (map :step (gt/events-of s :go-to.escalated))))
          (is (some #(= [:dig-route :replans-spent] ((juxt :step :reason) %)) (gt/events-of capped :go-to.escalation-stopped))
              "replans that cost what the other way does: the route ends :replans-spent"))))))

(deftest the-long-way-is-walked-on-request-under-the-sky-or-within-the-bound
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:arrived @(:out (await (settings/with-settings {:jobs.movement.go-to.detour/floor 1000} #(gt/go! in-loop {:pos across :unknown false})))))))
        (is (true? (:arrived @(:out (await (gt/go! (assoc in-loop :light [15 0]) {:pos across :unknown false}))))) "sky light on the body")
        (is (true? (:arrived @(:out (await (settings/with-settings {:jobs.movement.go-to.detour/floor 100}
                                             #(gt/go! in-loop {:pos across :unknown false}))))))
            "setting ::detour/floor")))))

(def line-20 (mapv (fn [x] {:x x :y 64 :z 0}) (range 21)))

(deftest a-way-is-a-detour-when-long-for-its-goal-and-mostly-unseen
  (are [steps straight unseen? out] (= out (detour/refusal steps straight unseen?))
    (mapv #(update % :x * 3) line-20) 6 (constantly true) {:detour true :route 60 :straight 6 :allowed 48}
    (mapv #(update % :x * 3) line-20) 6 (constantly false) nil
    (mapv #(update % :x * 3) line-20) 6 (fn [x _ _] (>= x 30)) {:detour true :route 60 :straight 6 :allowed 48}
    (mapv #(update % :x * 3) line-20) 6 (fn [x _ _] (> x 30)) nil
    (mapv #(update % :x * 3) line-20) 15 (constantly true) nil
    line-20 2 (constantly true) nil))

;; A body under ground reads rock it never saw as rock (jobs.lib.walk.rock): a cave mouth it saw is no portal to a cheap
;; way through unseen rock. A chamber (x 0..2, feet 60, z -1..1) in a stone mass, a side cave from its west wall stepping
;; down west away from the goal to feet 38, the goal 12 down and 25 east at the top of a stair down to a lower cave (feet
;; 41). The body has seen the chamber with the cave's mouth, and the goal's end; the rock between it never saw. No sky light.
(def phantom
  (let [cave (for [i (range 1 23) dy [0 1]] (str (- i) "," (+ (- 60 i) dy) ",0"))
        stair (for [i (range 8) dy [0 1]] (str (+ 26 i) "," (+ (- 48 i) dy) ",0"))
        lower (for [x (range 34 40) y [41 42] z [-1 0 1]] (str x "," y "," z))
        chamber (for [x (range 3) y [60 61] z [-1 0 1]] (str x "," y "," z))]
    (apply dissoc (box -24 36 -4 40 66 4 "stone") (concat cave stair lower chamber))))

(def in-phantom {:blocks phantom :self {:pos {:x 1.5 :y 60 :z 0.5}} :light [0 15] :seen [[-1 59 -2 3 62 2] [24 46 -2 28 50 2]]
                 :inventory [{:name "stone_pickaxe" :count 1}]})

(deftest a-buried-body-digs-toward-a-goal-past-unseen-rock-not-down-a-cave-mouth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (climb! in-phantom {:pos [26 48 0]}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (<= 45 (lowest-walk s)) "never more than 3 below the goal: the cave is not walked")
          (is (< 1 (count (filter #(= :dig-route (:step %)) (gt/events-of s :go-to.escalated)))) "the dig goes leg by leg")
          (is (<= 25 (first (gt/at p)))))
        (let [{:keys [out]} (await (climb! in-phantom {:pos [26 48 0] :worth 1}))]
          (is (= [false :dig-budget] [(:arrived @out) (:why (:escalation @out))]) "the dig is held to what the goal is worth"))))))

;; A long dark hall (x 0..34, feet 60..62, z -3..3) in rock, seen only round the body (x 0..6): the goal at its far end is
;; reached by walking along it as it shows, nothing dug.
(def hall (apply dissoc (box -2 58 -5 36 64 5 "stone") (for [x (range 35) y [60 61 62] z (range -3 4)] (str x "," y "," z))))

(deftest a-buried-body-walks-a-dark-hall-it-sees-only-in-part-to-a-goal-along-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! {:blocks hall :self {:pos {:x 1.5 :y 60 :z 0.5}} :light [0 15] :seen [[-2 59 -5 6 64 5]]
                                                  :inventory [{:name "stone_pickaxe" :count 1}]}
                                                 {:pos [32 60 0]}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (empty? (gt/events-of s :go-to.escalated)) "nothing dug"))))))

;; A sealed pocket (x 0..2, feet 60, z 0) in a stone mass whose top (feet 78) holds the goal straight above; the body has
;; seen only the pocket. A goal higher than it is across is climbed toward by a stair, not dug toward leg by leg.
(def pocket (apply dissoc (box -10 50 -6 10 77 6 "stone") (for [x (range 3) y [60 61]] (str x "," y ",0"))))

(deftest a-buried-body-cuts-a-stair-to-a-goal-steeply-above-not-dig-legs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! {:blocks pocket :self {:pos {:x 1.5 :y 60 :z 0.5}} :light [0 0] :seen [[-1 59 -1 3 62 1]]
                                                  :inventory [{:name "stone_pickaxe" :count 1}]}
                                                 {:pos [1 78 0]}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= :stair (:step (first (gt/events-of s :go-to.escalated)))) (pr-str (map #(select-keys % [:step :at]) (gt/events-of s :go-to.escalated)))))))))

;; A chamber (x 0..2, feet 60, z 0) in a stone mass and a pocket (x 9..10, feet 57) the body has seen, the rock between
;; never seen: the leg that ends short of the goal is followed by a last one into it.
(def near-pocket (apply dissoc (box -4 50 -4 20 70 4 "stone") (for [[x y] [[0 60] [1 60] [2 60] [9 57] [10 57]] dy [0 1]] (str x "," (+ y dy) ",0"))))

(deftest a-buried-body-digs-its-last-leg-into-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (climb! {:blocks near-pocket :self {:pos {:x 1.5 :y 60 :z 0.5}} :light [0 15]
                                                  :seen [[-1 59 -1 3 62 1] [8 56 -1 11 59 1]] :inventory [{:name "stone_pickaxe" :count 1}]}
                                                 {:pos [9 57 0]}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= #{:dig-route} (set (map :step (gt/events-of s :go-to.escalated)))) (pr-str (map #(select-keys % [:step :at]) (gt/events-of s :go-to.escalated)))))))))

(deftest a-detour-dig-whose-search-spends-its-nodes-goes-on-next-round-not-to-a-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (settings/with-settings {:jobs.lib.dig-route/max-nodes 30}
                                            #(climb! (assoc in-loop :blocks rock-loop :inventory [{:name "stone_pickaxe" :count 1}]) {:pos across :range 0 :unknown false})))]
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text])))
          (is (= [:dig-route] (map :step (gt/events-of s :go-to.escalated)))))))))
