(ns engine.reach-job-test
  "jobs.movement.reach: a stair down to a goal far below and a bridge over a gap, chosen by price over go-to; a walk that
  exists is taken before either (jobs.lib.escape.span)."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.cost.ways :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.span :as span]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.reach :as reach]
            [engine.test-util :as tu :refer [box]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.settings :as settings]
            [engine.sense-test :as st]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.dig-look :as dig-look]
            [jobs.movement.go-to.escalate-choose :as choose]
            [engine.go-to-escalate-support :refer [setup recording-parent tick-out! run-job! feet events-of]]))

(defn block-at-of-map [blocks] (fn [[x y z]] (get blocks (str x "," y "," z) "air")))

(def edge-floor (box 0 63 -2 1 63 2 "stone"))

(deftest stands-are-the-cells-of-a-walk-over-seen-ground
  (let [p (tu/fake {:blocks edge-floor :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        sensed (.-sensedAt p)
        _ (aset p "sensedAt" (fn [q] (if (= 1 (.-x q)) #js {:unknown true} (sensed q))))]
    (is (some #{[0 64 0]} (reach/stands p)))
    (is (not-any? #(= 1 (first %)) (reach/stands p)) "the column x 1 never seen: no stand there, not read off the true world")))

(deftest gap-along-reads-the-floor-cells-ahead
  (let [blocks (merge edge-floor (box 5 63 -2 8 63 2 "stone"))
        at (block-at-of-map blocks)
        never (constantly false)]
    (are [feet d want] (= want (span/gap-along at never feet d))
      [1 64 0] [1 0] {:length 3 :landing [5 64 0]}
      [1 64 0] [-1 0] nil
      [0 64 0] [-1 0] nil)))

(deftest gap-along-gives-up-on-unsensed-floor-and-solid-ahead
  (let [at (block-at-of-map (merge edge-floor (box 5 63 -2 8 63 2 "stone")))]
    (is (nil? (span/gap-along at #(= % [3 63 0]) [1 64 0] [1 0])) "a floor cell never seen is no known gap")
    (is (nil? (span/gap-along (block-at-of-map (box 0 63 -2 8 63 2 "stone")) (constantly false) [1 64 0] [1 0])) "solid floor ahead: nothing to bridge")))

(deftest bridge-span-picks-the-gap-nearest-the-goal-and-counts-the-blocks-short
  (let [at (block-at-of-map (merge edge-floor (box 5 63 -2 8 63 2 "stone")))
        opts (fn [count] {:pillar-item (when count {:item "dirt" :count count}) :skip #{} :fetch? false})]
    (is (= {:step :bridge :heading :east :length 3 :item "dirt" :short 0 :landing [5 64 0]}
           (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (opts 5))))
    (is (nil? (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (opts 2))) "short of blocks and not fetching")
    (is (= 1 (:short (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (assoc (opts 2) :fetch? true)))))
    (is (nil? (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (assoc (opts 5) :skip #{:bridge}))))
    (is (nil? (span/bridge-span at (constantly false) [1 64 0] [-8 64 0] (opts 5))) "the way west leads nowhere nearer than the way back")))

(deftest down-stair-needs-a-goal-two-below-and-cuts-it-may-dig
  (let [stone (box -25 40 -5 20 63 5 "stone")
        at (block-at-of-map stone)]
    (is (= {:step :stair :dir :down :heading :east :steps 10 :aside 2} (span/down-stair at [0 63 0] [8 53 0] (constantly true) #{})))
    (is (nil? (span/down-stair at [0 63 0] [8 62 0] (constantly true) #{})) "one below is a drop or a dig-route")
    (is (nil? (span/down-stair at [0 63 0] [8 53 0] (constantly true) #{[:stair :east] [:stair :south] [:stair :north]})) "west ends 18 blocks off the goal's column")
    (is (nil? (span/down-stair at [0 63 0] [8 53 0] (constantly false) #{})) "no cell may be dug")))

(def thin-mine
  "The live mine room: a 25x25x3 room (x 4..28, y 1..3) in a stone mass two blocks thick (x 2..30, y 0..16), a 1-wide air
  gap round the mass and a bedrock shell beyond it (x/z 0 and 32, floor y -1, up to y 21)."
  (let [mass (reduce dissoc (box 2 0 2 30 16 30 "stone") (for [x (range 4 29) y (range 1 4) z (range 4 29)] (str x "," y "," z)))
        shell (merge (box 0 -1 0 32 -1 32 "bedrock") (box 0 0 0 0 21 32 "bedrock") (box 32 0 0 32 21 32 "bedrock")
                     (box 0 0 0 32 21 0 "bedrock") (box 0 0 32 32 21 32 "bedrock"))]
    (merge shell mass)))

(def thin-mine-dark
  "The cells a body in thin-mine's room cannot see: the gap and the shell."
  (for [x (range 0 33) y (range -1 22) z (range 0 33)
        :when (or (#{0 1 31 32} x) (#{0 1 31 32} z))]
    [x y z]))

(defn dark-but-near
  "p that sees the cells of dark only as a player in the dark does: within 4 blocks of its eye, along a line of open cells."
  [p dark]
  (let [sensed (.-sensedAt p)
        open? (fn [cell] (= "air" (get-in @(fake/state p) [:blocks cell] "air")))
        near? (fn [cell]
                (let [pos (.-pos (.self p))
                      eye [(.-x pos) (+ 1.62 (.-y pos)) (.-z pos)]
                      to (mapv #(+ % 0.5) cell)
                      cell-of (fn [t] (mapv #(js/Math.floor (+ %1 (* t (- %2 %1)))) eye to))]
                  (and (<= (js/Math.hypot (- (to 0) (eye 0)) (- (to 1) (eye 1)) (- (to 2) (eye 2))) 4)
                       (every? open? (remove #{cell (cell-of 0)} (map cell-of (range 0.1 1 0.1)))))))]
    (aset p "sensedAt" (fn [q] (let [cell [(.-x q) (.-y q) (.-z q)]]
                                 (if (and (dark cell) (not (near? cell))) #js {:unknown true} (sensed q)))))))

(deftest a-body-in-a-thin-walled-mine-room-climbs-to-a-goal-on-top
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos {:x 5.5 :y 1 :z 16.5}} :blocks thin-mine
                                           :inventory [{:name "stone_pickaxe" :count 1} {:name "cobblestone" :count 24}]})
              out (atom :not-done)
              eng (assoc-in eng [:jobs 'recording-parent] (recording-parent out 'jobs.movement.reach {:pos [26 17 16] :range 1}))]
          (dark-but-near p (set thin-mine-dark))
          (core/submit! eng (tu/order '(recording-parent {})) {})
          (await (tick-out! eng 2000))
          (is (true? (:arrived @out)) (pr-str @out (feet p) (keep #(when (re-find #"^(go-to|reach)\." (name (:kind %))) [(:kind %) (:at %) (:text %)]) @seen)))
          (is (not-any? #(#{:undercuts-way :unbreakable} (:reason %)) (events-of seen :go-to.escalation-stopped)))
          ;; with 24 blocks it pillars up through the 13 ceiling cells from where it stands (the ways run back to back: go-to-depth-test)
          (is (= [{:step :pillar :why :start-enclosed :at [26 1 16]}] (map #(select-keys % [:step :why :at]) (events-of seen :go-to.escalated))) "one climb: a pillar from the floor under the goal, no walk to a wall first"))))))

(deftest a-climb-whose-guessed-rock-keeps-showing-open-ends-once-its-replans-are-spent
  ;; a body shut in a pocket in stone under a goal 12 across and 3 up; shafts of air along the way, floor to the goal's
  ;; head (the climb digs across them at the bottom: a stair over one has no floor), read as rock till the body is near:
  ;; each one seen sends the climb to be searched again, a replan, and once the replans have taken as long as the way
  ;; without a dig route costs its dig routes fail :replans-spent instead of the climb being chosen again for every guess the looks undo
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shafts (for [x [3 6 9] y (range 64 69)] [x y 0])
              blocks (reduce #(assoc %1 (str (%2 0) "," (%2 1) "," (%2 2)) "air")
                             (box -4 58 -4 16 72 4 "stone") (concat [[0 64 0] [0 65 0] [12 67 0] [12 68 0]] shafts))
              {:keys [eng p seen]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]})
              out (atom :not-done)
              replans (atom 0) replan esc/replan-route!
              eng (assoc-in eng [:jobs 'recording-parent] (recording-parent out 'jobs.movement.go-to {:pos [12 67 0] :range 0}))]
          (dark-but-near p (set shafts))
          (with-redefs [esc/replan-route! (fn [& args] (swap! replans inc) (apply replan args))
                        esc/replans-spent? (constantly true)]
            (core/submit! eng (tu/order '(recording-parent {})) {})
            (await (tick-out! eng 2000)))
          (is (not= :not-done @out) "the call ended")
          (is (some #(= [:dig-route :replans-spent] ((juxt :step :reason) %)) (events-of seen :go-to.escalation-stopped))
              (pr-str @replans (keep #(when (re-find #"^go-to\.esc" (name (:kind %))) [(:kind %) (:step %) (:reason %) (:why %) (:at %)]) @seen)))
          (is (<= @replans 2) "the first replan spends the replans"))))))

(deftest a-climb-to-a-goal-sealed-in-bedrock-ends-at-once-and-the-stair-is-taken
  ;; the goal sealed in bedrock 20 above a body shut in stone: no climb reaches it. Its search ends by itself (no stand-still
  ;; bound spent: the search back from the goal finds no way out) and the stair is taken from the start cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (-> (box -24 40 -24 24 72 24 "stone")
                         (merge (box -1 69 -1 1 72 1 "bedrock"))
                         (assoc "0,50,0" "air" "0,51,0" "air" "0,70,0" "air" "0,71,0" "air"))
              {:keys [eng p seen]} (setup {:self {:pos {:x 0.5 :y 50 :z 0.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]})
              out (atom :not-done)
              nodes (atom 0) search dig-route/search
              eng (assoc-in eng [:jobs 'recording-parent] (recording-parent out 'jobs.movement.go-to {:pos [0 70 0] :range 0}))]
          ;; the lane and band round the planned stair (seeded-search, :only) come first; the whole search ends at once
          (with-redefs [dig-route/search (fn [& args] (let [run (apply search args)]
                                                        (fn [n] (let [[_ used :as r] (run n)] (when-not (:only (nth args 3)) (swap! nodes + used)) r))))]
            (core/submit! eng (tu/order '(recording-parent {})) {})
            (await (tick-out! eng 3000)))
          (is (not= :not-done @out) "the call ended")
          (is (empty? (events-of seen :go-to.search-spent)) "the search ended by itself")
          (is (some #(and (= [0 50 0] (:at %)) (= :stair (:step %))) (events-of seen :go-to.escalated)) "the stair is taken from the start cell")
          (is (zero? @nodes) "the climb's whole search expands no cell: the search back from the goal ends it"))))))

(deftest the-ways-are-priced-in-seconds-by-their-blocks
  (is (< (cost/pillar-cost 3) (cost/stair-cost 3)))
  (is (< (cost/bridge-cost 4) (cost/bridge-cost 4 1)) "a block to fetch adds to the price"))

(def gulf
  "Stone floors at y 63 either side of a 5-wide void (x 1..5), z -20..20, the body on the west edge."
  (merge (box -20 63 -20 0 63 20 "stone") (box 6 63 -20 20 63 20 "stone")))

(deftest the-look-ahead-leaves-memory-holding-the-floor-in-view-not-just-the-cells-looked-at
  ;; the span search plans over memory: a look that only turned the head left it holding the cells looked at and their
  ;; neighbours, so the floor beside the headings stayed unseen to it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (st/seeing {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (box -8 63 -8 8 63 8 "stone")})
              remembered? (fn [[x y z]] (not (.-unknown (.seenBlockAt p #js {:x x :y y :z z}))))]
          (await (reach/look-ahead! (st/stub-ctx p) [0 64 0]))
          (is (pos? (st/looks p)))
          (is (some remembered? (for [x (range 2 6) z (range 2 6) [sx sz] [[1 1] [1 -1] [-1 1] [-1 -1]]] [(* sx x) 63 (* sz z)]))
              "a floor cell off every heading, in view of a look"))))))

(deftest a-dig-look-takes-a-sight-pass-so-memory-holds-the-floor-in-view
  ;; a look that only turned the head left memory with the cell and its 6 neighbours
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (st/seeing {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (box -8 63 -8 8 63 8 "stone")})
              remembered? (fn [[x y z]] (not (.-unknown (.seenBlockAt p #js {:x x :y y :z z}))))]
          (await (dig-look/look-at! (st/stub-ctx p) [4 63 0]))
          (is (remembered? [6 63 3]) "a floor cell two off the cell looked at, in view"))))))

(deftest a-span-ending-outside-the-work-area-is-no-span-from-inside-it
  (let [at (block-at-of-map (merge edge-floor (box 5 63 -2 8 63 2 "stone")))
        bridge-from (fn [cell] (span/bridge-span at (constantly false) cell [8 64 0] {:pillar-item {:item "dirt" :count 5} :skip #{} :fetch? false}))
        stair-from (fn [cell] (span/down-stair (block-at-of-map (box -25 40 -5 20 63 5 "stone")) cell [8 53 0] (constantly true) #{}))
        spans (fn [area feet]
                (settings/with-settings {:jobs.lib.walk.world/area area}
                  #(let [out? (wworld/outside-area-fn feet)]
                     [((reach/in-area out? [8 64 0] bridge-from) feet) ((reach/in-area out? [8 53 0] stair-from) [0 63 0])])))]
    (is (= [:bridge :stair] (map :step (spans {:min [0 40 -3] :max [20 70 3]} [1 64 0]))) "landing 5 and stair end inside the box")
    (is (= [nil nil] (spans {:min [0 40 -3] :max [4 70 3]} [1 64 0])) "landing at x 5 and the stair's end past x 4")
    (is (= [:bridge :stair] (map :step (spans {:min [20 60 -3] :max [30 70 3]} [1 64 0]))) "from outside the box nothing is refused")))

(deftest a-free-body-bridges-a-gap-the-planner-cannot-cross
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks gulf :inventory [{:name "dirt" :count 9}]}
                                                    'jobs.movement.reach
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (= [:bridge] (map :step (events-of seen :reach.span)))))))))

(def five-gulfs
  "Stone floors at y 63 over x -20..0, 6, 12, 18, 24 and 30..45, with a 5-wide void between each, z -20..20."
  (apply merge (map (fn [[x0 x1]] (box x0 63 -20 x1 63 20 "stone")) [[-20 0] [6 6] [12 12] [18 18] [24 24] [30 45]])))

(deftest a-fifth-gap-is-bridged-after-the-four-before-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks five-gulfs :inventory [{:name "dirt" :count 64}]}
                                                    'jobs.movement.reach
                                                    {:pos [40 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [40 64 0] (feet p)))
          (is (< 4 (count (events-of seen :reach.span))) "more spans than the old cap of four"))))))

(deftest a-walk-only-call-does-not-bridge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks gulf :inventory [{:name "dirt" :count 9}]}
                                                  'jobs.movement.reach
                                                  {:pos [10 64 0] :range 0 :ways #{:walk}}))]
          (is (= false (:arrived @out)) (pr-str @out (mapv (juxt :kind :text) @seen)))
          (is (empty? (events-of seen :reach.span))))))))

(deftest a-walk-round-the-gap-is-taken-before-a-bridge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [round (merge gulf (box 1 63 -20 5 63 -19 "stone"))
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks round :inventory [{:name "dirt" :count 9}]}
                                                    'jobs.movement.reach
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (empty? (events-of seen :reach.span))))))))

(deftest only-a-walk-with-no-route-is-no-case-for-a-span
  (are [kind why want] (= want (reach/no-way? {:kind kind :data {:why why :kind :gap-up}}))
    :nothing-possible :goal-cut-off true
    :nothing-possible :goal-enclosed true
    :nothing-possible :exhausted true
    :nothing-possible :detour true
    :nothing-possible :abilities true
    :nothing-possible :door-stuck false
    :nothing-possible :stuck false
    :nothing-possible :goal-dangerous false
    :nothing-possible :unseen-floor-dark true
    :nothing-possible :needs-health false
    :condition-failed :goal-cut-off false
    :limit-reached :goal-cut-off false))

(deftest a-sprint-or-door-the-body-lacks-is-no-case-for-a-span
  (are [kind] (false? (reach/no-way? {:kind :nothing-possible :data {:why :abilities :kind kind}}))
    :corner-jump :gap-sprint :open))

(deftest a-body-far-from-the-gap-walks-to-its-edge-and-bridges
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x -12.5 :y 64 :z 0.5}} :blocks gulf :inventory [{:name "dirt" :count 9}]}
                                                    'jobs.movement.reach
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (= [[:bridge true] [:bridge false]] (distinct (map (juxt :step :approach) (events-of seen :reach.span)))) "walked to the edge first"))))))

(deftest the-walk-lights-a-dark-way-before-a-span-is-built
  (are [walk want] (= want (:torches (reach/walk-args {:quote-call {} :args {:pos [1 2 3] :range 1 :walk walk}} false)))
    {} :fetch
    {:torches :never} :never))

(deftest a-body-in-a-mine-wider-than-a-room-stairs-up-to-a-surface-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [rock (box -30 0 -30 70 40 30 "stone") ; rock past every stair: no ledge branch (card a6be0289)
              mine (reduce dissoc rock (for [x (range -20 21) y (range 10 13) z (range -20 21)] (str x "," y "," z)))
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x -19.5 :y 10 :z 0.5}} :blocks mine :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    'jobs.movement.reach
                                                    {:pos [40 41 0] :range 1}))]
          (is (true? (:arrived @out)) (pr-str @out (mapv (juxt :kind :text) @seen)))
          (is (<= 40 (second (feet p)))))))))

(deftest reach-stair-and-bridge-children-may-craft-from-carried-goods-without-fetching
  (are [step fetch? ways] (= ways (:ways (second (reach/span-job {:step step :heading :east :steps 3 :length 3} false fetch?))))
    :stair false #{:carried :craft}
    :bridge false #{:carried :craft}
    :stair true #{:carried :chest :craft :gather}))

(deftest a-stair-stopped-over-a-shaft-it-guessed-as-rock-is-not-chosen-again
  ;; the pocket of the test above with shafts of air (x 5, 7, 9, y 64..68 bar 66) the climb's stairs rise over, read as rock
  ;; till the body is near: a stair that meets one stops :no-floor, and the next way is chosen on what it showed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shafts (for [x [5 7 9] y [64 65 67 68]] [x y 0])
              blocks (reduce #(assoc %1 (str (%2 0) "," (%2 1) "," (%2 2)) "air")
                             (box -4 58 -4 16 72 4 "stone") (concat [[0 64 0] [0 65 0] [12 67 0] [12 68 0]] shafts))
              {:keys [eng p seen]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]})
              out (atom :not-done)
              eng (assoc-in eng [:jobs 'recording-parent] (recording-parent out 'jobs.movement.go-to {:pos [12 67 0] :range 0}))]
          (dark-but-near p (set shafts))
          (core/submit! eng (tu/order '(recording-parent {})) {})
          (await (tick-out! eng 2000))
          (let [stops (filter #(= :no-floor (:reason %)) (events-of seen :go-to.escalation-stopped))
                after (fn [stop] (second (drop-while #(not= stop %) (filter #(#{:go-to.escalated :go-to.escalation-stopped} (:kind %)) @seen))))]
            (is (true? (:arrived @out)) (pr-str @out))
            (is (= (count stops) (count (distinct (map :cell stops)))) (pr-str (map (juxt :at :cell) stops)))
            (is (not-any? #(= [:stair (:at %)] ((juxt :step :at) (after %))) stops) "no stair again from where one stopped")))))))

(deftest approach-prices-only-stands-that-can-beat-the-best-found
  (let [cells (for [x (range 0 200)] [x 10 0])
        priced (atom 0)
        span-from (fn [cell] (swap! priced inc) {:aside (js/Math.abs (- (cell 0) 100))})
        got (reach/approach cells [0 10 0] span-from (fn [s _] (:aside s)) (fn [cell] (js/Math.abs (- (cell 0) 100))) nil)]
    (is (= [100 10 0] (:pos got)) "the stand under the goal")
    (is (<= @priced 3) "stands whose bound cannot beat it are never priced")
    (is (nil? (reach/approach cells [0 10 0] span-from (fn [s _] (:aside s)) (fn [cell] (js/Math.abs (- (cell 0) 100))) 0)) "none under the limit")))

(deftest approach-takes-the-nearer-of-two-stands-of-equal-rank
  ;; the far stand's bound sorts it first; the near one's bound equals the rank found, and still ties and is nearer
  (let [bound {[50 10 0] 0 [1 10 0] 5}
        got (reach/approach [[50 10 0] [1 10 0]] [0 10 0] (fn [_] {}) (fn [_ _] 5) bound nil)]
    (is (= [1 10 0] (:pos got)))))
