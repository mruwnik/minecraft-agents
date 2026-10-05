(ns engine.path-walk-test
  "engine.path.walk (the shared walk driver) against the fake world: one call plans, walks and re-plans to a goal."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.path.planner-tuned :as planner]
            [engine.path.walk :as walk]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn box
  "Blocks named name filling x0..x1, y0..y1, z0..z1."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(defn floor
  "Stone at y 63 for x in xs, z 0..2."
  [xs]
  (into {} (for [x xs z (range 3)] [(str x "," 63 "," z) "stone"])))

(defn ^:async walk-to
  "A body at pos over blocks; a parent job calls walk/walk-to! to goal. [the driver's answer, the plan and replan
  kinds it announced, the body's position]."
  [blocks goal pos]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake {:blocks blocks :self {:pos pos}})
        out (atom nil)
        announced (atom [])
        parent {:check (constantly true)
                :round (fn ^:async driving-round [c]
                         (reset! out (await (walk/walk-to! c {:to goal :range 0 :weight 1.2 :timeout-s 60
                                                              :announce! (fn [kind _] (swap! announced conj kind))})))
                         :done)}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'driving-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(driving-parent) {})
    (loop [i 0]
      (when (and (< i 200) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [at (.-pos (.self p))]
      [@out @announced [(.-x at) (.-y at) (.-z at)]])))

(def start {:x 0 :y 64 :z 1})

(deftest a-flat-floor-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [result walked walk-ms]} announced at] (await (walk-to (floor (range 12)) [10 64 1] start))]
          (is (= {:status :arrived :replans 0} (select-keys result [:status :replans])))
          (is (pos? walked))
          (is (pos? walk-ms))
          (is (= [:plan] announced))
          (is (> (first at) 9)))))))

(deftest a-step-up-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (floor (range 5)) (box 5 63 0 11 64 2 "stone"))
              [{:keys [result]} _ at] (await (walk-to blocks [10 65 1] start))]
          (is (= :arrived (:status result)))
          (is (= 65 (second at))))))))

(deftest a-trench-is-walked-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the wall at the trench's end keeps the walk round off the trench's corner cells (the fake's walker is a point
        ;; that cannot cross a hole's corner the way a body does)
        (let [blocks (merge (box 0 63 -8 4 63 10 "stone") (box 7 63 -8 12 63 10 "stone") (box 5 63 9 6 63 10 "stone")
                            (box 4 66 -8 6 66 8 "stone") (box 5 64 8 6 65 8 "stone"))
              [{:keys [result]} _ at] (await (walk-to blocks [10 64 1] start))]
          (is (= :arrived (:status result)))
          (is (> (first at) 9)))))))

;; two walkways x 0 and x 6 (stone at y 63, z 0..100) with no floor between them (5 cells, wider than any jump), joined
;; only at z 100: the way round runs past the planner's default box (64 blocks round start and goal)
(defn walkways [joined?]
  (merge (box 0 63 0 0 63 100 "stone") (box 6 63 0 6 63 100 "stone")
         (when joined? (box 1 63 100 5 63 100 "stone"))))

(defn plan-within-status
  "The planner status and reason of walk/plan-within from (0 64 0) to goal over blocks."
  [blocks goal]
  (let [p (tu/fake {:blocks blocks :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        {:keys [r]} (walk/plan-within {:primitives p} (.pathWorld p) goal 0 walk/default-weight)]
    [(.-status r) (.-reason r)]))

(deftest a-way-round-past-the-default-box-is-found-by-a-wider-one
  (are [joined? answer] (= answer (plan-within-status (walkways joined?) [6 64 0]))
    true ["found" nil]
    false ["none" "exhausted"]))

;; a search in chunks (walk/plan-walk!) lets the event loop run between them: a timer set before the search fires before
;; the search ends (live: a body's HTTP API went unanswered for the whole of a long search), and the plan is the
;; one the search in one go finds
(deftest a-long-search-yields-to-other-work-and-finds-the-same-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:blocks (walkways true) :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              c {:primitives p}
              order (atom [])
              chunk walk/chunk-expansions
              whole (walk/plan-walk c (.pathWorld p) [6 64 0] 0 walk/default-weight)]
          (set! walk/chunk-expansions 16)
          (js/setTimeout #(swap! order conj :timer) 0)
          (let [plan (await (walk/plan-walk! c (.pathWorld p) [6 64 0] 0 walk/default-weight))]
            (swap! order conj :planned)
            (set! walk/chunk-expansions chunk)
            (is (= [:timer :planned] @order))
            (is (= "found" (:status plan) (:status whole)))
            (is (= (:steps whole) (:steps plan)))
            (is (> (count (:steps plan)) 100))))))))

(deftest a-goal-sealed-in-stone-is-no-path
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [[x z] [[9 1] [11 1] [10 0] [10 2]] y [64 65]] [(str x "," y "," z) "stone"]))
              blocks (merge (floor (range 12)) wall {"10,66,1" "stone"})
              [{:keys [result]}] (await (walk-to blocks [10 64 1] start))]
          (is (= {:status :no-path :reason :goal-enclosed} (select-keys result [:status :reason]))))))))

;; an island (stone at y 63, feet 64) of x 0..4 over a lower floor of x 5..11; the goal stands on a 3-high pillar of the
;; lower floor, which no move reaches: the plan is partial and its nearest end is down the drop
(def island-and-pillar
  (merge (box 0 63 0 4 63 2 "stone") (box 5 60 0 11 60 2 "stone") (box 10 61 0 10 63 2 "stone")))

(deftest a-partial-plan-stops-at-the-one-way-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [result]} announced at] (await (walk-to island-and-pillar [10 64 1] start))]
          (is (= {:status :no-path :reason :one-way} (select-keys result [:status :reason])))
          (is (= :drop (:kind (:one-way result))))
          (is (number? (:near result)))
          (is (= [:plan] announced) "the island part was walked, no replan")
          (is (<= (first at) 5) "the body never went down the drop"))))))

;; walk-to! (walk-plan's driver) never takes a one-way step, even when the land below runs on into unloaded land (go-to
;; does, engine.path.near)
(deftest walk-to-stops-at-a-one-way-step-even-toward-unloaded-land
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cliff (merge (floor (range 11)) (box 11 60 0 47 60 2 "stone"))
              [{:keys [result]} _ at] (await (walk-to cliff [120 61 1] start))]
          (is (= {:status :no-path :reason :one-way} (select-keys result [:status :reason])))
          (is (< (first at) 11) "the body never went down the drop"))))))

;; a gap of 4 empty cells (x 5..8) is wider than any jump: no ability would help, so the answer is :exhausted, not :abilities, and the
;; walk goes to the edge
(deftest a-gap-wider-than-any-jump-ends-exhausted-at-the-edge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (floor (range 5)) (floor (range 9 14)))
              [{:keys [result]} _ at] (await (walk-to blocks [12 64 1] start))]
          (is (= {:status :no-path :reason :exhausted} (select-keys result [:status :reason])))
          (is (not (contains? result :kind)))
          (is (>= (first at) 4) "at the edge"))))))

;; an island of x 0..1 gets no nearer than 1 block to the goal: no plan is walked, the drop is the only way nearer
(def tiny-island-and-pillar
  (merge (box 0 63 0 1 63 2 "stone") (box 2 60 0 11 60 2 "stone") (box 10 61 0 10 63 2 "stone")))

(deftest a-drop-is-the-only-way-nearer-and-no-step-is-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [result]} announced at] (await (walk-to tiny-island-and-pillar [10 64 1] start))]
          (is (= {:status :no-path :reason :one-way} (select-keys result [:status :reason])))
          (is (= :drop (:kind (:one-way result))))
          (is (= [] announced) "nothing was planned to walk")
          (is (<= (first at) 1) "the body never went down the drop"))))))

;; the island, then a ledge one below it, then the lower floor 2 below the ledge: the walk goes on to the ledge, which it can
;; climb back from, and stops before the drop
(def island-ledge-and-pillar
  (merge (box 0 63 0 4 63 2 "stone") (box 5 62 0 6 62 2 "stone") (box 7 60 0 11 60 2 "stone") (box 10 61 0 10 63 2 "stone")))

(deftest the-walk-goes-down-a-one-block-drop-to-the-ledge-and-stops-before-the-bigger-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [result]} _ at] (await (walk-to island-ledge-and-pillar [10 64 1] start))]
          (is (= {:status :no-path :reason :one-way} (select-keys result [:status :reason])))
          (is (= [7 61 1] (:at (:one-way result))))
          (is (<= 5 (first at) 6) "on the ledge"))))))

(deftest dry-end-keeps-a-partial-plan-up-to-its-last-dry-step
  (let [s (fn [x swim?] (cond-> {:x x :move :walk} swim? (assoc :swim true)))]
    (are [steps kept] (= kept (mapv :x (walk/dry-end steps)))
      [(s 0 false) (s 1 false) (s 2 true) (s 3 true)] [0 1]
      [(s 0 true) (s 1 true)] []
      [(s 0 false) (s 1 false)] [0 1])))

(deftest with-walls-reads-the-named-cells-as-one-stone-state-and-leaves-the-rest
  (let [pw (.pathWorld (tu/fake {:blocks (merge (box 0 64 0 2 64 0 "oak_fence_gate") (box 0 65 0 0 65 0 "air"))}))
        walled (walk/with-walls pw [{:x 1 :y 64 :z 0}])
        id (walk/wall-id (.-table pw))
        at (fn [pw x] (.stateAt (.-snapshot pw) x 64 0))]
    (is (identical? pw (walk/with-walls pw [])) "no walls: the same pathWorld")
    (is (= [(at pw 0) id (at pw 2)] [(at walled 0) (at walled 1) (at walled 2)]))
    (is (not= id (at pw 1)))
    (is (= 1 (aget (.-kind (.-table walled)) id)) "a solid block")
    (is (= [16 0] [(aget (.-top (.-table walled)) id) (aget (.-openable (.-table walled)) id)]))
    (is (= [(.-table pw) (.-space pw)] [(.-table walled) (.-space walled)]))))

;; ---- the frontier and a walled-in goal ----

(defn plan-from
  "walk/plan-walk from the cell pos (feet) to goal over blocks with opts."
  [blocks [x y z] goal opts]
  (let [p (tu/fake {:blocks blocks :self {:pos {:x (+ x 0.5) :y y :z (+ z 0.5)}}})]
    (walk/plan-walk {:primitives p} (.pathWorld p) goal 0 walk/default-weight opts)))

;; a walkway (feet 80) x 18..47 at z 8 over a floor x 0..47, z 0..15; columns from x 48 on are not loaded
(def walkway-to-unloaded (merge (box 0 63 0 47 63 15 "stone") (box 18 79 8 47 79 8 "stone")))

(deftest plan-walk-takes-the-frontier-only-when-asked
  (let [asked (plan-from walkway-to-unloaded [18 80 8] [10 64 8] {:frontier true})
        not-asked (plan-from walkway-to-unloaded [18 80 8] [10 64 8] nil)]
    (is (nil? (walk/no-walk asked 0)))
    (is (= {:at [46 80 8]} (:frontier-taken asked)))
    (is (= [46 80 8] ((juxt :x :y :z) (peek (:steps asked)))))
    (is (= {:status :no-path :reason :exhausted :replans 0} (walk/no-walk not-asked 0)))
    (is (nil? (:frontier-taken not-asked)))))

;; at the frontier the plan walks nowhere: not back to the walkway's end nearest the goal (x 18), which would swing the
;; body between the two
(deftest plan-walk-at-its-frontier-walks-nowhere
  (let [plan (plan-from walkway-to-unloaded [46 80 8] [10 64 8] {:frontier true})]
    (is (= {:status :no-path :reason :exhausted :replans 0} (walk/no-walk plan 0)))
    (is (nil? (:frontier-taken plan)))))

;; a floor x -2..10, z -2..4 cut by a stone wall at x 5 (feet and head): the goal's side is walled in, the planner's
;; partial plan ends at the wall
(def walled-off (merge (box -2 63 -2 10 63 4 "stone") (box 5 64 -2 5 65 4 "stone")))

(deftest a-walled-in-goal-is-not-walked-towards
  (let [plan (plan-from walled-off [0 64 1] [8 64 1] {:frontier true})]
    (is (= ["partial" "goal-enclosed"] [(:status plan) (.-reason (:r plan))]))
    (is (= {:status :no-path :reason :goal-enclosed :replans 0} (walk/no-walk plan 0)))))

;; ---- one search where one will do; one bounded search a call (go-to) ----

(defn searches-of
  "How many planner searches (planner/plan) f runs."
  [f]
  (let [n (atom 0)
        plan planner/plan]
    (with-redefs [planner/plan (fn [snapshot query options] (swap! n inc) (plan snapshot query options))]
      (f))
    @n))

;; the walkways without their join run past the default box and hold no way: the answer was 4 searches (the default box,
;; the wide one, and both again without the walker's limits); the limits turned no move away, so it is one
(deftest plan-within-searches-once-where-the-limits-turned-nothing-away
  (is (= 1 (searches-of #(plan-within-status (walkways false) [6 64 0]))))
  (is (= ["none" "exhausted"] (plan-within-status (walkways false) [6 64 0]))))

;; a trench whose only way over is a gap jump up, which the executor does not walk: the search without the limits finds it
(def gap-up-only (merge (box 0 63 -8 4 63 10 "stone") (box 7 63 -8 12 64 10 "stone")))

(deftest plan-within-searches-without-the-limits-when-they-turned-a-move-away
  (let [p (tu/fake {:blocks gap-up-only :self {:pos {:x 0.5 :y 64 :z 1.5}}})
        within (atom nil)]
    (is (= 2 (searches-of #(reset! within (walk/plan-within {:primitives p} (.pathWorld p) [10 65 1] 0 walk/default-weight)))))
    (is (= :gap-up (:kind (:beyond @within))))))

(defn ^:async budgeted-calls
  "plan-walk! with budget (chunk-expansions 16) called from pos toward goal until a call answers with more than
  \"searching\": [that plan, the calls]."
  [blocks pos goal budget]
  (let [p (tu/fake {:blocks blocks :self {:pos pos}})
        c {:primitives p}
        chunk walk/chunk-expansions]
    (reset! walk/searches {})
    (set! walk/chunk-expansions 16)
    (loop [calls 1]
      (let [plan (await (walk/plan-walk! c (.pathWorld p) goal 0 walk/default-weight {:budget budget}))]
        (if (and (= "searching" (:status plan)) (< calls 100))
          (recur (inc calls))
          (do (set! walk/chunk-expansions chunk)
              [plan calls]))))))

;; the joined walkways' way round is ~200 cells along a walkway that leads away from the goal: no call gets nearer, so
;; each walks nowhere and the next goes on with the same search, which ends with the plan of a search in one go
(deftest a-budgeted-search-goes-on-at-the-next-call-and-ends-as-in-one-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:blocks (walkways true) :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              whole (walk/plan-walk {:primitives p} (.pathWorld p) [6 64 0] 0 walk/default-weight)
              [plan calls] (await (budgeted-calls (walkways true) {:x 0.5 :y 64 :z 0.5} [6 64 0] 32))]
          (is (> calls 3) "several calls")
          (is (= {:status :searching :replans 0} (walk/no-walk (walk/unfinished-plan {:walled nil :limited #js {:progress (fn [] nil)}} 1) 0)))
          (is (= "found" (:status plan) (:status whole)))
          (is (= (:steps whole) (:steps plan)))
          (is (= {} @walk/searches) "the search is over"))))))

;; a long corridor toward the goal: the first call's search is not over, but it got well on: that much is walked
(deftest a-budgeted-search-that-got-well-on-walks-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[plan calls] (await (budgeted-calls (floor (range 120)) {:x 0.5 :y 64 :z 1.5} [110 64 1] 32))]
          (is (= 1 calls))
          (is (= ["partial" "searching"] [(:status plan) (.-reason (:r plan))]))
          (is (nil? (walk/no-walk plan 0)))
          (is (>= (:x (peek (:steps plan))) walk/progress-blocks))
          (is (= {} @walk/searches) "the body walks on: a new search from where it gets to"))))))
