(ns engine.goto-bench
  "go-to's planning as a library for the go-to bench (bench-lang/goto.mjs): the rounds go-to plans for a course, the body
  moved to the end of each walked plan (a static world, no physics). Compiled by `tools/compile engine goto-bench`."
  (:require [jobs.lib.util :as u]
            [engine.path.executor :as executor]
            [engine.path.fixture :as fx]
            [engine.path.offsets :as offsets]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.walk :as walk]
            [engine.path.blocks :as blocks]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.world :as wworld]))

(def max-rounds 200)

(offsets/set-root! (fx/engine-root)) ;; the bench may start outside engine/

(defn body-at
  "A ctx whose primitives say a healthy, fed, unarmoured body stands at the step {x y z px pz} (the damage probe reads it)."
  [^js at]
  {:primitives #js {:self (fn [] #js {:pos #js {:x (.-px at) :y (.-y at) :z (.-pz at)}
                                      :health 20 :absorption 0 :food 20 :onFire false :effects #js [] :equipment #js {}})}})

(defn traced
  "f with each planner/create-plan step longer than 15 ms logged (GOTO_BENCH_TRACE set): the bench's look inside a round."
  [f]
  (if-not js/process.env.GOTO_BENCH_TRACE
    (f)
    (let [create planner/create-plan]
      (with-redefs [planner/create-plan
                    (fn [snapshot query options]
                      (let [^js p (create snapshot query options)
                            step (.-step p)]
                        (set! (.-step p) (fn [n] (let [t (js/performance.now) r (step n) ms (- (js/performance.now) t)]
                                                   (when (> ms 15) (js/console.error "step" n (.toFixed ms 1) r (some? (.-limits options))))
                                                   r)))
                        p))]
        (f)))))

(def see-radius "Blocks round a position the masked bench body has seen: horizontally, and 3 below to 4 above." 6)

(defn see!
  "Marks the cells round the step {x y z} as seen in mem (a Map of \"cx,sy,cz\" -> Uint8Array(4096), 1 per seen cell)."
  [^js mem ^js at]
  (let [x0 (js/Math.floor (.-x at)) y0 (js/Math.floor (.-y at)) z0 (js/Math.floor (.-z at))]
    (doseq [x (range (- x0 see-radius) (inc (+ x0 see-radius)))
            y (range (- y0 3) (+ y0 5))
            z (range (- z0 see-radius) (inc (+ z0 see-radius)))]
      (let [k (str (bit-shift-right x 4) "," (bit-shift-right y 4) "," (bit-shift-right z 4))
            sec (or (.get mem k) (let [a (js/Uint8Array. 4096)] (.set mem k a) a))]
        (aset sec (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15)) 1)))))

(defn masked-world
  "pw over a snapshot that reads a cell not marked in mem as void_air, carrying go-to's :unknown options (the default prices,
  daylight everywhere, a body with no torch and near 4)."
  [^js pw ^js mem unknown]
  (let [^js snapshot (.-snapshot pw)
        id (first (blocks/state-ids (.-table pw) "void_air"))
        view (js/Object.create snapshot)
        known? (fn [x y z]
                 (when-let [^js sec (.get mem (str (bit-shift-right x 4) "," (bit-shift-right y 4) "," (bit-shift-right z 4)))]
                   (pos? (aget sec (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15))))))
        spec (js/Object.assign #js {} unknown #js {:id id :seeDepth (wsight/see-depth 4) :seeLit (fn [_ _ _] 1)
                                                   :known (fn [cx sy cz] (.get mem (str cx "," sy "," cz)))})]
    (set! (.-stateAt view) (fn [x y z] (let [s (.stateAt snapshot x y z)] (if (or (== s 0xFFFF) (known? x y z)) s id))))
    (set! (.-unknown view) spec)
    (js/Object.assign #js {} pw #js {:snapshot view})))

(defn ^:async plan-round
  "One go-to round's plan from at, as go-to's round makes it (door policy, :one-way :open, :frontier, round-budget): [the
  plan, its ms]."
  [pw at to range]
  (let [t (js/performance.now)
        plan (await (walk/plan-walk! (body-at at) pw to range (walk/default-weight)
                                     {:policy executor/door-policy :one-way :open :frontier true
                                      :budget wsearch/round-budget}))]
    [plan (- (js/performance.now) t)]))

(defn ^:async simulate
  "The rounds of go-to over pw from {x y z px pz} to the goal cell {x y z} within range: each round plans; a plan that is
  walked moves the body to its last step, one still searching walks nowhere and the next round goes on with it. Ends
  arrived, on a plan not walked (its no-walk status and reason), after 3 walked rounds that get no nearer, or after
  max-rounds. A JS object {answer rounds: [{ms searches status}]}: one search a round, searches its ms. masked: the body
  plans over what it has seen only (see!: round every step it has walked), as go-to :unknown does."
  [^js pw0 ^js from ^js goal range masked]
  (reset! wsearch/searches {})
  (let [mem (js/Map.)
        unknown (wworld/unknown-spec)
        pw-at (fn [] (if masked (masked-world pw0 mem unknown) pw0))
        to [(.-x goal) (.-y goal) (.-z goal)]
        goal-cell {:x (.-x goal) :y (.-y goal) :z (.-z goal)}
        done (fn [answer rounds] #js {:answer answer :rounds (clj->js rounds)})]
    (loop [at from rounds [] best (u/dist {:x (.-x from) :y (.-y from) :z (.-z from)} goal-cell) blocked 0]
      (let [cell {:x (.-x at) :y (.-y at) :z (.-z at)}]
        (cond
          (u/within? cell goal-cell range) (done "arrived" rounds)
          (or (>= (count rounds) max-rounds) (>= blocked 3)) (done "blocked" rounds)
          :else
          (let [_ (when masked (see! mem at))
                [plan ms] (await (traced #(plan-round (pw-at) at to range)))
                no (wplan/no-walk plan 0 executor/door-policy)
                rounds (conj rounds {:ms ms :searches [ms] :status (str (or (:status no) (:status plan)))})]
            (cond
              (= :searching (:status no)) (recur at rounds best blocked)
              no (done (str "no-walk:" (name (or (:reason no) (:status no)))) rounds)
              :else
              (let [end (peek (:steps plan))
                    _ (when masked (doseq [step (:steps plan)] (see! mem #js {:x (:x step) :y (:y step) :z (:z step)})))
                    next-at #js {:x (:x end) :y (:y end) :z (:z end) :px (:px end) :pz (:pz end)}
                    left (u/dist {:x (:x end) :y (:y end) :z (:z end)} goal-cell)]
                (recur next-at rounds (min best left) (if (< left (dec best)) 0 (inc blocked)))))))))))
