(ns engine.goto-bench
  "go-to's planning as a library for the go-to bench (bench-lang/goto.mjs): the rounds go-to plans for a course, the body
  moved to the end of each walked plan (a static world, no physics). Compiled by `tools/compile engine goto-bench`."
  (:require [jobs.lib.util :as u]
            [engine.path.executor :as executor]
            [engine.path.fixture :as fx]
            [engine.path.offsets :as offsets]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]))

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

(defn ^:async plan-round
  "One go-to round's plan from at, as go-to's round makes it (door policy, :one-way :open, :frontier, round-budget): [the
  plan, its ms]."
  [pw at to range]
  (let [t (js/performance.now)
        plan (await (walk/plan-walk! (body-at at) pw to range walk/default-weight
                                     {:policy executor/door-policy :one-way :open :frontier true
                                      :budget wsearch/round-budget}))]
    [plan (- (js/performance.now) t)]))

(defn ^:async simulate
  "The rounds of go-to over pw from {x y z px pz} to the goal cell {x y z} within range: each round plans; a plan that is
  walked moves the body to its last step, one still searching walks nowhere and the next round goes on with it. Ends
  arrived, on a plan not walked (its no-walk status and reason), after 3 walked rounds that get no nearer, or after
  max-rounds. A JS object {answer rounds: [{ms searches status}]}: one search a round, searches its ms."
  [^js pw ^js from ^js goal range]
  (reset! wsearch/searches {})
  (let [to [(.-x goal) (.-y goal) (.-z goal)]
        goal-cell {:x (.-x goal) :y (.-y goal) :z (.-z goal)}
        done (fn [answer rounds] #js {:answer answer :rounds (clj->js rounds)})]
    (loop [at from rounds [] best (u/dist {:x (.-x from) :y (.-y from) :z (.-z from)} goal-cell) blocked 0]
      (let [cell {:x (.-x at) :y (.-y at) :z (.-z at)}]
        (cond
          (u/within? cell goal-cell range) (done "arrived" rounds)
          (or (>= (count rounds) max-rounds) (>= blocked 3)) (done "blocked" rounds)
          :else
          (let [[plan ms] (await (traced #(plan-round pw at to range)))
                no (wplan/no-walk plan 0 executor/door-policy)
                rounds (conj rounds {:ms ms :searches [ms] :status (str (or (:status no) (:status plan)))})]
            (cond
              (= :searching (:status no)) (recur at rounds best blocked)
              no (done (str "no-walk:" (name (or (:reason no) (:status no)))) rounds)
              :else
              (let [end (peek (:steps plan))
                    next-at #js {:x (:x end) :y (:y end) :z (:z end) :px (:px end) :pz (:pz end)}
                    left (u/dist {:x (:x end) :y (:y end) :z (:z end)} goal-cell)]
                (recur next-at rounds (min best left) (if (< left (dec best)) 0 (inc blocked)))))))))))
