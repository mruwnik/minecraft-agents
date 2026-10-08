(ns jobs.movement.path-preview
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.near :as near]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.result :as end]))

(def doc
  "Dry run of a go-to: plan the route to :pos (or :place) from where the body stands, with the policy, costs, dangers and
  tolls go-to plans with, and report it. Nothing is walked, opened or dug. One round, one plan (a search of any length, in
  slices that yield), then :done.
  Takes the go-to args :pos (a column [x z] too) :place :range :doors :dangers :dark :tolls :drop-cost :costs :landing :gait :min-health :max-damage :hp-seconds :food :one-way :unknown, same defaults, and the danger prices :flee-factor :fight-factor :danger-max-rate :danger-shape.
  Returns {:status :completed :found true :length :seconds :summary :steps :moves :doors :waypoints (:drown hp where it drowns)}: length is the horizontal
  blocks, seconds the planner's cost in seconds (tolls, dark and danger costs not included), summary its words (drops, swims,
  doors), steps the step count, moves a count of each move kind (:drop :jump :open ...), doors the cells [x y z] it
  would open, waypoints the cells [x y z] where the route turns or changes move kind, ending at the goal's end of the plan.
  A route that only gets nearer (a partial plan) or none is {:status :stopped :found false :reason (the planner's, as
  go-to's :why; :abilities with :kind) :near} plus :partial {the same fields up to where it ends} when there is one.
  With :unknown, a route that checks unseen floors adds :unseen-floors true, and one refused for a dark drop go-to would
  give up on is :reason :unseen-floor-dark with :dark-best (the stand {:x :y :z}).
  An arg its spec refuses (as go-to's) is refused at submit. A :pos outside the world, a bad or unknown :place, or a body
  without pathWorld sensing, is {:status :stopped :found false :reason :bad-pos|:bad-name|:unknown-place|:unsupported :text}.")

(a/defargs args
  {:pos {:doc "as go-to :pos: [x y z], {:x :y :z}, or [x z]/{:x :z} for the ground of that column" :spec column/pos-or-column? :default nil}
   :place {:doc "name of a place in body memory (:home, :bed, ...) to preview the walk to instead of :pos" :spec (a/or-of keyword? string?) :default nil}
   :range {:doc "how close counts as there, in cells" :spec (a/num-in 0 nil) :default 1}
   :doors {:doc "as go-to: :shut or :leave-open let the route open doors, gates and trapdoors; :never makes them walls" :spec #{:shut :leave-open :never} :default :shut}
   :dangers {:doc "false: plan straight past known dangers; true: keep away from them" :spec boolean? :default true}
   :dark {:doc "false: plan dark cells like lit ones" :spec boolean? :default true}
   :tolls {:doc "cells to cross only as a last resort, [{:x :y :z :factor}]" :spec (a/valid-by "a list of {:x :y :z :factor} of finite numbers, :factor >= 0" wworld/tolls-problem) :default nil}
   :drop-cost {:doc "number: scales the cost of a drop; false: no drop of 2 or 3 at all" :spec (a/or-of (a/num-in 0 nil) false?) :default 1}
   :costs {:doc "as go-to :costs" :spec (a/valid-by "a map of planner price name to a number of seconds >= 0" cost/planner-costs-problem) :default nil}
   :landing {:doc "as go-to :landing: block name -> share of a fall's damage taken landing on it (negative: a bounce, no damage but settle time)" :spec (a/valid-by "a map of block name to a number" cost/landing-problem) :default nil}
   :gait {:doc "as go-to :gait: :auto, :walk (no sprint) or :sneak (slow, no sprint); nil: the body's setting, else :auto" :spec #{:auto :walk :sneak} :default nil}
   :min-health {:doc "go-to's :min-health: the hp the walk may not spend below" :spec (a/num-in 1 20) :default nil}
   :max-damage {:doc "go-to's :max-damage: at most this many hp spent on drops and plants" :spec (a/num-in 0 nil) :default nil}
   :food {:doc "go-to's :food: the food level (0-20) the damage budget counts on; default the body's own" :spec (a/num-in 0 20) :default nil}
   :hp-seconds {:doc "go-to's :hp-seconds: seconds an hp costs at full health" :spec a/pos-num? :default cost/hp-seconds}
   :flee-factor {:doc "go-to's :flee-factor" :spec (a/num-in 0 nil) :default (:flee cost/danger-stances)}
   :fight-factor {:doc "go-to's :fight-factor" :spec (a/num-in 0 nil) :default (:fight cost/danger-stances)}
   :danger-max-rate {:doc "go-to's :danger-max-rate" :spec a/pos-num? :default cost/danger-max-rate}
   :danger-shape {:doc "go-to's :danger-shape" :spec (a/valid-by "a map of :sensed, :remembered or :creeper to {:close :radius} numbers >= 0, :close under :radius" cost/danger-shape-problem) :default nil}
   :unknown {:doc "as go-to :unknown: plan over what the body has seen or felt only" :spec (a/valid-by "true, false or a map of :cell :floor :peek-s :aim-s :see-lit-depth :light-depth to numbers >= 0" wworld/unknown-problem) :default false}
   :one-way {:doc ":closed takes no drop of 2 or 3 or gap jump down that the body cannot climb back; :open (default) takes one toward unloaded land" :spec #{:open :closed} :default :open}})

(defn check [_c] true)

(defn waypoints
  "The cells [x y z] of steps where the heading or the move kind changes, and of the last step."
  [steps]
  (let [cell (fn [s] [(:x s) (:y s) (:z s)])
        dir (fn [a b] [(compare (:px b) (:px a)) (compare (:pz b) (:pz a))])]
    (if (< (count steps) 2)
      (mapv cell steps)
      (let [pairs (map vector steps (rest steps) (drop 2 steps))
            turns (for [[a b nx] pairs
                        :when (or (not= (dir a b) (dir b nx)) (not= (:move b) (:move nx)))]
                    (cell b))]
        (vec (concat turns [(cell (peek steps))]))))))

(defn route
  "The report of a plan's steps and planner path: length, seconds, summary, steps, moves, doors, waypoints, and :drown (hp) where the plan drowns."
  [steps ^js path]
  (cond-> {:length (walk/path-length steps)
   :seconds (/ (js/Math.round (* 10 (.-seconds (.-cost path)))) 10)
   :summary (.-summary path)
   :steps (count steps)
   :moves (dissoc (frequencies (map :move (rest steps))) :walk)
   :doors (vec (distinct (for [s steps o (:opens s)] [(:x o) (:y o) (:z o)])))
   :waypoints (waypoints steps)}
    (pos? (or (.-drown (.-cost path)) 0)) (assoc :drown (/ (js/Math.round (* 10 (.-drown (.-cost path)))) 10))))

(defn u-dist
  "The horizontal distance in blocks, rounded, from the body to the goal cell to."
  [c to] (js/Math.round (js/Math.hypot (- (:x to) (:x (wworld/body-cell c))) (- (:z to) (:z (wworld/body-cell c))))))

(defn report
  "The job's result for a plan (jobs.lib.near/plan!); with unknown, what the masked plan says of unseen floors
  (near/with-sight), a dark drop as go-to's :unseen-floor-dark."
  [c plan to policy unknown]
  (let [{:keys [r steps status]} plan
        no (wplan/no-walk plan 0 policy)
        here (u-dist c to)
        result (cond
                 (and (nil? no) (= "found" status)) (assoc (route steps (.-path r)) :status :completed :found true)
                 (nil? no) {:status :stopped :found false :reason :partial :near here :partial (route steps (.-path r))}
                 :else (assoc (dissoc no :replans) :status :stopped :found false :near here))]
    (if-not unknown
      result
      (let [seen (near/with-sight result plan [(:x to) (:y to) (:z to)])]
        (cond-> seen
          (end/dark-end? (if (= :partial (:reason seen))
                           (assoc seen :status :off-plan :partial true)
                           (assoc seen :status :no-path)))
          (assoc :reason :unseen-floor-dark))))))

(defn refuse!
  "End the preview with a refusal: {:status :stopped :found false :reason :text}."
  [c {:keys [reason message]}]
  (ctx/emit! c :refused :warn {:reason reason :text message})
  (end/finish! c {:status :stopped :found false :reason reason :text message}))

(defn ^:async round [c]
  (let [c (go-to/with-body-food c)
        {:keys [doors range dangers dark tolls drop-cost costs one-way unknown]} (:args c)
        refusal (go-to/args-refusal c)
        pos (some->> (when-not refusal (:pos (go-to/target c))) (#(if (nil? (:y %)) (:pos (go-to/aimed c %)) %)) (wworld/stand-goal (wworld/path-world (:primitives c))))]
    (cond
      refusal (refuse! c refusal)
      (nil? (wworld/path-world (:primitives c)))
      (refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})
      :else
      (let [doors (or doors :shut)
            policy (cond-> (wworld/body-policy c)
                     (not= :never doors) (update :moves conj :open)
                     (some? drop-cost) (assoc :drop-cost drop-cost)
                     (some? costs) (assoc :costs costs))
            spec (when unknown (wworld/unknown-spec (when (map? unknown) unknown)))
            plan (await (near/plan! c [(:x pos) (:y pos) (:z pos)] (or range 1) doors policy [] false
                                    (when-not (= :closed one-way) :open) nil true
                                    (not (false? dangers)) nil (not (false? dark)) tolls spec))
            result (report c plan pos policy spec)]
        (ctx/result! c result)
        (ctx/emit! c :path-preview :info (assoc (dissoc result :partial) :nodes (some-> plan :r .-expanded) :text (if (:found result) (str "route: " (:summary result)) (str "no route: " (name (:reason result))))))
        :done))))
