(ns jobs.movement.go-to.preview
  "go-to's dry run, what its quote reports: the route the planner finds to the goal from where the body stands, with
  go-to's policy, costs, dangers and tolls. Nothing is walked, opened or dug.

  Rules of go-to:
  - Its quote (./bin/quote) is a dry run: one plan from where the body stands with the call's args, nothing walked,
    opened or dug, and :risk its walk risk (jobs.lib.walk-risk/estimate of the plan, the one danger figure a caller weighs). :uses :job-time [s s 3s] from the planner's seconds (partial or no route: no most; none and no way that
    changes the world: ##Inf), :health the planned damage, :moves-to the goal cell, and :route {:found :length :seconds
    :summary :steps :moves :doors :waypoints :damage :drown :nodes :dark :dark-seconds} (:dark: feet cells seen dark, or unseen at night; :dark-seconds the planner's extra seconds for them), or {:found false :reason :near :partial} (:reason
    as :why, :bad-pos and other refusals, :searching while the quote's budgeted search has not ended); with :unknown :unseen-floors and :dark-best."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.near :as near]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.aim :as gaim]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.tuning :refer [quote-most-factor quote-unseen-factor]]))

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
  "The report of a plan's steps and planner path: length, seconds, :priced (the planner's price, seconds with
  darkness, dangers, damage and tolls on top), summary, steps, moves, doors, waypoints, :damage (hp
  its drops and plants cost), :dark-seconds (the planner's extra seconds for dark cells) and :drown (hp) where the plan drowns."
  [steps ^js path]
  (cond-> {:length (walk/path-length steps)
   :seconds (/ (js/Math.round (* 10 (.-seconds (.-cost path)))) 10)
   :summary (.-summary path)
   :steps (count steps)
   :moves (dissoc (frequencies (map :move (rest steps))) :walk)
   :doors (vec (distinct (for [s steps o (:opens s)] [(:x o) (:y o) (:z o)])))
   :waypoints (waypoints steps)}
    (number? (.-priced (.-cost path))) (assoc :priced (/ (js/Math.round (* 10 (.-priced (.-cost path)))) 10))
    (pos? (or (.-darkSeconds (.-cost path)) 0)) (assoc :dark-seconds (/ (js/Math.round (* 10 (.-darkSeconds (.-cost path)))) 10))
    (some :damage steps) (assoc :damage (/ (js/Math.round (* 10 (reduce + (keep :damage steps)))) 10))
    (pos? (or (.-drown (.-cost path)) 0)) (assoc :drown (/ (js/Math.round (* 10 (.-drown (.-cost path)))) 10))))

(defn dark-cells
  "How many feet cells of steps sh-sense/dark-fn reads dark (seen dark; unseen at night unless unknown, whose mask prices
  them), nil without a perception."
  [p steps unknown]
  (when-let [{:keys [at]} (sh-sense/dark-fn p {:unseen-dark? (nil? unknown)})]
    (count (into #{} (comp (map (fn [s] (mapv #(js/Math.floor (% s)) [:x :y :z]))) (filter (fn [[x y z]] (= 1 (at x y z))))) steps))))

(defn u-dist
  "The horizontal distance in blocks, rounded, from the body to the goal cell to."
  [c to] (js/Math.round (js/Math.hypot (- (:x to) (:x (wworld/body-cell c))) (- (:z to) (:z (wworld/body-cell c))))))

(defn report
  "The route of a plan (jobs.lib.near/plan!), a found one with :dark (dark-cells); with unknown, what the masked plan says of unseen floors
  (near/with-sight), a dark drop as go-to's :unseen-floor-dark."
  [c plan to policy unknown]
  (let [{:keys [r steps status]} plan
        path (or (some-> r .-path) (some-> r .-oneWay .-path))
        no (wplan/no-walk plan 0 policy)
        here (u-dist c to)
        result (cond
                 (and (nil? no) (= "found" status)) (let [dark (dark-cells (:primitives c) steps unknown)]
                                                      (cond-> (assoc (route steps path) :found true) dark (assoc :dark dark)))
                 (nil? no) {:found false :reason :partial :near here :partial (route steps path)}
                 :else (assoc (dissoc no :replans :status) :found false :near here))]
    (if-not unknown
      result
      (let [seen (near/with-sight result plan [(:x to) (:y to) (:z to)])]
        (cond-> seen
          (end/dark-end? (if (= :partial (:reason seen))
                           (assoc seen :status :off-plan :partial true)
                           (assoc seen :status :no-path)))
          (assoc :reason :unseen-floor-dark))))))

(def refusals "Route reasons that say the call cannot start, not that no way was found." #{:bad-pos :bad-name :unknown-place :unsupported})

(defn ^:async route!
  "The dry run of go-to call c (args with the body's food and the walk limits): its route, {:found true ...} or {:found
  false :reason ...} (report), with :goal the cell planned to, :nodes the search's expansions and :risk the walk risk of
  its plan (jobs.lib.walk-risk/estimate: past a plan not found, the rest priced as walk-risk prices it); a refusal {:found
  false :reason :text}; :searching (with :near, the blocks to the goal) when the search spent its round budget: it goes on at the next quote of the same walk (walk.search/searches)."
  [c]
  (let [{:keys [doors range tolls one-way unknown]} (:args c)
        refusal (gaim/args-refusal c)
        pos (some->> (when-not refusal (:pos (gaim/target c))) (#(if (nil? (:y %)) (:pos (gaim/aimed c %)) %)) (wworld/stand-goal (wworld/path-world (:primitives c))))]
    (cond
      refusal {:found false :reason (:reason refusal) :text (:message refusal)}
      (nil? (wworld/path-world (:primitives c))) {:found false :reason :unsupported :text "the body cannot sense the world for path planning"}
      :else
      (let [doors (or doors :shut)
            policy (cond-> (wworld/body-policy c)
                     (not= :never doors) (update :moves conj :open))
            spec (when unknown (wworld/with-seeds (wworld/unknown-spec) [(:x pos) (:y pos) (:z pos)] nil))
            plan (await (near/plan! c [(:x pos) (:y pos) (:z pos)] (or range 1) doors policy [] false
                                    (when-not (= :closed one-way) :open) wsearch/round-budget false
                                    (wworld/dangers?) (set (:fault-cells (ctx/mem c))) (wworld/dark?)
                                    (cond-> tolls (wworld/zone-tolls?) (-> vec (into (tc/zone-walk-tolls c pos)))) spec true))
            goal [(:x pos) (:y pos) (:z pos)]]
        (assoc (if (= "searching" (:status plan))
                 {:found false :reason :searching :near (u-dist c pos) :goal goal :text "the quote's search goes on at the next quote"}
                 (assoc (report c plan pos policy spec) :goal goal :nodes (some-> plan :r .-expanded)))
               :risk (risk/estimate c pos {:plan (select-keys plan [:status :steps])
                                   :back? (:back (:args c)) :stay-s (:stay-s (:args c))}))))))

(defn rest-s
  "Seconds a quote prices blocks still to go past what is planned: quote-unseen-factor walk blocks (cost.planner/walk-floor) each."
  [blocks]
  (* blocks cost-planner/walk-floor (quote-unseen-factor)))

(defn quote-of-route
  "The quote of route r: time from the planner's seconds (most quote-most-factor times them); a partial route or none
  adds quote-unseen-factor walk blocks (cost.planner/walk-floor) for each block still to go, from the route's end to the goal, and has no most; a search still going on (:searching) is priced so too, from its :near blocks, marked :estimate true; none at all, or a refusal, is ##Inf unless escalating? (a way
  that changes the world may still get there). Health: the planned damage. :moves-to the goal. :risk the route's walk risk.
  `back?` (go-to :back): the time is the walk there and back, as the risk prices it."
  [{:keys [found seconds partial near reason damage goal] :as r} escalating? back?]
  (let [searching? (= :searching reason)
        legs (if back? 2 1)
        s (* legs (or seconds (:seconds partial) 0))
        [ex _ ez] (last (:waypoints partial))
        rest-blocks (if (and goal ex) (js/Math.round (js/Math.hypot (- (first goal) ex) (- (nth goal 2) ez))) (or near 0))
        left-s (* legs (rest-s rest-blocks))
        time (cond
               found [s s (* s (quote-most-factor))]
               (refusals reason) (get-in contract/too-expensive [:uses :job-time])
               (or partial searching? escalating?) [s (+ s left-s) ##Inf]
               :else (get-in contract/too-expensive [:uses :job-time]))]
    (cond-> {:uses {:job-time time} :route (dissoc r :goal :risk)}
      (:risk r) (assoc :risk (:risk r))
      (and found damage) (assoc-in [:uses :health] [0 damage damage])
      searching? (assoc :estimate true)
      goal (assoc :moves-to goal))))
