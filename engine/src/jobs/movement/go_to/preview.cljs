(ns jobs.movement.go-to.preview
  "go-to's dry run, what its quote reports: the route the planner finds to the goal from where the body stands, with
  go-to's policy, costs, dangers and tolls. Nothing is walked, opened or dug."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
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
  "The report of a plan's steps and planner path: length, seconds, summary, steps, moves, doors, waypoints, :damage (hp
  its drops and plants cost), :dark-seconds (the planner's extra seconds for dark cells) and :drown (hp) where the plan drowns."
  [steps ^js path]
  (cond-> {:length (walk/path-length steps)
   :seconds (/ (js/Math.round (* 10 (.-seconds (.-cost path)))) 10)
   :summary (.-summary path)
   :steps (count steps)
   :moves (dissoc (frequencies (map :move (rest steps))) :walk)
   :doors (vec (distinct (for [s steps o (:opens s)] [(:x o) (:y o) (:z o)])))
   :waypoints (waypoints steps)}
    (pos? (or (.-darkSeconds (.-cost path)) 0)) (assoc :dark-seconds (/ (js/Math.round (* 10 (.-darkSeconds (.-cost path)))) 10))
    (some :damage steps) (assoc :damage (/ (js/Math.round (* 10 (reduce + (keep :damage steps)))) 10))
    (pos? (or (.-drown (.-cost path)) 0)) (assoc :drown (/ (js/Math.round (* 10 (.-drown (.-cost path)))) 10))))

(defn dark-cells
  "How many feet cells of steps look/dark-fn reads dark (seen dark; unseen at night unless unknown, whose mask prices
  them), nil without a perception."
  [p steps unknown]
  (when-let [{:keys [at]} (look/dark-fn p {:unseen-dark? (nil? unknown)})]
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

(defn with-deadline
  "c whose search is cut (ctx/alive? false, wplan/stop-if-cut!) once the clock passes its :quote-deadline."
  [c]
  (let [alive? (:alive? c) qd (:quote-deadline c)]
    (cond-> c qd (assoc :alive? #(and (or (nil? alive?) (alive?)) (<= (ctx/now c) qd))))))

(def refusals "Route reasons that say the call cannot start, not that no way was found." #{:bad-pos :bad-name :unknown-place :unsupported :late})

(defn ^:async route!
  "The dry run of go-to call c (args with the body's food and the walk limits): its route, {:found true ...} or {:found
  false :reason ...} (report), with :goal the cell planned to and :nodes the search's expansions; a refusal {:found
  false :reason :text}, :late when the search outran the quote deadline."
  [c]
  (let [{:keys [doors range dangers dark tolls drop-cost costs one-way unknown]} (:args c)
        refusal (gaim/args-refusal c)
        pos (some->> (when-not refusal (:pos (gaim/target c))) (#(if (nil? (:y %)) (:pos (gaim/aimed c %)) %)) (wworld/stand-goal (wworld/path-world (:primitives c))))]
    (cond
      refusal {:found false :reason (:reason refusal) :text (:message refusal)}
      (nil? (wworld/path-world (:primitives c))) {:found false :reason :unsupported :text "the body cannot sense the world for path planning"}
      :else
      (let [doors (or doors :shut)
            policy (cond-> (wworld/body-policy c)
                     (not= :never doors) (update :moves conj :open)
                     (some? drop-cost) (assoc :drop-cost drop-cost)
                     (some? costs) (assoc :costs costs))
            spec (when unknown (wworld/unknown-spec))
            plan (try (await (near/plan! (with-deadline c) [(:x pos) (:y pos) (:z pos)] (or range 1) doors policy [] false
                                         (when-not (= :closed one-way) :open) nil true
                                         (not (false? dangers)) nil (not (false? dark)) tolls spec))
                      (catch :default e
                        (when-not (= "cut" (.-code e)) (throw e))))]
        (if (nil? plan)
          {:found false :reason :late :text "the search outran the quote's deadline"}
          (assoc (report c plan pos policy spec) :goal [(:x pos) (:y pos) (:z pos)] :nodes (some-> plan :r .-expanded)))))))

(defn quote-of-route
  "The quote of route r: time from the planner's seconds (most quote-most-factor times them); a partial route or none
  adds quote-unseen-factor walk blocks (cost.planner/walk-floor) for each block still to go, from the route's end to the goal, and has no most; none at all, or a refusal, is ##Inf unless escalating? (a way
  that changes the world may still get there). Health: the planned damage. :moves-to the goal."
  [{:keys [found seconds partial near reason damage goal] :as r} escalating?]
  (let [s (or seconds (:seconds partial) 0)
        [ex _ ez] (last (:waypoints partial))
        rest-blocks (if (and goal ex) (js/Math.round (js/Math.hypot (- (first goal) ex) (- (nth goal 2) ez))) (or near 0))
        rest-s (* rest-blocks cost-planner/walk-floor (quote-unseen-factor))
        time (cond
               found [s s (* s (quote-most-factor))]
               (refusals reason) (get-in contract/too-expensive [:uses :job-time])
               (or partial escalating?) [s (+ s rest-s) ##Inf]
               :else (get-in contract/too-expensive [:uses :job-time]))]
    (cond-> {:uses {:job-time time} :route (dissoc r :goal)}
      (and found damage) (assoc-in [:uses :health] [0 damage damage])
      goal (assoc :moves-to goal))))
