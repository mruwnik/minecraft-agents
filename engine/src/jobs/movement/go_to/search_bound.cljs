(ns jobs.movement.go-to.search-bound
  "How long go-to's escalation stands still while its dig-route searches go on over rounds: till standing has cost what
  the way it would take without them costs (the most a search can save), so a search with no route ends."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.escape.way :as escape-way]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.escalate-choose :as choose]))

(defn fallback-s
  "Seconds the way escape-way/choose finds from feet with no dig route costs to the goal: its step (escape-way/price, an
  approach its walk), the blocks on from where the step leaves the body as a quote prices them (cost-planner/rest-s), and
  the walk's risk to the goal (walk-risk/estimate); with none, an unpassable one, or one that does not lead into a goal
  the walk found walled in (memory :give-up, choose/leads-in?), what the call may spend (choose/call-budget-s: the goal's worth within its time left), all a search can save."
  [c feet goal may-dig? opts]
  (let [e (escape-way/choose (:primitives c) feet goal may-dig? (update opts :skip (fnil conj #{}) :dig-route))
        walled? (= :goal-enclosed (some-> (get-in (ctx/mem c) [:give-up :result :reason]) keyword))
        cell #(zipmap [:x :y :z] %)
        end (or (:pos e) feet)]
    (let [total (when-not (or (= :none (:step e)) (and walled? (not (choose/leads-in? e feet goal))))
                  (+ (if (= :approach (:step e))
                       (:cost (cost/walk-cost {:distance (u/dist (cell feet) (cell end))}))
                       (escape-way/price (:primitives c) feet e))
                     (cost-planner/rest-s (js/Math.round (js/Math.hypot (- (goal 0) (end 0)) (- (goal 1) (end 1)) (- (goal 2) (end 2)))))
                     (risk/risk-s c (risk/estimate c (cell goal)))))]
      (if (and total (js/isFinite total)) total (choose/call-budget-s c)))))

(defn stood-s
  "Seconds standing still since `since` (ctx/now ms) has cost the body: the time and what staying at its cell risks over
  it (walk-risk/stay-estimate, risk/risk-s)."
  [c since]
  (let [secs (/ (- (ctx/now c) since) 1000)]
    (+ secs (risk/risk-s c (risk/stay-estimate c (assoc (risk/stay-inputs c (sh-sense/feet (:primitives c)) 0) :secs secs))))))

(defn spent?
  "Whether the searches begun as memory m's :route-search {:since :fallback-s} have stood the body longer than their fallback is worth."
  [c m]
  (when-let [{:keys [since fallback-s]} (:route-search m)]
    (>= (stood-s c since) fallback-s)))
