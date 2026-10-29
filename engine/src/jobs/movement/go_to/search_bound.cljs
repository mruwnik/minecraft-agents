(ns jobs.movement.go-to.search-bound
  "How long go-to's escalation stands still while its dig-route searches go on over rounds: till standing has cost what
  the way it would take without them costs (the most a search can save), so a search with no route ends."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.escape :as escape]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk.one-way :as one-way]
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.escalate-choose :as choose]))

(defn fallback-s
  "Seconds the way escape/choose finds from feet with no dig route costs to the goal: its step (choose/price, an
  approach its walk), the blocks on from where the step leaves the body as a quote prices them (cost-planner/rest-s), and
  the walk's risk to the goal (walk-risk/estimate); with none, or an unpassable one, what the goal is worth (one-way/worth: the caller's
  :worth, else a tour's), all a search can save."
  [c feet goal may-dig? opts]
  (let [e (escape/choose (:primitives c) feet goal may-dig? (update opts :skip (fnil conj #{}) :dig-route))
        cell #(zipmap [:x :y :z] %)
        end (or (:pos e) feet)]
    (let [worth #(one-way/worth (u/dist (cell feet) (cell goal)) (:worth (:args c)))
          total (when-not (= :none (:step e))
                  (+ (if (= :approach (:step e))
                       (:cost (cost/walk-cost {:distance (u/dist (cell feet) (cell end))}))
                       (choose/price (:primitives c) feet e))
                     (cost-planner/rest-s (js/Math.round (js/Math.hypot (- (goal 0) (end 0)) (- (goal 1) (end 1)) (- (goal 2) (end 2)))))
                     (risk/risk-s c (risk/estimate c (cell goal)))))]
      (if (and total (js/isFinite total)) total (worth)))))

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
