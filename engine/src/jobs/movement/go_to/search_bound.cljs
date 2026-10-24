(ns jobs.movement.go-to.search-bound
  "How long go-to's escalation stands still while its dig-route searches go on over rounds: till standing has cost what
  the way it would take without them costs (the most a search can save), so a search with no route ends."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.escalate-choose :as choose]))

(defn fallback-s
  "Seconds the way escape/choose finds from feet with no dig route takes (choose/price, an approach its walk); with none,
  the call's budget (choose/call-budget-s)."
  [c feet goal may-dig? opts]
  (let [e (escape/choose (:primitives c) feet goal may-dig? (update opts :skip (fnil conj #{}) :dig-route))]
    (case (:step e)
      :none (choose/call-budget-s c)
      :approach (:cost (cost/walk-cost {:distance (u/dist (zipmap [:x :y :z] feet) (zipmap [:x :y :z] (:pos e)))}))
      (choose/price (:primitives c) feet e))))

(defn stood-s
  "Seconds standing still since `since` (ctx/now ms) has cost the body: the time and what staying at its cell risks over
  it (walk-risk/stay-estimate) at its hp-price."
  [c since]
  (let [p (:primitives c)
        secs (/ (- (ctx/now c) since) 1000)]
    (+ secs (* (cost/hp-price (.-health (.self p)))
               (:hp (risk/stay-estimate c (assoc (risk/stay-inputs c (sh-sense/feet p) 0) :secs secs)))))))

(defn spent?
  "Whether the searches begun as memory m's :route-search {:since :fallback-s} have stood the body longer than their fallback is worth."
  [c m]
  (when-let [{:keys [since fallback-s]} (:route-search m)]
    (>= (stood-s c since) fallback-s)))
