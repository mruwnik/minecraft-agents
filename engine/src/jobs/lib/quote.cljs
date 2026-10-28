(ns jobs.lib.quote
  "A job's quote with its risk (docs/job-risk-design.md): :uses :health the expected hurt in hp, :death {:chance :death}
  the chance the run kills the body and what that costs. standard: the quote of a job that walks to its place (f c) and
  works there; then: children's quotes folded; risk-s: a quote's risk in seconds at the body's health, the one place a
  quote becomes seconds; keep! and kept: a quote kept for a sync check of the same call."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def none {:uses {:job-time [0 0 0] :health [0 0 0]}})

(defn add-figs [a b] (mapv + (or a [0 0 0]) (or b [0 0 0])))

(defn add-uses [a b] (if (map? b) (merge-with add-figs a b) (add-figs a b)))

(defn then
  "The quote of running the jobs quoted qs one after another: time, hurt and food add, items and wear add per name, the
  deaths combine by cost/either (not a sum: a body dies once), :estimate when any is one."
  [& qs]
  (let [deaths (keep :death qs)]
    (cond-> {:uses (apply merge-with add-uses (:uses none) (map :uses qs))}
      (seq deaths) (assoc :death (apply cost/either deaths))
      (some :estimate qs) (assoc :estimate true))))

(defn work
  "The quote of a job's own work of category (:click, :work, :long): its stay (risk/work-s) and the hurt it costs (risk/work-hp)."
  [category]
  (let [s (risk/work-s category)
        hp (risk/work-hp category)]
    {:uses {:job-time [0 s s] :health [0 hp hp]}}))

(defn walk-args
  "The go-to args of the walk to pos and back with category's stay there priced at the goal."
  [pos category]
  {:pos pos :back true :stay-s (risk/work-s category)})

(defn standard
  "The quote-of of a job that walks to its place (f c) ({:x :y :z}, nil: works where it stands) and works there as category:
  go-to's quote of the walk there and back with the stay priced at the goal, then the work."
  [{:keys [place category]}]
  (fn ^:async standard-quote [c]
    (let [pos (place c)]
      (if pos
        (then (await (ctx/quote c 'jobs.movement.go-to (walk-args pos category))) (work category))
        (work category)))))

(defn hp-s
  "Seconds quote q's risk costs a body at health (cost/risk-s): its mean hurt and its death in hp."
  [health q]
  (cost/risk-s health (second (get-in q [:uses :health] [0 0 0])) (if-let [d (:death q)] (cost/lethal-hp d) 0)))

(defn risk-s
  "Seconds quote q's risk costs c's body at its health now."
  [c q]
  (hp-s (.-health (.self (:primitives c))) q))

(def kept-kind ::kept)

(defn kept-key
  "What a quote of call (job args) is valid for: the body's cell, its memory epoch and day or night (the price reads both)."
  [c job args]
  {:job job :args args :from (mapv js/Math.floor ((juxt :x :y :z) (u/self-pos c)))
   :epoch (sense/known-memory (:primitives c)) :night (boolean (sh-sense/night? (:primitives c)))})

(defn keep!
  "Keep quote q of call (job args) for kept."
  [c job args q]
  (ctx/remember! c kept-kind (assoc (kept-key c job args) :quote q) {:cap 16 :ttl :forever}))

(defn kept
  "The quote keep! kept of call (job args) under the same kept-key, else nil (asked again once the body moves, its
  memory or the light changes)."
  [c job args]
  (let [k (kept-key c job args)]
    (some #(when (= k (dissoc (:data %) :quote)) (:quote (:data %))) (rseq (ctx/entries c kept-kind)))))
