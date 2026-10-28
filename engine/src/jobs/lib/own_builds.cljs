(ns jobs.lib.own-builds
  "What the body counts as its own: cells covered by its plans, the wall of the own pen it stands in, blocks go-to put
  back or its scaffold ledger holds. Read by go-to's escalations, reach and blocks.dig."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.own-pen :as own-pen]
            [jobs.lib.reach :as reach]
            [jobs.lib.world :as known]))

(defn feet-cell [c]
  (let [{:keys [x y z]} (reach/standing-cell (:primitives c))]
    [x y z]))

(defn own-plan-fn
  "(own-plan cell): the id of the plan this body made that covers cell, else nil."
  [c]
  (let [footprints (known/footprints c)
        own (access/own-plans c)]
    (fn [cell] (some own [(get footprints (vec cell))]))))

(defn with-own-pen
  "c with the own pen holding livestock the body stands in (jobs.lib.own-pen/shut-in, read on first use) as :own-pen: the
  fns below built from it share one read."
  [c]
  (assoc c :own-pen (own-pen/shut-in c (feet-cell c))))

(defn pen-of [c] (or (:own-pen c) (own-pen/shut-in c (feet-cell c))))

(defn own-build-fn
  "(own-build cell): :plan when a plan this body made covers cell, :pen when it is the wall of the own pen holding
  livestock the body stands in (pen-of), else nil."
  [c]
  (let [own-plan (own-plan-fn c)
        pen-wall? (:wall? (pen-of c))]
    (fn [cell]
      (cond (own-plan cell) :plan
            (pen-wall? cell) :pen))))

(defn own-fn
  "(own? cell block) of a way that may cut the body's own builds: own-placed's, or a cell of its own builds (own-build-fn)."
  [c own-placed]
  (let [own-build (own-build-fn c)]
    (fn [cell n] (or (own-placed cell n) (some? (own-build cell))))))

(def placed-policy
  "Body memory policy of :escalation-placed, the cells go-to put a block back in: later escalations may dig them."
  {:cap 100 :ttl (* 6 60 60 1000)})

(defn own-placed-fn
  "(own? cell block): go-to itself placed block at cell (a put-back), or the body's scaffold ledger holds it there
  (a pillar, a plug: its own temporary block; an :intent entry counts when the cell holds the intended item, as
  jobs.lib.ledger/decide reads it), so it is not someone's build."
  [c]
  (let [put-back (set (map (comp (juxt :cell :block) :data) (ctx/entries c :escalation-placed)))
        entries (into {} (map (juxt :cell identity)) (ledger/open-entries (ctx/view c)))]
    (fn [cell block]
      (or (contains? put-back [(vec cell) block])
          (when-let [e (get entries (vec cell))]
            (case (:state e)
              :intent (= :placed (ledger/decide e (constantly block)))
              (and (#{:placed :removing} (:state e)) (= block (:item e)))))))))

(defn own-dig-fn
  "(own? cell block) of a call that digs: own-placed-fn's, the body's own builds too when it may cut them (own-cut?)."
  [c own-cut?]
  (cond->> (own-placed-fn c) own-cut? (own-fn c)))

(defn not-diggable
  "The block at cell when an escalation's stair or clear-path child may not cut it (escape/diggable?: natural terrain, or
  the body's own blocks, its own builds too when the call does not spare them), else nil. For a cell first seen while digging."
  [c block-at cell]
  (let [own? (own-dig-fn c (not (access/spare-own-builds)))]
    (when-not (escape/diggable? block-at (constantly true) own? cell)
      (block-at cell))))
