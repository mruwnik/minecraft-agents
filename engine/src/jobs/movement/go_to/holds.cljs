(ns jobs.movement.go-to.holds
  "What makes go-to wait, beside its safe? checker: a retreat plug with a hostile near."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [jobs.lib.body :as body]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.threats :as threats]
            [jobs.movement.go-to.tuning :refer [plug-wait-ms plug-wait-min-food plug-wait-min-health]]
            [jobs.movement.go-to.way-steps :as ways]))

(def plug-since
  "Root job id -> when its plug-wait began (in process, like approaches); one clock for all the go-to children of a parent, kept while the hold is due, so a deadline stays passed."
  (atom {}))

(defn in-sight?
  "Whether hostile e is in line of sight now; a mob seen before the plug went in and since heard or remembered is not."
  [e]
  (true? (.-visible e)))

(defn plug-hostiles
  "The known hostiles that hold a body shut in with a retreat plug of its ledger standing, neither starving nor hurt:
  one out of sight now not counting beyond the near band or while heard mobs are muted (waiting costs food and health,
  and underground a hostile may stay for ever). Empty when the body is not so held."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (when (and (some #(= :retreat-plug (:purpose %)) (ledger/open-entries (ctx/view c)))
               (> (or (.-food self) 20) (plug-wait-min-food))
               (> (or (.-health self) 20) (plug-wait-min-health))
               (escape/enclosed? p (some-> (:doors (:args c)) keyword)))
      (cond->> (danger-q/known-hostiles p threats/max-follow-range {:ranged-radius (danger-q/ranged-radius)})
        (body/heard-muted? (ctx/view c)) (filter in-sight?)
        :always (remove #(and (not (in-sight? %)) (> (danger-q/mob-distance p %) obs/near-band)))))))

(defn hostile-facts
  "{:mob :band :sensed} of the nearest of hostiles: its kind, :near or :far, and :seen or :heard."
  [p hostiles]
  (let [e (apply min-key #(danger-q/mob-distance p %) hostiles)]
    {:mob (.-name e)
     :band (if (<= (danger-q/mob-distance p e) obs/near-band) :near :far)
     :sensed (if (danger-q/seen-only? e) :seen :heard)}))

(defn plug-hold
  "Later :sealed-hostile-near (data: :mob :band :sensed) while plug-hostiles hold the body and this walk may not dig out
  (no escalating way), until :plug-wait-ms have passed since it began. Ready/threatened does not cover it: a sealed body
  has no way a mob can walk."
  [c]
  (if-let [hostiles (when-not (ways/escalating? c) (seq (plug-hostiles c)))]
    (let [since (get (swap! plug-since update (:root c) #(or % (ctx/now c))) (:root c))
          {:keys [mob band sensed]} (hostile-facts (:primitives c) hostiles)]
      (if (< (- (ctx/now c) since) (plug-wait-ms))
        (contract/later :sealed-hostile-near (str "sealed in: waiting for a " (name sensed) " " mob " (" (name band) ") to go")
                        :mob mob :band band :sensed sensed)
        (contract/ok :plug-wait-spent "the plug wait is spent")))
    (do (swap! plug-since dissoc (:root c))
        (contract/ok :no-plug-hostile "no hostile holds the plug"))))
