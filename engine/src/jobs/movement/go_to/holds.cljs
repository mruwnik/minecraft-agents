(ns jobs.movement.go-to.holds
  "What makes a listed go-to wait instead of walking: the night in a shelter, and a retreat plug with a hostile near."
  (:require [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [jobs.lib.body :as body]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.shelter :as sh]
            [jobs.lib.threats :as threats]
            [jobs.movement.go-to.aim :refer [target]]
            [jobs.movement.go-to.tuning :refer [at-night plug-wait-ms plug-wait-min-food plug-wait-min-health]]
            [jobs.movement.go-to.way-steps :as ways]))

(defn exclusive-run?
  "Whether this call runs for the exclusive run's job: the agent's own order."
  [c]
  (let [slot (some-> (:exclusive (:engine c)) deref)]
    (and (some? slot) (= slot (:root c)))))

(defn night-wait
  "The wait {:reason :night-sheltered :shelter :goal} while the walk would take the body out of its shelter at night
  (sh/walk-leaves-shelter?), else nil. Never under a reflex job, the manual driver's own go-to or with :at-night :go
  (the call's, else the setting's)."
  [c]
  (let [p (:primitives c)]
    (when (and (not (:reflex c)) (not (exclusive-run? c)) (not= :go (keyword (or (:at-night (:args c)) (at-night)))) (sh/night? p))
      (let [{goal :pos :as t} (target c)
            kind (when (and goal (not (:reason t)))
                   (sh/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) goal (:range (:args c) 1)))]
        (when kind {:reason :night-sheltered :shelter kind :goal goal})))))

(def plug-since
  "Root job id -> when its plug-wait began (in process, like approaches); one clock for all the go-to children of a parent, kept while the hold is due, so a deadline stays passed."
  (atom {}))

(defn plug-hostiles
  "The known hostiles that hold a body shut in with a retreat plug of its ledger standing, neither starving nor hurt:
  a heard-only one not counting beyond the near band or while heard mobs are muted (waiting costs food and health, and
  underground a hostile may stay for ever). Empty when the body is not so held."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (when (and (some #(= :retreat-plug (:purpose %)) (ledger/open-entries (ctx/view c)))
               (> (or (.-food self) 20) (plug-wait-min-food))
               (> (or (.-health self) 20) (plug-wait-min-health))
               (escape/enclosed? p (some-> (:doors (:args c)) keyword)))
      (cond->> (danger-q/known-hostiles p threats/max-follow-range {:ranged-radius (danger-q/ranged-radius)})
        (body/heard-muted? (ctx/view c)) (filter danger-q/seen-only?)
        :always (remove #(and (not (danger-q/seen-only? %)) (> (danger-q/mob-distance p %) obs/near-band)))))))

(defn hostile-facts
  "{:mob :band :sensed} of the nearest of hostiles: its kind, :near or :far, and :seen or :heard."
  [p hostiles]
  (let [e (apply min-key #(danger-q/mob-distance p %) hostiles)]
    {:mob (.-name e)
     :band (if (<= (danger-q/mob-distance p e) obs/near-band) :near :far)
     :sensed (if (danger-q/seen-only? e) :seen :heard)}))

(defn plug-wait
  "The wait {:reason :sealed-hostile-near} while plug-hostiles hold it and this walk may not dig out (no escalating way), until
  :plug-wait-ms have passed since it began. Never under a reflex job."
  [c]
  (when (and (not (:reflex c)) (not (ways/escalating? c)))
    (if-let [hostiles (seq (plug-hostiles c))]
      (let [since (get (swap! plug-since update (:root c) #(or % (ctx/now c))) (:root c))]
        (when (< (- (ctx/now c) since) (plug-wait-ms))
          (assoc (hostile-facts (:primitives c) hostiles) :reason :sealed-hostile-near)))
      (do (swap! plug-since dissoc (:root c)) nil))))

(defn wait-reason [c] (or (night-wait c) (plug-wait c)))
