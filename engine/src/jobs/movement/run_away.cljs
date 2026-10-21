(ns jobs.movement.run-away
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [jobs.movement.chase :as chase]))

(def doc
  "One flee step: a jobs.movement.chase leg of at most :leg-s seconds toward :pos, the cell a flight picked away from its
  danger. A flee never digs or pillars (ways #{:walk :clear-seal}), keeps the walk's look-round off, may take the body down
  to 1 health (the danger is the cost that counts), plans no route deeper or higher than :y-margin blocks past body and
  target (nil: the walk setting), and prices known dangers as any walk.
  Ends as chase does: done {:arrived true}, limit-reached {:reason :leg} with {:arrived false :leg true :nearer bool}, or
  go-to's own ending. The caller (retreat) picks the cell, weighs the danger between steps and calls again. A reflex step:
  it never waits on a check and never returns :continue except while go-to waits on the world.")

(a/defargs args
  {:pos {:doc "the cell to run to" :spec ::a/pos :default nil}
   :range {:doc "how close counts as there, in blocks" :spec (a/num-in 0 nil) :default 1}
   :leg-s {:doc "seconds the step may take" :spec (a/num-in 0.1 120) :default nil}
   :y-margin {:doc "blocks past body and target, up and down, the route may plan; nil: the walk setting" :spec (a/num-in 0 nil) :default nil}
   :ignore-zones? :shared})

(def target [:pos])

(def meta {:category :reflex})

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time 130}})

(def safe? (fn [_] (contract/ok :holds "a flee step is never held")))
(def fit? safe?)

(def can-start?
  (checks/args-fit "needs a :pos and a :leg-s" (fn [c] (let [{:keys [pos leg-s]} (:args c)] (and pos (number? leg-s))))))

(defn done?
  [c]
  (let [{:keys [pos range]} (:args c)]
    (checks/goal (u/within? (u/self-pos c) pos range) [:there "already at the cell"] [:running "not at the cell yet"] {:result {:arrived true}})))

(defn quote-of [c]
  (let [{:keys [leg-s pos]} (:args c)]
    {:uses {:job-time [0 leg-s leg-s]} :moves-to [(:x pos) (:y pos) (:z pos)]}))

(defn ^:async round
  [c]
  (let [{:keys [pos range leg-s y-margin]} (:args c)
        e (await (child/call-chase* c :run
                                    {:pos pos :range range :leg-s leg-s
                                     :walk {:ways #{:walk :clear-seal}
                                            :settings (cond-> {:jobs.movement.go-to.tuning/look-round false}
                                                        y-margin (assoc :jobs.lib.walk.world/y-margin y-margin))
                                            :floor {:health 1}}}))]
    (if (= :continue e) :continue (chase/forward c e))))
