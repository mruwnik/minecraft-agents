(ns jobs.survival.plug-pit
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.shelter :as sh]
            [jobs.survival.plug-way :as way]
            [jobs.survival.retreat-pit :as pit]))

(def doc
  "A plug way: dig down 2 (3 on flat ground) under a solid side and plug the hole over the head with carried or dug blocks.
  Needs a pit that fits (every cell solid rock, no fluid or hazard in or beside it, none at the roof, solid under the bottom) and a
  block to plug with, carried or dug (a block the tools cannot harvest is dug by hand once one is in hand and the hand breaks it within :hand-clear-ms).
  One call is a whole attempt: :done {:pos :way :pit :roof :start} once enclosed, at once when it already is; nothing-possible :failed
  {:reason :text :needs} when a pit fails, the reason a stop of jobs.survival.retreat-pit: :no-floor, :fluid-here, :fluid-adjacent,
  :fluid-above, :hazard-below, :no-tool (:needs the block), :no-blocks, :dig-failed, :descent-stalled, :roof-failed, :lays-open (a dig
  laid a cave or fluid open). The pit is never dug into what the body sees as fluid, a hazard or open air, and it is planned again
  when the body has left its column. Lava a dig lays open is filled first (:on-lava :seal). Every plug goes to the scaffold ledger
  with :purpose (default :retreat-plug, taken back by jobs.access.cleanup once no hostile is near; :shelter is left alone). Quote:
  time (##Inf when no pit fits). Picked by jobs.survival.plug-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:purpose {:doc "ledger purpose of the placed blocks (:retreat-plug: taken back by the cleanup near no hostile; :shelter: left alone)" :spec #{:retreat-plug :shelter} :default :retreat-plug}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks stand in the way of a cell" :spec (a/num-in 0 nil) :default 8}
   :hand-clear-ms {:doc "a block the carried tools cannot harvest is dug by hand, once a block to roof with is in hand, when the hand breaks it within this many ms; default jobs.lib.tools quick-clear-ms" :spec (a/int-in 0 nil)}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pit is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::job-time-s {:default 90 :doc "Seconds a pit attempt may take." :spec (a/num-in 0 nil)}
   ::pit-s {:default 12 :doc "Seconds a pit is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :pit))

(defn quote-of [c]
  (way/quote-of c (if (pit/pit-plan c (sh/feet (:primitives c))) (settings/get settings ::pit-s) ##Inf)
                (settings/get settings ::job-time-s)))

(defn ^:async step!
  "One step of the pit: its plan is made once and kept in job memory, made again when the body has left its column. :done,
  :again, or nil with no pit that fits or one that failed (the stop in memory)."
  [c]
  (let [kept (:pit (ctx/mem c))
        plan (if (and kept (pit/pit-index kept (sh/feet (:primitives c)))) kept (await (pit/plan! c)))]
    (if (:stop plan)
      (way/stop! c plan)
      (do (ctx/update-mem! c assoc :pit plan)
          (let [r (await (pit/pit-round! c plan))]
            (if (:stop r)
              (way/stop! c r)
              (case r :hidden :done :again :again nil)))))))

(defn ^:async round [c] (await (way/run! c :pit step!)))
