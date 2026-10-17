(ns jobs.survival.set-spawn
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.places :as places]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.util :as u]))

(def doc
  "Set the respawn point at a bed without sleeping: by day, click the bed (jobs.blocks.use-on walks to within 2 cells where the eye sees it (the server sets the point only within 3 blocks), through the door of a hut) and the
  server answers \"respawn point set\".
  Needs a bed: the :bed argument, else the remembered :bed within :bed-radius (forgotten when its block is seen to be gone), else a bed the body sees.
  Conditions (at the start only): not night (at night the sleep job sets it), else it waits :night; a bed, else condition-failed :no-bed.
  Done at once (:already) when the :spawn-bed it writes is recorded at the bed. Done {:spawn pos} when the server set the point, or at once with :why :unchanged when the spawn is already
  known at that bed (jobs.lib.shelter.spawn/spawn-known?: :spawn-bed or :spawn-set near it, not reset since). A click that did not set it
  is nothing-possible :not-set {:why reason}. Writes :spawn-bed {:pos bed} (cap 1, forever),
  which jobs.survival.sleep also writes, and :home at the bed unless a live :home is recorded.
  Quote: time of one click and walk; done 1 spawn.")

(a/defargs args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :spec (a/num-in 0 nil) :default sh-beds/default-bed-radius}
   :bed {:doc "bed position [x y z] or {:x :y :z} to use instead of the remembered :bed (no radius)" :spec ::a/pos :default nil}})

(defn remembered-gone?
  "Whether the remembered :bed (no :bed arg) is loaded and no longer a bed."
  [c]
  (let [p (:primitives c)
        pos (when-not (:bed (:args c)) (sh-beds/bed c (:bed-radius (:args c))))]
    (boolean (and pos (places/gone? :bed (u/block-name p pos))))))

(defn bed-of
  "The :bed arg, else the remembered bed unless it is gone, else a bed the body sees."
  [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (or (when-not (remembered-gone? c) (sh-beds/bed c (:bed-radius (:args c))))
        (sh-beds/seen-bed (:primitives c)))))

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds the walk to the bed and the click may take." :spec (a/num-in 0 nil)}
   ::click-s {:default 10 :doc "Seconds the walk to the bed and the click take, for the quote." :spec (a/num-in 0 nil)}})

(defn has-bed [c] (if (bed-of c) (contract/ok :bed "a bed") (contract/later :no-bed "no bed given, remembered or seen")))

(def can-start? (cond/and (checks/day :night) has-bed))

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night bed-of))

(def fit? ready/ok)

(defn done?
  "The spawn is set at the bed and recorded (:spawn-bed, which the round writes with the home offer): a spawn the body only
  saw set stays :later until the round has written it."
  [c]
  (let [bed (bed-of c)
        d (boolean (and bed (not (remembered-gone? c)) (sh-spawn/spawn-bed-at? (ctx/view c) bed)))]
    (checks/goal d [:spawn-set "the spawn is set at the bed"] [:spawn-unset "the spawn is not set at the bed"]
                 {:units :spawns :done (if d 1 0) :left (if d 0 1)})))

(defn quote-of [_c]
  (let [s (settings/get settings ::click-s)]
    {:uses {:job-time [0 s (* 3 s)]} :done [0 1 1]}))

(defn ^:async round [c]
  (when (remembered-gone? c)
    (sh-spawn/lose-spawn! c (sh-beds/bed c (:bed-radius (:args c))) :missing)
    (places/retract-if-missing! c :bed (sh-beds/bed c (:bed-radius (:args c))) "missing"))
  (let [bed (bed-of c)]
    (cond
      (nil? bed) (ctx/end c :condition-failed :no-bed)
      (sh-spawn/spawn-known? (ctx/view c) bed (ctx/now c))
      (do (ctx/remember! c :spawn-bed {:pos bed} sh-spawn/spawn-bed-policy)
          (places/offer! c :home bed)
          (ctx/end c :done nil {:spawn bed :why :unchanged :text "the respawn point is already at this bed"}))
      :else
      (let [e (await (ctx/call c :use 'jobs.blocks.use-on {:pos bed :see true :range 2}))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (ctx/remember! c :spawn-bed {:pos bed} sh-spawn/spawn-bed-policy)
                                  (places/offer! c :home bed)
                                                          (ctx/end c :done nil {:spawn bed}))
          :else (let [why (get-in e [:data :reason] (contract/cause-key e))]
                  (ctx/end c :nothing-possible :not-set
                           {:why why :pos bed :text (str "clicking the bed did not set the respawn point: " (name (or why :unknown)))})))))))
