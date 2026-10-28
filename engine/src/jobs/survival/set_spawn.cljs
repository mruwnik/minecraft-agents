(ns jobs.survival.set-spawn
  (:require [clojure.string :as string]
            [jobs.lib.ready :as ready]
            [jobs.lib.price :as price]
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

(def meta {:tags [:goal]})

(def doc
  "Set the respawn point at a bed without sleeping: by day, click the bed (jobs.items.use walks to within 2 cells where the eye sees it (the server sets the point only within 3 blocks), through the door of a hut) and the
  server answers \"respawn point set\".
  Needs a bed: the :bed argument, else the remembered :bed within the default-bed-radius setting (forgotten when its block is seen to be gone), else the nearest bed the body has seen (memory, still a bed as last seen) within that radius.
  Conditions (at the start only): not night (at night the sleep job sets it), else it waits :night; a bed: with none the start check looks round first, then waits (:later); with none found it ends condition-failed :no-bed.
  Done at once (:already) when the :spawn-bed it writes is recorded at the bed. Done {:spawn pos} when the server set the point, or at once with :why :unchanged when the spawn is already
  known at that bed (jobs.lib.shelter.spawn/spawn-known?: :spawn-bed or :spawn-set near it, not reset since). A click that reached the bed
  and changed nothing means the server already has the spawn there (it answers only when the point moves): done :why :unchanged. Any other click that did not set it
  is nothing-possible :not-set {:why reason}. Writes :spawn-bed {:pos bed} (cap 1, forever),
  which jobs.survival.sleep also writes, and :home at the bed unless a live :home is recorded.
  Quote: time of one click and walk; done 1 spawn.")

(a/defargs args
  {:bed {:doc "bed position [x y z] or {:x :y :z} to use instead of the remembered :bed (no radius)" :spec ::a/pos :default nil}})

(defn remembered-gone?
  "Whether the remembered :bed (no :bed arg) is loaded and no longer a bed."
  [c]
  (let [p (:primitives c)
        pos (when-not (:bed (:args c)) (sh-beds/bed c (sh-beds/default-bed-radius)))]
    (boolean (and pos (places/gone? :bed (u/seen-name p pos))))))

(defn bed-of
  "The :bed arg, else the remembered bed unless it is gone, else the nearest bed the body has seen."
  [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (or (when-not (remembered-gone? c) (sh-beds/bed c (sh-beds/default-bed-radius)))
        (first (sh-beds/seen-bed-places (:primitives c) (sh-beds/default-bed-radius))))))

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds the walk to the bed and the click may take." :spec (a/num-in 0 nil)}
   ::click-s {:default 10 :doc "Seconds the walk to the bed and the click take, for the quote." :spec (a/num-in 0 nil)}})

(defn has-bed [c] (if (bed-of c) (contract/ok :bed "a bed") (contract/look :no-bed "no bed given, remembered or seen")))

(def can-start? (cond/and (checks/day :night) has-bed))

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (price/stays (ready/safe-at-by-night bed-of) (price/trip-s bed-of 3)))

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
    (sh-spawn/lose-spawn! c (sh-beds/bed c (sh-beds/default-bed-radius)) :missing)
    (places/retract-if-missing! c :bed (sh-beds/bed c (sh-beds/default-bed-radius)) "missing"))
  (let [bed (bed-of c)]
    (cond
      (nil? bed) (ctx/end c :condition-failed :no-bed)
      (sh-spawn/spawn-known? (ctx/view c) bed (ctx/now c))
      (do (sh-spawn/remember-spawn-bed! c bed)
          (places/offer! c :home bed)
          (ctx/end c :done nil {:spawn bed :why :unchanged :text "the respawn point is already at this bed"}))
      :else
      (let [e (await (ctx/call c :use 'jobs.items.use {:on bed :see true :range 2}))]
        (cond
          (= :continue e) :continue
          (or (= :done (:kind e)) (= :unchanged (get-in e [:data :reason])))
          (do (sh-spawn/remember-spawn-bed! c bed)
              (places/offer! c :home bed)
              (ctx/end c :done nil (cond-> {:spawn bed :text (str "the respawn point is set at the bed at " (string/join " " (u/cell-vec bed)))}
                                     (not= :done (:kind e)) (assoc :why :unchanged))))
          :else (let [why (get-in e [:data :reason] (contract/cause-key e))]
                  (ctx/end c :nothing-possible :not-set
                           {:why why :pos bed :text (str "clicking the bed did not set the respawn point: " (name (or why :unknown)))})))))))
