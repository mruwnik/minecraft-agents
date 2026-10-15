(ns jobs.survival.set-spawn
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.places :as places]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def doc
  "Set the respawn point at a bed without sleeping: by day, click the bed (jobs.blocks.use-on walks to where the eye sees it, through the door of a hut) and the
  server answers \"respawn point set\".
  Needs a bed: the :bed argument, else the remembered :bed within :bed-radius (forgotten with a bed_missing warn when its block is seen to be gone), else a bed the body sees.
  Conditions (at the start only): not night (at night the sleep job sets it), else condition-failed :night; a bed, else :no-bed.
  Done {:spawn pos} when the server set the point, or at once with a spawn_unchanged event and :why :unchanged when the spawn is already
  known at that bed (jobs.lib.shelter/spawn-known?: :spawn-bed or :spawn-set near it, not reset since). A click that did not set it
  is nothing-possible :not-set {:why reason}, with a spawn_not_set warn. Writes :spawn-bed {:pos bed} (cap 1, forever),
  which jobs.survival.sleep also writes, and :home at the bed unless a live :home is recorded.
  Quote: time of one click and walk; done 1 spawn.")

(a/defargs args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :spec (a/num-in 0 nil) :default sh/default-bed-radius}
   :bed {:doc "bed position [x y z] or {:x :y :z} to use instead of the remembered :bed (no radius)" :spec ::a/pos :default nil}})

(defn remembered-gone?
  "Whether the remembered :bed (no :bed arg) is loaded and no longer a bed."
  [c]
  (let [p (:primitives c)
        pos (when-not (:bed (:args c)) (sh/bed c (:bed-radius (:args c))))]
    (boolean (and pos (places/gone? :bed (u/block-name p pos))))))

(defn bed-of
  "The :bed arg, else the remembered bed unless it is gone, else a bed the body sees."
  [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (or (when-not (remembered-gone? c) (sh/bed c (:bed-radius (:args c))))
        (sh/seen-bed (:primitives c)))))

(a/defargs settings
  {::time-s {:default 60 :doc "Seconds the walk to the bed and the click may take." :spec (a/num-in 0 nil)}
   ::click-s {:default 10 :doc "Seconds the walk to the bed and the click take, for the quote." :spec (a/num-in 0 nil)}})

(defn start-fact
  "The fact that bars starting, or nil: :night, or :no-bed."
  [c]
  (when-not (ctx/frozen-call c)
    (cond
      (sh/night? (:primitives c)) :night
      (nil? (bed-of c)) :no-bed)))

(def conditions [[:fn start-fact]])

(def target [])

(def defaults {:spend {:job-time ::time-s}})

(defn spawn-here? [c]
  (let [bed (bed-of c)]
    (boolean (and bed (not (remembered-gone? c)) (sh/spawn-known? (ctx/view c) bed (ctx/now c))))))

(defn done?
  "The spawn is already set at the bed and the call recorded it (a call that ended reports its own end)."
  [c]
  (let [d (and (:ended (ctx/mem c)) (spawn-here? c))]
    {:done? (boolean d) :units :spawns :done (if d 1 0) :left (if d 0 1)}))

(defn quote-of [_c]
  (let [s (settings/get settings ::click-s)]
    {:uses {:job-time [0 s (* 3 s)]} :done [0 1 1]}))

(defn ^:async round [c]
  (when (remembered-gone? c)
    (sh/lose-spawn! c (sh/bed c (:bed-radius (:args c))) :missing)
    (places/retract-if-missing! c :bed (sh/bed c (:bed-radius (:args c))) "missing"))
  (let [bed (bed-of c)]
    (cond
      (nil? bed) (ctx/end c :condition-failed :no-bed)
      (sh/spawn-known? (ctx/view c) bed (ctx/now c))
      (do (ctx/remember! c :spawn-bed {:pos bed} sh/spawn-bed-policy)
          (places/offer! c :home bed)
          (ctx/emit! c :spawn_unchanged :info {:pos bed :text "the respawn point is already at this bed"})
          (ctx/update-mem! c assoc :ended true)
          (ctx/end c :done nil {:spawn bed :why :unchanged}))
      :else
      (let [e (await (ctx/call c :use 'jobs.blocks.use-on {:pos bed :see true}))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (ctx/remember! c :spawn-bed {:pos bed} sh/spawn-bed-policy)
                                  (places/offer! c :home bed)
                                  (ctx/update-mem! c assoc :ended true)
                                  (ctx/end c :done nil {:spawn bed}))
          :else (let [why (get-in e [:data :reason] (contract/cause-key e))]
                  (ctx/emit! c :spawn_not_set :warn {:pos bed :text (str "clicking the bed did not set the respawn point: " (name (or why :unknown)))})
                  (ctx/end c :nothing-possible :not-set {:why why :pos bed})))))))
