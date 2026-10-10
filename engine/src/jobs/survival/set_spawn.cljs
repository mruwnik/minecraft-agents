(ns jobs.survival.set-spawn
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def doc
  "Set the respawn point at a bed without sleeping: by day, click the bed (jobs.blocks.use-on walks to where the eye sees it, through the door of a hut) and the
  server answers \"respawn point set\".
  Needs a bed: the :bed argument, else the remembered :bed within :bed-radius (forgotten with a bed_missing warn when its block is seen to be gone), else a bed the body sees. Waits (:no-bed, :night); at night the
  sleep job sets it. Done at once, with a spawn_unchanged event and :why :unchanged, when the spawn is already known at that bed (jobs.lib.shelter/spawn-known?: :spawn-bed or :spawn-set near it, not reset since).
  Returns {:status :completed :spawn pos} when the server set the point (or it was already known); {:status :stopped
  :reason ...} when the click did not set it (:no-bed, :night, :not-set). Writes :spawn-bed {:pos bed} (cap 1, forever),
  which jobs.survival.sleep also writes, and :home at the bed unless a live :home is recorded; warns spawn_not_set when the click did not set it.")

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

(defn check [c]
  (cond
    (sh/night? (:primitives c)) (ctx/wait c {:reason :night :text "at night the sleep job sets the spawn"})
    (nil? (bed-of c)) (ctx/wait c {:reason :no-bed :text "no bed given or remembered within the bed radius"})
    :else true))

(defn ^:async round [c]
  (when (remembered-gone? c)
    (places/retract-if-missing! c :bed (sh/bed c (:bed-radius (:args c))) "missing"))
  (let [bed (bed-of c)]
    (cond
      (nil? bed) (do (ctx/result! c {:status :stopped :reason :no-bed}) :done)
      (sh/spawn-known? (ctx/view c) bed (ctx/now c)) (do (ctx/remember! c :spawn-bed {:pos bed} sh/spawn-bed-policy)
                                                          (places/offer! c :home bed)
                                                          (ctx/emit! c :spawn_unchanged :info {:pos bed :text "the respawn point is already at this bed"})
                                                          (ctx/result! c {:status :completed :spawn bed :why :unchanged}) :done)
      :else
      (let [w (await (ctx/call-child c :use 'jobs.blocks.use-on {:pos bed :see true}))
            r (ctx/child-result c :use)]
        (cond
          (= :continue w) :continue
          (:used r) (do (ctx/remember! c :spawn-bed {:pos bed} sh/spawn-bed-policy)
                        (places/offer! c :home bed)
                        (ctx/result! c {:status :completed :spawn bed})
                        :done)
          :else (do (ctx/emit! c :spawn_not_set :warn {:pos bed :text (str "clicking the bed did not set the respawn point: " (name (or (:reason r) :unknown)))})
                    (ctx/result! c {:status :stopped :reason :not-set :why (:reason r)})
                    :done))))))
