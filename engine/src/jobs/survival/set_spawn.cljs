(ns jobs.survival.set-spawn
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]
            [jobs.lib.shelter :as sh]
            [jobs.survival.sleep :as sleep]))

(def doc
  "Set the respawn point at a bed without sleeping: by day, click the bed (jobs.blocks.use-on walks into reach) and the
  server answers \"respawn point set\".
  Needs a bed: the :bed argument, else the remembered :bed within :bed-radius. Waits (:no-bed, :night); at night the
  sleep job sets it. Done at once when the spawn is already known at that bed (:spawn-bed, not reset since).
  Returns {:status :completed :spawn pos} when the server set the point (or it was already known); {:status :stopped
  :reason ...} when the click did not set it (:no-bed, :night, :not-set). Writes :spawn-bed {:pos bed} (cap 1, forever),
  which jobs.survival.sleep also writes; warns spawn_not_set when the click did not set it.")

(a/defargs args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :spec (a/num-in 0 nil) :default sh/default-bed-radius}
   :bed {:doc "bed position [x y z] or {:x :y :z} to use instead of the remembered :bed (no radius)" :spec ::a/pos :default nil}})

(defn bed-of [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (sh/bed c (:bed-radius (:args c)))))

(defn check [c]
  (cond
    (sh/night? (:primitives c)) (ctx/wait c {:reason :night :text "at night the sleep job sets the spawn"})
    (nil? (bed-of c)) (ctx/wait c {:reason :no-bed :text "no bed given or remembered within the bed radius"})
    :else true))

(defn known?
  "Whether the live :spawn-bed entry is at bed."
  [c bed]
  (some-> (sleep/spawn-bed-live c) :data :pos (= bed)))

(defn ^:async round [c]
  (let [bed (bed-of c)]
    (cond
      (nil? bed) (do (ctx/result! c {:status :stopped :reason :no-bed}) :done)
      (known? c bed) (do (ctx/result! c {:status :completed :spawn bed}) :done)
      :else
      (let [w (await (ctx/call-child c :use 'jobs.blocks.use-on {:pos bed}))
            r (ctx/child-result c :use)]
        (cond
          (= :continue w) :continue
          (:used r) (do (ctx/remember! c :spawn-bed {:pos bed} sleep/spawn-bed-policy)
                        (ctx/result! c {:status :completed :spawn bed})
                        :done)
          :else (do (ctx/emit! c :spawn_not_set :warn {:pos bed :text (str "clicking the bed did not set the respawn point: " (name (or (:reason r) :unknown)))})
                    (ctx/result! c {:status :stopped :reason :not-set :why (:reason r)})
                    :done))))))
