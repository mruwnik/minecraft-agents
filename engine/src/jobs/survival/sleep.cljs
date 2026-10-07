(ns jobs.survival.sleep
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]))

(def doc
  "Walk to a bed and sleep in it.
  Waits unless it is night and a bed is known (reasons :not-night, :no-bed, :bed-unreachable, :bed-occupied): the :bed argument, else the remembered :bed within :bed-radius.
  Also declines while the bed is in :bed-unreachable (an unexpired entry with the same position), and for a bed
  another player occupies (sh/bed-permit).
  Ends when asleep, or when it turns day.
  A :bed argument that is not a position declines too.
  Retries three times, then warns and ends: a taken bed, a monster nearby, an unreachable bed.
  The failure kinds are bed_unreachable, bed_unloaded, bed_missing and bed_unusable (\"cannot sleep: <status>\").
  A bed whose chunk is not loaded is retried the same way.
  A bed that is not there is not retried: the job ends and the shelter chooser falls through.
  If it was the remembered bed, :bed is overwritten with {:gone true :was pos}, so it no longer reads as a place.
  With a :bed argument the radius is ignored. After sleeping there it is recorded as :bed
  when none is recorded or the recorded one is gone. A live recorded :bed is kept (one place.kept event).
  Warns spawn_not_set when it sleeps and no :spawn-set (the body event for the Respawn point set line) follows, unless one
  was seen within 3 blocks of the bed before (kept as :spawn-bed, forever, until a later :spawn-reset):
  death would then send the body to the world spawn.
  Memory: reads :bed, :bed-unreachable, :spawn-set and :spawn-reset. Writes :slept (cap 10, seven in-game days),
  :spawn-bed {:pos bed} (cap 1, forever),
  :bed (see above) and :bed-unreachable {:pos bed} after an unreachable bed (cap 5, ten minutes).")

(a/defargs args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :spec (a/num-in 0 nil) :default sh/default-bed-radius}
   :bed {:doc "bed position [x y z] or {:x :y :z} to sleep in instead of the remembered :bed (no radius); it is recorded as :bed when that is unset or gone" :spec ::a/pos
         :default nil}})

(def slept-policy {:cap 10 :ttl (* 7 sh/ms-per-day)})

(def unreachable-policy {:cap 5 :ttl 600000})

(defn unreachable-bed?
  "Whether an unexpired :bed-unreachable entry names bed."
  [c bed]
  (boolean (some #(= bed (:pos (:data %))) (ctx/entries c :bed-unreachable))))

(defn bed-of
  "The bed to sleep in: the :bed argument (nil when it is not a position), else the remembered :bed within
  :bed-radius."
  [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (sh/bed c (:bed-radius (:args c)))))

(defn check
  "Night and a bed that is not given up on and not occupied; else false, with the reason the job waits."
  [c]
  (let [bed (bed-of c)
        p (:primitives c)]
    (cond
      (not (sh/night? p)) (ctx/wait c :not-night)
      (nil? bed) (ctx/wait c {:reason :no-bed :text "no bed given or remembered within the bed radius"})
      (unreachable-bed? c bed) (ctx/wait c {:reason :bed-unreachable :pos bed})
      (not ((sh/bed-permit p (:world (:engine c))) bed)) (ctx/wait c {:reason :bed-occupied :pos bed})
      :else true)))

(defn give-up-unreachable! [c bed]
  (let [r (u/fail! c :bed_unreachable "cannot reach the bed")]
    (when (= :done r)
      (ctx/remember! c :bed-unreachable {:pos bed} unreachable-policy))
    r))

(def spawn-near
  "A :spawn-set entry within this many blocks of a bed is that bed's respawn point."
  3)

(def spawn-bed-policy {:cap 1 :ttl :forever})

(defn spawn-bed-live
  "The durable :spawn-bed entry unless a :spawn-reset came after it."
  [c]
  (let [e (last (ctx/entries c :spawn-bed))
        reset (last (ctx/entries c :spawn-reset))]
    (when-not (and e reset (> (:t reset) (:t e)))
      e)))

(defn spawn-known?
  "Whether the body has seen the respawn point set at bed since t0, or earlier (the server says it only when the
  point changes, so a bed slept in before is silent; :spawn-bed outlives the :spawn-set entry)."
  [c bed t0]
  (let [near? #(some-> (:pos (:data %)) (u/dist bed) (<= spawn-near))
        known? (boolean (or (some #(or (>= (:t %) t0) (near? %))
                                  (ctx/entries c :spawn-set))
                            (some-> (spawn-bed-live c) near?)))]
    (when known?
      (ctx/remember! c :spawn-bed {:pos bed} spawn-bed-policy))
    known?))

(defn ^:async sleep-at! [c bed]
  (let [t0 (ctx/now c)
        r (await (ctx/act c :sleep (clj->js {:pos bed})))]
    (case (.-status r)
      "sleeping" (do (ctx/remember! c :slept {:pos bed} slept-policy)
                     (when-not (spawn-known? c bed t0)
                       (ctx/emit! c :spawn_not_set :warn {:pos bed :text "slept in the bed but the server did not say the respawn point was set"}))
                     (when (:bed (:args c)) (places/offer! c :bed bed))
                     :done)
      "not-night" :done
      "missing" (if (nil? (u/sensed (:primitives c) bed))
                  (u/fail! c :bed_unloaded "bed chunk not loaded yet")
                  (do (if (= bed (mem/place (ctx/view c) :bed))
                        (do (ctx/remember! c :bed {:gone true :was bed} mem/place-policy)
                            (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the remembered place"}))
                        (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the given place"}))
                      :done))
      (u/fail! c :bed_unusable (str "cannot sleep: " (.-status r))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (bed-of c)]
    (if (or (nil? bed) (sh/sleeping? p) (not (sh/night? p)))
      :done
      (let [walk (await (ctx/call-child c :go 'jobs.movement.go-to {:pos bed :range 2}))]
        (cond
          (= :continue walk) :continue
          (not (:arrived (ctx/child-result c :go))) (give-up-unreachable! c bed)
          :else (await (sleep-at! c bed)))))))
