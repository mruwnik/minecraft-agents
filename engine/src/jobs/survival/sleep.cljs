(ns jobs.survival.sleep
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]
            [engine.memory :as mem]
            [engine.places :as places]))

(def doc
  "Walk to a bed and sleep in it.
  Declines unless it is night and a bed is known: the :bed argument, else the remembered :bed within :bed-radius.
  Also declines while the bed is in :bed-unreachable (an unexpired entry with the same position).
  Ends when asleep, or when it turns day.
  Retries three times, then warns and ends: a taken bed, a monster nearby, an unreachable bed.
  A bed whose chunk is not loaded is retried the same way.
  A bed that is not there is not retried: the job ends and the shelter chooser falls through.
  If it was the remembered bed, :bed is overwritten with {:gone true :was pos}, so it no longer reads as a place.
  With a :bed argument the radius is ignored. After sleeping there it is recorded as :bed
  when none is recorded or the recorded one is gone. A live recorded :bed is kept (one place.kept event).
  Memory: reads :bed and :bed-unreachable. Writes :slept (cap 10, seven in-game days),
  :bed (see above) and :bed-unreachable {:pos bed} after an unreachable bed (cap 5, ten minutes).")

(def args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :default sh/default-bed-radius}
   :bed {:doc "bed position [x y z] or {:x :y :z} to sleep in instead of the remembered :bed (no radius); it is recorded as :bed when that is unset or gone"
         :default nil}})

(def backoff
  "Off: its retries are already bounded (three tries of go-to, then
  :bed-unreachable), and at night a backoff would only delay the switch to
  dig-in by about a minute."
  false)

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

(defn check [c]
  (let [bed (bed-of c)]
    (and (sh/night? (:primitives c))
         (some? bed)
         (not (unreachable-bed? c bed)))))

(defn give-up-unreachable! [c bed]
  (let [r (u/fail! c :bed_unreachable "cannot reach the bed")]
    (when (= :done r)
      (ctx/remember! c :bed-unreachable {:pos bed} unreachable-policy))
    r))

(defn ^:async sleep-at! [c bed]
  (let [r (await (ctx/act c :sleep (clj->js {:pos bed})))]
    (case (.-status r)
      "sleeping" (do (ctx/remember! c :slept {:pos bed} slept-policy)
                     (when (:bed (:args c)) (places/offer! c :bed bed))
                     :done)
      "not-night" :done
      "missing" (if (nil? (u/block-name (:primitives c) bed))
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
