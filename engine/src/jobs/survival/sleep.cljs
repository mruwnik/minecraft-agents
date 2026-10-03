(ns jobs.survival.sleep
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def doc
  "Walk to the remembered :bed (go-to as a child) and sleep. Check: it is
  night and a :bed entry lies within :bed-radius of the body. Done when asleep
  or when it turns out to be day; a :slept entry (cap 10, kept seven in-game
  days) is written when the sleep primitive succeeds. A bed that is not at its
  place is retracted by writing a :bed entry {:gone true :was pos}, which has
  no :pos, so it no longer reads as a known place, and the job ends so the
  shelter chooser falls through; when the bed's chunk is not loaded (the block
  reads nil) it is not retracted but retried like the failures below. A taken bed, a monster nearby or an
  unreachable bed is retried three times, then warns and ends; an unreachable
  bed is then also remembered in a :bed-unreachable entry {:pos bed} (cap 5,
  kept ten minutes), and while an unexpired entry has the same pos as the
  remembered bed the check declines, so the body does not walk at it again.
  Memory: reads :bed and :bed-unreachable; writes :slept, :bed (a gone bed)
  and :bed-unreachable.")

(def args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :default sh/default-bed-radius}})

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

(defn check [c]
  (let [bed (sh/bed c (:bed-radius (:args c)))]
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
      "sleeping" (do (ctx/remember! c :slept {:pos bed} slept-policy) :done)
      "not-night" :done
      "missing" (if (nil? (u/block-name (:primitives c) bed))
                  (u/fail! c :bed_unloaded "bed chunk not loaded yet")
                  (do (ctx/remember! c :bed {:gone true :was bed} mem/place-policy)
                      (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the remembered place"})
                      :done))
      (u/fail! c :bed_unusable (str "cannot sleep: " (.-status r))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (sh/bed c (:bed-radius (:args c)))]
    (if (or (nil? bed) (sh/sleeping? p) (not (sh/night? p)))
      :done
      (let [walk (await (ctx/call-child c :go 'jobs.movement.go-to {:pos bed :range 2}))]
        (cond
          (= :continue walk) :continue
          (not (:arrived (ctx/child-result c :go))) (give-up-unreachable! c bed)
          :else (await (sleep-at! c bed)))))))
