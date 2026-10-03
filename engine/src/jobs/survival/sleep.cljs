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
  shelter chooser falls through. A taken bed, a monster nearby or an
  unreachable bed is retried three times, then warns and ends.")

(def args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :default sh/default-bed-radius}})

(def slept-policy {:cap 10 :ttl (* 7 sh/ms-per-day)})

(defn check [c]
  (and (sh/night? (:primitives c))
       (some? (sh/bed c (:bed-radius (:args c))))))

(defn ^:async sleep-at! [c bed]
  (let [r (await (ctx/act c :sleep (clj->js {:pos bed})))]
    (case (.-status r)
      "sleeping" (do (ctx/remember! c :slept {:pos bed} slept-policy) :done)
      "not-night" :done
      "missing" (do (ctx/remember! c :bed {:gone true :was bed} mem/place-policy)
                    (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the remembered place"})
                    :done)
      (u/fail! c :bed_unusable (str "cannot sleep: " (.-status r))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (sh/bed c (:bed-radius (:args c)))]
    (if (or (nil? bed) (sh/sleeping? p) (not (sh/night? p)))
      :done
      (let [walk (await (ctx/call-child c :go 'jobs.movement.go-to {:pos bed :range 2}))]
        (cond
          (= :continue walk) :continue
          (not (:arrived (ctx/child-result c :go))) (u/fail! c :bed_unreachable "cannot reach the bed")
          :else (await (sleep-at! c bed)))))))
