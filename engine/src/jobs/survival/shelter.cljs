(ns jobs.survival.shelter
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]))

(def doc
  "Survive the night by getting the body sheltered. Check: the night-unsafe
  condition (night, awake, no solid block within :roof-height blocks above the
  body). A round ends :done at once when the body is asleep, is roofed within
  :roof-height, or it is not night. Otherwise it tries, in order, and the first
  that does not decline decides:
  1. jobs.survival.sleep, with a known bed within :bed-radius;
  2. jobs.survival.dig-in, which roofs the body in.
  Logging out so another player can skip the night is not done here: it is the
  player-sleeping-nearby reflex's job, which holds whether or not the body is
  roofed.
  A child that is actually working (:continue) makes the round :continue.
  dig-in ending :done with the body roofed is :done; every case where nothing
  could be done (all children declined, or dig-in ended without a roof) is
  :declined, so the reflex is dropped and the trigger re-fires after its
  cooldown if the body is still unsafe. It does not wait for day or open the
  shelter again; leaving a shelter at dawn is the stuck reflex's business.
  A sleep that ends without sleeping (the bed was gone, or unreachable) falls
  through to the next choice in the same round and is recorded as
  :sleep-failed, so later rounds of this shelter do not call sleep again (and
  walk back toward the bed) while the other choices work. A body whose latest
  :slept entry is more than :max-days-awake in-game days (20 minutes each)
  old, which phantoms attack, looks for a bed within :urgent-bed-radius rather
  than :bed-radius and emits a needs_bed warn at most once an in-game day (a :needs-bed body-memory
  entry, so a re-fired reflex does not repeat it) so a job that can find or
  craft a bed can act. A body with no :slept entry has no known last sleep and
  is never counted as overdue. Memory: reads :slept, reads and writes :needs-bed (and, through the children, :bed and :bed-unreachable); dig-in writes :shelter, which nothing
  here reads.")

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :default 128}
   :max-days-awake {:doc "in-game days without sleep before finding a bed becomes urgent" :default 3}})

(def backoff
  "Off: it bounds itself (declines when no child can act) and is time-critical
  at night, so a backoff would leave the body unsheltered longer."
  false)

(defn check [c]
  (sh/unsafe-night? (:primitives c) (:roof-height (:args c))))

(defn overdue? [c]
  (let [days (sh/days-awake c)]
    (and (some? days) (>= days (:max-days-awake (:args c))))))

(def needs-bed-policy {:cap 1 :ttl sh/ms-per-day})

(defn note-needs-bed! [c]
  (when (empty? (ctx/entries c :needs-bed))
    (ctx/remember! c :needs-bed {} needs-bed-policy)
    (ctx/emit! c :needs_bed :warn {:days (sh/days-awake c)
                                   :text "not slept for too long; phantoms will come, find or make a bed"})))

(defn child-args [c radius]
  {:sleep {:bed-radius radius}
   :dig-in {:roof-height (:roof-height (:args c))}})

(defn ^:async choose-round [c]
  (let [p (:primitives c)
        urgent (overdue? c)
        radius (if urgent (:urgent-bed-radius (:args c)) (:bed-radius (:args c)))
        a (child-args c radius)
        started (ctx/now c)]
    (when urgent (note-needs-bed! c))
    (let [s (if (:sleep-failed (ctx/mem c))
              :declined
              (await (ctx/call-child c :sleep 'jobs.survival.sleep (:sleep a))))]
      (cond
        (= :continue s) :continue
        (and (= :done s) (or (sh/sleeping? p) (not (sh/night? p)) (seq (ctx/since c :slept started)))) :done
        :else
        (let [_ (when (= :done s) (ctx/update-mem! c assoc :sleep-failed true))
              d (await (ctx/call-child c :dig-in 'jobs.survival.dig-in (:dig-in a)))]
          (cond
            (= :continue d) :continue
            (and (= :done d) (sh/roofed? p (:roof-height (:args c)))) :done
            :else :declined))))))

(defn ^:async round [c]
  (let [p (:primitives c)]
    (if (or (sh/sleeping? p)
            (sh/roofed? p (:roof-height (:args c)))
            (not (sh/night? p)))
      :done
      (await (choose-round c)))))
