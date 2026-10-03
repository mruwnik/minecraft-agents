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
  2. jobs.survival.log-out, when there is no usable bed and another player
     sleeps, so that player can skip the night;
  3. jobs.survival.dig-in, which roofs the body in.
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
  than :bed-radius and emits a needs_bed warn once so a job that can find or
  craft a bed can act. A body with no :slept entry has no known last sleep and
  is never counted as overdue. Memory: reads :slept (and, through the children,
  :bed, :bed-unreachable and :log-out); dig-in writes :shelter, which nothing
  here reads.")

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :default 128}
   :max-days-awake {:doc "in-game days without sleep before finding a bed becomes urgent" :default 3}
   :offline-allowed {:doc "false forbids log-out" :default true}
   :offline-ms {:doc "how long log-out stays away" :default 300000}
   :player-radius {:doc "log-out looks for sleeping players this far away" :default 128}})

(defn check [c]
  (sh/unsafe-night? (:primitives c) (:roof-height (:args c))))

(defn overdue? [c]
  (let [days (sh/days-awake c)]
    (and (some? days) (>= days (:max-days-awake (:args c))))))

(defn note-needs-bed! [c]
  (when-not (:needs-bed-noted (ctx/mem c))
    (ctx/update-mem! c assoc :needs-bed-noted true)
    (ctx/emit! c :needs_bed :warn {:days (sh/days-awake c)
                                   :text "not slept for too long; phantoms will come, find or make a bed"})))

(defn child-args [c radius]
  (let [{:keys [offline-allowed offline-ms player-radius]} (:args c)]
    {:log-out {:bed-radius radius :offline-allowed offline-allowed :offline-ms offline-ms
               :player-radius player-radius}
     :sleep {:bed-radius radius}
     :dig-in {:roof-height (:roof-height (:args c))}}))

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
              l (await (ctx/call-child c :log-out 'jobs.survival.log-out (:log-out a)))]
          (cond
            (= :continue l) :continue
            (= :done l) :done
            :else
            (let [d (await (ctx/call-child c :dig-in 'jobs.survival.dig-in (:dig-in a)))]
              (cond
                (= :continue d) :continue
                (and (= :done d) (sh/roofed? p (:roof-height (:args c)))) :done
                :else :declined))))))))

(defn ^:async round [c]
  (let [p (:primitives c)]
    (if (or (sh/sleeping? p)
            (sh/roofed? p (:roof-height (:args c)))
            (not (sh/night? p)))
      :done
      (await (choose-round c)))))
