(ns jobs.survival.shelter
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]))

(def doc
  "Survive the night. Check: night, awake and no solid block within
  :roof-height blocks above the body (the night-unsafe condition), or a built
  :shelter entry at the body's place whose roof is still over it. A round
  tries, in order, and the first that does not decline decides:
  1. jobs.survival.sleep, with a known bed within :bed-radius;
  2. jobs.survival.log-out, when there is no usable bed and another player
     sleeps, so that player can skip the night;
  3. jobs.survival.dig-in, which roofs the body in.
  A sleep that ends without sleeping (the bed was gone, or unreachable) falls
  through to the next choice in the same round and is recorded as
  :sleep-failed, so later rounds of this shelter do not call sleep again (and
  walk back toward the bed) while the other choices work. Once dug in it waits for day
  (jobs.time.wait-for-day), then opens the roof (or climbs out of the pit) and
  writes the :shelter entry again with :state :reopened. A body whose latest
  :slept entry is more than :max-days-awake in-game days (20 minutes each)
  old, which phantoms attack, looks for a bed within :urgent-bed-radius rather
  than :bed-radius and emits a needs_bed warn once so a job that can find or
  craft a bed can act. A body with no :slept entry has no known last sleep and
  is never counted as overdue.")

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :default 128}
   :max-days-awake {:doc "in-game days without sleep before finding a bed becomes urgent" :default 3}
   :offline-allowed {:doc "false forbids log-out" :default true}
   :offline-ms {:doc "how long log-out stays away" :default 300000}
   :player-radius {:doc "log-out looks for sleeping players this far away" :default 128}})

(defn check [c]
  (let [p (:primitives c)
        roof-height (:roof-height (:args c))]
    (boolean (or (sh/unsafe-night? p roof-height)
                 (and (sh/active-shelter c) (sh/roofed? p roof-height))))))

(defn overdue? [c]
  (let [days (sh/days-awake c)]
    (and (some? days) (>= days (:max-days-awake (:args c))))))

(defn note-needs-bed! [c]
  (when-not (:needs-bed-noted (ctx/mem c))
    (ctx/update-mem! c assoc :needs-bed-noted true)
    (ctx/emit! c :needs_bed :warn {:days (sh/days-awake c)
                                   :text "not slept for too long; phantoms will come, find or make a bed"})))

(defn ^:async dig-out!
  "At day: open the roof, climb out of the pit, and mark the shelter reopened."
  [c shelter]
  (let [roof (:roof shelter)]
    (when roof
      (await (ctx/act c :dig (clj->js {:pos roof})))
      (await (ctx/act c :moveTo (clj->js {:pos roof :range 1}))))
    (ctx/remember! c :shelter (assoc shelter :state :reopened) {:cap 10 :ttl sh/ms-per-day})))

(defn ^:async wait-round [c shelter]
  (let [w (await (ctx/call-child c :wait 'jobs.time.wait-for-day {}))]
    (if (= :done w)
      (do (await (dig-out! c shelter)) :done)
      :continue)))

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
                (= :declined d) :done
                (= :continue d) :continue
                :else (if-let [shelter (sh/active-shelter c)]
                        (await (wait-round c shelter))
                        :continue)))))))))

(defn ^:async round [c]
  (let [p (:primitives c)]
    (cond
      (sh/sleeping? p) :done
      (sh/active-shelter c) (await (wait-round c (sh/active-shelter c)))
      (not (check c)) :done
      :else (await (choose-round c)))))
