(ns jobs.survival.shelter
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]
            [jobs.survival.dig-in :as dig-in]))

(def doc
  "Survive the night: the job owns the whole night, so the normal job loop does not get the body back until day.
  Check: the night-unsafe condition (night, awake, no solid block within :roof-height blocks above the body). A
  round by day ends :done (after a dig-in it first gets the body out of the pit, below); a first round that finds
  the body asleep or roofed ends :done at once. Otherwise it tries, in order, and the first that does not decline
  decides:
  1. jobs.survival.sleep, with a known bed within :bed-radius;
  2. jobs.survival.log-out {:others :online}, when another player is in the server's player list (the tab list):
     away until morning (the time is worked out from the time of day); back while it is still night, the next round
     logs out again. A log-out whose status is not ok or cut (unsupported, closed) falls through to dig-in in the same
     round and is not tried again by this shelter (unsupported is also remembered, so log-out declines after it);
  3. jobs.survival.dig-in, which roofs the body in.
  Once asleep or dug in it holds: each round waits hold-ms (the wait does not wake a sleeper) and ends :continue
  while it is night and the body is still asleep or roofed; woken or unroofed at night, it chooses again. By day,
  after a dig-in, each round calls jobs.survival.dig-in/leave! (the shelter's own way out) and the job ends only once
  it reports the body :out; :unsafe (a hostile near) or :no-way-out (leave! warns dig-in.trapped) waits hold-ms and
  tries again, so a trapped body keeps the job (and its agent sees the warns).
  The player-sleeping-nearby reflex still logs out (20 s) for a sleeper whatever the roof.
  A child that is actually working (:continue) makes the round :continue. Every case where nothing could be done
  (all children declined, or dig-in ended without a roof) is :declined, so the reflex is dropped and the trigger
  re-fires after its cooldown if the body is still unsafe.
  A sleep that ends without sleeping (the bed was gone, or unreachable) falls through to the next choice in the
  same round and is recorded as :sleep-failed, so later rounds of this shelter do not call sleep again (and walk
  back toward the bed) while the other choices work. A body whose latest :slept entry is more than :max-days-awake
  in-game days (20 minutes each) old, which phantoms attack, looks for a bed within :urgent-bed-radius rather than
  :bed-radius and emits a needs_bed warn at most once an in-game day (a :needs-bed body-memory entry, so a re-fired
  reflex does not repeat it) so a job that can find or craft a bed can act. A body with no :slept entry has no known
  last sleep and is never counted as overdue. Job memory: :sheltered (:slept or :dug-in), :sleep-failed,
  :log-out-failed, and leave!'s :dig-out. Body memory: reads :slept, reads and writes :needs-bed (and, through the
  children, :bed, :bed-unreachable, :log-out and :shelter).")

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
   ;; the sleep step already judged the bed; a bed it failed to use must not stop the log-out
   :log-out {:others :online :bed-radius 0}
   :dig-in {:roof-height (:roof-height (:args c))}})

(def hold-ms
  "How long one holding round waits (the wait primitive does not wake a sleeping body); a higher reflex cuts it."
  5000)

(defn ^:async hold [c]
  (await (ctx/act c :wait #js {:ms hold-ms}))
  :continue)

(defn sheltered! [c how] (ctx/update-mem! c assoc :sheltered how))

(defn ^:async dig-in-step [c a]
  (let [p (:primitives c)
        d (await (ctx/call-child c :dig-in 'jobs.survival.dig-in (:dig-in a)))]
    (cond
      (= :continue d) :continue
      (and (= :done d) (sh/roofed? p (:roof-height (:args c)))) (do (sheltered! c :dug-in) :continue)
      :else :declined)))

(defn ^:async log-out-step [c a]
  (let [p (:primitives c)
        l (if (:log-out-failed (ctx/mem c))
            :declined
            (await (ctx/call-child c :log-out 'jobs.survival.log-out (:log-out a))))
        status (:status (ctx/child-result c :log-out))]
    (cond
      (= :continue l) :continue
      (and (= :done l) (#{"ok" "cut"} status)) (if (sh/night? p) :continue :done)
      :else (do (when (= :done l) (ctx/update-mem! c assoc :log-out-failed true))
                (await (dig-in-step c a))))))

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
        (and (= :done s) (not (sh/night? p))) :done
        (and (= :done s) (or (sh/sleeping? p) (seq (ctx/since c :slept started)))) (do (sheltered! c :slept) :continue)
        :else
        (do (when (= :done s) (ctx/update-mem! c assoc :sleep-failed true))
            (await (log-out-step c a)))))))

(defn ^:async day-round
  "By day: after a dig-in, get the body out of the shelter with dig-in/leave! and end only once it reports :out; any
  other answer (:unsafe, a hostile near; :no-way-out, which leave! warns about) waits hold-ms and tries again."
  [c]
  (if (= :dug-in (:sheltered (ctx/mem c)))
    (let [r (await (dig-in/leave! c))]
      (cond
        (= :continue r) :continue
        (= :out (:reason r)) :done
        :else (await (hold c))))
    :done))

(defn ^:async round [c]
  (let [p (:primitives c)
        covered (or (sh/sleeping? p) (sh/roofed? p (:roof-height (:args c))))]
    (cond
      (not (sh/night? p)) (await (day-round c))
      (and covered (:sheltered (ctx/mem c))) (await (hold c))
      covered :done
      :else (await (choose-round c)))))
