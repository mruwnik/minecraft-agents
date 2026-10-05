(ns jobs.survival.shelter
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]
            [engine.triggers.hungry :as hungry]
            [engine.triggers.night-unsafe :as night-unsafe]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.eat :as eat]))

(def doc
  "Survive the night: the job owns the whole night, so the normal job loop does not get the body back until day.
  Check: the night-unsafe condition (night, awake, and no solid block within :roof-height blocks above the body or
  the body shut in its own latest :shelter), or by
  day the body shut in its own latest dig-in shelter (the shut-in-by-day trigger: a body restarted while sealed, or
  sealed by a dig-in submitted directly, is let out: the day round calls leave! whenever the body is shut in its
  recorded :shelter or a leave! is under way, whatever its job memory says; a :no-way-out writes a :shelter-trapped entry for the cell, kept
  5 minutes, so the trigger does not refire on it). Every hold round eats one carried food (jobs.survival.eat's
  choice, :shelter.ate) when the hungry trigger's condition holds: in the shipped registers night-unsafe sits above
  hungry and health-low, so those reflexes cannot cut a held body (registered above it, they would). A
  round by day ends :done (after a dig-in it first gets the body out of the pit, below); a first round that finds
  the body asleep, or roofed other than in its own shelter, ends :done at once. A first round (no child run yet) that finds the body
  shut in its own latest :shelter at night holds as after a dig-in (:sheltered :dug-in): the job keeps nothing it
  needs to resume, so a shelter cut by a higher reflex is fired again by night-unsafe and simply holds. Otherwise it tries, in order, and the first that does not decline
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
  it reports the body :out (hostiles never hold it: a real danger is the hostile reflex's); :unsafe (leave! says
  :night at the day/night boundary) waits hold-ms and tries again; :no-way-out (leave! warns dig-in.trapped) ends
  the job failed (a shelter.failed warn and a result {:status :failed :reason :no-way-out :at :tries}, :at the pit position) since the shelter holds the body only until day.
  A sleeper elsewhere on the server is served by step 2 (log-out until morning); the player-sleeping-nearby reflex
  sits below night-unsafe in the shipped registers, so it does not cut a holding shelter.
  A child that is actually working (:continue) makes the round :continue. At night it never declines: when nothing
  could be done (all children declined, or dig-in ended without a roof) it holds the body anyway, exposed (job memory
  :sheltered :exposed, one shelter.exposed warn), each round waiting hold-ms and then choosing again, so a player who
  comes online (log-out) or blocks picked up (dig-in after a material-only failure) are used; dig-in leaves a
  :dig-in-futile entry for every way it ends unroofed, so a failing dig is not rerun every round. A reflex that
  holds is never fired again, so the night-unsafe trigger does not churn; after a higher reflex cuts the shelter it
  fires again once that reflex ends (its 10 s cooldown counts only from a shelter that ended on its own). By day an exposed shelter calls leave! too (a half-dug pit), then ends.
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

(defn check
  "The night-unsafe condition (night, awake, unroofed or shut in its own shelter), or (by day) the body shut in its own
  recorded shelter (the shut-in-by-day condition)."
  [c]
  (let [p (:primitives c)]
    (or (night-unsafe/holds? p (ctx/view c) (:roof-height (:args c)))
        (sh/shut-in-by-day? p (:data (ctx/latest c :shelter)) (:data (ctx/latest c :shelter-trapped))))))

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

(defn ^:async eat-if-hungry!
  "The hungry reflex (below night-unsafe in the shipped registers) cannot reach a body the shelter holds, so a hold round eats one carried food (jobs.survival.eat's
  choice, engine.foods) when the body is hungry by the hungry trigger's own condition (food below 6, or below 14 when
  hurt). A sleeping body does not eat."
  [c]
  (let [p (:primitives c)
        self (.self p)
        health (.-health self)
        best (when (and (not (sh/sleeping? p)) (hungry/hungry? (.-food self) health {}))
               (eat/best-food (u/inventory p) false nil health))]
    (when best
      (await (ctx/act c :equip #js {:item best}))
      (let [r (await (ctx/act c :eat #js {:item best}))]
        (when (= "ate" (.-status r))
          (ctx/emit! c :shelter.ate :info {:item best :food (.-food r) :text (str "ate " best " while holding the shelter")}))))))

(defn ^:async hold [c]
  (await (eat-if-hungry! c))
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

(defn ^:async hold-exposed
  "Night, and no choice could shelter the body: hold it anyway until day (the shelter owns the night; a declined reflex
  would only fire again after its cooldown and fail the same way). The first time, :sheltered :exposed and one
  shelter.exposed warn; every round waits hold-ms, and the next round chooses again."
  [c]
  (when-not (= :exposed (:sheltered (ctx/mem c)))
    (sheltered! c :exposed)
    (ctx/emit! c :shelter.exposed :warn {:pos (sh/feet (:primitives c))
                                         :text "cannot shelter here (no bed, nobody else online, dig-in cannot roof); holding until day"}))
  (hold c))

(defn ^:async day-round
  "By day: after a dig-in (or a night held exposed), get the body out of the shelter with dig-in/leave!. :out ends :done; :no-way-out (leave! has
  warned dig-in.trapped) ends the job failed (shelter.failed warn and a result, both with the reason and :at, the
  pit position), so the agent gets the body back; :unsafe (only :night, at the day/night boundary) waits hold-ms and
  tries again. Hostiles never hold it: leave! opens by day whatever is around."
  [c]
  (if (or (#{:dug-in :exposed} (:sheltered (ctx/mem c))) (:dig-out (ctx/mem c)) (dig-in/sheltered-in c))
    (let [r (await (dig-in/leave! c))]
      (cond
        (= :continue r) :continue
        (= :out (:reason r)) :done
        (= :no-way-out (:reason r))
        (let [result {:status :failed :reason :no-way-out :at (:at r) :tries (:tries r)}]
          (ctx/emit! c :shelter.failed :warn (assoc result :text "no way out of the shelter by day; giving the body back"))
          (ctx/remember! c :shelter-trapped {:pos (sh/feet (:primitives c))} sh/trapped-policy)
          (ctx/result! c result)
          :done)
        :else (await (hold c))))
    :done))

(defn ^:async round [c]
  (let [p (:primitives c)
        covered (or (sh/sleeping? p) (sh/roofed? p (:roof-height (:args c))))]
    (cond
      (not (sh/night? p)) (await (day-round c))
      (and covered (:sheltered (ctx/mem c))) (await (hold c))
      (and covered (empty? (:children (ctx/mem c))) (not (sh/sleeping? p)) (dig-in/sheltered-in c))
      (do (sheltered! c :dug-in) (await (hold c)))
      covered :done
      :else (let [r (await (choose-round c))]
              (if (= :declined r) (await (hold-exposed c)) r)))))
