(ns jobs.survival.rest
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.eat :as eat]))

(def doc
  "Rest a hurt body until it has healed, as a player would before going on: shut in where it stands (jobs.survival.shut-in,
  ways :room, :walls, :pit, :pocket; its blocks go to the scaffold ledger and jobs.access.cleanup takes them back), then hold
  still (:healing) while natural regeneration works, a bite of carried food (jobs.survival.eat) whenever food is under 20 so
  it goes on. A shut-in that fails warns rest.unsheltered and the body rests where it
  stands. Needs a body that regenerates by waiting (jobs.lib.body/heals-by-rest?: food 18+, not burning or poisoned).
  Done {:health :why} once health reaches ::rested-health (:healed) or the body stops regenerating (:food-low: food fell
  under 18, the hungry trigger eats; or burning or poisoned). Nothing-possible :hurt-resting {:health} when health falls
  while resting, :not-healing when no hp came back in ::stall-holds holds in a row. Run by the low-health trigger.
  Quote: job time up to ::job-time-s.")

(a/defargs args {})

(a/defargs settings
  {::rested-health {:default 12 :doc "Health (of 20) at which the rest ends." :spec (a/num-in 0 20)}
   ::pause-s {:default 4 :doc "Seconds of one hold between two health readings (natural regeneration heals 1 hp in 4 s at food 18)." :spec (a/num-in 0 nil)}
   ::stall-holds {:default 8 :doc "Holds in a row with no hp gained after which the rest gives up (counted, not timed)." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 600 :doc "Seconds a rest may take." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :reflex :tags [:shelter]})

(def safe? ready/ok)

(def fit? ready/ok)

(defn health [c] (.-health (.self (:primitives c))))

(defn done? [c]
  (let [healed? (>= (health c) (settings/get settings ::rested-health))
        d (or healed? (not (body/heals-by-rest? (:primitives c))))]
    (checks/goal d [:rested "healed, or resting heals no more"] [:resting "resting to heal"]
                 {:result {:health (health c) :why (if healed? :healed :food-low)}})))

(defn quote-of [_] {:uses {:job-time [0 60 (settings/get settings ::job-time-s)]}})

(defn ^:async shelter!
  "Shut in once per rest; a failure is told and the rest goes on in place."
  [c]
  (let [e (await (ctx/call c :shut-in 'jobs.survival.shut-in {:ways #{:room :walls :pit :pocket}}))]
    (when-not (= :continue e)
      (ctx/update-mem! c assoc :sheltered (= :done (:kind e)))
      (when-not (= :done (:kind e))
        (ctx/emit! c :rest.unsheltered :warn {:pos (sh-sense/feet (:primitives c))
                                               :text (str "could not shut in to rest (" (or (:text (:data e)) (:cause e)) "); resting here")})))
    :again))

(defn ^:async heal-step!
  "A bite while food is under 20 and food is carried, one hold of ::pause-s, then judge the health it read before against now."
  [c]
  (when (and (< (.-food (.self (:primitives c))) 20) (eat/carried-best c))
    (await (child/run!* c :eat 'jobs.survival.eat {:until 20 :max-bites 1 :get-food? false})))
  (let [{:keys [last-health still]} (ctx/mem c)
        h (health c)
        still (if (and last-health (> h last-health)) 0 (inc (or still -1)))]
    (cond
      (and last-health (< h last-health))
      (ctx/end c :nothing-possible :hurt-resting {:health h :text (str "health fell to " (.toFixed h 1) " while resting")})

      (>= still (settings/get settings ::stall-holds))
      (ctx/end c :nothing-possible :not-healing {:health h :text (str "no hp came back in " still " holds of rest")})

      :else
      (do (ctx/update-mem! c assoc :last-health h :still still)
          (let [r (await (child/pause! c (settings/get settings ::pause-s) :healing))]
            (if (= :continue r) :continue :again))))))

(defn ^:async round
  "The whole rest in one round: shelter, then holds until done? holds."
  [c]
  (await (pace/steps! c #(if (contains? (ctx/mem c) :sheltered) (heal-step! c) (shelter! c)))))
