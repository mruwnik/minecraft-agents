(ns jobs.survival.log-out
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.danger :as danger]
            [jobs.lib.foods :as foods]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]
            [jobs.lib.threats :as threats]))

(def doc
  "Leave the server for a short stint, then come back: the night job's way to let another player's sleep skip the
  night. Waits for night (:not-night); impossible once a log-out
  was unsupported (:log-out-unsupported).
  Stays away :offline-ms, at most until morning; until morning when back-unsafe (a known hostile could reach or shoot the
  spot it comes back to, or the body is hurt past healing). Back while it is still night, it waits :news-ms for the server's sleep
  count (sent on a join while anyone sleeps) before it ends.
  A listed job that finds another job logged the body out since it was queued is done at once without acting.
  Ends with the result {:ms :how}. How is ok, cut, unsupported or closed.
  Memory: writes :log-out {:ms :status} (cap 10, one in-game day).")

(a/defargs args
  {:offline-ms {:doc "how long to stay away at most; the stint ends at morning" :spec (a/num-in 0 nil) :default 30000}
   :news-ms {:doc "back at night, how long to wait for the sleep count" :spec (a/num-in 0 nil) :default 3000}})

(def log-out-policy {:cap 10 :ttl sh-clock/ms-per-day})

(defn listed? [c] (and (empty? (:slots c)) (nil? (:reflex c))))

(defn logged-out-meanwhile
  "The latest ok or cut :log-out entry written after this listed job was queued, or nil; a child or reflex job never
  asks."
  [c]
  (when (listed? c)
    (let [v (ctx/view c)
          queued (:queued-t (mem/job-mem v (:root c) []))
          out (mem/latest v :log-out)]
      (when (and queued out (> (:t out) queued) (#{"ok" "cut"} (:status (:data out))))
        out))))

(defn supported [c]
  (if (sh-sleepers/log-out-unsupported? (ctx/view c))
    (contract/never :log-out-unsupported "a log-out was unsupported")
    (contract/ok :log-out-supported "log-out not known unsupported")))

(def can-start? (cond/and (checks/night :not-night) supported))

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds a log-out stint and the wait for the sleep count may take (a stint lasts at most until morning)." :spec (a/num-in 0 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def meta {:category :phase :part-of '[jobs.survival.night]})

(def safe? ready/ok)

(def fit? ready/ok)

(def target [])

(defn done? [c]
  (let [out (logged-out-meanwhile c)]
    (checks/goal (some? out) [:logged-out "logged out meanwhile"] [:online "not logged out meanwhile"] {:result (when out {:ms (:ms (:data out)) :how (:status (:data out))})})))

(defn hurt-past-healing?
  "One hit may kill (foods/fragile?) and food is under foods/top-up-food: back online in the dark the body neither heals
  nor is safe."
  [self]
  (and (foods/fragile? self) (< (.-food self) foods/top-up-food)))

(defn back-unsafe
  "Why coming back online at night to the spot the body leaves would walk into danger, as text, else nil: a known
  hostile within its follow range (jobs.lib.threats) is a real danger there (jobs.lib.danger/dangers; a heard one counts
  as seen, the unseen being the worse case; one that hurt the body lately counts whatever the way test says): it stays
  near the spot while the body is away. Or the body is hurt-past-healing?."
  [c]
  (let [p (:primitives c)
        self (.self p)
        ds (->> (danger/dangers p threats/max-follow-range {:ranged-radius (danger/ranged-radius)}
                                {:sight? false :hit-ids (danger/hit-ids (ctx/view c))})
                (filter #(<= (danger/mob-distance p %) (threats/follow-range (.-name %)))))]
    (cond (seq ds) (str "a " (.-name (first ds)) " could reach the spot")
          (hurt-past-healing? self) (str "health " (.toFixed (.-health self) 1) " and food " (.-food self) " heal nothing"))))

(defn away-ms [c]
  (let [until-morning (sh-clock/ms-until-morning (.-timeOfDay (.self (:primitives c))))]
    (cond (not (pos? until-morning)) (:offline-ms (:args c))
          (back-unsafe c) until-morning
          :else (min (:offline-ms (:args c)) until-morning))))

(defn ^:async quote-of [c]
  (let [s (/ (+ (away-ms c) (:news-ms (:args c))) 1000)]
    {:uses {:job-time [0 s s]}}))

(defn ^:async log-out! [c]
  (when-let [why (and (pos? (sh-clock/ms-until-morning (.-timeOfDay (.self (:primitives c))))) (back-unsafe c))]
    (ctx/emit! c :log-out.till-morning :info {:text (str "staying logged out until morning: " why)}))
  (let [r (await (ctx/act c :offline #js {:ms (away-ms c) :why "logged-out-for-sleeping-player"}))
        how (.-status r)
        out {:ms (.-ms r) :how how}]
    (when (and (= "ok" how) (sh-sense/night? (:primitives c)))
      (await (child/pause! c (/ (:news-ms (:args c)) 1000) :sleep-news)))
    (ctx/remember! c :log-out {:ms (.-ms r) :status how} log-out-policy)
    (case how
      ("ok" "cut") (ctx/end c :done nil out)
      "unsupported" (ctx/end c :impossible :unsupported out)
      (ctx/end c :nothing-possible (keyword how) out))))

(defn ^:async round [c] (await (log-out! c)))
