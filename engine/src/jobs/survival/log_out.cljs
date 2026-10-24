(ns jobs.survival.log-out
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.cost :as cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.shut-in :as shut-in]))

(def doc
  "Leave the server for a short stint, then come back: the night job's way to let another player's sleep skip the
  night. Waits for night (:not-night); impossible once a log-out
  was unsupported (:log-out-unsupported).
  Stays away :offline-ms, at most until morning; until morning when back-unsafe (the death coming back to the spot risks,
  jobs.lib.cost/death-chance over the known threats, costs more than the longer stint, or the body is hurt past healing). Back while it is still night, it waits :news-ms for the server's sleep
  count (sent on a join while anyone sleeps) before it ends.
  A listed job that finds another job logged the body out since it was queued is done at once without acting.
  Ends with the result {:ms :how}. How is ok, cut, unsupported or closed.
  Memory: writes :log-out {:ms :status :assumed} (cap 10, one in-game day); :assumed when no sleeper was known at the start (the night does not stay out again on the news not coming).")

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
  {::job-time-s {:default 900 :doc "Seconds a log-out stint and the wait for the sleep count may take (a stint lasts at most until morning)." :spec (a/num-in 0 nil)}
   ::return-s {:default 3 :doc "Seconds a body back online stands at its spot before it can answer a mob there (back-death-cost)." :spec (a/num-in 0 nil)}})

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

(defn back-death-cost
  "Seconds the death coming back online to the spot the body leaves risks: route-death-cost of the spot held ::return-s
  with the mobs met spawning there by its light (spawn-light/spawns-at, as the stay price counts them) and the known threats (threats/threats-of: sensed, heard counting as seen, fled from or met at a death): one
  whose field holds the spot counts in full, it has the whole stint to walk there, and costs the answer the body would
  take to it there (walling in for the night when it carries blocks: shut-in/covers)."
  [c]
  (let [p (:primitives c)
        at (u/pos-of (.-pos (.self p)))
        body (assoc (threats/death-body c) :covers (shut-in/covers (assoc c :args {:for :night}) [:walls]))
        secs (settings/get settings ::return-s)]
    (cost/route-death-cost body {:cells [(assoc at :s secs)] :spawns (spawn-light/spawns-at p at secs)
                                 :threats (mapv (fn [{:keys [reach] :as t}] (assoc t :reach #(when (reach %1 %2 %3) 1))) (threats/threats-of c))} at)))

(defn back-unsafe
  "Why coming back online at night to the spot the body leaves costs more than staying away until morning, as text, else
  nil: its back-death-cost is more than the seconds the longer stint adds (until-morning-ms less the stint of :offline-ms), or the body is
  hurt-past-healing?."
  [c until-morning-ms]
  (let [self (.self (:primitives c))
        more-s (/ (- until-morning-ms (min (:offline-ms (:args c)) until-morning-ms)) 1000)
        risk (back-death-cost c)]
    (cond (> risk more-s) (str "coming back risks a death worth " (js/Math.round risk) " s, more than " (js/Math.round more-s) " s more away")
          (hurt-past-healing? self) (str "health " (.toFixed (.-health self) 1) " and food " (.-food self) " heal nothing"))))

(defn away-ms [c]
  (let [until-morning (sh-clock/ms-until-morning (.-timeOfDay (.self (:primitives c))))]
    (cond (not (pos? until-morning)) (:offline-ms (:args c))
          (back-unsafe c until-morning) until-morning
          :else (min (:offline-ms (:args c)) until-morning))))

(defn ^:async quote-of [c]
  (let [s (/ (+ (away-ms c) (:news-ms (:args c))) 1000)]
    {:uses {:job-time [0 s s]}}))

(defn ^:async log-out! [c]
  (when-let [why (let [until (sh-clock/ms-until-morning (.-timeOfDay (.self (:primitives c))))] (and (pos? until) (back-unsafe c until)))]
    (ctx/emit! c :log-out.till-morning :info {:text (str "staying logged out until morning: " why)}))
  (let [assumed (not (sh-sleepers/asleep-known? (:primitives c) (ctx/view c)))
        r (await (ctx/act c :offline #js {:ms (away-ms c) :why "logged-out-for-sleeping-player"}))
        how (.-status r)
        out {:ms (.-ms r) :how how}]
    (when (and (= "ok" how) (sh-sense/night? (:primitives c)))
      (await (child/pause! c (/ (:news-ms (:args c)) 1000) :sleep-news)))
    (ctx/remember! c :log-out {:ms (.-ms r) :status how :assumed assumed} log-out-policy)
    (case how
      ("ok" "cut") (ctx/end c :done nil out)
      "unsupported" (ctx/end c :impossible :unsupported out)
      (ctx/end c :nothing-possible (keyword how) out))))

(defn ^:async round [c] (await (log-out! c)))
