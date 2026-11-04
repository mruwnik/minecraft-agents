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
  With :stay-ms and/or :until (wall-clock epoch ms; both: the later) it works at any hour and stays offline that long
  (the stint's end is kept in the job's memory, so a restart does not start it over): chained offline stints of the
  primitive's cap each, one per round, then back; :spend :job-time is the stay. No night rule, no sleep-count wait.
  Stays away until morning (the skip wakes everyone then). Back while it is still night, it waits :news-ms for the server's sleep
  count (sent on a join while anyone sleeps) before it ends.
  A listed job that finds another job logged the body out since it was queued is done at once without acting.
  Ends with the result {:ms :how}. How is ok, cut, unsupported or closed.
  Memory: writes :log-out {:ms :status :assumed} (cap 10, one in-game day); :assumed when no sleeper was known at the start (the night does not stay out again on the news not coming).")

(a/defargs args
  {:stay-ms {:doc "stay offline this long, at any hour (with :until: whichever ends later)" :spec (a/num-in 0 nil)}
   :until {:doc "stay offline until this wall-clock instant, epoch ms, at any hour" :spec (a/num-in 0 nil)}
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

(defn staying?
  "Whether the call asks for a stay (:stay-ms or :until)."
  [c]
  (or (some? (:stay-ms (:args c))) (some? (:until (:args c)))))

(defn night-or-stay [c]
  (if (staying? c)
    (contract/ok :staying "a stay is asked, any hour")
    (contract/asked c (checks/night :not-night))))

(def can-start? (cond/and night-or-stay supported))

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds a log-out stint and the wait for the sleep count may take (a stint lasts at most until morning)." :spec (a/num-in 0 nil)}
   ::return-s {:default 1.4 :doc "Seconds a body back online stands at its spot before it can answer a mob there (back-death-cost): the mean of the trial bodies' 12 returns (event logs, 2026-10-25) whose hostile-near fired within 2.5 s, a mob there at the return, from :online to respond's first decision (hostile.decided; median 1.2, 1.1 to 2.4 s)." :spec (a/num-in 0 nil)}})

(defn stay-deadline
  "Wall-clock ms the stay ends: the one kept in the job's memory, else from now and the args (nil without a stay)."
  [c]
  (or (:stay-until (ctx/mem c))
      (let [{:keys [stay-ms until]} (:args c)
            now (ctx/now c)]
        (when (staying? c)
          (max (if stay-ms (+ now stay-ms) 0) (or until 0))))))

(defn default-time
  "The call's default :spend :job-time: ::job-time-s, or the stay (and the return) when one is asked."
  [{:keys [stay-ms until]}]
  (let [from-now (max (or stay-ms 0) (if until (- until (js/Date.now)) 0))]
    (if (or stay-ms until)
      (+ (/ from-now 1000) (settings/get settings ::job-time-s))
      (settings/get settings ::job-time-s))))

(def defaults {:spend {:job-time default-time}})

(def meta {:category :phase :part-of '[jobs.survival.night] :tags [:outranks-comfort]})

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

(defn away-ms
  "How long the stint lasts: till morning, so the sleepers' night is not cut by a short check-back."
  [c]
  (max 0 (sh-clock/ms-until-morning (.-timeOfDay (.self (:primitives c))))))

(defn ^:async quote-of [c]
  (let [s (if (staying? c)
            (/ (max 0 (- (stay-deadline c) (ctx/now c))) 1000)
            (/ (+ (away-ms c) (:news-ms (:args c))) 1000))]
    {:uses {:job-time [0 s s]}}))

(defn ^:async log-out! [c]
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

(defn ^:async stay! [c]
  (let [deadline (stay-deadline c)
        _ (ctx/update-mem! c assoc :stay-until deadline)
        left (- deadline (ctx/now c))]
    (if (<= left 0)
      (ctx/end c :done nil {:ms (:stayed-ms (ctx/mem c) 0) :how "ok"})
      (let [r (await (ctx/act c :offline #js {:ms left :why "logged-out-to-stay-away"}))
            how (.-status r)
            stayed (+ (:stayed-ms (ctx/mem c) 0) (or (.-ms r) 0))]
        (ctx/update-mem! c assoc :stayed-ms stayed)
        (cond
          (not= "ok" how) (do (ctx/remember! c :log-out {:ms stayed :status how :assumed true} log-out-policy)
                              (case how
                                "cut" (ctx/end c :done nil {:ms stayed :how how})
                                "unsupported" (ctx/end c :impossible :unsupported {:ms stayed :how how})
                                (ctx/end c :nothing-possible (keyword how) {:ms stayed :how how})))
          (>= (ctx/now c) deadline) (do (ctx/remember! c :log-out {:ms stayed :status how :assumed true} log-out-policy)
                                        (ctx/end c :done nil {:ms stayed :how how}))
          :else :continue)))))

(defn ^:async round [c] (if (staying? c) (await (stay! c)) (await (log-out! c))))
