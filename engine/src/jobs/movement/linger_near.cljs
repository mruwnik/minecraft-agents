(ns jobs.movement.linger-near
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]))

(def idle-ok true)

(def doc
  "Stay within :range of :pos for :wait-s seconds, for a wait that needs the body near (leaf decay before the drops
  despawn, crop growth, a birth). Needs :pos and a positive :wait-s (else declined at submit).
  Each round: within range it pauses (a noop child holding :lingering) for 500 ms and yields :continue; out of range
  (pushed away, or at the start) it walks back with a go-to child (:ways #{:walk :clear-seal}).
  Ends with a result:
  - {:lingered true :reason \"waited\"} when :wait-s of time in range has been waited, with the body in range.
  - {:status :stopped :lingered false :reason :unreachable} when go-to does not arrive; go-to's ending is the :because.
  Only time held in range counts (a pause that ended done adds its 500 ms, kept in job memory): walking back, a cut
  pause and time the job was preempted add nothing, and a resume checks the range again before it can end.
  Quote: the wait plus the walk back; most is the job-time budget, since being pushed out can repeat the walk. A cut leaves nothing to undo; a caller whose wait
  ends early cuts the job.")

(a/defargs args
  {:pos {:doc "the place to stay near" :spec ::a/pos :default nil}
   :range {:doc "stay within this many blocks of :pos" :spec (a/num-in 0 nil) :default 3}
   :wait-s {:doc "seconds to stay" :spec (a/num-in 0 nil) :default nil}})

(def idle-ms 500)

(def target [:pos])

(def bad-args
  "Never :bad-args unless a position and a positive :wait-s are given."
  (checks/args-fit "needs a position and a positive :wait-s"
                   (fn [c] (let [{:keys [pos wait-s]} (:args c)] (and (some? pos) (number? wait-s) (pos? wait-s))))))

(a/defargs settings
  {::job-time-s {:default 3600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(defn budget-s [] (settings/get settings ::job-time-s))

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))
(def fit? ready/fit?)

(def defaults {:spend {:job-time ::job-time-s}})

(def can-start? bad-args)

(defn done?
  "Seconds waited in range against :wait-s; done once all are waited with the body in range and no pause under way."
  [c]
  (let [{:keys [pos range wait-s]} (:args c)
        done (/ (:waited (ctx/mem c) 0) 1000)
        data [:units :seconds :done done :left (max 0 (- wait-s done)) :result {:lingered true :reason "waited"}]]
    (if (and (>= done wait-s) (u/within? (u/self-pos c) pos range) (not (checks/child-under-way? c)))
      (apply contract/ok :waited (str "waited " wait-s " s in range") data)
      (apply contract/later :lingering (str "waited " done " of " wait-s " s") data))))

(defn ^:async quote-of
  "The wait, which only time in range counts, and the walk back as go-to quotes it when the body is out of range."
  [c]
  (let [{:keys [pos range wait-s]} (:args c)
        [wl wm wx] (if (u/within? (u/self-pos c) pos range)
                     [0 0 0]
                     (or (get-in (await (ctx/quote c 'jobs.movement.go-to {:pos pos :range range})) [:uses :job-time]) [0 0 0]))]
    {:uses {:job-time [(+ wait-s wl) (+ wait-s wm) (max (+ wait-s wx) (budget-s))]}}))

(defn finish!
  "Release the hold and end the call."
  [c kind cause data]
  (ctx/hold-still! c nil)
  (ctx/end c kind cause data))

(defn ^:async walk-back!
  "Walk within range of pos: :continue while the child waits or after an arrival, else go-to's failure ends the job stopped."
  [c pos range]
  (let [r (await (child/call-walk* c :walk {:pos pos :range range :ways #{:walk :clear-seal}}))]
    (if (or (= :continue r) (:arrived (:data r)))
      :continue
      (finish! c :nothing-possible :unreachable
               {:lingered false :text (str "cannot get within " range " of " (pr-str pos))}))))

(defn ^:async round
  "One bounded step: in range, end when the waited time is up, else hold still and count the wait; out of range,
  walk back."
  [c]
  (let [{:keys [pos range wait-s]} (:args c)]
    (cond
      (not (u/within? (u/self-pos c) pos range))
      (do (ctx/hold-still! c nil)
          (await (walk-back! c pos range)))

      (>= (:waited (ctx/mem c) 0) (* 1000 wait-s))
      (finish! c :done nil {:lingered true :reason "waited"})

      :else
      (let [r (await (child/pause! c (/ idle-ms 1000) :lingering))]
        (when (= :done (:kind r))
          (ctx/update-mem! c update :waited (fnil + 0) idle-ms))
        :continue))))
