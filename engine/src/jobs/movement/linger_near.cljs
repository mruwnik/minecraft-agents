(ns jobs.movement.linger-near
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]))

(def idle-ok true)

(def doc
  "Stay within :range of :pos for :wait-s seconds, for a wait that needs the body near (leaf decay before the drops
  despawn, crop growth, a birth). Needs :pos and a positive :wait-s (else declined at submit).
  Each round: within range it pauses (a noop child holding :lingering) for 500 ms and yields :continue; out of range
  (pushed away, or at the start) it walks back with a go-to child (:ways #{:walk :clear-seal}).
  Ends with a result:
  - {:lingered true :reason \"waited\"} when :wait-s of time in range has been waited, with the body in range.
  - {:status :stopped :lingered false :reason :unreachable :why <go-to's reason>} (warn linger.unreachable) when go-to
    does not arrive three times in a row.
  Only time held in range counts (500 ms per round, kept in job memory): walking back and time the job was
  preempted add nothing, and a resume checks the range again before it can end. A cut leaves nothing to undo; a caller whose wait
  ends early cuts the job.")

(a/defargs args
  {:pos {:doc "the place to stay near" :spec ::a/pos :default nil}
   :range {:doc "stay within this many blocks of :pos" :spec (a/num-in 0 nil) :default 3}
   :wait-s {:doc "seconds to stay" :spec (a/num-in 0 nil) :default nil}})

(def idle-ms 500)
(def max-blocked 3)

(def target [:pos])

(defn bad-args
  "The cause :bad-args unless a position and a positive :wait-s are given."
  [c]
  (let [{:keys [pos wait-s]} (:args c)]
    (when-not (and (some? pos) (number? wait-s) (pos? wait-s)) (contract/impossible :bad-args))))

(a/defargs settings
  {::time-s {:default 3600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def defaults {:spend {:time ::time-s}})

(def conditions [[:fn bad-args]])

(defn done?
  "Seconds waited in range against :wait-s."
  [c]
  (let [wait-s (:wait-s (:args c))
        done (/ (:waited (ctx/mem c) 0) 1000)]
    {:done? false :units :seconds :done done :left (max 0 (- wait-s done))}))

(defn quote-of [c]
  (let [wait-s (:wait-s (:args c))]
    {:uses {:time [wait-s wait-s (+ wait-s (* 4 wait-s))]}}))

(defn finish!
  "Release the hold and end the call."
  [c kind cause data]
  (ctx/hold-still! c nil)
  (ctx/end c kind cause data))

(defn ^:async walk-back!
  "Walk within range of pos: :continue while the child waits or after an arrival or one failure, else the third
  failure in a row ends the job stopped."
  [c pos range]
  (let [r (await (child/call-walk* c :walk {:pos pos :range range :ways #{:walk :clear-seal}}))
        res (:data r)]
    (cond
      (= :continue r) :continue
      (:arrived res) (do (ctx/update-mem! c assoc :blocked 0) :continue)
      :else
      (let [blocked (inc (:blocked (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :blocked blocked)
        (if (< blocked max-blocked)
          :continue
          (do (ctx/emit! c :linger.unreachable :warn {:text (str "cannot get within " range " of " (pr-str pos))})
              (finish! c :nothing-possible :unreachable {:lingered false :why (when (not= :done (:kind r)) (contract/cause-key r))})))))))

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
      (finish! c :done nil {:lingered true})

      :else
      (do (await (child/pause! c (/ idle-ms 1000) :lingering))
          (ctx/update-mem! c update :waited (fnil + 0) idle-ms)
          :continue))))
