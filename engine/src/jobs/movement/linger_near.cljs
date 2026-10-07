(ns jobs.movement.linger-near
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(def doc
  "Stay within :range of :pos for :wait-s seconds, for a wait that needs the body near (leaf decay before the drops
  despawn, crop growth, a birth). Needs :pos and a positive :wait-s (else declined at submit).
  Each round: within range it holds still (declared hold :lingering) for 500 ms and yields :continue; out of range
  (pushed away, or at the start) it walks back with a go-to child (:escalate false).
  Ends with a result:
  - {:lingered true :reason \"waited\"} when :wait-s has passed.
  - {:status :stopped :lingered false :reason :unreachable :why <go-to's reason>} (warn linger.unreachable) when go-to
    does not arrive three times in a row.
  The wait starts at the first round and is kept in job memory. A cut leaves nothing to undo; a caller whose wait
  ends early cuts the job.")

(def args
  {:pos {:doc "the place to stay near" :type :pos :default nil}
   :range {:doc "stay within this many blocks of :pos" :default 3}
   :wait-s {:doc "seconds to stay" :default nil}})

(def idle-ms 500)
(def max-blocked 3)

(defn check
  "A position and a positive :wait-s are given."
  [c]
  (let [{:keys [pos wait-s]} (:args c)]
    (and (some? pos) (number? wait-s) (pos? wait-s))))

(defn finish!
  [c result]
  (ctx/hold-still! c nil)
  (ctx/result! c result)
  :done)

(defn ^:async walk-back!
  "Walk within range of pos: :continue while the child waits or after an arrival or one failure, else the third
  failure in a row ends the job stopped."
  [c pos range]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range range :escalate false}))
        res (ctx/child-result c :walk)]
    (cond
      (= :continue r) :continue
      (:arrived res) (do (ctx/update-mem! c assoc :blocked 0) :continue)
      :else
      (let [blocked (inc (:blocked (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :blocked blocked)
        (if (< blocked max-blocked)
          :continue
          (do (ctx/emit! c :linger.unreachable :warn {:text (str "cannot get within " range " of " (pr-str pos))})
              (finish! c {:status :stopped :lingered false :reason :unreachable :why (:reason res)})))))))

(defn ^:async round
  "One bounded step: end when the wait is over, hold still in range, else walk back."
  [c]
  (let [now (ctx/now c)
        _ (when-not (contains? (ctx/mem c) :started)
            (ctx/update-mem! c assoc :started now))
        {:keys [pos range wait-s]} (:args c)]
    (cond
      (>= (- now (:started (ctx/mem c))) (* 1000 wait-s))
      (finish! c {:lingered true :reason "waited"})

      (u/within? (u/self-pos c) pos range)
      (do (ctx/hold-still! c :lingering)
          (await (ctx/act c :wait #js {:ms idle-ms}))
          :continue)

      :else
      (do (ctx/hold-still! c nil)
          (await (walk-back! c pos range))))))
