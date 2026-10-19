(ns jobs.movement.follow
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]))

(def doc
  "Keep the body within :range of the player :player, who must be within the radius setting (64) to be seen.
  A player in range is looked at (the head) and waited on for 500 ms, then the job yields (:continue): it is an
  ongoing watch. One farther off is walked to in one round (jobs.movement.go-to calls, re-aimed at where the
  player is, doors :shut) until in range. A player out of sight is walked to where they were last seen, then
  waited for there. A player never seen is waited for beside the body (held as :player-unseen, or :player-out-of-sight at the last-seen cell; a started call, so :wall-time and :job-time bound it).
  Ends with a result {:reason ...}:
  - \"unreachable\": a walk go-to gave up on (it retries itself).
  - \"out-of-range\": three walks in a row that arrived or got closer but left the body out of range.
  - The engine ends it :limit-reached when :spend :job-time (or :wall-time) is used up, with {:met} (the player was seen). Nothing to undo then.
  Idling is look and wait, neutral for backoff. A cut leaves nothing to undo. The last-seen position stays in job memory.")

(a/defargs args
  {:player {:doc "username to follow" :spec a/name? :default nil}
   :range {:doc "stay within this many blocks of the player" :spec (a/num-in 0 nil) :default 3}})

(def target [:player])

(def idle-ms 500)
(def max-blocked "One go-to give-up ends the job: go-to already retries." 1)
(def max-out 3)
(def head-height 1.6)
(def last-seen-reach 1)

(def bad-args
  "Never :bad-args unless a player name is given."
  (checks/args-fit "no player name" #(string? (:player (:args %)))))

(a/defargs settings
  {::radius {:default 64 :doc "How far the player may be and still be seen, in blocks." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 3600 :doc "Seconds one call may take before it gives up (:limit-reached), pauses not counted." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the idle pauses beside the player included (:limit-reached)." :spec (a/num-in 0 nil)}})

(defn radius [] (settings/get settings ::radius))

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)

(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn find-player
  "The position {:x :y :z} of player named name within the given distance, or nil."
  [p name within]
  (some->> (array-seq (.entities p #js {:radius within :kind "player"}))
           (filter #(or (= name (.-username %)) (= name (.-name %))))
           first
           .-pos
           u/pos-of))

(def can-start? bad-args)

(defn ^:async idle!
  "A noop pause held for why, then :continue."
  [c why]
  (await (child/pause! c (/ idle-ms 1000) why))
  :continue)

(defn ^:async look-at-head!
  "Look at the head of a player standing at pos."
  [c pos]
  (await (ctx/act c :look (clj->js {:pos (update pos :y + head-height)}))))

(defn next-out
  "The out-of-range count after a walk: reset when blocked or when the body is within range of where the target
  stands now, else one more."
  [out blocked self now range]
  (if (or (pos? blocked) (u/within? self now range)) 0 (inc out)))

(defn time-limit "Seconds the call may follow, or nil." [c]
  (get-in (ctx/frozen-call c) [:spend :job-time]))

(defn done?
  "Never done: a follow ends with a cause. Counts the player as met once seen."
  [c]
  (let [met (some? (:seen-t (ctx/mem c)))]
    (contract/later :following (if met "following the player" "no player met yet")
                    :units :players :done (if met 1 0) :left (if met 0 1) :result {:met met})))

(defn quote-of [c]
  (let [most (or (time-limit c) ##Inf)
        now (find-player (:primitives c) (:player (:args c)) (radius))]
    (cond-> {:uses {:job-time [0 (min 60 most) most]}}
      now (assoc :moves-to [(:x now) (:y now) (:z now)]))))

(defn finish!
  "End the call: kind and cause, data the old result fields."
  [c kind cause & [data]]
  (ctx/end c kind cause data))

(defn ^:async walk!
  "Walk to pos within range with go-to calls (then pos is read again with
  target-now, which returns nil when the target is out of sight). The counts are per call: a walk that did not arrive
  counts as blocked when go-to stopped; the first ends the job (result
  {:reason \"unreachable\"}); three legs that got closer but left the body out of range of where the target stands now
  end it. :continue (in range, out of sight, or go-to waiting) or :done."
  ([c pos range] (walk! c pos range (constantly pos)))
  ([c pos range target-now]
   (loop [pos pos blocked 0 out 0]
     (let [r (await (child/call-walk* c :walk
                                     {:pos pos :range range :doors :shut :ways #{:walk :clear-seal}}))
           res (:data r)
           blocked (if (:arrived res) 0 (inc blocked))
           now (target-now)
           out (next-out out blocked (u/self-pos c) (or now pos) range)]
       (cond
         (= :continue r) :continue
         (>= out max-out)
         (finish! c :nothing-possible :out-of-range {:text (str "still out of range of " (:player (:args c)))})

         (>= blocked max-blocked)
         (finish! c :nothing-possible :unreachable {:text (str "cannot reach " (:player (:args c)))})

         (or (nil? now) (u/within? (u/self-pos c) now range)) :continue
         :else (recur now blocked out))))))

(defn ^:async follow-seen!
  "The player stands at pos: look and wait when in range, else walk."
  [c pos]
  (if (u/within? (u/self-pos c) pos (:range (:args c)))
    (do (await (look-at-head! c pos))
        (await (idle! c :following)))
    (await (walk! c pos (:range (:args c))
                  #(find-player (:primitives c) (:player (:args c)) (radius))))))

(defn ^:async follow-unseen!
  "The player is out of sight, seen last at last-seen: walk there, then wait."
  [c]
  (let [{:keys [last-seen]} (ctx/mem c)]
    (if (or (nil? last-seen) (u/within? (u/self-pos c) last-seen last-seen-reach))
      (await (idle! c (if last-seen :player-out-of-sight :player-unseen)))
      (await (walk! c last-seen 1)))))

(defn ^:async round
  "One bounded step: follow the player seen or the trail."
  [c]
  (let [player (:player (:args c))
        pos (find-player (:primitives c) player (radius))]
    (if pos
      (do (ctx/update-mem! c assoc :last-seen pos :seen-t (ctx/now c))
          (await (follow-seen! c pos)))
      (await (follow-unseen! c)))))
