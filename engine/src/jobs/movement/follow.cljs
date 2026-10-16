(ns jobs.movement.follow
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]))

(def doc
  "Keep the body within :range of the player :player, who must be within :radius to be seen.
  A player in range is looked at (the head) and waited on for 500 ms, then the job yields (:continue): it is an
  ongoing watch. One farther off is walked to in one round (jobs.movement.go-to legs of 5 s, re-aimed at where the
  player is, doors :shut) until in range. A player out of sight is walked to where they were last seen.
  Ends with a result {:reason ...}:
  - \"lost\" (with :last-seen): the player was out of sight for :lost-s.
  - \"unreachable\": a walk go-to gave up on (it retries itself).
  - \"out-of-range\": three walks in a row that arrived or got closer but left the body out of range.
  - \"absent\": the player was never seen within 2 s of the first round
    (the grace lets the world's entities arrive).
  - The engine ends it :limit-reached when :spend :job-time is used up, with {:met} (the player was seen). Nothing to undo then.
  Idling is look and wait, neutral for backoff. A cut leaves nothing to undo. The last-seen position stays in job memory.")

(a/defargs args
  {:player {:doc "username to follow" :spec a/name? :default nil}
   :range {:doc "stay within this many blocks of the player" :spec (a/num-in 0 nil) :default 3}
   :radius {:doc "how far the player may be and still be seen" :spec (a/num-in 0 nil) :default 64}
   :lost-s {:doc "give up after the player has been out of sight this long" :spec (a/num-in 0 nil) :default 10}})

(def target [:player])

(def absent-grace-ms 2000)
(def idle-ms 500)
(def walk-leg-s 5)
(def max-blocked "One go-to give-up ends the job: go-to already retries." 1)
(def max-out 3)
(def head-height 1.6)
(def last-seen-reach 1)

(def bad-args
  "Never :bad-args unless a player name is given."
  (checks/args-fit "no player name" #(string? (:player (:args %)))))

(a/defargs settings
  {::job-time-s {:default 3600 :doc "Seconds one call may take before it gives up (:limit-reached), pauses not counted." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the idle pauses beside the player included (:limit-reached)." :spec (a/num-in 0 nil)}})

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)

(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(def can-start? bad-args)

(defn find-player
  "The position {:x :y :z} of player named name within radius, or nil."
  [p name radius]
  (some->> (array-seq (.entities p #js {:radius radius :kind "player"}))
           (filter #(or (= name (.-username %)) (= name (.-name %))))
           first
           .-pos
           u/pos-of))

(defn ^:async idle!
  "A noop pause, then :continue."
  [c]
  (await (child/pause! c (/ idle-ms 1000) :following))
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
        now (find-player (:primitives c) (:player (:args c)) (:radius (:args c)))]
    (cond-> {:uses {:job-time [0 (min 60 most) most]}}
      now (assoc :moves-to [(:x now) (:y now) (:z now)]))))

(defn finish!
  "End the call: kind and cause, data the old result fields."
  [c kind cause & [data]]
  (ctx/end c kind cause data))

(defn ^:async walk!
  "Walk to pos within range with go-to calls (legs of leg-s seconds while the player moves, then pos is read again with
  target-now, which returns nil when the target is out of sight). The counts are per call: a walk that did not arrive
  counts as blocked when go-to stopped; the first ends the job (result
  {:reason \"unreachable\"}); three legs that got closer but left the body out of range of where the target stands now
  end it. :continue (in range, out of sight, or go-to waiting) or :done."
  ([c pos range] (walk! c pos range (constantly pos) nil))
  ([c pos range target-now leg-s]
   (loop [pos pos blocked 0 out 0]
     (let [r (await (child/call-walk* c :walk
                                     (cond-> {:pos pos :range range :doors :shut :ways #{:walk :clear-seal} :warn false}
                                       leg-s (assoc-in [:spend :job-time] leg-s))))
           res (:data r)
           blocked (if (or (:arrived res) (:leg res)) 0 (inc blocked))
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
        (await (idle! c)))
    (await (walk! c pos (:range (:args c))
                  #(find-player (:primitives c) (:player (:args c)) (:radius (:args c)))
                  walk-leg-s))))

(defn ^:async follow-unseen!
  "The player is out of sight at now, seen last at seen-t at last-seen."
  [c now]
  (let [{:keys [started last-seen seen-t]} (ctx/mem c)
        lost-ms (* 1000 (:lost-s (:args c)))]
    (cond
      (and (nil? seen-t) (< (- now started) absent-grace-ms))
      (await (idle! c))

      (nil? seen-t)
      (finish! c :condition-failed :absent {:text (str (:player (:args c)) " is not here")})

      (>= (- now seen-t) lost-ms)
      (finish! c :nothing-possible :lost {:text (str "lost " (:player (:args c))) :last-seen last-seen})

      (u/within? (u/self-pos c) last-seen last-seen-reach)
      (await (idle! c))

      :else
      (await (walk! c last-seen 1)))))

(defn ^:async round
  "One bounded step: follow the player seen or the trail."
  [c]
  (let [now (ctx/now c)
        _ (when-not (contains? (ctx/mem c) :started)
            (ctx/update-mem! c assoc :started now))
        {:keys [player radius]} (:args c)
        pos (find-player (:primitives c) player radius)]
    (cond
      pos
      (do (ctx/update-mem! c assoc :last-seen pos :seen-t now)
          (await (follow-seen! c pos)))

      :else
      (await (follow-unseen! c now)))))
