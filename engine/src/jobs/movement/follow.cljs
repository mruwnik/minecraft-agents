(ns jobs.movement.follow
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(def doc
  "Keep the body within :range of the player :player, who must be within :radius to be seen.
  A player in range is looked at (the head) and waited on for 500 ms, then the job yields (:continue): it is an
  ongoing watch. One farther off is walked to in one round (jobs.movement.go-to legs of 5 s, re-aimed at where the
  player is, doors :shut) until in range. A player out of sight is walked to where they were last seen.
  Ends with a result {:reason ...}:
  - \"lost\" (info follow.lost, with :last-seen): the player was out of sight for :lost-s.
  - \"unreachable\" (warn follow.unreachable): three walks in a row go-to gave up on (counted per round).
  - \"out-of-range\" (warn follow.out-of-range): three walks in a row that arrived or got closer but left the body out of range.
  - \"absent\" (info follow.absent): the player was never seen within 2 s of the first round
    (the grace lets the world's entities arrive).
  - \"timeout\": :timeout-s passed. With :timeout-s nil it follows until cancelled.
  Idling is look and wait, neutral for backoff. A cut leaves nothing to undo. The last-seen position stays in job memory.")

(def args
  {:player {:doc "username to follow" :default nil}
   :range {:doc "stay within this many blocks of the player" :default 3}
   :radius {:doc "how far the player may be and still be seen" :default 64}
   :lost-s {:doc "give up after the player has been out of sight this long" :default 10}
   :timeout-s {:doc "stop after this long; nil follows until cancelled" :default nil}})

(def absent-grace-ms 2000)
(def idle-ms 500)
(def walk-leg-s 5)
(def max-blocked 3)
(def head-height 1.6)
(def last-seen-reach 1)

(defn check
  "A player name is given."
  [c]
  (string? (:player (:args c))))

(defn find-player
  "The position {:x :y :z} of player named name within radius, or nil."
  [p name radius]
  (some->> (array-seq (.entities p #js {:radius radius :kind "player"}))
           (filter #(or (= name (.-username %)) (= name (.-name %))))
           first
           .-pos
           u/pos-of))

(defn ^:async idle!
  "A neutral wait, then :continue."
  [c]
  (await (ctx/act c :wait #js {:ms idle-ms}))
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

(defn finish!
  "Hand the parent result and return :done."
  [c result]
  (ctx/result! c result)
  :done)

(defn ^:async walk!
  "Walk to pos within range with go-to calls (:leg-s legs while the player moves, then pos is read again with
  target-now, which returns nil when the target is out of sight). The counts are per call: a walk that did not arrive
  counts as blocked when go-to stopped; the third in a row ends the job (warn follow.unreachable, result
  {:reason \"unreachable\"}); three legs that got closer but left the body out of range of where the target stands now
  end it (warn follow.out-of-range). :continue (in range, out of sight, or go-to waiting) or :done."
  ([c pos range] (walk! c pos range (constantly pos) nil))
  ([c pos range target-now leg-s]
   (loop [pos pos blocked 0 out 0]
     (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to
                                    (cond-> {:pos pos :range range :doors :shut :escalate false :warn false}
                                      leg-s (assoc :leg-s leg-s))))
           res (ctx/child-result c :walk)
           blocked (if (and (= :done r) (or (:arrived res) (:leg res))) 0 (inc blocked))
           now (target-now)
           out (next-out out blocked (u/self-pos c) (or now pos) range)]
       (cond
         (= :continue r) :continue
         (>= out max-blocked)
         (do (ctx/emit! c :follow.out-of-range :warn {:text (str "still out of range of " (:player (:args c)))})
             (finish! c {:reason "out-of-range"}))

         (>= blocked max-blocked)
         (do (ctx/emit! c :follow.unreachable :warn {:text (str "cannot reach " (:player (:args c)))})
             (finish! c {:reason "unreachable"}))

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
      (do (ctx/emit! c :follow.absent :info {:text (str (:player (:args c)) " is not here")})
          (finish! c {:reason "absent"}))

      (>= (- now seen-t) lost-ms)
      (do (ctx/emit! c :follow.lost :info {:text (str "lost " (:player (:args c))) :last-seen last-seen})
          (finish! c {:reason "lost" :last-seen last-seen}))

      (u/within? (u/self-pos c) last-seen last-seen-reach)
      (await (idle! c))

      :else
      (await (walk! c last-seen 1)))))

(defn ^:async round
  "One bounded step: end on timeout, else follow the player seen or the trail."
  [c]
  (let [now (ctx/now c)
        _ (when-not (contains? (ctx/mem c) :started)
            (ctx/update-mem! c assoc :started now))
        {:keys [player radius timeout-s]} (:args c)
        pos (find-player (:primitives c) player radius)]
    (cond
      (and timeout-s (>= (- now (:started (ctx/mem c))) (* 1000 timeout-s)))
      (finish! c {:reason "timeout"})

      pos
      (do (ctx/update-mem! c assoc :last-seen pos :seen-t now)
          (await (follow-seen! c pos)))

      :else
      (await (follow-unseen! c now)))))
