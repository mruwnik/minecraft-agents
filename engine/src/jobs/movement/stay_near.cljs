(ns jobs.movement.stay-near
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.places :as places]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]))

(def idle-ok true)

(def doc
  "Stay within :range of a player (:player, who must be within the radius setting (64) to be seen) or of a cell (:pos) for
  :for-s seconds, or until cancelled when :for-s is nil. For a wait that needs the body near (leaf decay before the drops
  despawn, crop growth, a birth) or a companion walk.
  Each round: within range it looks at the player's head, pauses 500 ms (a noop child holding :following for a player,
  :lingering for a cell) and yields :continue; out of range (pushed away, the player moved, or at the start) it walks with
  jobs.movement.chase legs (re-aimed at the player, doors :shut, ways #{:walk :dig}, :may-dig #{:own}). A player out of sight is walked to
  where they were last seen, then waited for there; one never seen is waited for beside the body (held as :player-unseen,
  or :player-out-of-sight at the last-seen cell).
  Only time held in range counts toward :for-s (a pause that ended done adds its 500 ms, kept in job memory): walking, a cut
  pause and time the job was preempted add nothing, and a resume checks the range again before it can end.
  Ends with a result:
  - {:stayed true :reason \"waited\"} (done) when :for-s of time in range has been waited, with the body in range.
  - nothing-possible \"unreachable\": a walk go-to gave up on (its ending is the :because).
  - nothing-possible \"out-of-range\": three legs in a row that arrived or got closer but left the body out of range of a moving player.
  - The engine ends it :limit-reached when :spend :job-time (or :wall-time) is used up. Nothing to undo then.
  A cut leaves nothing to undo. Quote: the wait plus a walk; most is the job-time budget, since being pushed out can repeat the walk.")

(a/defargs args
  {:player {:doc "username to stay near" :spec a/name? :default nil}
   :pos {:doc "the place to stay near, instead of :player" :spec ::a/pos :default nil}
   :range {:doc "stay within this many blocks" :spec (a/num-in 0 nil) :default 3}
   :for-s {:doc "seconds to stay in range; nil: until cancelled" :spec (a/num-in 0 nil) :default nil}})

(def target [:pos :player])

(def bad-args
  "Never :bad-args unless exactly one of a player name and a position is given and :for-s, when given, is positive."
  (checks/args-fit "needs a player name or a position (one of them), and a positive :for-s when given"
                   (fn [c] (let [{:keys [player pos for-s]} (:args c)]
                             (and (not= (some? player) (some? pos))
                                  (or (nil? player) (string? player))
                                  (or (nil? for-s) (and (number? for-s) (pos? for-s))))))))

(def can-start? bad-args)

(a/defargs settings
  {::idle-ms {:default 500 :doc "Milliseconds one pause beside the target lasts before the round yields." :spec (a/num-in 1 nil)}
   ::last-seen-reach {:default 1 :doc "How close, in blocks, the body walks to where a vanished player was last seen." :spec (a/num-in 0 nil)}
   ::max-out-walks {:default 3 :doc "Legs in a row that arrived or got closer but left the body out of range before the call gives up (:out-of-range)." :spec (a/num-in 1 nil)}
   ::radius {:default 64 :doc "How far the player may be and still be seen, in blocks." :spec (a/num-in 0 nil)}
   ::leg-s {:default 15 :doc "Seconds one walk leg may take: a moving player is aimed at again after each." :spec (a/num-in 0.1 120)}
   ::job-time-s {:default 3600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the idle pauses beside the target included (:limit-reached)." :spec (a/num-in 0 nil)}})

(defn budget-s [] (settings/get settings ::job-time-s))
(defn radius [] (settings/get settings ::radius))
(defn max-out [] (settings/get settings ::max-out-walks))
(defn idle-ms [] (settings/get settings ::idle-ms))
(defn last-seen-reach [] (settings/get settings ::last-seen-reach))
(defn leg-s [] (settings/get settings ::leg-s))

(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(def safe? (ready/safe-at-or-if-lit (fn [c] (some-> (:pos (:args c)) a/cell))))
(def fit? ready/fit?)

(defn find-player
  "The position {:x :y :z} of player named name within the given distance, or nil."
  [p name within]
  (some->> (array-seq (.entities p #js {:radius within :kind "player"}))
           (filter #(or (= name (.-username %)) (= name (.-name %))))
           first
           .-pos
           u/pos-of))

(defn target-now
  "Where the thing to stay near is: the :pos, or the player's position when seen, else nil."
  [c]
  (let [{:keys [player pos]} (:args c)]
    (if player (find-player (:primitives c) player (radius)) (:pos (places/parse-pos pos)))))

(defn hold-reason [c] (if (:player (:args c)) :following :lingering))

(defn in-range? [c pos] (u/within? (u/self-pos c) pos (:range (:args c))))

(defn waited-ms [c] (:waited (ctx/mem c) 0))

(defn done?
  "Done once :for-s of time is waited with the body in range and no pause under way; never done without :for-s."
  [c]
  (let [{:keys [for-s]} (:args c)
        at (target-now c)
        met (some? (or (:seen-t (ctx/mem c)) (:pos (:args c))))
        waited (/ (waited-ms c) 1000)]
    (cond
      (nil? for-s) (contract/later :staying (if met "staying near" "not near yet") :units :players :done (if met 1 0) :left (if met 0 1) :result {:met met})
      (and (>= waited for-s) at (in-range? c at) (not (checks/child-under-way? c)))
      (contract/ok :waited (str "waited " for-s " s in range") :units :seconds :done waited :left 0 :result {:stayed true :reason "waited"})
      :else (contract/later :staying (str "waited " waited " of " for-s " s") :units :seconds :done waited :left (max 0 (- for-s waited))
                            :result {:stayed true :reason "waited"}))))

(defn quote-of
  "The wait, and the walk to the target by straight distance (4 blocks a second, up to 3 times that when pushed out again)
  when the body is out of range."
  [c]
  (let [{:keys [for-s range]} (:args c)
        at (target-now c)
        wait (or for-s 0)
        d (if at (max 0 (- (u/dist (u/self-pos c) at) range)) 0)]
    (cond-> {:uses {:job-time [(+ wait (/ d 6)) (+ wait (/ d 4)) (max (+ wait (* 3 (/ d 4)) 10) (budget-s))]}}
      at (assoc :moves-to [(:x at) (:y at) (:z at)]))))

(defn finish!
  "Release the hold and end the call."
  [c kind cause data]
  (ctx/hold-still! c nil)
  (ctx/end c kind cause data))

(defn next-out
  "The out-of-range count after a leg: reset when it was blocked or when the body is within range of where the target
  stands now, else one more."
  [out blocked self now range]
  (if (or (pos? blocked) (u/within? self now range)) 0 (inc out)))

(defn ^:async walk!
  "Walk legs toward pos (re-aimed at the player each leg) until within range or the first leg that is neither an arrival
  nor nearer; three legs that left the body out of range of the target end the job out-of-range. :continue or an ending."
  [c pos range]
  (loop [blocked 0 out 0]
    (let [{:keys [player]} (:args c)
          r (await (child/call-chase* c :walk (cond-> {:pos pos :range range :leg-s (leg-s) :walk {:doors :shut :ways #{:walk :dig} :may-dig #{:own}}}
                                                 player (assoc :player player))))
          res (:data r)
          blocked (if (or (:arrived res) (:nearer res)) 0 (inc blocked))
          now (target-now c)
          out (next-out out blocked (u/self-pos c) (or now pos) range)]
      (cond
        (= :continue r) :continue
        (>= out (max-out)) (finish! c :nothing-possible :out-of-range {:stayed false :text (str "still out of range of " (or player (pr-str pos)))})
        (pos? blocked) (finish! c :nothing-possible :unreachable {:stayed false :text (str "cannot get within " range " of " (or player (pr-str pos)))})
        (or (nil? now) (in-range? c now)) :continue
        :else (recur blocked out)))))

(defn ^:async stay!
  "In range of pos: look at a player, pause for the idle time, count it, yield."
  [c pos]
  (ctx/hold-still! c nil)
  (when (:player (:args c))
    (await (ctx/act c :look (clj->js {:pos (update pos :y + game/eye-height)}))))
  (let [r (await (child/pause! c (/ (idle-ms) 1000) (hold-reason c)))]
    (when (= :done (:kind r))
      (ctx/update-mem! c update :waited (fnil + 0) (idle-ms)))
    :continue))

(defn ^:async wait-unseen!
  "The player is out of sight, last seen at last-seen: walk there, then wait."
  [c]
  (let [{:keys [last-seen]} (ctx/mem c)]
    (if (or (nil? last-seen) (u/within? (u/self-pos c) last-seen (last-seen-reach)))
      (do (await (child/pause! c (/ (idle-ms) 1000) (if last-seen :player-out-of-sight :player-unseen)))
          :continue)
      (await (walk! c last-seen (last-seen-reach))))))

(defn ^:async round
  "One bounded step: end when the wait is over, else stay beside the target, walk to it, or wait for a player out of sight."
  [c]
  (let [{:keys [for-s player]} (:args c)
        at (target-now c)]
    (when (and player at) (ctx/update-mem! c assoc :last-seen at :seen-t (ctx/now c)))
    (cond
      (and for-s (>= (waited-ms c) (* 1000 for-s)) at (in-range? c at)) (finish! c :done nil {:stayed true :reason "waited"})
      (nil? at) (await (wait-unseen! c))
      (in-range? c at) (await (stay! c at))
      :else (do (ctx/hold-still! c nil)
                (await (walk! c at (:range (:args c))))))))
