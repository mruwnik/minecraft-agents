(ns triggers.survival.stuck
  "The stuck trigger and the condition the unstick job shares with it.
  Both read the :moved entries: engine.core/act writes one after every moveTo, jobs.lib.near one after every walk
  (go-to and walk-near!).
    A move is bad only when it did not carry the body anywhere. A walk that took the body away, whatever its status,
    shows the body is free.
    An entry with :no-path true is a walk that found no way to its goal. That is evidence about the goal, not the
    body, so it counts only when the body is enclosed (jobs.lib.reach/enclosed?).
    Bad moves made somewhere else (the body was carried, teleported or fell away since) do not hold it here."
  (:require [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [triggers.survival.wedged :as wedged]
            [engine.memory :as mem]))

(def defaults
  {:n 4 :min-move 1.5 :window-ms 60000 :quiet-ms 300000})

(defn dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn bad-move?
  "Whether a :moved entry's data is a bad move: the body moved less than min-move blocks, whatever the status.
  A move that asked for a spot within min-move of where the body already stood is a no-op, not bad."
  [min-move {:keys [from to target]}]
  (and (< (dist from to) min-move)
       (or (nil? target) (>= (dist from target) min-move))))

(defn stuck?
  "True when the last :n :moved entries are all bad moves and the newest is less than :window-ms old.
  Only the newest is checked against the window: a moveTo that makes no progress runs its whole 20 s bound, so four
  of them span 60 s or more and the oldest would always be out.
  False with fewer than :n entries.
  Moves before the latest :stuck entry (a failed unstick) or the latest restart are not counted.
  So a body the job gave up on needs :n fresh bad moves to fire again, and not before that entry is :quiet-ms old."
  [view args]
  (let [{:keys [n min-move window-ms quiet-ms]} (merge defaults args)
        gave-up (or (:t (mem/latest view :stuck)) 0)
        restarted (or (:t (mem/latest view :restart)) 0)
        counted? (fn [{:keys [t]}] (and (> t gave-up) (>= t restarted)))
        last-n (take-last n (filter counted? (mem/entries view :moved)))]
    (and (>= (- (:now view) gave-up) quiet-ms)
         (= n (count last-n))
         (< (- (:now view) (:t (last last-n))) window-ms)
         (every? #(bad-move? min-move (:data %)) last-n))))

(defn counted-moves
  "The last :n :moved entries' data that stuck? reads (moves after the latest :stuck and restart)."
  [view args]
  (let [{:keys [n]} (merge defaults args)
        gave-up (or (:t (mem/latest view :stuck)) 0)
        restarted (or (:t (mem/latest view :restart)) 0)]
    (mapv :data (take-last n (filter (fn [{:keys [t]}] (and (> t gave-up) (>= t restarted))) (mem/entries view :moved))))))

(defn held-here?
  "Whether every one of moves ended within (* 2 min-move) blocks of where the body of primitives p stands now: bad moves
  made somewhere else (before a fall, a teleport, a knock-back) say nothing about this spot."
  [p moves min-move]
  (let [here (u/self-pos {:primitives p})]
    (every? #(< (dist (:to %) here) (* 2 min-move)) moves)))

(defn body-stuck?
  "stuck?, and the body is really held where it stands (held-here?, when p is given).
  Held means some bad move had a way and still did not move the body, or every one found no path and the body is
  enclosed. No-path walks while the body has room around it mean a goal out of reach, not a stuck body."
  [p view args]
  (let [moves (counted-moves view args)]
    (and (stuck? view args)
         (or (nil? p) (nil? (wedged/wedged-cell p)))
         (or (nil? p) (held-here? p moves (:min-move (merge defaults args))))
         (or (not-every? :no-path moves)
             (and (some? p) (reach/enclosed? p))))))

(defn stuck
  "Holds when body-stuck? does, with :n, :min-move, :window-ms and :quiet-ms from the args.
  Starts (jobs.maintenance.unstick); see its doc for how the two interact.
  After a give-up the :stuck entry silences the trigger for :quiet-ms (5 min). The 60 s cooldown covers only a spell
  that ended well."
  [world memory args]
  (body-stuck? world memory args))
