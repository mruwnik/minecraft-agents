(ns engine.triggers.stuck
  "The stuck trigger and the condition the unstick job shares with it. Both
  read the :moved entries: engine.core/act writes one after every moveTo, engine.path.near one after every walk (go-to and
  walk-near!). Only a move that did not carry the body anywhere is bad: a walk that took the body away, whatever its
  status (a partial path that ended farther from the goal), shows the body is free. An entry with :no-path true is a walk
  that found no way to its goal: evidence about the goal, not the body, so the trigger counts it only when the body is
  enclosed (engine.jobs.reach/enclosed?). Bad moves made somewhere else (the body has been carried, teleported or has
  fallen away since) do not hold it where it stands."
  (:require [engine.jobs.reach :as reach]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def defaults
  {:n 4 :min-move 1.5 :window-ms 60000 :quiet-ms 300000})

(def moved-policy
  "Policy of the :moved entries, written by engine.core/act after each moveTo and by engine.path.near after each walk (go-to, walk-near!)."
  {:cap 20 :ttl (* 10 60 1000)})

(defn dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn bad-move?
  "A :moved entry's data is a bad move when the body moved less than min-move blocks, whatever the status: a walk that
  carried the body farther (even one that ended blocked, farther from its goal) did not find it held. A move that asked
  for a spot within min-move of where the body already stood is a no-op, not bad."
  [min-move {:keys [from to target]}]
  (and (< (dist from to) min-move)
       (or (nil? target) (>= (dist from target) min-move))))

(defn stuck?
  "True when the last :n :moved entries are all bad moves and the newest of
  them is less than :window-ms old (the body is still trying; a real moveTo
  that makes no progress runs its whole 20 s time bound, so four of them span
  60 s or more and the oldest would always be out of the window). False with
  fewer than :n entries. Moves written before the latest :stuck entry (a
  failed unstick) are not counted, so a body the job gave up on needs :n fresh
  bad moves to fire again, and not even then until that entry is :quiet-ms
  old. Moves from before the latest restart are evidence about a previous
  process and do not count."
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
  "stuck?, and the body is really held where it stands (held-here?, when p is given): some of the bad moves had a way and
  still did not move the body, or (every one found no path) the body of primitives p is enclosed. Walks that found no
  path while the body has room around it are a goal out of reach (a target across water, a raw pathfinder that takes a
  door for a wall), not a stuck body."
  [p view args]
  (let [moves (counted-moves view args)]
    (and (stuck? view args)
         (or (nil? p) (held-here? p moves (:min-move (merge defaults args))))
         (or (not-every? :no-path moves)
             (and (some? p) (reach/enclosed? p))))))

(def stuck
  "Holds when body-stuck? does, with :n, :min-move, :window-ms and :quiet-ms from
  the args. Moves from before the latest restart do not count. The job it starts is (jobs.maintenance.unstick); see its doc for
  how the two interact. After a give-up the :stuck entry silences the trigger
  for :quiet-ms (5 min), so a body still blocked under the resumed job does
  not re-fire it every cooldown; the 60 s cooldown only covers the spell
  that ended well."
  {:name :stuck
   :when (fn [world memory args] (body-stuck? world memory args))
   :job '(jobs.maintenance.unstick)
   :args defaults
   :persistence :cooldown
   :cooldown-s 60})
