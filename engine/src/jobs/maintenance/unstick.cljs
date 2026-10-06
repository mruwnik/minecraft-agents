(ns jobs.maintenance.unstick
  (:require [engine.ctx :as ctx]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [triggers.survival.stuck :as stuck]))

(def doc
  "Get a body out of being stuck: a job kept calling moveTo and the body got nowhere (the stuck trigger).
  - It waits (50 ms steps, up to 1 s) for the body to land.
  - It walks to the goal of the latest :moved entry with jobs.movement.go-to (range 1, :escalate true), one go-to
    call. go-to opens doors, and when the body is shut in it pillars, stairs or digs a door out and
    puts back what it dug.
  - Done when go-to arrives, or when the body was shut in at the start and is not any more (it is out, though
    the goal may stay out of reach).
  - Else it gives up (ends stopped with the reason): warn unstick.failed {:pos :why :escalation :text}, a :stuck memory entry (cap 10, ttl 1 hour)
    that keeps the trigger quiet for :quiet-ms, and it ends so the job list resumes. With no goal in the latest
    :moved entry it gives up at once (:why :no-goal).
  The check is the stuck trigger's condition (triggers.survival.stuck), or a spell already begun.")

(def args
  {:n {:doc "bad moves in a row that count as stuck" :default (:n stuck/defaults)}
   :min-move {:doc "blocks a move must cover to count as progress" :default (:min-move stuck/defaults)}
   :window-ms {:doc "the newest of the bad moves must be at most this many ms old" :default (:window-ms stuck/defaults)}
   :quiet-ms {:doc "after giving up, the trigger stays quiet this many ms" :default (:quiet-ms stuck/defaults)}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to go-to's escalation); the rules of the game allow it" :default false}})

(def stuck-policy {:cap 10 :ttl (* 60 60 1000)})

(def land-step-ms 50)

(def land-max-steps 20)

(defn on-ground?
  "False only when the body says it is airborne (a missing key counts as on the ground)."
  [c]
  (not (false? (.-onGround (.self (:primitives c))))))

(defn ^:async land!
  "Wait in short steps until the body is on the ground, up to land-max-steps."
  [c]
  (loop [i 0]
    (when (and (< i land-max-steps) (not (on-ground? c)))
      (await (ctx/act c :wait (clj->js {:ms land-step-ms})))
      (recur (inc i)))))

(defn goal
  "The target of the latest :moved entry: where the stuck job was going."
  [c]
  (:target (:data (ctx/latest c :moved))))

(defn give-up! [c fields]
  (let [pos (u/self-pos c)]
    (ctx/emit! c :unstick.failed :warn (merge {:pos pos :text (str "still stuck: " (name (:why fields)))} fields))
    (ctx/remember! c :stuck {:pos pos} stuck-policy)
    (result/stop! c (:why fields) (str "still stuck: " (name (:why fields))))))

(defn check [c]
  (or (contains? (ctx/mem c) :goal)
      (stuck/stuck? (ctx/view c) (:args c))))

(defn ^:async round [c]
  (when-not (contains? (ctx/mem c) :goal)
    (await (land! c))
    (ctx/update-mem! c assoc :goal (goal c) :enclosed (reach/enclosed? (:primitives c))))
  (let [{:keys [goal enclosed]} (ctx/mem c)]
    (if-not goal
      (give-up! c {:why :no-goal})
      (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos goal :range 1 :escalate true
                                                          :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
            res (ctx/child-result c :go)]
        (cond
          (:arrived res) :done
          (and enclosed (not (reach/enclosed? (:primitives c)))) :done
          :else (give-up! c (merge {:why (if (= :continue r) :yielded (or (:why res) (:reason res) :declined))}
                                   (select-keys res [:escalation :kind :detail]))))))))
