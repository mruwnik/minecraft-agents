(ns jobs.movement.pace
  (:require [engine.ctx :as ctx]))

(def doc
  "Walk a, b, a, b (:laps times each), one jobs.movement.go-to call per leg (doors :shut). One call is the whole run.
  Ends stopped :leg-unfinished (warn :leg-unfinished with the target and go-to's reason) when a leg does not arrive,
  and yields (:continue) while go-to waits on the world.
  Points are [x y z] or {:x :y :z}; a bad one is refused at submit (:type :pos).
  A harmless long job, for showing that a reflex cuts a running one.")

(def args
  {:a {:doc "first point" :type :pos :default nil}
   :b {:doc "second point" :type :pos :default nil}
   :laps {:doc "a-b laps" :default 24}
   :range {:doc "walk range" :default 1}})

(defn check [_c] true)

(defn ^:async walk-legs
  "Walk the legs in order; nil when all arrived, :continue when go-to yielded, else the first leg's pos and reason."
  [c range legs]
  (loop [legs legs]
    (when-let [pos (first legs)]
      (let [r (await (ctx/call-child c :leg 'jobs.movement.go-to {:pos pos :range range :escalate false}))]
        (cond
          (= :continue r) :continue
          (and (= :done r) (:arrived (ctx/child-result c :leg))) (recur (rest legs))
          :else [pos (or (:reason (ctx/child-result c :leg)) :unreachable)])))))

(defn ^:async round [c]
  (let [{:keys [a b laps range]} (:args c)
        unfinished (await (walk-legs c range (take (* 2 laps) (cycle [a b]))))]
    (cond
      (= :continue unfinished) :continue
      (nil? unfinished) :done
      :else (let [[pos reason] unfinished]
              (ctx/emit! c :leg-unfinished :warn {:to pos :status (name reason)
                                                  :text (str "a leg to " pos " did not arrive: " (name reason))})
              (ctx/result! c {:status :stopped :reason :leg-unfinished :to pos :why reason})
              :done))))
