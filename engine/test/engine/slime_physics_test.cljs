(ns engine.slime-physics-test
  "A planned drop onto a slime pad walked in prismarine-physics by the executor: the body bounces on the pad (the client
  reverses its fall there), and the executor waits on the drop step until the bounce settles, so the bounce is never off
  the plan."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.executor :as ex]
            [engine.path.planner-tuned :as planner]
            [engine.stairs-physics-test :as sp]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]))

(def max-ticks 1200)

;; a stone tower 10 high whose east face drops onto a 3x3 slime pad, then stone ground on east to the goal
(def tower-over-slime
  [[0 64 -2 4 73 2 "stone" {}]
   [5 63 -1 7 63 1 "slime_block" {}]
   [8 63 -1 14 63 1 "stone" {}]])

(defn plan
  "The executor's steps from feet cell from to goal over fills (slime priced as a bounce), or nil."
  [fills [x y z] [gx gy gz]]
  (let [pw (sp/path-world fills)
        r (planner/plan (.-snapshot pw)
                        #js {:from #js {:x x :y y :z z :px (+ x 0.5) :pz (+ z 0.5)}
                             :goal #js {:kind "near" :x gx :y gy :z gz :range 0}}
                        #js {:table (.-table pw) :space (.-space pw) :weight 1.2 :maxDrop 12 :landing (cost-planner/planner-landing nil)
                             :limits (ex/planner-limits ex/policy (wworld/solid-fn pw))})]
    (when (= "found" (.-status r))
      (wplan/plan-steps pw r))))

(defn walk
  "Walk the plan in physics: {:done :steps :ys}, the executor's done map, the planned steps and the feet heights walked."
  [fills from goal]
  (let [{:keys [mc physics]} @sp/lib
        world (sp/world-of fills)
        phys ((.-Physics physics) mc world)
        _ (set! (.-playerHalfWidth phys) 0.31)
        steps (plan fills from goal)
        bot (sp/bot-at from)]
    (loop [t 0 state (ex/start steps 0) ys []]
      (let [pose (sp/pose-of bot)
            {:keys [done controls yaw] :as r} (ex/tick ex/policy state pose)]
        (if (or done (>= t max-ticks))
          {:done done :steps steps :ys ys}
          (do (set! (.-yaw (.-entity bot)) yaw)
              (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js controls)) world) bot)
              (recur (inc t) (:state r) (conj ys (:y pose)))))))))

(deftest a-planned-slime-drop-is-walked-through-its-bounce
  (let [{:keys [done steps ys]} (walk tower-over-slime [2 74 0] [12 64 0])
        d (first (filter #(= :drop (:move %)) steps))]
    (is (true? (:bounce d)) (pr-str steps))
    (is (= 6 (count (:pad d))) "the cliff-foot pad: every slime cell round the landing")
    (is (> (apply max (drop-while #(> % 64.5) ys)) 67) "the body bounced")
    (is (= :arrived (:status done)) (pr-str done))))
