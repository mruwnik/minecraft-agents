(ns engine.courses-physics-test
  "The replayed live courses (engine.path.courses: bamboo, dripstone, fence) walked in prismarine-physics by the executor:
  the planner's plan, every step walked by the body's own client physics (bamboo and dripstone boxes at their vanilla
  offsets, as every bound bot has them). A plan whose straight legs cut through a stalk or post sticks the body there, so
  a found plan must arrive; a course the planner cannot thread ends with no plan, never with a body stuck on its way."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.courses :as courses]
            [engine.path.executor :as ex]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.walk :as walk]
            [engine.planner-fixture :as pf]
            [engine.stairs-physics-test :as sp]
            [engine.test-util :as tu]))

(def shapes (delay (tu/require-here "./js/offset-shapes.mjs")))

(def max-ticks 2400)

(defn physics-world
  "A prismarine world over the course's snapshot, bamboo and dripstone at their server offsets; unloaded reads as stone."
  [^js snapshot]
  (let [{:keys [block]} @sp/lib]
    #js {:getBlock (fn [pos]
                     (let [cell (.floored pos)
                           id (.stateAt snapshot (.-x cell) (.-y cell) (.-z cell))
                           b (.fromStateId block (if (= id 0xFFFF) 1 id) 0)]
                       (set! (.-position b) cell)
                       (.withServerShapes ^js @shapes b)))}))

(defn walk-course
  "Plan the course and walk the plan: {:status} of the executor's done map (:arrived, :stuck ...) or the planner's status
  when it found no plan."
  [name]
  (let [{:keys [snapshot from goal]} (courses/course-snapshot name)
        {:keys [mc physics]} @sp/lib
        pw #js {:snapshot snapshot :table @pf/table :space @pf/space}
        r (planner/plan snapshot (clj->js {:from from :goal goal})
                        #js {:table @pf/table :space @pf/space :weight 1.2
                             :limits (ex/planner-limits ex/policy (walk/solid-fn pw))})
        world (physics-world snapshot)
        phys ((.-Physics physics) mc world)
        _ (set! (.-playerHalfWidth phys) 0.31)
        steps (when (= "found" (.-status r)) (walk/plan-steps pw r))
        bot (sp/bot-at [(:x from) (:y from) (:z from)] [(:px from) (:pz from)])]
    (if (nil? steps)
      {:status (.-status r)}
      (loop [t 0 state (ex/start steps 0)]
        (let [pose (sp/pose-of bot)
              {:keys [done controls yaw] :as res} (ex/tick ex/policy state pose)]
          (if (or done (>= t max-ticks))
            (assoc (select-keys done [:status :why]) :at [(:x pose) (:z pose)])
            (do (set! (.-yaw (.-entity bot)) yaw)
                (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js controls)) world) bot)
                (recur (inc t) (:state res)))))))))

(deftest a-found-plan-over-a-replayed-course-is-walked-to-the-end
  (doseq [name ["checker-ew" "checker-we" "dripstone" "fence-diag" "rand50-we" "rand50-ew" "target-in-rand50" "full-open"
                "rand30-we" "rand30-ew" "pace-rand50" "pace-rand30" "mineshaft"]]
    (let [r (walk-course name)]
      (is (= :arrived (:status r)) (pr-str name r)))))

;; a solid block of offset bamboo: the body would have to weave inside cells, which one stand point per free region cannot
;; say; before the legs were checked the planner found a plan the body stuck on at its first stalk
(deftest a-full-bamboo-wall-has-no-plan-rather-than-a-stuck-one
  (is (= {:status "partial"} (walk-course "full-walled")))
  (is (= {:status "none"} (walk-course "target-in-full"))))
