(ns engine.go-to-see-test
  "jobs.movement.go-to :see: an arrival within range counts only where the eye sees the goal block (jobs.lib.aim)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.aim :as aim]))

;; Ground of stone y 60-64 (feet y 65 on top). A pit at x 3 z 1 two deep (feet y 63) under the surface, a step out of it
;; at x 2 (feet y 64), then the surface. The goal block at (5 65 0) stands on the surface: from the pit floor it is within
;; range 3 (2 across, 2 up, 1 aside) but the surface stone is between the eye and every face of it.
(def goal [5 65 0])

(def pit-world
  {:blocks (-> (box -2 60 -3 12 64 3 "stone")
               (dissoc "3,63,1" "3,64,1" "2,64,1")
               (assoc "5,65,0" "gold_block"))
   :self {:pos {:x 3.5 :y 63 :z 1.5}}})

(defn eye-sees? [p cell]
  (let [[x y z] (gt/at p)]
    (aim/sees-cell? (aim/seen-kind-at p) (aim/eye-at {:x x :y y :z z}) cell)))

(deftest a-goal-hidden-from-the-pit-is-not-yet-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (gt/setup pit-world)]
          (is (not (eye-sees? p goal)) "the pit floor does not see the goal"))))))

(deftest see-walks-out-of-the-pit-to-where-the-eye-sees-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (gt/go! pit-world {:pos goal :range 3 :see true}))]
          (is (= {:arrived true} @out) (pr-str @out))
          (is (>= (second (gt/at p)) 64) (str "out of the pit, at " (gt/at p)))
          (is (eye-sees? p goal) (str "sees the goal from " (gt/at p))))))))

(deftest without-see-the-pit-floor-is-within-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! pit-world {:pos goal :range 3}))]
          (is (true? (:arrived @out)))
          (is (= [3.5 63 1.5] (gt/at p)) "no walk"))))))

(deftest see-gives-up-on-a-block-no-stand-in-range-sees
  ;; the goal is buried in the ground under the surface: every stand within range 3 has stone between the eye and it.
  ;; :ways #{:walk :dig} :may-dig #{:own}, as jobs.lib.blocks walks (the small test ground floods as shut in)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out ticks] :as s} (await (gt/go! (update pit-world :blocks assoc "5,62,0" "gold_block")
                                                       {:pos [5 62 0] :range 3 :see true :ways #{:walk :dig} :may-dig #{:own}}))]
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= :out-of-sight (:why @out)) (pr-str @out))
          (is (< ticks 30) "ends"))))))

(deftest see-with-no-stand-in-the-loaded-land-does-not-tour-to-its-edge
  ;; the buried goal is in loaded land: the search running out of it is no frontier to explore, so the body ends
  ;; where the search left it instead of walking out to the loaded edge away from the goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [start {:x 9.5 :y 65 :z 0.5}
              {:keys [out p]} (await (gt/go! (-> pit-world
                                                 (update :blocks #(-> (merge % (box 12 60 -3 15 64 3 "stone")) (assoc "5,62,0" "gold_block")))
                                                 (assoc :self {:pos start} :unloaded (keys (box 16 55 -3 40 70 3 "x"))))
                                             {:pos [5 62 0] :range 3 :see true :ways #{:walk :dig} :may-dig #{:own}}))]
          (is (= :out-of-sight (:why @out)) (pr-str @out))
          (is (< (first (gt/at p)) 11) (str "did not walk out to the loaded edge: " (gt/at p))))))))

(deftest the-first-blocker-is-the-solid-cell-nearest-the-eye
  (let [solid #{[2 5 0] [3 5 0]}
        kind-at (fn [x y z] (if (solid [x y z]) :solid :open))]
    (is (= [2 5 0] (aim/first-blocker kind-at [0.5 5.5 0.5] [5 5 0])))
    (is (nil? (aim/first-blocker kind-at [0.5 7.5 0.5] [5 7 0])) "a clear line has no blocker")))

(deftest lava-between-the-eye-and-the-goal-is-no-blocker
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (update pit-world :blocks assoc "4,65,0" "lava")
              {:keys [p]} (gt/setup world)
              {p2 :p} (gt/setup (assoc-in world [:blocks "4,65,0"] "stone"))
              eye [2.5 66.1 0.5]]
          (is (nil? (aim/first-blocker (aim/seen-kind-at p) eye goal)))
          (is (= [4 65 0] (aim/first-blocker (aim/seen-kind-at p2) eye goal))))))))
