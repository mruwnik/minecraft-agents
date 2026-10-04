(ns engine.path-walk-test
  "engine.path.walk (the shared walk driver) against the fake world: one call plans, walks and re-plans to a goal."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.path.walk :as walk]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn box
  "Blocks named name filling x0..x1, y0..y1, z0..z1."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(defn floor
  "Stone at y 63 for x in xs, z 0..2."
  [xs]
  (into {} (for [x xs z (range 3)] [(str x "," 63 "," z) "stone"])))

(defn ^:async walk-to
  "A body at pos over blocks; a parent job calls walk/walk-to! to goal. [the driver's answer, the plan and replan
  kinds it announced, the body's position]."
  [blocks goal pos]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake {:blocks blocks :self {:pos pos}})
        out (atom nil)
        announced (atom [])
        parent {:check (constantly true)
                :round (fn ^:async driving-round [c]
                         (reset! out (await (walk/walk-to! c {:to goal :range 0 :weight 1.2 :timeout-s 60
                                                              :announce! (fn [kind _] (swap! announced conj kind))})))
                         :done)}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'driving-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(driving-parent) {})
    (loop [i 0]
      (when (and (< i 200) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [at (.-pos (.self p))]
      [@out @announced [(.-x at) (.-y at) (.-z at)]])))

(def start {:x 0 :y 64 :z 1})

(deftest a-flat-floor-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [result walked walk-ms]} announced at] (await (walk-to (floor (range 12)) [10 64 1] start))]
          (is (= {:status :arrived :replans 0} (select-keys result [:status :replans])))
          (is (pos? walked))
          (is (pos? walk-ms))
          (is (= [:plan] announced))
          (is (> (first at) 9)))))))

(deftest a-step-up-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (floor (range 5)) (box 5 63 0 11 64 2 "stone"))
              [{:keys [result]} _ at] (await (walk-to blocks [10 65 1] start))]
          (is (= :arrived (:status result)))
          (is (= 65 (second at))))))))

(deftest a-trench-is-walked-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the wall at the trench's end keeps the walk round off the trench's corner cells (the fake's walker is a point
        ;; that cannot cross a hole's corner the way a body does)
        (let [blocks (merge (box 0 63 -8 4 63 10 "stone") (box 7 63 -8 12 63 10 "stone") (box 5 63 9 6 63 10 "stone")
                            (box 4 66 -8 6 66 8 "stone") (box 5 64 8 6 65 8 "stone"))
              [{:keys [result]} _ at] (await (walk-to blocks [10 64 1] start))]
          (is (= :arrived (:status result)))
          (is (> (first at) 9)))))))

(deftest a-goal-sealed-in-stone-is-no-path
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [[x z] [[9 1] [11 1] [10 0] [10 2]] y [64 65]] [(str x "," y "," z) "stone"]))
              blocks (merge (floor (range 12)) wall {"10,66,1" "stone"})
              [{:keys [result]}] (await (walk-to blocks [10 64 1] start))]
          (is (= {:status :no-path :reason :exhausted} (select-keys result [:status :reason]))))))))

;; an island (stone at y 63, feet 64) of x 0..4 over a lower floor of x 5..11; the goal stands on a 3-high pillar of the
;; lower floor, which no move reaches: the plan is partial and its nearest end is down the drop
(def island-and-pillar
  (merge (box 0 63 0 4 63 2 "stone") (box 5 60 0 11 60 2 "stone") (box 10 61 0 10 63 2 "stone")))

(deftest a-partial-plan-stops-at-the-one-way-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [result]} announced at] (await (walk-to island-and-pillar [10 64 1] start))]
          (is (= {:status :no-path :reason :one-way} (select-keys result [:status :reason])))
          (is (= :drop (:kind (:one-way result))))
          (is (number? (:near result)))
          (is (= [:plan] announced) "the island part was walked, no replan")
          (is (<= (first at) 5) "the body never went down the drop"))))))

(deftest dry-end-keeps-a-partial-plan-up-to-its-last-dry-step
  (let [s (fn [x swim?] (cond-> {:x x :move :walk} swim? (assoc :swim true)))]
    (are [steps kept] (= kept (mapv :x (walk/dry-end steps)))
      [(s 0 false) (s 1 false) (s 2 true) (s 3 true)] [0 1]
      [(s 0 true) (s 1 true)] []
      [(s 0 false) (s 1 false)] [0 1])))
