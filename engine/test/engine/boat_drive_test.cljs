(ns engine.boat-drive-test
  "jobs.movement.boat-drive against the fake world: steer the boat the body is in to a water cell."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

(defn pond
  "Water at y 63 over x in [x0, x1] and z in [z0, z1]."
  [x0 x1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) z (range z0 (inc z1))] [(str x ",63," z) "water"])))

(def lake (pond 0 20 0 20))

(defn boat-at [x z & {:as extra}]
  (merge {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x x :y 63.5 :z z} :yaw 0} extra))

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.boat-drive args 60))]
    {:res res :p p :seen seen :eng eng}))

(defn boat-pos [p]
  (let [[x _ z] (:pos (first (filter #(= 9 (:id %)) (fake/entities p))))]
    {:x x :z z}))

(defn dist-to [{:keys [x z]} [tx _ tz]]
  (js/Math.hypot (- x (+ tx 0.5)) (- z (+ tz 0.5))))

(deftest drives-to-the-water-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [target [[10 63 2] [2 63 12] [0 63 0] [15 63 15]]]
          (let [{:keys [res p seen]} (await (run {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos target}))]
            (is (= :done (:status res)) (pr-str [target res]))
            (is (<= (dist-to (boat-pos p) target) 1.5) (pr-str [target (boat-pos p)]))
            (is (pos? (:strokes res)))
            (is (some #(= :boat.driven (:kind %)) @seen))))))))

(deftest already-there-is-done-without-a-stroke
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [2 63 2]}))]
          (is (= :done (:status res)))
          (is (zero? (:strokes res))))))))

(deftest stops-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world args reason]
                [["on foot" {:blocks lake :entities [(boat-at 2.5 2.5)]} {:pos [10 63 2]} :not-aboard]
                 ["not a boat" {:blocks lake :entities [(boat-at 2.5 2.5 :name "minecart")] :self {:vehicle 9}} {:pos [10 63 2]} :not-a-boat]
                 ["no pos" {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {} :bad-args]
                 ["the cell is not water" {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [10 64 2]} :not-water]
                 ["land in the way" {:blocks (merge (pond 0 10 0 3) (pond 0 10 8 12)) :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [5 63 10]} :blocked]
                 ["out of strokes" {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [18 63 18] :max-strokes 2} :timeout]]]
          (let [{:keys [res]} (await (run world args))]
            (is (= {:status :stopped :reason reason} (select-keys res [:status :reason])) (pr-str [label res]))))))))

(deftest blocked-boat-stays-on-the-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run {:blocks (merge (pond 0 10 0 3) (pond 0 10 8 12)) :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [5 63 10]}))
              {:keys [z]} (boat-pos p)]
          (is (< z 4) "the boat stopped at the shore"))))))

(deftest an-unseen-target-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks lake :unloaded ["10,63,2"] :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}})]
          (core/submit! eng '(jobs.movement.boat-drive {:pos [10 63 2]}) {})
          (await (tu/run-until-empty eng 3))
          (is (= [:unseen] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest a-target-that-turns-unsensed-after-the-check-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (lt/setup {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}})
              real (.-blockAt p)
              calls (atom 0)]
          (aset p "blockAt" (fn [pos] (if (and (= 10 (.-x pos)) (> (swap! calls inc) 1)) nil (real pos))))
          (core/submit! eng '(jobs.movement.boat-drive {:pos [10 63 2]}) {})
          (await (tu/run-until-empty eng 3))
          (is (empty? (filterv #(= :stopped (:kind %)) @seen)) (pr-str (mapv #(select-keys % [:kind :reason :data]) @seen)))
          (is (= [:unseen] (distinct (mapv :reason (filterv #(= :waiting (:kind %)) @seen))))))))))
