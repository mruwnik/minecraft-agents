(ns engine.boat-drive-test
  "jobs.movement.boat-drive against the fake world: steer the boat the body is in to a water cell."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [jobs.movement.boat-drive :as boat-drive]))

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
          (let [{:keys [res p]} (await (run {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos target}))]
            (is (= :done (:status res)) (pr-str [target res]))
            (is (<= (dist-to (boat-pos p) target) 1.5) (pr-str [target (boat-pos p)]))
            (is (not (:already? res)) "it took strokes")))))))

(deftest already-there-is-done-without-a-stroke
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [2 63 2]}))]
          (is (= :done (:status res)))
          (is (true? (:already? res)) "the engine ends it before a stroke"))))))

(deftest stops-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world args reason]
                [["land in the way" {:blocks (merge (pond 0 10 0 3) (pond 0 10 8 12)) :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [5 63 10]} :blocked]
                 ["out of time" {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [18 63 18] :spend {:job-time 0}} :job-time]]]
          (let [{:keys [res]} (await (run world args))]
            (is (= {:status :stopped :reason reason} (select-keys res [:status :reason])) (pr-str [label res]))))))))

(deftest can-run-asks-the-boat-and-the-cell-between-strokes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world args expect]
                [["aboard a boat" {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [10 63 2]} [:ok :aboard]]
                 ["on foot" {:blocks lake :entities [(boat-at 2.5 2.5)]} {:pos [10 63 2]} [:later :not-aboard]]
                 ["not a boat" {:blocks lake :entities [(boat-at 2.5 2.5 :name "minecart")] :self {:vehicle 9}} {:pos [10 63 2]} [:never :not-a-boat]]
                 ["land cell" {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [10 64 2]} [:never :not-water]]]]
          (let [{:keys [p]} (lt/setup world)
                s (boat-drive/can-run? {:primitives p :args args})]
            (is (= expect [(first s) (:reason (nth s 2))]) label)))))))

(deftest on-foot-fails-its-condition-not-aboard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks lake :entities [(boat-at 2.5 2.5)]} {:pos [10 63 2]}))]
          (is (= [:condition-failed :not-aboard] [(:kind (:ending res)) (:reason res)])))))))

(deftest blocked-boat-stays-on-the-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run {:blocks (merge (pond 0 10 0 3) (pond 0 10 8 12)) :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}} {:pos [5 63 10]}))
              {:keys [z]} (boat-pos p)]
          (is (< z 4) "the boat stopped at the shore"))))))

(deftest an-unloaded-target-is-steered-to-not-waited-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks lake :unloaded ["10,63,2"] :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}})]
          (core/submit! eng '(jobs.movement.boat-drive {:pos [10 63 2]}) {})
          (await (tu/run-until-empty eng 30))
          (is (empty? (filterv #(= :waiting (:kind %)) @seen)))
          (is (some #(= :completed (:kind %)) @seen)))))))

(deftest a-target-that-turns-unsensed-on-the-way-is-still-steered-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (lt/setup {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}})
              real (.-blockAt p)
              calls (atom 0)]
          (aset p "blockAt" (fn [pos] (if (and (= 10 (.-x pos)) (> (swap! calls inc) 1)) nil (real pos))))
          (core/submit! eng '(jobs.movement.boat-drive {:pos [10 63 2]}) {})
          (await (tu/run-until-empty eng 30))
          (is (empty? (filterv #(= :stopped (:kind %)) @seen)) (pr-str (mapv #(select-keys % [:kind :reason :data]) @seen)))
          (is (empty? (filterv #(= :waiting (:kind %)) @seen)))
          (is (some #(= :completed (:kind %)) @seen)))))))

(deftest a-call-naming-no-cell-is-impossible
  (is (= [:never :bad-args] ((juxt first (comp :reason #(nth % 2))) (boat-drive/bad-args {:args {}}))))
  (is (= :ok (first (boat-drive/bad-args {:args {:pos [10 63 2]}})))))

(deftest the-time-spend-counts-the-time-the-job-has-run-not-the-wall-clock
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (lt/setup {:blocks lake :entities [(boat-at 2.5 2.5)] :self {:vehicle 9}})
              _ (.override (.-world p) "paddle" (fn [token a impl] (swap! clock + 40000) (impl token a)))
              res (await (tu/child-outcome eng 'jobs.movement.boat-drive {:pos [18 63 18] :spend {:job-time 100}} 60))]
          (is (= {:status :stopped :reason :job-time} (select-keys res [:status :reason])) (pr-str res)))))))
