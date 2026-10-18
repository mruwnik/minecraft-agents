(ns engine.go-to-danger-replan-test
  "go-to over a long route when a danger is sensed near the way ahead and the re-plan's search needs more than the
  replan budget: the walk stops, go-to searches on over its rounds, and the body arrives (it never stop-and-goes until
  :moved-while-searching)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.go-to-test :as gt]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [engine.test-util :as tu :refer [box]]))

(defn wall
  "A 3-high stone wall across x, the cells of z-from..z-to open."
  [x [z-from z-to]]
  (apply dissoc (box x 64 -20 x 66 20 "stone")
         (for [z (range z-from (inc z-to)) y [64 65 66]] (str x "," y "," z))))

;; a serpentine: walls every 10 blocks, the gaps at the north and south ends in turn
(def floor-blocks
  (merge (box -5 63 -20 70 63 20 "stone")
         (wall 10 [16 20]) (wall 20 [-20 -16]) (wall 30 [16 20]) (wall 40 [-20 -16]) (wall 50 [16 20])))

(def goal [60 64 0])

(def zombie {:id 9 :uuid "u9" :name "zombie" :kind "hostile" :pos [15 64 8]})

(defn ^:async run-go-to!
  "go-to from the origin to goal with a budget of 40 expansions a search slice (a re-plan 4 times that, 160); the
  zombie appears beside the way at the second search slice; when wander? is true it steps one block a slice after.
  {:out :ticks :calls :unfinished :searches}: unfinished the re-plan searches (budget 160) that ended unfinished."
  [wander?]
  (let [budget wsearch/round-budget
        plan-walk wsearch/plan-walk-budgeted!
        {:keys [eng p] :as s} (gt/setup {:blocks floor-blocks})
        out (atom :not-done)
        searches (atom 0)
        calls (atom 0)
        unfinished (atom 0)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out {:pos goal :range 0})))]
    (reset! wsearch/searches {})
    (set! wsearch/round-budget 40)
    (set! wsearch/plan-walk-budgeted!
          (fn [& args]
            (let [n (swap! calls inc)]
              (when (= 2 n) (swap! (fake/state p) update :entities conj zombie))
              (when (and wander? (< 2 n))
                (swap! (fake/state p) update :entities (fn [es] (mapv #(update-in % [:pos 2] inc) es)))))
            (.then (apply plan-walk args)
                   (fn [pl]
                     (when (:fresh pl) (swap! searches inc))
                     (when (and (= 160 (:budget (last args))) (= :searching (:status (wplan/no-walk pl 0 nil))))
                       (swap! unfinished inc))
                     pl))))
    (core/submit! eng '(recording-parent) {})
    (let [ticks (await (tu/with-expansion-budget 20 #(gt/tick-out! eng 400)))]
      (set! wsearch/plan-walk-budgeted! plan-walk)
      (set! wsearch/round-budget budget)
      (reset! wsearch/searches {})
      (assoc s :out out :ticks ticks :calls @calls :unfinished @unfinished :searches @searches))))

(deftest a-danger-sensed-on-a-long-route-whose-replan-outgrows-its-budget-still-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out unfinished]} (await (run-go-to! false))]
          (is (pos? unfinished) "a replan search outgrew its budget")
          (is (true? (:arrived @out)) (pr-str @out)))))))

(deftest a-danger-that-keeps-moving-on-a-long-route-still-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (run-go-to! true))]
          (is (true? (:arrived @out)) (pr-str @out)))))))
