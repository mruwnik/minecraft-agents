(ns engine.go-to-stale-search-test
  "go-to's search kept over rounds (engine.path.walk/searches) when the land round the goal loads after it began (card
  9c4471aa; live j53: a search begun while the chunks round a goal on a sealed platform were still arriving read the goal
  unloaded, never ran the goal flood, and gave up :searching after 100 rounds)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.go-to-test :as gt]
            [engine.path.walk :as walk]
            [engine.test-util :as tu :refer [box]]))

;; a stone floor x 0..47, z 0..47 (only those columns are loaded); the body near its east edge, so no cell of the loaded
;; land is 8 blocks nearer the goal far east (no round walks while the search goes on)
(def floor-blocks (box 0 63 0 47 63 47 "stone"))

;; a sealed stone cell round the goal (105 64 24): inside x 104..106, z 23..25, feet 64, head 65, roof 66
(def sealed-cell
  (apply dissoc (box 103 63 22 107 66 26 "stone") (keys (box 104 64 23 106 65 25 "stone"))))

(def goal [105 64 24])

(defn ^:async run-go-to!
  "go-to to goal from (44 64 24) with a budget of 16 expansions a round; after rounds ticks the sealed cell's blocks
  are added (their columns load), when load? is true. {:out :ticks :eng :p}."
  [load?]
  (let [budget walk/round-budget
        chunk walk/chunk-expansions
        {:keys [eng p] :as s} (gt/setup {:blocks floor-blocks :self {:pos {:x 44.5 :y 64 :z 24.5}}})
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out {:pos goal :range 0})))]
    (reset! walk/searches {})
    (set! walk/round-budget 16)
    (set! walk/chunk-expansions 16)
    (core/submit! eng '(recording-parent) {})
    (let [early (await (gt/tick-out! eng 3))]
      (when load?
        (swap! (fake/state p) update :blocks merge (fake/cells sealed-cell identity)))
      (let [later (await (gt/tick-out! eng 400))]
        (set! walk/round-budget budget)
        (set! walk/chunk-expansions chunk)
        (reset! walk/searches {})
        (assoc s :eng eng :p p :out out :ticks (+ early later))))))

;; the goal's land loads while the search goes on: the next round begins a new search over it, whose goal flood proves
;; the cell walled in, and go-to gives up :goal-enclosed without walking
(deftest a-search-begun-with-the-goal-unloaded-starts-again-once-it-loads
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p ticks]} (await (run-go-to! true))]
          (is (= {:arrived false :reason :unreachable :why :goal-enclosed} (select-keys @out [:arrived :reason :why])))
          (is (< ticks 20) "ends in the round after the goal loads, not after the old search")
          (is (= 44 (js/Math.floor (first (gt/at p)))) "the body did not walk"))))))

;; the goal stays unloaded: the kept search goes on as before (no new search every round)
(deftest a-search-whose-goal-stays-unloaded-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plans (atom [])
              new-search walk/new-search]
          (set! walk/new-search (fn [& args] (swap! plans conj 1) (apply new-search args)))
          (let [{:keys [out]} (await (run-go-to! false))]
            (set! walk/new-search new-search)
            (is (not= :goal-enclosed (:why @out)))
            (is (< (count @plans) 10) "one search kept over its rounds, not one a round")))))))
