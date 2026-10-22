(ns engine.go-to-stale-search-test
  "go-to's search kept over rounds (jobs.lib.walk.search/searches) when the land round the goal loads after it began (card
  9c4471aa; live j53: a search begun while the chunks round a goal on a sealed platform were still arriving read the goal
  unloaded, never ran the goal flood, and gave up :searching after 100 rounds)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.go-to-test :as gt]
            [jobs.lib.near :as near]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [engine.test-util :as tu :refer [box]]))

;; a stone floor x 0..47, z 0..47 (only those columns are loaded); the body near its east edge, so no cell of the loaded
;; land is 8 blocks nearer the goal far east (no round walks while the search goes on)
(def floor-blocks (box 0 63 0 47 63 47 "stone"))

;; a sealed stone cell round the goal (105 64 24): inside x 104..106, z 23..25, feet 64, head 65, roof 66
(def sealed-cell
  (apply dissoc (box 103 63 22 107 66 26 "stone") (keys (box 104 64 23 106 65 25 "stone"))))

(def goal [105 64 24])

(defn ^:async run-go-to!
  "go-to to goal from (44 64 24) with a budget of 16 expansions a search slice; when load? is true, the sealed cell's
  blocks are added (their columns load) at the first search slice after the body walked to the loaded edge (x 47), in
  the same go-to call. {:out :ticks :eng :p}."
  [load?]
  (let [budget wsearch/round-budget
        plan-walk wsearch/plan-walk-budgeted!
        {:keys [eng p] :as s} (gt/setup {:blocks floor-blocks :self {:pos {:x 44.5 :y 64 :z 24.5}}})
        out (atom :not-done)
        loaded (atom false)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out {:pos goal :range 0})))]
    (reset! wsearch/searches {})
    (set! wsearch/round-budget 16)
    (set! wsearch/plan-walk-budgeted!
          (fn [& args]
            (when (and load? (not @loaded) (= 47 (js/Math.floor (first (gt/at p)))))
              (reset! loaded true)
              (swap! (fake/state p) update :blocks merge (fake/cells sealed-cell identity)))
            (apply plan-walk args)))
    (core/submit! eng '(recording-parent) {})
    (let [ticks (await (tu/with-expansion-budget 16 #(gt/tick-out! eng 400)))]
      (set! wsearch/plan-walk-budgeted! plan-walk)
      (set! wsearch/round-budget budget)
      (reset! wsearch/searches {})
      (assoc s :eng eng :p p :out out :ticks ticks :loaded @loaded))))

;; the goal's land loads while the search goes on: the next search slice begins a new search over it, whose goal flood proves
;; the cell walled in, and go-to gives up :goal-enclosed without walking
(deftest a-search-begun-with-the-goal-unloaded-starts-again-once-it-loads
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p ticks loaded]} (await (run-go-to! true))]
          (is loaded "the goal loaded while go-to searched")
          (is (= {:arrived false :reason :unreachable :why :goal-enclosed} (select-keys @out [:arrived :reason :why])))
          (is (= 1 ticks) "ends in the same call, not after the old search")
          (is (= 47 (js/Math.floor (first (gt/at p))))
              "the body walked to the loaded edge (its frontier) before the goal loaded, and no farther"))))))

;; the goal stays unloaded: the kept search goes on as before (no new search every slice)
(deftest a-search-whose-goal-stays-unloaded-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plans (atom [])
              new-search wsearch/new-search]
          (set! wsearch/new-search (fn [& args] (swap! plans conj 1) (apply new-search args)))
          (let [{:keys [out]} (await (run-go-to! false))]
            (set! wsearch/new-search new-search)
            (is (not= :goal-enclosed (:why @out)))
            (is (pos? (count @plans)) "the stub saw the searches")
            (is (< (count @plans) 10) "one search kept over its rounds, not one a round")))))))

;; the goal stays unloaded far past the loaded floor's east edge (card 7a031d15): no node of the loaded land is 8 blocks
;; nearer than the start, so only an ended search walks; ending only once all loaded land is searched took more rounds
;; than go-to's max-searching (live: ~200k nodes at ~2000 a round). The search ends at the first node it expands at the
;; loaded edge, and go-to walks there (its frontier) instead of giving up :searching
(deftest a-far-goal-past-the-loaded-edge-walks-to-the-frontier
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-go-to! false))]
          (is (not= :searching (:why @out)) (str "result " @out))
          (is (<= 46 (js/Math.floor (first (gt/at p)))) (str "walked to the east edge, at " (gt/at p))))))))

;; a cut during a search ends the call at once: no search slice runs after it (ctx/alive? per iteration)
(deftest a-cut-mid-search-runs-no-more-slices
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [slices (atom 0)
              plan-walk wsearch/plan-walk-budgeted!
              p-atom (atom nil)
              cut-at (atom nil)]
          (set! wsearch/plan-walk-budgeted!
                (fn [& args]
                  (swap! slices inc)
                  (when (and @p-atom (not @cut-at))
                    (reset! cut-at @slices)
                    (.setOwner @p-atom "other"))
                  (apply plan-walk args)))
          (let [budget wsearch/round-budget]
            (reset! wsearch/searches {})
            (set! wsearch/round-budget 16)
            (let [{:keys [eng p]} (gt/setup {:blocks floor-blocks :self {:pos {:x 44.5 :y 64 :z 24.5}}})
                  out (atom :not-done)
                  eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out {:pos goal :range 0})))]
              (reset! p-atom p)
              (core/submit! eng '(recording-parent) {})
              (await (tu/with-expansion-budget 16 #(gt/tick-out! eng 1))))
            (set! wsearch/plan-walk-budgeted! plan-walk)
            (set! wsearch/round-budget budget)
            (reset! wsearch/searches {})
            (is (some? @cut-at) "a search slice ran")
            (is (= @cut-at @slices) "no slice after the cut")))))))

;; A body at the foot of a 6 deep water shaft (x 4) with half its air used (oxygen 10); a sealed 12 cell tunnel (feet 64,
;; head 65) to a second shaft (x 17); land over all (feet 70); goal the land beside the far shaft. The search needs more
;; than one round: a submerged body's policy carries a new :air-seen fn every round, so its key changed every round,
;; each round searched afresh, and go-to gave up :moved-while-searching (live: the air ran out first, :why :air).
(def shaft-blocks
  (merge (box -4 60 -4 40 63 70 "stone") (box 0 64 0 31 69 63 "stone")
         (box 4 64 8 4 69 8 "water") (box 5 64 8 16 65 8 "water") (box 17 64 8 17 69 8 "water")
         (box 11 70 0 11 73 58 "bedrock")))

(deftest a-submerged-body-goes-on-with-its-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! wsearch/searches {})
        (let [{:keys [eng]} (gt/setup {:blocks shaft-blocks :self {:pos {:x 4.5 :y 64 :z 8.5} :inWater true :oxygen 10}})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out {:pos [18 70 8] :range 1 :ways #{:walk :clear-seal}})))]
          (core/submit! eng '(recording-parent) {})
          (await (gt/tick-out! eng 30))
          (reset! wsearch/searches {})
          (is (true? (:arrived @out)) (str "result " @out)))))))

(deftest a-search-key-ignores-the-air-seen-fn-and-the-air-used
  (let [c {:primitives (tu/fake {:self {:pos [4.5 64 8.5]}})}
        policy {:sprint true :air-used 7 :air-seen (fn [_ _ _] true)}]
    (is (= (wsearch/search-key c [18 70 8] 1 1 policy nil)
           (wsearch/search-key c [18 70 8] 1 1 (assoc policy :air-used 9 :air-seen (fn [_ _ _] true) ) nil)))
    (is (not= (wsearch/search-key c [18 70 8] 1 1 policy nil)
              (wsearch/search-key c [18 70 8] 1 1 (dissoc policy :air-used :air-seen) nil))
        "a search begun with the head out is not gone on with under water")))

(deftest a-resumed-search-ends-once-the-air-ran-down-past-its-plan
  (let [k [:same-key]
        kept {:key k :t (js/Date.now) :air-used 7}]
    (is (wsearch/go-on? {} kept k nil [0 0 0] {:air-used 7.75}) "a bubble more used: the plan's air still holds")
    (is (not (wsearch/go-on? {} kept k nil [0 0 0] {:air-used 8.5})) "two bubbles more used: a new search plans with the air left")
    (is (not (wsearch/go-on? {} (dissoc kept :air-used) k nil [0 0 0] {:air-used 8.5})) "begun with the head out, now 8.5 s used")
    (is (wsearch/go-on? {} (dissoc kept :air-used) k nil [0 0 0] {}) "never under water")))

(deftest a-search-key-ignores-the-health-a-drowning-body-loses
  (let [c {:primitives (tu/fake {:self {:pos [4.5 64 8.5]}})}
        policy {:sprint true :air-used 15 :health 10 :drown-prices [0 1 2] :damage-weight 4 :damage-budget 6 :max-drop 6}]
    (is (= (wsearch/search-key c [18 70 8] 1 1 policy nil)
           (wsearch/search-key c [18 70 8] 1 1 (assoc policy :health 8 :drown-prices [0 2] :damage-weight 5 :damage-budget 4 :max-drop 5) nil))
        "one drowning hit does not make the search key new")
    (is (not= (wsearch/search-key c [18 70 8] 1 1 (dissoc policy :air-used :health :drown-prices) nil)
              (wsearch/search-key c [18 70 8] 1 1 (dissoc policy :air-used :health :drown-prices :damage-budget) nil))
        "with the head out the damage budget stays in the key")))

(deftest a-resumed-search-ends-once-the-body-lost-more-than-one-drowning-hit
  (let [k [:same-key]
        kept {:key k :t (js/Date.now) :air-used 15 :health 10}]
    (is (wsearch/go-on? {} kept k nil [0 0 0] {:air-used 15 :health 8}) "one hit (2 hp) lost: the plan's lethal line still holds")
    (is (not (wsearch/go-on? {} kept k nil [0 0 0] {:air-used 15 :health 6})) "two hits lost: a new search plans with the hp left")
    (is (wsearch/go-on? {} kept k nil [0 0 0] {:air-used 15 :health 10.5}) "healed a little: the plan was the more careful")))

;; a search that goes on from the same cell (no fresh start) is no gave-up for max-searching rounds; a leg call is bounded in time
(deftest a-leg-call-whose-search-goes-on-ends-once-its-leg-seconds-passed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              rounds (atom 0)]
          (with-redefs [near/walk-round! (fn [c _ _ _]
                                           (swap! rounds inc)
                                           (swap! clock + 1000)
                                           (js/Promise.resolve {:result {:status :searching :fresh false} :status "searching"
                                                                :from (:pos c) :to (:pos c)}))]
            (let [{:keys [out]} (await (gt/go! {:self {:pos [0.5 64 0.5]} :blocks floor-blocks :clock clock}
                                               {:pos [30 64 0] :range 1 :ways #{:walk :clear-seal} :spend {:job-time 3}}))]
              (is (false? (:arrived @out)) (pr-str @out))
              (is (= :search-spent (:why @out)) (pr-str @out))
              (is (<= @rounds 6) "it gave up within a few slices"))))))))

;; a walk whose steer was cut short by the call's own seconds (:out-of-time) is a leg's end, not a stuck body that gives the walk up
(deftest a-steer-cut-by-the-calls-seconds-ends-a-leg-not-a-stuck-giveup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)]
          (with-redefs [near/walk-round! (fn [c _ _ _]
                                           (swap! clock + 1000)
                                           (js/Promise.resolve {:result {:status :stuck :why "walk timed out after 2 s" :at [0 64 0] :out-of-time true}
                                                                :status "failed" :from (:pos c) :to (:pos c)}))]
            (let [{:keys [out]} (await (gt/go! {:self {:pos [0.5 64 0.5]} :blocks floor-blocks :clock clock}
                                               {:pos [30 64 0] :range 1 :ways #{:walk :clear-seal} :spend {:job-time 3 :tries 1}}))]
              (is (false? (:arrived @out)) (pr-str @out))
              (is (= :search-spent (:why @out)) (pr-str @out)))))))))
