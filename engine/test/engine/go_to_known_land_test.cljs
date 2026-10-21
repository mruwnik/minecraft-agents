(ns engine.go-to-known-land-test
  "go-to over a world whose loaded land follows the body (the fake's :view-chunks), as a server's view distance does:
  land a search covered to its end reads as a loaded edge again once the body walks away and it unloads (live: soak
  j29, a walled walkway at y 100 whose goal lies below it, 38 rounds walking end to end)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-support :as gts]
            [engine.go-to-test :as gt]
            [engine.events :as events]
            [engine.fake :as fake]
            [jobs.lib.cost :as cost]
            [engine.memory :as mem]
            [jobs.lib.walk.search :as wsearch]
            [engine.registry :as registry]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]))

(defn setup [world p]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn recording-parent [out args]
  {:round (fn ^:async recording-round [c]
            (let [e (await (ctx/call c :kid 'jobs.movement.go-to args))]
              (if (= :continue e) :continue (do (reset! out (gts/result-of e)) (ctx/end c :done nil)))))})

(defn ^:async tick-out! [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async go-on!
  "go-to args over the fake p of world, ticked out: {:eng :p :seen :out :ticks}."
  [world p args]
  (let [{:keys [eng] :as s} (setup world p)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (reset! wsearch/searches {})
    (reset! wsearch/known-land {})
    (core/submit! eng '(recording-parent) {})
    (assoc s :eng eng :out out :ticks (await (tick-out! eng 200)))))

(defn go! [world args] (go-on! world (tu/fake world) args))

(defn at [p] (let [pos (.-pos (.self p))] [(.-x pos) (.-y pos) (.-z pos)]))

(defn moved [eng] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))

(defn turns
  "How often the body's walk along x turned back, over the :moved entries' ends from start-x."
  [eng start-x]
  (let [xs (cons start-x (map (comp :x :to) (moved eng)))
        dirs (remove zero? (map #(js/Math.sign (- %2 %1)) xs (rest xs)))]
    (count (filter true? (map not= dirs (rest dirs))))))

(defn walkway
  "A walkway (feet 80) at z 8 from x -40 to x east, beside and above a ground strip (feet 64) at z 9 from x -56 to 47,
  with no way down but a stair at its west end (z 8, x -41 .. -56, one step down each, onto the ground's west end).
  With the body's loaded land 1 chunk round it, the way down lies out of sight from the walkway's east part, and so does
  each end of the walkway from the other."
  [east]
  (merge (box -56 63 9 47 63 9 "stone")
         (box -40 79 8 east 79 8 "stone")
         (apply merge (for [i (range 1 17)] (box (- -40 i) (- 79 i) 8 (- -40 i) (- 79 i) 8 "stone")))))

;; the goal on the ground beside and below the walkway, the walkway running on 40 blocks east past it: from each end the
;; other end (and from the east part the stair) is unloaded land. Every search the body has finished covers land that
;; unloads behind it; a frontier there must not win over the unexplored west, or the body walks end to end
(deftest go-to-does-not-walk-back-to-land-it-has-searched-when-it-unloads
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p]} (await (go! {:blocks (walkway 87) :self {:pos {:x 40.5 :y 80 :z 8.5}} :viewChunks 1}
                                              {:pos [40 64 9] :range 1}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out " at " (at p)))
          (is (<= (turns eng 40) 2)
              (str "east, then west to the stair, then east on the ground: " (mapv (juxt :status :to) (moved eng)))))))))

;; the same walkway with its stair gone: no way down at all. go-to gives up once the land it can reach is searched,
;; without walking back over it
(deftest go-to-gives-up-on-a-walkway-with-no-way-down-when-the-land-follows-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -56 63 9 47 63 9 "stone") (box -40 79 8 87 79 8 "stone"))
              {:keys [out eng p]} (await (go! {:blocks blocks :self {:pos {:x 40.5 :y 80 :z 8.5}} :viewChunks 1}
                                              {:pos [40 64 9] :range 1}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])) (str "result " @out))
          (is (= 80 (second (at p))) "still on the walkway")
          (is (<= (turns eng 40) 1)
              (str "east, then west, never back over land searched: " (mapv (juxt :status :to) (moved eng)))))))))

;; the planner state of a body is process memory kept out of the job's: it must not outlive the call
(deftest a-finished-go-to-keeps-no-planner-state
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -56 63 9 47 63 9 "stone") (box -40 79 8 87 79 8 "stone"))
              _ (reset! wsearch/goal-floods {})
              {:keys [out]} (await (go! {:blocks blocks :self {:pos {:x 40.5 :y 80 :z 8.5}} :viewChunks 1}
                                        {:pos [40 64 9] :range 1}))]
          (is (= :unreachable (:reason @out)) (str "result " @out))
          (is (= [{} {} {}] [@wsearch/known-land @wsearch/goal-floods @wsearch/searches])))))))

;; the walkway runs 160 blocks on past the goal: the way down lies several view distances behind the body
(deftest go-to-walks-back-along-a-long-walkway-to-the-stair-out-of-view
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p]} (await (go! {:blocks (walkway 207) :self {:pos {:x 200.5 :y 80 :z 8.5}} :viewChunks 1}
                                              {:pos [40 64 9] :range 1}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out " at " (at p)))
          (is (<= (turns eng 200) 2)
              (str "west to the stair, then east on the ground: " (mapv (juxt :status :to) (moved eng)))))))))

;; no way down at all: it may look back east once at what it left out of view, then it gives up
(deftest go-to-gives-up-on-a-long-walkway-with-no-way-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -56 63 9 207 63 9 "stone") (box -40 79 8 207 79 8 "stone"))
              {:keys [out eng p]} (await (go! {:blocks blocks :self {:pos {:x 200.5 :y 80 :z 8.5}} :viewChunks 1}
                                              {:pos [40 64 9] :range 1}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])) (str "result " @out))
          (is (<= (turns eng 200) 3)
              (str "west, once back east to see its far end, then done: " (count (moved eng)))))))))

;; the goal lies at the west end, below a walkway whose west end is a dead end; the stair is at its east end, 250 blocks off,
;; out of view from the west end. The search that took the west edge never saw the east one (it ends at the first edge it
;; expands): the body must walk back to it, not give up with the west searched out
(deftest go-to-walks-on-east-to-a-way-it-left-out-of-view
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -56 63 9 240 63 9 "stone")
                            (box -40 79 8 207 79 8 "stone")
                            (apply merge (for [i (range 1 17)] (box (+ 207 i) (- 79 i) 8 (+ 207 i) (- 79 i) 8 "stone"))))
              {:keys [out eng p]} (await (go! {:blocks blocks :self {:pos {:x 40.5 :y 80 :z 8.5}} :viewChunks 1}
                                              {:pos [-50 64 9] :range 1}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out " at " (at p) (mapv (juxt :status :to) (moved eng)))))))))

;; the stair lies far east, past the planner's frontier reach of a goal at the dead west end: the body sees the east edge
;; of the walkway (over 256 from the goal) while it starts, walks west to the dead end, and must walk back east to it
(deftest go-to-walks-back-east-to-a-way-far-from-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -56 63 9 340 63 9 "stone")
                            (box -40 79 8 300 79 8 "stone")
                            (apply merge (for [i (range 1 17)] (box (+ 300 i) (- 79 i) 8 (+ 300 i) (- 79 i) 8 "stone"))))
              {:keys [out eng p]} (await (go! {:blocks blocks :self {:pos {:x 200.5 :y 80 :z 8.5}} :viewChunks 1}
                                              {:pos [-50 64 9] :range 1}))]
          (is (= {:arrived true} (select-keys @out [:arrived]))
              (str "result " @out " at " (at p) (mapv (juxt :status :to) (moved eng)))))))))

;; ---- a sealed platform holding a pool: the flood goes through the water, so the goal is proved walled in at once ----

;; a stone floor 141 x 141 and a platform at y 99 (x 30..36, z -33..-27) with a 2 x 2 pool on a stone bed; the loaded land is 2
;; chunks round the body (live: a 100 x 110 slab with a pool, 95 s of frontier walks, then :why :searching)
(def pool-platform
  (merge (box -70 63 -70 70 63 70 "stone")
         (box 30 99 -33 36 99 -27 "stone")
         (box 31 98 -32 32 98 -31 "stone")
         (box 31 99 -32 32 99 -31 "water")))

(defn dist-to [[x y z] [gx gy gz]]
  (js/Math.sqrt (+ (* (- x gx) (- x gx)) (* (- y gy) (- y gy)) (* (- z gz) (- z gz)))))

(deftest go-to-a-goal-on-a-platform-with-a-pool-is-enclosed-without-wandering
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng]} (await (go! {:blocks pool-platform :self {:pos {:x 0.5 :y 64 :z 0.5}} :viewChunks 2}
                                            {:pos [34 100 -30] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived false :reason :unreachable :why :goal-cut-off} (select-keys @out [:arrived :reason :why]))
              (str "result " @out))
          (is (every? #(<= (dist-to [(:x (:to %)) (:y (:to %)) (:z (:to %))] [34 100 -30]) 50) (moved eng))
              (str "walks: " (mapv (juxt :status :to) (moved eng)))))))))

(def high-deck-floor
  (merge (box -2 63 -2 60 63 60 "stone") (box 10 70 10 30 70 30 "stone")))

;; a search that outlasts max-searching rounds of one search goes on from the same cell: it is not a body moved off its start
(deftest go-to-does-not-give-up-a-search-that-goes-on-from-the-same-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [wsearch/round-budget 1000]
          (let [{:keys [out]} (await (go! {:blocks high-deck-floor :self {:pos {:x 20.5 :y 64 :z 20.5}}}
                                          {:pos [20 71 20] :range 1 :ways #{:walk :clear-seal} :spend {:health 0}
                                           :settings {:jobs.movement.go-to.tuning/max-searching 2}}))]
            (is (= {:arrived false :reason :unreachable :why :goal-cut-off} (select-keys @out [:arrived :reason :why]))
                (str "result " @out))))))))

(deftest go-to-says-where-it-gave-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go! {:blocks high-deck-floor :self {:pos {:x 20.5 :y 64 :z 20.5}}}
                                        {:pos [20 71 20] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= {:reason :unreachable :at [20 64 20]} (select-keys @out [:reason :at])) (str "result " @out))
          (is (number? (:near @out)) (str "result " @out)))))))

;; a small deck 11 blocks over the ground, 15 blocks from the body (live fixture go-to-sealed-platform-pool): the goal is proved
;; cut off in the first round; the body walks under it (the nearest cell it can reach, well within budget) and gives up there
(def small-deck
  (merge (box -70 63 -70 70 63 70 "stone") (box 13 74 13 19 74 19 "stone")))

(def small-deck-pool
  (merge small-deck (box 14 73 14 15 73 15 "stone") (box 14 74 14 15 74 15 "water")))

(defn ^:async small-deck-cut-off! [blocks]
  (let [{:keys [out eng p]} (await (go! {:blocks blocks :self {:pos {:x 2.5 :y 64 :z 16.5}}}
                                        {:pos [17 75 17] :range 1 :ways #{:walk :clear-seal} :spend {:health 0}}))]
    (is (= {:arrived false :reason :unreachable :why :goal-cut-off} (select-keys @out [:arrived :reason :why]))
        (str "result " @out))
    (is (< (dist-to (at p) [17 64 17]) 1.5)
        (str "body at " (at p) " walks: " (mapv (juxt :status :to) (moved eng))))))

(deftest go-to-a-small-sealed-deck-with-the-default-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go! {:blocks small-deck :self {:pos {:x 2.5 :y 64 :z 16.5}}}
                                        {:pos [17 75 17] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived false :reason :unreachable :why :goal-cut-off} (select-keys @out [:arrived :reason :why]))
              (str "result " @out)))))))

(deftest go-to-a-small-sealed-deck-is-cut-off-and-walked-under
  (async done (tu/run-async done (fn ^:async t [] (await (small-deck-cut-off! small-deck))))))

(deftest go-to-a-small-sealed-deck-with-a-pool-is-cut-off-and-walked-under
  (async done (tu/run-async done (fn ^:async t [] (await (small-deck-cut-off! small-deck-pool))))))

;; ---- a long tour: no cap on frontier walks while a frontier is unspent ----

;; the goal on a walkway (y 80) 40 blocks from the body's start, the only stair up at the walkway's far west end, 600
;; blocks off: every frontier on the way lies farther from the goal than the last, 4 or more legs of a tour with a loaded
;; land 1 chunk round the body; go-to arrives rather than giving up after 3 legs that got no nearer
(deftest go-to-tours-frontiers-away-from-the-goal-to-a-far-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -616 63 9 47 63 9 "stone")
                            (box -600 79 8 47 79 8 "stone")
                            (apply merge (for [i (range 1 16)] (box (- -600 i) (- 79 i) 8 (- -600 i) (- 79 i) 8 "stone"))))
              {:keys [out eng p]} (await (go! {:blocks blocks :self {:pos {:x 40.5 :y 64 :z 9.5}} :viewChunks 1}
                                              {:pos [0 80 8] :range 1}))]
          (is (= {:arrived true} (select-keys @out [:arrived]))
              (str "result " @out " at " (at p) (mapv (juxt :status :to) (moved eng)))))))))

;; ---- a tour's price (jobs.lib.cost/tour-cost) against the goal's worth (the tour-worth setting, default jobs.lib.cost/tour-worth) ----

;; a floor strip (feet 64) 16 wide along z over open air, the goal 400 blocks west of it across the void (live: the plot grid's
;; west edge, go-to-start-enclosed open field): every frontier lies along the strip, none nearer the goal
(def abyss-strip
  (merge (box 0 63 -200 15 63 200 "stone")
         ;; one block deep down in each chunk west of and round the strip: those chunks are loaded (open air), as live
         (into {} (for [cx (range -30 2) cz (range -13 13)] [(str (* 16 cx) ",0," (* 16 cz)) "stone"]))))

(deftest go-to-gives-up-a-tour-past-its-worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng]} (await (go! {:blocks abyss-strip :self {:pos {:x 2.5 :y 64 :z 0.5}} :viewChunks 1}
                                            {:pos [-400 64 0] :range 1 :ways #{:walk :clear-seal} :settings {:jobs.lib.cost.value/tour-worth 12}}))
              walked (reduce + (map #(dist-to [(:x (:from %)) 0 (:z (:from %))] [(:x (:to %)) 0 (:z (:to %))]) (moved eng)))]
          (is (= {:arrived false :reason :unreachable :why :explore-spent} (select-keys @out [:arrived :reason :why]))
              (str "result " @out " walks: " (mapv (juxt :status :from :to) (moved eng))))
          (is (= :nothing-possible (:kind (:ending (meta @out)))) "no way found within what the goal is worth")
          (is (< 12 (:cost (cost/tour-cost (select-keys @out [:explored :walks])))) (str "the tour cost over its worth: " @out))
          (is (< (:explored @out) 100) (str "result " @out))
          (is (< walked 100) (str "walks: " (mapv (juxt :status :to) (moved eng)))))))))

;; walking straight on toward a far unloaded goal comes nearer all the way: a worth smaller than the walk's price never runs out
(deftest go-to-a-far-unloaded-goal-straight-on-arrives-within-a-small-tour-worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! {:blocks (box -150 63 0 15 63 15 "stone") :self {:pos {:x 10.5 :y 64 :z 8.5}} :viewChunks 1}
                                          {:pos [-140 64 8] :range 1 :settings {:jobs.lib.cost.value/tour-worth 6}}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out " at " (at p))))))))

(defn ^:async go-walked!
  "go! that also sums the blocks the body walks (along x plus along z, over every move of the fake's body): {... :walked}."
  [world args]
  (let [walked (atom 0)
        p (tu/fake world)]
    (add-watch (fake/state p) ::walked
               (fn [_ _ old new]
                 (let [[ax _ az] (get-in old [:self :pos]) [bx _ bz] (get-in new [:self :pos])]
                   (when (and ax bx) (swap! walked + (js/Math.abs (- bx ax)) (js/Math.abs (- bz az)))))))
    (assoc (await (go-on! world p args)) :walked @walked)))

;; the bare abyss strip, no way on: the tour walks each end once (about 400 blocks along the strip and back, plus the turns
;; across it), never an end it has searched again because its land unloaded and loaded once more
(deftest go-to-tours-each-end-of-a-dead-end-strip-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng walked]} (await (go-walked! {:blocks abyss-strip :self {:pos {:x 2.5 :y 64 :z 0.5}} :viewChunks 1}
                                                         {:pos [-400 64 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])) (str "result " @out))
          (is (< walked 800) (str "walked " walked ": " (mapv (juxt :status :from :to) (moved eng)))))))))

;; the strip with the way on at its north end: the tour arrives within a bound on the blocks it walks
(deftest go-to-tours-a-strip-to-a-way-at-its-far-end-within-a-walk-bound
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge abyss-strip (box -401 63 199 -1 63 200 "stone") (box -401 63 -1 -399 63 198 "stone"))
              {:keys [out walked]} (await (go-walked! {:blocks blocks :self {:pos {:x 2.5 :y 64 :z 0.5}} :viewChunks 1}
                                                      {:pos [-400 64 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out))
          ;; south end 191, back to the north end 390, the bridge 400 and back south 200
          (is (< walked 1500) (str "walked " walked)))))))

;; j138: a goal 2 blocks over the floor, on a deck whose flood leaks along a bridge into unloaded land, so it is never proved
;; walled in; the body's only frontiers lie along the strip, away from the goal. With the default worth (a goal this near is
;; worth little) the tour is bounded by the floor's worth (about 670 blocks), not a tour of each end of the strip (about 800)
(deftest go-to-a-near-goal-it-cannot-prove-walled-in-ends-a-tour-by-its-price
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge abyss-strip (box 4 65 -1 6 65 1 "stone") (box -100 65 0 3 65 0 "stone"))
              {:keys [out eng walked]} (await (go-walked! {:blocks blocks :self {:pos {:x 10.5 :y 64 :z 0.5}} :viewChunks 1}
                                                         {:pos [5 66 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason]))
              (str "result " @out " walks: " (mapv (juxt :status :to) (moved eng))))
          (is (= [true true] [(vector? (:at @out)) (number? (:near @out))]) (str "result " @out))
          (is (< walked 700) (str "walked " walked ": " (mapv (juxt :status :to) (moved eng)))))))))

;; a ravine: the goal 6 blocks north across a 4-wide gap in the strip, the only way a ring road out past the loaded edge at
;; both ends of the strip (about 400 blocks round). The goal is near, yet worth a long tour: the floor of the tour-floor-blocks setting
(def ravine
  (merge (box 0 63 2 15 63 100 "stone") (box 0 63 -100 15 63 -3 "stone")
         (box -20 63 -100 -18 63 100 "stone")
         (box -17 63 98 -1 63 100 "stone") (box -17 63 -100 -1 63 -98 "stone")
         (into {} (for [cx (range -30 2) cz (range -7 7)] [(str (* 16 cx) ",0," (* 16 cz)) "stone"]))))

(deftest go-to-a-near-goal-across-a-ravine-arrives-by-a-long-tour
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng]} (await (go! {:blocks ravine :self {:pos {:x 10.5 :y 64 :z 3.5}} :viewChunks 1}
                                            {:pos [10 64 -3] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out " walks: " (mapv (juxt :status :to) (moved eng)))))))))

(deftest go-to-a-tour-floor-of-zero-gives-the-ravine-up-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go! {:blocks ravine :self {:pos {:x 10.5 :y 64 :z 3.5}} :viewChunks 1}
                                        {:pos [10 64 -3] :range 1 :ways #{:walk :clear-seal} :settings {:jobs.lib.cost.value/tour-floor-blocks 0}}))]
          (is (= :explore-spent (:why @out)) (str "result " @out)))))))

(deftest a-solid-cell-the-body-feels-but-has-not-seen-does-not-loop-the-replan
  ;; the walk's look-ahead finds the unseen cell on the way solid and ends the leg for a replan; a replan that plans
  ;; the same way again (the cell still read as air) used to repeat for ever, never yielding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:floor [-2 -3 10 6] :self {:pos {:x 1.5 :y 64 :z 2.5}} :blocks {"2,64,2" "stone" "2,65,2" "stone"}}
              {:keys [out ticks]} (await (go-on! world (sky/with-sky (tu/fake-on-floor world)) {:pos [4 63 3] :range 0}))]
          (is (map? @out) (pr-str @out))
          (is (< ticks 200) "ends"))))))
