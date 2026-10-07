(ns engine.go-to-known-land-test
  "go-to over a world whose loaded land follows the body (the fake's :view-chunks), as a server's view distance does:
  land a search covered to its end reads as a loaded edge again once the body walks away and it unloads (live: soak
  j29, a walled walkway at y 100 whose goal lies below it, 38 rounds walking end to end)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.lib.walk.search :as wsearch]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn recording-parent [out args]
  {:check (constantly true)
   :round (fn ^:async recording-round [c]
            (let [r (await (ctx/call-child c :kid 'jobs.movement.go-to args))]
              (when (= :done r) (reset! out (ctx/child-result c :kid)))
              r))})

(defn ^:async tick-out! [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async go! [world args]
  (let [{:keys [eng] :as s} (setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (reset! wsearch/searches {})
    (reset! wsearch/known-land {})
    (core/submit! eng '(recording-parent) {})
    (assoc s :eng eng :out out :ticks (await (tick-out! eng 200)))))

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
                                            {:pos [34 100 -30] :range 1 :escalate false}))]
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
        (with-redefs [jobs.movement.go-to/max-searching 2
                      wsearch/round-budget 1000]
          (let [{:keys [out]} (await (go! {:blocks high-deck-floor :self {:pos {:x 20.5 :y 64 :z 20.5}}}
                                          {:pos [20 71 20] :range 1 :escalate false}))]
            (is (= {:arrived false :reason :unreachable :why :goal-cut-off} (select-keys @out [:arrived :reason :why]))
                (str "result " @out))))))))

(deftest go-to-says-where-it-gave-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go! {:blocks high-deck-floor :self {:pos {:x 20.5 :y 64 :z 20.5}}}
                                        {:pos [20 71 20] :range 1 :escalate false}))]
          (is (= {:reason :unreachable :at [20 64 20]} (select-keys @out [:reason :at])) (str "result " @out))
          (is (number? (:near @out)) (str "result " @out)))))))
