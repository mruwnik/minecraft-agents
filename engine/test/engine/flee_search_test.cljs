(ns engine.flee-search-test
  "jobs.lib.flee-search: flee ends planned by the go-to planner and scored by when the chasers get there (against the
  fake world), and pick's choice among scored ends (pure)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.flee-search :as search]
            [jobs.survival.respond.goal :as respond.goal]))

(defn box
  "Blocks named name filling x0..x1, y0..y1, z0..z1."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(defn carve
  "blocks with the cells of each [x0 z0 x1 z1] at feet and head height (y 64, 65) made air."
  [blocks rects]
  (reduce (fn [b [x0 z0 x1 z1]] (merge b (box x0 64 z0 x1 65 z1 "air"))) blocks rects))

(defn ^:async ends-of
  "The flee search's ends from the body at start over blocks, run from chasers, with named targets; self merged into the
  fake's :self."
  [blocks start chasers named self]
  (let [p (tu/fake {:blocks blocks :self (merge {:pos start} self)})
        c {:primitives p :view (fn [] {:data mem/empty-data :now 0})}]
    (await (settings/with-settings {::search/budget-ms 10000}
             (fn ^:async run [] (await (search/search! c chasers named {})))))))

(def rock
  "Stone at y 63 and walls two high over x -32..32, z -32..32: carve opens the ways."
  (merge (box -32 63 -32 32 63 32 "stone") (box -32 64 -32 32 65 32 "stone")))

(def junction
  "A corridor along z 0 from a field in the west (x -30..-9) to a dead end at x 7, and at x 2 a branch north to a field
  (z -30..-14): straight east of the body is a pocket, the way out turns north."
  (carve rock [[-30 -10 -9 10] [-8 0 7 0] [2 -13 2 -1] [-10 -30 14 -14]]))

(deftest a-flight-turns-off-to-the-open-way-not-into-the-dead-end-straight-away
  ;; the old rays took the full 6-block step straight east, into the pocket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ends (await (ends-of junction {:x 0.5 :y 64 :z 0.5} [{:name "zombie" :pos {:x -4.5 :y 64 :z 0.5}}] [] nil))
              end (search/pick ends {})]
          (is (some #(and (not (:found? %)) (= 0 (:z (:end %)))) ends) "east is planned only part way: a pocket")
          (is (= 2 (:x end)) (str "the step turns into the branch north: " (pr-str end)))
          (is (neg? (:z end)) (pr-str end)))))))

(def field (box -32 63 -32 32 63 32 "stone"))

(deftest a-chaser-faster-than-the-body-catches-every-way-away
  ;; the old rays ignored the chaser's speed: a baby zombie 3 blocks behind a hungry body still got a step east
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [from {:x 0.5 :y 64 :z 0.5}
              mob-pos {:x -2.5 :y 64 :z 0.5}
              baby (await (ends-of field from [{:name "baby" :pos mob-pos}] [] {:food 6}))
              zombie (await (ends-of field from [{:name "zombie" :pos mob-pos}] [] {:food 6}))]
          (is (seq baby) "the ways are planned")
          (is (every? :caught? baby) "a baby zombie runs faster than a body too hungry to sprint")
          (is (nil? (search/pick baby {})) "no flee step: the body is cornered")
          (is (some? (search/pick zombie {})) "a grown zombie is left behind"))))))

(def cliff
  "A plateau (feet y 64) for x <= 0 over a lowland (feet y 54) east of it, a stair one block wide down the cliff along
  z 0 from x 1 to x 9."
  (merge (box -32 63 -32 0 63 32 "stone") (box -32 54 -32 0 62 32 "stone") (box 1 53 -32 32 53 32 "stone")
         (into {} (for [k (range 1 10) y (range 54 (- 64 k))] [(str k "," y ",0") "stone"]))))

(deftest a-goal-at-a-cliff-foot-with-a-way-down-is-a-safe-run
  ;; the old verdict dropped it :steep (10 down, 8.5 across) without looking for the stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [goal {:x 6 :y 54 :z 6}
              ends (await (ends-of cliff {:x 0.5 :y 64 :z 0.5} [{:name "zombie" :pos {:x -20.5 :y 64 :z 0.5}}] [{:pos goal :kind :goal}] nil))
              e (search/named ends goal)]
          (is (:found? e) "the planner walks the stair down")
          (is (nil? (respond.goal/verdict goal e {})) (pr-str (dissoc e :cells))))))))

;; ------------------------------------------------------------------ pick (pure)

(defn end-of
  "A scored ring end at x z (feet y 64) a second's walk off, open (found?) unless :found? false, with score."
  [x z score & {:as more}]
  (let [e (merge {:end {:x x :y 64 :z z} :found? true :score score :gain 1 :s 0.5 :cells [{:x 0 :y 64 :z 0 :t 0} {:x x :y 64 :z z :t 1}] :target {:x x :z z}}
                 more)]
    (merge {:open? (:found? e)} e)))

(deftest a-flee-step-walks-the-first-part-of-the-route-to-the-end
  (is (= {:x 5 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 1.3} {:x 9 :y 64 :z 0 :t 2}]})))
  (is (= {:x 9 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :cover? true :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 1.3} {:x 9 :y 64 :z 0 :t 2}]}))
      "a cover end is walked to whole")
  (is (= {:x 9 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 1.3} {:x 9 :y 64 :z 0 :t 2}]} #{[5 64 0]}))
      "a cell go-to refused is walked past on the route")
  (is (nil? (search/step-of {:end {:x 5 :y 64 :z 0} :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 1.3}]} #{[5 64 0]})))
  (is (nil? (search/step-of {:end {:x 0 :y 64 :z 0} :cover? true :cells [{:x 0 :y 64 :z 0 :t 0} {:x 1 :y 64 :z 0 :t 0.2} {:x 0 :y 64 :z 0 :t 0.4}]}))
      "an end where the route starts is no step"))

(deftest pick-takes-the-best-open-end-and-leaves-caught-and-refused-ones-out
  (let [ends [(end-of 6 0 9 :caught? true) (end-of 0 6 5) (end-of 0 -6 4) (end-of 3 3 8 :found? false)]]
    (is (= {:x 0 :y 64 :z 6} (search/pick ends {})) "a caught end is no way; an open end beats a pocket")
    (is (= {:x 0 :y 64 :z -6} (search/pick ends {:refused #{[0 64 6]}})))
    (is (nil? (search/pick [(end-of 6 0 9 :caught? true) (end-of 3 3 8 :found? false)] {}))
        "every open way caught: the pocket is a trap, the body is cornered")
    (is (= {:x 3 :y 64 :z 3} (search/pick [(end-of 3 3 8 :found? false) (end-of 2 2 1 :found? false)] {}))
        "no way out at all (a closed cave): the best pocket")
    (is (nil? (search/pick [(end-of 0 6 5 :gain 0) (end-of 3 3 8 :found? false :gain -1)] {}))
        "no end gains on the chasers over standing still: no step")
    (is (nil? (search/pick [(end-of 3 3 8 :found? false :gain 0.2 :s 0.3)] {}))
        "a pocket that buys less time than its walk takes is no step")
    (is (= {:x 3 :y 64 :z 3} (search/pick [(end-of 3 3 8 :found? false :gain 0.2 :s 0.3 :unseen [4 64 3])] {}))
        "unless it stops before a cell the body has not seen: a step round the corner to look")))

(deftest pick-orders-cover-then-a-safe-home-then-the-score
  (let [home (assoc (end-of 0 9 1) :target {:pos {:x 0 :y 64 :z 9} :kind :home})
        ends [(end-of 6 0 9) home (end-of -2 4 0 :cover? true)]]
    (is (= {:x -2 :y 64 :z 4} (search/pick ends {})) "out of the line of fire first")
    (is (= {:x 0 :y 64 :z 9} (search/pick (pop ends) {:prefer? #(= :home (:kind (:target %)))})) "then the safe home")
    (is (= {:x 6 :y 64 :z 0} (search/pick (pop ends) {})) "else the best score")))

(deftest pick-keeps-dry-and-wanted-ends-while-it-can
  (let [wet? #(pos? (:x %))
        ends [(end-of 6 0 9) (end-of -6 0 2)]]
    (is (= {:x -6 :y 64 :z 0} (search/pick ends {:wet? wet?})) "a dry end beats a better wet one")
    (is (= {:x 6 :y 64 :z 0} (search/pick [(end-of 6 0 9)] {:wet? wet?})) "only water: a swim")
    (is (nil? (search/pick [(end-of 6 0 9)] {:wet? wet? :dry-only? true})) "a drowned chases: no swim")
    (is (= {:x -6 :y 64 :z 0} (search/pick ends {:avoid [wet? (constantly true)]})) "an avoid that leaves none is skipped")
    (is (= {:x -6 :y 64 :z 0} (search/pick ends {:never? wet?})))
    (is (nil? (search/pick [(end-of 6 0 9)] {:never? wet?})) "never? leaves none: cornered")
    (is (= {:x -6 :y 64 :z 0} (search/pick ends {:hazards [{:x 6 :y 64 :z 1}]})) "a route past a hazard is left out")))

(deftest the-unseen-cells-the-routes-stopped-before-are-where-to-look-nearest-first
  (is (= [[2 64 -1] [5 64 0]] (search/unseen-ahead [(end-of 4 0 1 :found? false :s 0.8 :unseen [5 64 0])
                                                    (end-of 2 0 1 :found? false :s 0.4 :unseen [2 64 -1])
                                                    (end-of 0 2 1 :found? false :s 0.4)]))))
