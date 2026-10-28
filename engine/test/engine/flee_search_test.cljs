(ns engine.flee-search-test
  "jobs.lib.flee-search: flee ends planned by the go-to planner and scored by when the chasers get there (against the
  fake world), and pick's choice among scored ends (pure)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake-world]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.perception.store :as store]
            [engine.settings :as settings]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.lib.flee-search :as search]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.walk.world :as wworld]
            [jobs.survival.respond.goal :as respond.goal]))

(defn box
  "Blocks named name filling x0..x1, y0..y1, z0..z1."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(defn carve
  "blocks with the cells of each [x0 z0 x1 z1] at feet and head height (y 64, 65) made air."
  [blocks rects]
  (reduce (fn [b [x0 z0 x1 z1]] (merge b (box x0 64 z0 x1 65 z1 "air"))) blocks rects))

(defn ctx-of
  "A job ctx over p for the search; its events go into events (an atom of [kind fields])."
  ([p] (ctx-of p (atom [])))
  ([p events] {:primitives p :view (fn [] {:data mem/empty-data :now 0}) :emit (fn [kind _ fields] (swap! events conj [kind fields]))}))

(defn ^:async ends-of
  "The flee search's ends from the body at start over blocks, run from chasers, with named targets; self merged into the
  fake's :self."
  [blocks start chasers named self]
  (let [p (tu/fake {:blocks blocks :self (merge {:pos start} self)})]
    (await (search/search! (ctx-of p) chasers named {}))))

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
        (let [ends (await (ends-of junction {:x 0.5 :y 64 :z 0.5} [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}] [] nil))
              end (search/pick ends {})]
          (is (some #(and (not (:found? %)) (= 0 (:z (:end %)))) ends) "east is planned only part way: a pocket")
          (is (= 2 (:x end)) (str "the step turns into the branch north: " (pr-str end)))
          (is (neg? (:z end)) (pr-str end)))))))

(deftest a-flee-search-is-bounded-by-the-nodes-its-ring-expands
  ;; the old search was cut by the clock: a slow machine planned fewer ways
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [work (fn ^:async search-work [ring-nodes]
                     (let [events (atom [])
                           c (ctx-of (tu/fake {:blocks junction :self {:pos {:x 0.5 :y 64 :z 0.5}}}) events)]
                       (await (settings/with-settings {::search/ring-nodes ring-nodes}
                                (fn ^:async run [] (await (search/search! c [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}] [] {})))))
                       (some (fn [[kind fields]] (when (= :retreat.search-work kind) fields)) @events)))
              whole (await (work 7200))
              cut (await (work 300))]
          (is (= 12 (:planned whole)) (pr-str whole))
          (is (number? (:ms whole)))
          (is (< (:planned cut) 12) (str "the ring stops once 300 nodes are spent " (pr-str cut whole)))
          (is (<= 300 (:nodes cut) 310) (pr-str cut)))))))

(defn ^:async open-field-prices
  "{ring end: price} of the flee search on open ground at time (a zombie 5 blocks west of the body), with spawns-over
  forced to 0 when unspawned?."
  [time unspawned?]
  (let [p (sky/with-sky (tu/fake {:floor [-32 -32 32 32] :time time :self {:pos {:x 0.5 :y 64 :z 0.5}}
                                  :entities [{:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x -4.5 :y 64 :z 0.5}}]}))
        spawns-over spawn-light/spawns-over]
    (when unspawned? (set! spawn-light/spawns-over (constantly 0)))
    (try (into {} (map (juxt :target :price)) (await (search/search! (ctx-of p) [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}] [] {})))
         (finally (set! spawn-light/spawns-over spawns-over)))))

(deftest a-flight-under-the-night-sky-is-priced-with-the-mobs-met-spawning-on-its-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[night night-unspawned day day-unspawned] [(await (open-field-prices st/night false)) (await (open-field-prices st/night true))
                                                         (await (open-field-prices st/noon false)) (await (open-field-prices st/noon true))]]
          (is (= 12 (count night)) (pr-str night))
          (is (every? #(> (night %) (night-unspawned %)) (keys night)) (str "each way under the night sky costs its spawns " (pr-str night night-unspawned)))
          (is (every? #(< (* 2 (- (day %) (day-unspawned %))) (- (night %) (night-unspawned %))) (keys night))
              (str "by day only the unseen ground the sky may not light spawns " (pr-str day day-unspawned))))))))

(deftest a-flight-down-a-dark-corridor-is-a-priced-way-on
  ;; the old search priced an unseen open cell lethal and cut each route before it: in the dark every end was a pocket
  ;; one cell off, and the body held "cornered"
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks (merge junction (box -32 66 -32 32 66 32 "stone")) :light-default [0 0]
                            :entities [{:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x -4.5 :y 64 :z 0.5}}]
                            :self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 0})
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000)}))
              _ (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
              ends (await (search/search! (ctx-of p) [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}] [] {}))
              dark (filter :unseen ends)
              e (search/choose ends {})]
          (is (some #(and (:open? %) (:unseen %)) ends) "a way on through the dark is planned to the ring")
          (is (seq dark))
          (is (every? #(> (:price %) (:priced %)) dark) "its unseen part is priced on top of the planner's: the zombie met on the way back from a dead end")
          (is (some? e) "a step, not cornered")
          (is (<= (:x (:step e)) 2) (str "the step stays in what the body has seen, a cell into the dark at most: " (pr-str (:step e)))))))))

(deftest a-dark-dead-end-is-trapped-at-the-cheapest-cornered-answer-a-cover-where-the-body-can-make-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks (merge junction (box -32 66 -32 32 66 32 "stone")) :light-default [0 0]
                            :entities [{:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x -4.5 :y 64 :z 0.5}}]
                            :self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 0})
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000)}))
              _ (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
              chasers [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}]
              trapped (fn ^:async trapped [opts] (->> (await (search/search! (ctx-of p) chasers [] opts)) (keep :trapped) (reduce max 0)))
              fist (await (trapped {}))
              walled (await (trapped {:covers (constantly {:walls {:open-s 0 :seconds 40}}) :covers-key :walls}))]
          (is (pos? walled))
          (is (< walled (/ fist 2)) "unarmed, walls built unhurt cost far less than the fist fight with the zombie"))))))

(deftest the-covers-are-built-only-at-a-dead-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks junction :light-default [15 15]
                            :entities [{:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x -4.5 :y 64 :z 0.5}}]
                            :self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 0})
              p (tu/seeing-all raw)
              built (atom 0)
              ends (await (search/search! (ctx-of p) [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}] []
                                          {:covers (fn [] (swap! built inc) {})}))]
          (is (seq ends))
          (is (not-any? :unseen ends) "all seen: no dead end in the dark")
          (is (zero? @built) "no dead end met: the covers' quotes are never asked for"))))))

;; card 6699a4ea: the old search priced a dead end the same wherever it lay, so the straight way on through the dark
;; beat the branch behind, and the body learnt of the dead end only once the zombie held the branch
(def long-dead-end
  "junction's corridor run on east to a dead end at x 30: past the branch at x 2 the way on east may be shut."
  (carve rock [[-30 -10 -9 10] [-8 0 30 0] [2 -13 2 -1] [-10 -30 14 -14]]))

(defn seeing-only
  "p sensing only the cells known? ([x z] -> bool) holds of: a cell elsewhere is {:unknown true}, never seen."
  [p known?]
  (let [seen? #(known? [(js/Math.floor (.-x %)) (js/Math.floor (.-z %))])]
    (tu/set-sensed! p (fn [pos] (if (seen? pos) (.blockAt (tu/oracle p) pos) #js {:unknown true :pos pos})))
    (aset p "seenBlockAt" (fn [pos] (if (seen? pos)
                                      (let [b (.blockAt (tu/oracle p) pos)] #js {:name (.-name b) :properties (.-properties b) :pos pos :age-ms 0})
                                      #js {:unknown true :pos pos})))
    p))

(deftest a-flight-past-a-dark-branch-turns-back-to-it-before-a-dead-end-ahead-would-cut-its-way-back
  ;; as in the dark corridor of flee-dead-end-pocket: the body has walked 9 blocks past the branch, seeing 3 cells ahead
  ;; and 3 into it; at 6 past it, on its way back, it keeps going back (no pacing)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:x -8.5 :y 64 :z 0.5}]
          (doseq [x [8.5 5.5]]
            (let [p (seeing-only (tu/fake {:blocks long-dead-end :self {:pos {:x x :y 64 :z 0.5}}
                                           :entities [{:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos zombie}]})
                                 (fn [[x z]] (and (<= -12 x 11) (<= -3 z 1))))
                  ends (await (search/search! (ctx-of p) [{:name "zombie" :mob "zombie" :pos zombie}] [] {}))
                  e (search/choose ends {})]
              (is (some #(and (:unseen %) (> (:x (:end %)) 11)) ends) "the way on east runs into the dark")
              (is (< (:x (:step e)) x) (str "at " x " the step turns back toward the branch: " (pr-str (select-keys e [:end :step :score :unseen]))))
              (is (neg? (:z (:end e))) (pr-str (:end e))))))))))

(defn way
  "A priced end along cells [[x z] ...] from the body at [0 0], a second a cell, its first seen-n seen: the rest unseen,
  trapped 100 at a dead end there (nothing seen near: way-on-chance 1/2)."
  [cells seen-n price]
  (let [cs (vec (map-indexed (fn [i [x z]] {:x x :y 64 :z z :t i}) cells))]
    (cond-> {:target {:x 9 :z 9} :found? true :open? true :end (select-keys (peek cs) [:x :y :z]) :s (:t (peek cs))
             :price price :priced price :cells cs :seen-n seen-n :counts {:k 0 :n 0}}
      (< seen-n (count cs)) (assoc :unseen [(:x (nth cs seen-n)) 64 (:z (nth cs seen-n))] :trapped 100))))

(def east (way [[0 0] [1 0] [2 0] [3 0] [4 0]] 3 4))
(def north (way [[0 0] [0 1] [0 2] [0 3] [0 4]] 3 4))
(def home (way [[0 0] [-1 0] [-2 0]] 3 5))

(defn dead-ends [ends] (into {} (map (juxt :end :dead-end)) (search/scored [] {:x 0.5 :y 64 :z 0.5} ends)))

(deftest a-dark-way-pays-its-dead-end-at-the-cost-of-the-way-back-out-and-no-other-dark-way-vouches-for-it
  ;; card 19df5758: the reverted pricing counted any other unseen way as a sure way out, so two dark ways each paid no
  ;; dead end; one alone paid it whole whatever else lay behind
  (let [alone (get (dead-ends [east]) (:end east))
        twins (dead-ends [east north])
        homed (dead-ends [east home])]
    (is (= 50 alone) "no other way out: trapped at the dead end's chance")
    (is (< 0 (get twins (:end east)) alone) "another dark way: its own dead end at the same chance, not a sure way out")
    (is (= (get twins (:end east)) (get twins (:end north))))
    (is (= (* 0.5 (+ 2 5)) (get homed (:end east))) "a seen way home: the walk back to the fork and the way home")
    (is (zero? (get homed (:end home))) "a seen way pays none")))

(deftest a-cover-past-the-seen-part-of-a-way-pays-its-dead-end
  ;; card 645e6abc: the reverted pricing dropped the dead end of every cover end
  (let [from {:x 0.5 :y 64 :z 0.5}
        ends (search/scored [] from [east home])
        cover-at #(into {} (map (juxt :end :dead-end)) (search/covers [] from (fn [c] (= c %)) ends))]
    (is (pos? (get (cover-at {:x 4 :y 64 :z 0}) {:x 4 :y 64 :z 0})) "the cover lies past the unseen cell")
    (is (zero? (get (cover-at {:x 1 :y 64 :z 0}) {:x 1 :y 64 :z 0})) "the cover lies in what the body has seen")))

(def field (box -32 63 -32 32 63 32 "stone"))

(deftest a-chaser-faster-than-the-flight-catches-every-way-away
  ;; the old search timed the body at a sprint (5.6 blocks a second): a spider 4 blocks behind was left behind on open
  ;; ground; a flight goes at the measured flee pace (steps at 3.72 with a 0.23 s stop between). Too hungry to sprint
  ;; (food 6) it steps at 3.72 x the game's walk/sprint = 2.86, 2.33 with the stops, a hair over a zombie's 2.28 (a ratio
  ;; model: no counted flight was hungry), so the zombie 7 behind is left behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [from {:x 0.5 :y 64 :z 0.5}
              behind (fn [d] {:x (- 0.5 d) :y 64 :z 0.5})
              baby (await (ends-of field from [{:name "baby" :pos (behind 3)}] [] {:food 6}))
              spider (await (ends-of field from [{:name "spider" :mob "spider" :pos (behind 4)}] [] {:food 20}))
              far (await (ends-of field from [{:name "zombie" :mob "zombie" :pos (behind 7)}] [] {:food 20}))
              hungry-far (await (ends-of field from [{:name "zombie" :mob "zombie" :pos (behind 7)}] [] {:food 6}))
              creeper (await (ends-of field from [{:name "creeper" :mob "creeper" :pos (behind 7)}] [] {:food 0}))]
          (is (seq baby) "the ways are planned")
          (is (every? :caught? baby) "a baby zombie runs faster than a body too hungry to sprint")
          (is (nil? (search/pick baby {})) "no flee step: the body is cornered")
          (is (= 1 (:chance (search/caught-chance baby {:k 0 :n 0} {} nil))) "the fight-or-flee weighing prices the flight caught")
          (is (every? :caught? spider) "a spider 4 behind catches a flight")
          (is (some? (search/pick far {})) "one 7 behind is left behind over the ring")
          (is (some? (search/pick hungry-far {})) "too hungry to sprint, still a hair faster than a zombie 7 behind")
          (is (< (:chance (search/caught-chance far {:k 0 :n 0} {} nil)) 1) "a way left: not caught for sure")
          ;; card fbe87241: a hungry body's walk was slowed twice (the planner's walk, then the walk share again): every way caught
          (is (some? (search/pick creeper {})) "a body too hungry to sprint walks away from a creeper 7 behind"))))))

(deftest a-chaser-behind-a-wall-is-timed-along-its-way-round-it
  ;; the old arrival ran a straight line through the wall: every way was caught
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [from {:x 0.5 :y 64 :z 0.5}
              zombie [{:name "baby" :mob "zombie" :pos {:x -2.5 :y 64 :z 0.5}}]
              ends (fn ^:async near-ends [blocks] (await (search/search! (ctx-of (tu/fake {:blocks blocks :self {:pos from}})) zombie [] {})))
              open (await (ends field))
              walled (await (ends (merge field (box -2 64 -6 -2 65 6 "stone"))))
              arrive (fn [ends] (into {} (map (juxt :target :arrive)) ends))
              later (merge-with - (arrive walled) (arrive open))]
          (is (every? :caught? open) "in the open a baby zombie keeps up")
          (is (some #(> % 1) (vals later)) (str "round a wall it comes later to the ends behind it: " (pr-str later))))))))

(deftest a-shooter-shut-in-is-timed-in-a-straight-line
  ;; it shoots where it cannot walk: timed by the way out of its box it has none, so every end read as never reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [boxed (merge field (box 6 64 -2 10 66 2 "stone") (box 7 64 -1 9 65 1 "air"))
              ends (await (ends-of boxed {:x 0.5 :y 64 :z 0.5} [{:name "skeleton" :mob "skeleton" :pos {:x 8.5 :y 64 :z 0.5}}] [] nil))]
          (is (seq ends))
          (is (every? #(js/isFinite (:arrive %)) ends) (pr-str (map :arrive ends))))))))

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
              ends (await (ends-of cliff {:x 0.5 :y 64 :z 0.5} [{:name "zombie" :mob "zombie" :pos {:x -30.5 :y 64 :z 0.5}}] [{:pos goal :kind :goal}] nil))
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
  (is (= {:x 5 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 3} {:x 9 :y 64 :z 0 :t 5}]})))
  (is (= {:x 9 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :cover? true :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 3} {:x 9 :y 64 :z 0 :t 5}]}))
      "a cover end is walked to whole")
  (is (= {:x 9 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 3} {:x 9 :y 64 :z 0 :t 5}]} #{[5 64 0]}))
      "a cell go-to refused is walked past on the route")
  (is (nil? (search/step-of {:end {:x 5 :y 64 :z 0} :cells [{:x 0 :y 64 :z 0 :t 0} {:x 5 :y 64 :z 0 :t 3}]} #{[5 64 0]})))
  (is (= {:x 1 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :seen-n 2 :cells [{:x 0 :y 64 :z 0 :t 0} {:x 1 :y 64 :z 0 :t 0.4} {:x 5 :y 64 :z 0 :t 3} {:x 9 :y 64 :z 0 :t 5}]}))
      "only the seen part of the route is walked")
  (is (= {:x 1 :y 64 :z 0} (search/step-of {:end {:x 9 :y 64 :z 0} :seen-n 1 :cells [{:x 0 :y 64 :z 0 :t 0} {:x 1 :y 64 :z 0 :t 0.4} {:x 5 :y 64 :z 0 :t 3} {:x 9 :y 64 :z 0 :t 5}]}))
      "seen no further than where it stands: one cell into the dark")
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
    (is (= {:x 3 :y 64 :z 3} (search/pick [(end-of 0 6 5 :gain -1) (end-of 3 3 2 :found? false :gain 2)] {}))
        "the open way leads back to the chaser (a wall's foot): a pocket along the wall")
    (is (nil? (search/pick [(end-of 3 3 8 :found? false :gain 0.2 :s 0.3)] {}))
        "a pocket that buys less time than its walk takes is no step")
    (is (nil? (search/pick [(end-of 3 3 8 :found? false :gain 0.2 :s 0.3 :unseen [4 64 3])] {}))
        "nor one whose route goes on unseen: unseen counts as the worse case, a dead end")))

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

(deftest the-unseen-cells-the-routes-stopped-before-are-where-to-look-nearest-first-each-once
  (let [ends [(end-of 4 0 1 :found? false :s 0.8 :unseen [5 64 0])
              (end-of 2 0 1 :found? false :s 0.4 :unseen [2 64 -1])
              (end-of 0 2 1 :found? false :s 0.4)]]
    (is (= [[2 64 -1] [5 64 0]] (search/unseen-ahead ends nil)))
    (is (= [[5 64 0]] (search/unseen-ahead ends [[2 64 -1]])) "a cell looked at this flight is not looked at again")
    (is (nil? (search/unseen-ahead ends [[2 64 -1] [5 64 0]])))))

(def lake
  "A lake 7 deep: water at x -4..4, y 57..63, z -4..4 over stone; its shore the floor at y 63."
  (into {} (for [x (range -4 5) z (range -4 5) y (range 56 64)] [(str x "," y "," z) (if (= y 56) "stone" "water")])))

(deftest a-mob-beside-a-flight-lands-its-blows-only-until-the-flight-pulls-away
  ;; the old answer priced a flight any blow lands on as caught all the flight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [beside (await (ends-of field {:x 0.5 :y 64 :z 0.5} [{:name "zombie" :mob "zombie" :pos {:x -0.5 :y 64 :z 0.5}}] [] nil))
              {:keys [chance]} (search/caught-chance beside {:k 0 :n 0} {} nil)
              p (tu/fake-on-floor {:floor [-50 -45 50 45] :blocks lake :self {:pos {:x 3.5 :y 63 :z 0.5}}})
              afloat (await (search/search! (ctx-of p) [{:name "drowned" :mob "drowned" :pos {:x 1.5 :y 63 :z 0.5}}] [] {}))]
          (is (some :caught? beside) "the ways past a zombie beside are caught")
          (is (not-every? :caught? beside) "the way away is not: its blows while the flight starts are cost.answer/flee-mob's")
          (is (< chance 1))
          (is (< (:seconds (search/caught-chance afloat {:k 0 :n 0} {} nil) ##Inf) (reduce min (map :s afloat))) "a drowned two blocks off an afloat body is in reach only part of the swim to the shore"))))))

(deftest a-flee-search-is-planned-again-only-when-what-it-reads-changed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks junction :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000)}))
              _ (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
              c (ctx-of p)
              zombie (fn [x] {:name "zombie" :mob "zombie" :pos {:x x :y 64 :z 0.5}})
              ring (fn [chasers] (search/ring {:x 0.5 :z 0.5} chasers 10 4))
              price (fn ^:async price [chasers] (await (search/price! c (ring chasers) {:chasers chasers})))
              _ (await (price [(zombie -4.5)]))
              a (await (price [(zombie -4.5)]))
              again (await (price [(zombie -4.5)]))
              moved (await (price [(zombie -2.5)]))]
          (is (identical? a again) "the same chasers ask the same")
          (is (not (identical? a moved)) "a chaser elsewhere is priced afresh")
          (is (not (identical? a (await (settings/with-settings {::search/max-nodes 100} #(price [(zombie -4.5)]))))) "other settings are priced afresh"))))))

(deftest a-search-that-routed-nothing-is-no-certain-catch
  (let [open (search/caught-chance [] {:k 0 :n 0} {} nil)
        boxed (search/caught-chance [] {:k 0 :n 40} {} nil)]
    (is (< 0 (:chance open) 1) "unseen is the worse case, priced by the ways seen round the body")
    (is (< (:chance open) (:chance boxed) 1) "seen walls on every side make it likelier")
    (is (nil? (:seconds open)) "no way timed: the blows run over the whole flight")))

(deftest the-flight-is-caught-at-the-prior-on-the-best-way-and-its-blows-run-over-the-caught-headings
  (let [free (end-of 4 0 5 :caught? false :caught-s 0 :arrive 5)
        caught (fn [s] (end-of 0 4 9 :caught? true :caught-s s :arrive 5))
        c (fn [ends] (update (search/caught-chance ends {:k 10 :n 10} {} nil) :chance #(/ (js/Math.round (* 1e6 %)) 1e6)))]
    (is (= {:chance 0 :seconds 0.5} (c [free (caught 2) (caught 0.5) (caught 1)])) "the other headings only choose the way: nothing on top of the prior, the least seconds of a caught one")
    (is (= {:chance 0} (c [free])) "no heading caught: nothing bounds the blows")
    (is (= {:chance 1 :seconds 0.5 :cornered? true} (c [(caught 2) (caught 0.5)])) "every way caught: the flight is the cornered step")
    (is (= {:chance 1 :cornered? false} (c [(end-of 4 0 1 :found? false :caught? false :caught-s 0 :gain 2)]))
        "a pocket the flight still runs to, with no way out to call free: a walk caught for certain")))

(deftest ways-that-stop-at-unseen-cells-are-priced-by-the-ways-seen-not-caught
  (let [stops (fn [& more] (apply end-of 4 0 1 :found? false :unseen [5 64 0] :caught-s 0 :caught? false :arrive 5 more))
        chance #(:chance (search/caught-chance % {:k 10 :n 10} {} nil))]
    (is (< (js/Math.abs (- (- 1 (/ 11 12)) (chance [(stops) (stops)]))) 1e-9) "a way into the unseen: its dead end, (k+1)/(n+2) of the seen ways on, not a catch")
    (is (= 1 (chance [(end-of 4 0 1 :found? false :caught-s 0 :caught? false)])) "a pocket with nothing unseen in it is a trap")))

(deftest ways-all-caught-keep-the-seconds-they-are-in-reach-when-they-run-into-the-unseen
  (let [caught (fn [s & more] (apply end-of 4 0 1 :found? false :caught? true :caught-s s more))
        seconds #(:seconds (search/caught-chance % {:k 10 :n 10} {} nil))]
    (is (= 0.6 (seconds [(caught 0.6 :unseen [5 64 0]) (caught 1.5 :unseen [5 64 0])])) "the blows run over the least seconds a way keeps the chaser in reach")
    (is (nil? (seconds [(caught 0.6)])) "a closed pocket is no flight: the whole of it")))

(deftest every-way-caught-but-an-unseen-cell-to-look-at-is-priced-by-the-look-not-cornered
  (let [caught (fn [& more] (apply end-of 4 0 1 :found? false :caught? true :caught-s 0.6 more))
        looked (search/caught-chance [(caught :unseen [5 64 0])] {:k 10 :n 10} {} nil)]
    (is (not (:cornered? looked)) "the flight looks first: it is not yet cornered")
    (is (< (js/Math.abs (- (- 1 (/ 11 12)) (:chance looked))) 1e-9) "the look finds a way at (k+1)/(n+2): priced as an unseen way")
    (is (:cornered? (search/caught-chance [(caught)] {:k 10 :n 10} {} nil)) "nothing left to look at: cornered, caught for sure")
    (is (= {:chance 1 :cornered? true :seconds 0.6} (search/caught-chance [(caught :unseen [5 64 0])] {:k 10 :n 10} {} [[5 64 0]]))
        "the flight looked at that cell already and looks at it no more: cornered")
    (is (= 1 (:chance (search/caught-chance [(caught)] {:k 10 :n 10} {} nil))))))

(deftest a-kept-flee-price-is-planned-again-when-any-input-of-the-price-changed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks junction :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000)}))
              _ (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
              now (atom 0)
              ctx (fn [args] (assoc (ctx-of p) :args args :view (fn [] {:data mem/empty-data :now @now})))
              chasers [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}]
              ends (search/ring {:x 0.5 :z 0.5} chasers 10 4)
              price (fn ^:async price [c] (await (search/price! c ends {:chasers chasers})))
              _ (await (price (ctx nil)))
              base (await (price (ctx nil)))
              changed? (fn [a b] (not (identical? a b)))]
          (is (identical? base (await (price (ctx nil)))) "the same asks the same")
          (is (changed? base (await (price (ctx {:gait :sneak})))) "a job arg the walk reads")
          (is (changed? base (await (price (ctx {:max-damage 3})))) "the damage the walk keeps")
          (is (changed? base (await (settings/with-settings {:jobs.lib.cost.threat/difficulty 1} #(price (ctx nil))))) "the server difficulty")
          (let [base (await (price (ctx nil)))]
            (reset! now 61000)
            (is (identical? base (await (price (ctx nil)))) "the clock alone is no input, whatever the landing memory's window")
            (swap! (fake-world/state raw) assoc :time 18000)
            (let [dark (await (price (ctx nil)))]
              (is (changed? base dark) "the sky's light")
              (swap! (fake-world/state raw) assoc-in [:self :onFire] true)
              (is (changed? dark (await (price (ctx nil)))) "the body on fire"))))))))

(deftest a-kept-flee-price-is-planned-again-when-the-raw-world-changed-out-of-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks junction :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000)}))
              _ (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
              c (ctx-of p)
              chasers [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}]
              ends (search/ring {:x 0.5 :z 0.5} chasers 10 4)
              price (fn ^:async price [] (await (search/price! c ends {:chasers chasers})))
              _ (await (price))
              base (await (price))]
          (is (identical? base (await (price))))
          (swap! (fake-world/state raw) assoc-in [:blocks "-3,64,0"] "stone")
          (is (not (identical? base (await (price)))) "a cell changed with no write the body sensed: the planner reads it live"))))))

(deftest a-death-by-terrain-fades-in-the-kept-price-as-the-clock-moves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks junction :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000)}))
              _ (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
              now (atom 0)
              data (mem/add-entry mem/empty-data :deaths {:t 0 :data {:pos {:x 3 :y 64 :z 0} :cause "lava" :death-t 0}} nil)
              c (assoc (ctx-of p) :view (fn [] {:data data :now @now}))
              chasers [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}]
              ends (search/ring {:x 0.5 :z 0.5} chasers 10 4)
              price (fn ^:async price [] (await (search/price! c ends {:chasers chasers})))
              _ (await (price))
              base (await (price))]
          (is (identical? base (await (price))) "the same death, the same price")
          (reset! now 30000)
          (is (not (identical? base (await (price)))) "the death has faded: the danger it gives the plan is another"))))))

(deftest a-memory-that-aged-with-no-write-to-the-world-is-priced-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              raw (tu/fake {:blocks (assoc junction "3,64,0" "oak_door") :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              rw (fake-raw/create raw)
              per (perception/create rw {:now #(deref clock)})
              p (perception/wrap raw per)
              _ (perception/pass! per {:effort 1 :reuse? true})
              ^js st (:st per)
              _ (store/record! st (store/store-of st "overworld") 3 64 0 (.stateAt ^js rw 3 64 0) @clock)
              c (ctx-of p)
              chasers [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}]
              ends (search/ring {:x 0.5 :z 0.5} chasers 10 4)
              price (fn ^:async price [] (await (search/price! c ends {:chasers chasers})))
              _ (await (price))
              base (await (price))]
          (is (identical? base (await (price))) "the memory as it was: the kept price")
          (swap! clock + 11000)
          (is (not (identical? base (await (price)))) "the clock passed the memory limit: priced again"))))))

(deftest a-landing-seen-again-with-no-write-to-the-world-is-priced-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/fake {:blocks junction :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000)}))
              _ (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
              c (ctx-of p)
              seen? (atom false)
              plan-to! search/plan-to!
              chasers [{:name "zombie" :mob "zombie" :pos {:x -4.5 :y 64 :z 0.5}}]
              ends (search/ring {:x 0.5 :z 0.5} chasers 10 4)
              answer wworld/landing-answer
              price (fn ^:async price []
                      (set! wworld/landing-answer (fn [_ _ _ _ _] @seen?))
                      (set! search/plan-to! (fn ^:async asking [c pw policy q n memo]
                                              ((wworld/landing-seen p memo) 2 63 0 false)
                                              (await (plan-to! c pw policy q n memo))))
                      (let [v (await (search/price! c ends {:chasers chasers}))]
                        (set! wworld/landing-answer answer)
                        (set! search/plan-to! plan-to!)
                        v))
              _ (await (price))
              base (await (price))]
          (is (identical? base (await (price))) "the landing as it was: the kept price")
          (reset! seen? true)
          (is (not (identical? base (await (price)))) "an aged landing seen again, the store untouched: priced again"))))))
