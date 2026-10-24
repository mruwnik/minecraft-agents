(ns engine.flee-search-test
  "jobs.lib.flee-search: flee ends planned by the go-to planner and scored by when the chasers get there (against the
  fake world), and pick's choice among scored ends (pure)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake-world]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.flee-search :as search]
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

(def field (box -32 63 -32 32 63 32 "stone"))

(deftest a-chaser-faster-than-the-flight-catches-every-way-away
  ;; the old search timed the body at a sprint (5.6 blocks a second): a spider 4 blocks behind was left behind on open
  ;; ground; a flight goes at the measured flee pace (steps at 3.62 with a 0.29 s stop between)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [from {:x 0.5 :y 64 :z 0.5}
              behind (fn [d] {:x (- 0.5 d) :y 64 :z 0.5})
              baby (await (ends-of field from [{:name "baby" :pos (behind 3)}] [] {:food 6}))
              spider (await (ends-of field from [{:name "spider" :mob "spider" :pos (behind 4)}] [] {:food 20}))
              far (await (ends-of field from [{:name "zombie" :mob "zombie" :pos (behind 7)}] [] {:food 6}))]
          (is (seq baby) "the ways are planned")
          (is (every? :caught? baby) "a baby zombie runs faster than a body too hungry to sprint")
          (is (nil? (search/pick baby {})) "no flee step: the body is cornered")
          (is (= 1 (:chance (search/caught-chance baby {:k 0 :n 0} {} nil))) "the fight-or-flee weighing prices the flight caught")
          (is (every? :caught? spider) "a spider 4 behind catches a flight")
          (is (some? (search/pick far {})) "one 7 behind is left behind over the ring")
          (is (< (:chance (search/caught-chance far {:k 0 :n 0} {} nil)) 1) "a way left: not caught for sure"))))))

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
    (is (< 0.15 (:chance open) 1) "unseen is the worse case, priced by the ways seen round the body")
    (is (< (:chance open) (:chance boxed) 1) "seen walls on every side make it likelier")
    (is (nil? (:seconds open)) "no way timed: the blows run over the whole flight")))

(deftest the-flight-is-caught-at-the-prior-on-the-best-way-and-its-blows-run-over-the-caught-headings
  (let [free (end-of 4 0 5 :caught? false :caught-s 0 :arrive 5)
        caught (fn [s] (end-of 0 4 9 :caught? true :caught-s s :arrive 5))
        c (fn [ends] (update (search/caught-chance ends {:k 10 :n 10} {} nil) :chance #(/ (js/Math.round (* 1e6 %)) 1e6)))]
    (is (= {:chance 0.15 :seconds 0.5} (c [free (caught 2) (caught 0.5) (caught 1)])) "the other headings only choose the way: the prior, the least seconds of a caught one")
    (is (= {:chance 0.15} (c [free])) "no heading caught: nothing bounds the blows")
    (is (= {:chance 1 :seconds 0.5 :cornered? true} (c [(caught 2) (caught 0.5)])) "every way caught: the flight is the cornered step")
    (is (= {:chance 1 :cornered? false} (c [(end-of 4 0 1 :found? false :caught? false :caught-s 0 :gain 2)]))
        "a pocket the flight still runs to, with no way out to call free: a walk caught for certain")))

(deftest ways-that-stop-at-unseen-cells-are-priced-by-the-ways-seen-not-caught
  (let [stops (fn [& more] (apply end-of 4 0 1 :found? false :unseen [5 64 0] :caught-s 0 :caught? false :arrive 5 more))
        chance #(:chance (search/caught-chance % {:k 10 :n 10} {} nil))]
    (is (< (js/Math.abs (- (- 1 (* 0.85 (/ 11 12))) (chance [(stops) (stops)]))) 1e-9) "a way into the unseen: the prior and its dead end, (k+1)/(n+2) of the seen ways on, not a catch")
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
    (is (< (js/Math.abs (- (- 1 (* 0.85 (/ 11 12))) (:chance looked))) 1e-9) "the look finds a way at (k+1)/(n+2): priced as an unseen way")
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
