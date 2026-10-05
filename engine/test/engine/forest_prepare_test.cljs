(ns engine.forest-prepare-test
  "jobs.forestry.prepare: a forest plan's planting spots made ready (cleared, soiled, planted), against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.forest-maintain-test :refer [ground forest-plan item start ticks digs places warns listed? give! with-zones]]
            [engine.harvest-test :as h]
            [engine.test-util :as tu]
            [engine.world :as world]
            [jobs.forestry.prepare :as prepare]))

(def job 'jobs.forestry.prepare)

(def zero {:cleared 0 :soiled 0 :planted 0 :dammed 0 :short {} :wrong [] :no-soil [] :cramped [] :wet [] :refused []})

(defn expect [m] (merge zero m))

(defn oak-plan [& cells] (forest-plan ["a" (vec cells) "oak"]))

(def one-cell (oak-plan [3 64 0]))
(def two-cells (oak-plan [3 64 0] [4 64 0]))

(defn ^:async outcome
  "Run the job over plans in a world of spec: the start map with :result (the job's result, :not-done when it never ended)."
  ([spec plans] (outcome spec plans {:plan "forest"}))
  ([spec plans args]
   (let [s (start spec plans)]
     (assoc s :result (await (h/child-outcome (:eng s) job args 300))))))

(defn ^:async run
  "Submit the job with args in a world over plans and tick it n times: the start map."
  [spec plans args n]
  (let [s (start spec plans)]
    (core/submit! (:eng s) (list job args) {})
    (await (ticks (:eng s) n))
    s))

(defn world [blocks & inventory]
  {:blocks (merge (ground [[3 0] [4 0]]) blocks) :inventory (vec inventory)})

(defn calls-made [p] (count (.-calls (.-world p))))

(defn wet-world
  "A world where the cells in levels {\"x,y,z\" n} hold water of that level (the fake does not flow)."
  [levels & inventory]
  (-> (apply world (into {} (map (fn [[cell _]] [cell "water"])) levels) inventory)
      (assoc :states (into {} (map (fn [[cell n]] [cell {:level n}])) levels))))

(defn drain!
  "Remove the water of the cell from the world, as a receding stream does."
  [p cell]
  (let [pos (fake/parse-cell cell)]
    (fake/remove-block! p pos)
    (swap! (fake/state p) update :states dissoc pos)))

;; ------------------------------------------------------------------ pure

(deftest a-tree-grows-through-air-leaves-saplings-and-plants-only
  (are [name free] (= free (prepare/tree-free? name))
    "air" true "cave_air" true "oak_leaves" true "short_grass" true "tall_grass" true "poppy" true "snow" true
    "vine" true "birch_sapling" true "stone" false "dirt" false "oak_log" false "oak_planks" false "water" false))

(deftest headroom-is-the-species-table-and-the-argument-overrides-it
  (are [species over expected] (= expected (prepare/headroom-of species over))
    "oak" nil 7
    "birch" nil 8
    "oak" {"oak" 3} 3
    "oak" {"birch" 3} 7
    "mangrove" nil nil))

(deftest only-natural-ground-is-replaced
  (are [name natural] (= natural (prepare/natural-ground? name))
    "stone" true "sand" true "gravel" true "cobblestone" true "deepslate" true "sandstone" true
    "oak_planks" false "oak_log" false "chest" false "white_wool" false "dirt" false "water" false))

(defn levels-at [m] (fn [pos] (get m pos)))

(deftest upstream-walks-a-stream-back-to-its-source
  (are [levels start source] (= source (prepare/upstream (levels-at levels) start))
    {[0 64 0] 0} [0 64 0] [0 64 0]
    {[0 64 0] 0 [1 64 0] 1 [2 64 0] 2} [2 64 0] [0 64 0]
    {[0 66 0] 0 [0 65 0] 8 [0 64 0] 8} [0 64 0] [0 66 0]
    {[2 66 1] 0 [2 65 1] 8 [2 64 1] 8 [2 64 0] 1} [2 64 0] [2 66 1]
    {[5 64 0] 0 [4 64 0] 1 [3 64 0] 2 [2 64 0] 1 [1 64 0] 0} [3 64 0] [5 64 0]
    {[1 64 0] 3 [2 64 0] 2} [2 64 0] nil
    {[1 64 0] 2 [0 64 0] 3} [1 64 0] nil
    {[2 64 0] 2} [1 64 0] nil
    {[2 64 0] 2} [2 64 0] nil))

(deftest upstream-gives-up-after-sixteen-steps
  (are [top source] (= source (prepare/upstream (levels-at (into {[0 top 0] 0} (map (fn [y] [[0 y 0] 8])) (range 64 top))) [0 64 0]))
    80 [0 80 0]
    81 nil))

;; ------------------------------------------------------------------ the cell itself

(deftest a-stray-in-the-cell-is-dug-and-the-sapling-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [stray ["short_grass" "tall_grass" "poppy" "snow" "oak_leaves" "stone" "fern"]]
          (let [{:keys [p result]} (await (outcome (world {"3,64,0" stray} (item "oak_sapling" 1)) {"forest" one-cell}))]
            (is (= [[3 64 0]] (digs p)) stray)
            (is (= [[3 64 0 "oak_sapling"]] (places p)) stray)
            (is (= "oak_sapling" (h/block-at p 3 64 0)) stray)
            (is (= (expect {:cleared 1 :planted 1}) result) stray)))))))

(deftest a-pickaxe-that-breaks-in-the-clearing-dig-says-tool-broke-and-tool-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {"3,64,0" "stone"} (item "stone_pickaxe" 1)) {"forest" one-cell})]
          (.override (.-world (:p s)) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (swap! (fake/state (:p s)) assoc :inventory [])
                         r)))
          (core/submit! (:eng s) (list job {:plan "forest"}) {})
          (await (ticks (:eng s) 20))
          (is (= [[3 64 0]] (digs (:p s))))
          (is (= 1 (count (h/events-of (:seen s) :tool.broke))))
          (is (= 1 (count (h/events-of (:seen s) :tool.none)))))))))

(deftest a-bare-cell-over-soil-is-just-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result]} (await (outcome (world {} (item "oak_sapling" 1)) {"forest" one-cell}))]
          (is (= [] (digs p)))
          (is (= [[3 64 0 "oak_sapling"]] (places p)))
          (is (= (expect {:planted 1}) result)))))))

;; ------------------------------------------------------------------ the soil under it

(deftest natural-ground-under-the-cell-is-replaced-with-carried-dirt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [under ["stone" "sand" "gravel" "cobblestone"]]
          (let [{:keys [p result]} (await (outcome (world {"3,63,0" under} (item "dirt" 1) (item "oak_sapling" 1)) {"forest" one-cell}))]
            (is (= [[3 63 0]] (digs p)) under)
            (is (= [[3 63 0 "dirt"] [3 64 0 "oak_sapling"]] (places p)) under)
            (is (= "dirt" (h/block-at p 3 63 0)) under)
            (is (= (expect {:soiled 1 :planted 1}) result) under)))))))

(deftest without-dirt-the-cell-is-left-and-reported-as-no-soil
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result seen]} (await (outcome (world {"3,64,0" "short_grass" "4,63,0" "stone"} (item "oak_sapling" 2)) {"forest" two-cells}))]
          (is (= [[3 64 0]] (digs p)) "the stone under the other cell is not dug")
          (is (= [[3 64 0 "oak_sapling"]] (places p)))
          (is (= "stone" (h/block-at p 4 63 0)))
          (is (= (expect {:cleared 1 :planted 1 :no-soil [{:pos [4 64 0] :why :no-dirt}]}) result))
          (is (= [{:pos {:x 4 :y 64 :z 0} :why :no-dirt}] (warns seen :prepare.no-soil))))))))

(deftest ground-that-is-not-natural-is-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[under why] [["oak_planks" :other-block] ["chest" :kept] ["air" :hollow] ["water" :fluid] ["oak_log" :other-block]]]
          (let [{:keys [p result]} (await (outcome (world {"4,63,0" under "3,64,0" "short_grass"} (item "dirt" 3) (item "oak_sapling" 2))
                                                   {"forest" two-cells}))]
            (is (= [[3 64 0]] (digs p)) under)
            (is (= [[3 64 0 "oak_sapling"]] (places p)) under)
            (is (= (expect {:cleared 1 :planted 1 :no-soil [{:pos [4 64 0] :why why}]}) result) under)))))))

(deftest a-plan-cell-under-the-cell-is-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "forest"
                    :parts [{:id "a" :cells [[3 64 0] [4 64 0]] :want {:tree "oak"}} {:id "b" :cells [[4 63 0]] :want "stone"}]}
              {:keys [p result]} (await (outcome (world {"4,63,0" "stone" "3,64,0" "short_grass"} (item "dirt" 3) (item "oak_sapling" 2))
                                                 {"forest" plan}))]
          (is (= [[3 64 0]] (digs p)))
          (is (= (expect {:cleared 1 :planted 1 :no-soil [{:pos [4 64 0] :why :planned}]}) result)))))))

;; ------------------------------------------------------------------ the sapling

(deftest cells-without-a-carried-sapling-are-listed-as-short-with-one-note
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [carried [[] [(item "birch_sapling" 4)]]]
          (let [{:keys [p result seen]} (await (outcome (apply world {"3,64,0" "short_grass" "4,64,0" "poppy"} carried) {"forest" two-cells}))]
            (is (= [[3 64 0] [4 64 0]] (digs p)))
            (is (= (expect {:cleared 2 :short {"oak" 2}}) result))
            (is (= [{:species "oak" :missing 2}] (warns seen :prepare.short)))))
        (let [{:keys [p result]} (await (outcome (world {"3,64,0" "short_grass" "4,64,0" "poppy"} (item "oak_sapling" 1)) {"forest" two-cells}))]
          (is (= [[3 64 0 "oak_sapling"]] (places p)) "the nearest cell is planted first")
          (is (= (expect {:cleared 2 :planted 1 :short {"oak" 1}}) result)))))))

(deftest a-sapling-of-another-species-in-the-cell-is-left-and-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [found ["birch_sapling" "birch_log" "chest"]]
          (let [{:keys [p result seen]} (await (outcome (world {"3,64,0" found "4,64,0" "short_grass"} (item "oak_sapling" 2)) {"forest" two-cells}))]
            (is (= [[4 64 0]] (digs p)) found)
            (is (= [[4 64 0 "oak_sapling"]] (places p)) found)
            (is (= found (h/block-at p 3 64 0)) found)
            (is (= (expect {:cleared 1 :planted 1 :wrong [{:pos [3 64 0] :found found :species "oak"}]}) result) found)
            (is (= [{:pos {:x 3 :y 64 :z 0} :found found :species "oak"}] (warns seen :prepare.wrong)) found)))))))

(deftest the-species-own-sapling-and-log-are-left-without-a-word
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [here ["oak_sapling" "oak_log"]]
          (let [{:keys [p result seen]} (await (outcome (world {"3,64,0" here "4,64,0" "short_grass"} (item "oak_sapling" 2)) {"forest" two-cells}))]
            (is (= [[4 64 0]] (digs p)) here)
            (is (= (expect {:cleared 1 :planted 1}) result) here)
            (is (= [] (warns seen :prepare.wrong)) here)))))))

;; ------------------------------------------------------------------ headroom

(deftest blocks-in-the-growth-space-keep-the-cell-untouched-and-leaves-do-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[blocks args reported] [[{"3,70,0" "stone"} {:plan "forest"} {:pos [3 64 0] :at [3 70 0] :block "stone"}]
                                        [{"3,67,0" "oak_log"} {:plan "forest"} {:pos [3 64 0] :at [3 67 0] :block "oak_log"}]
                                        [{"3,67,0" "stone"} {:plan "forest" :headroom {"oak" 4}} {:pos [3 64 0] :at [3 67 0] :block "stone"}]]]
          (let [{:keys [p result seen]} (await (outcome (world (merge {"3,64,0" "short_grass" "4,64,0" "short_grass"} blocks) (item "oak_sapling" 2))
                                                        {"forest" two-cells} args))]
            (is (= [[4 64 0]] (digs p)) (pr-str blocks))
            (is (= [[4 64 0 "oak_sapling"]] (places p)) (pr-str blocks))
            (is (= (expect {:cleared 1 :planted 1 :cramped [reported]}) result) (pr-str blocks))
            (is (= 1 (count (warns seen :prepare.cramped))))))
        (doseq [blocks [{"3,71,0" "stone"} {"3,66,0" "oak_leaves" "3,67,0" "short_grass" "3,68,0" "vine"} {"3,70,0" "birch_sapling"}]]
          (let [{:keys [p result]} (await (outcome (world (merge {"3,64,0" "short_grass"} blocks) (item "oak_sapling" 1)) {"forest" one-cell}))]
            (is (= [[3 64 0]] (digs p)) (pr-str blocks))
            (is (= (expect {:cleared 1 :planted 1}) result) (pr-str blocks))))))))

(deftest a-taller-species-needs-more-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[3 64 0]] "birch"])
              {:keys [p seen]} (await (run (world {"3,64,0" "short_grass" "3,71,0" "stone"} (item "birch_sapling" 1)) {"forest" plan} {:plan "forest"} 20))]
          (is (= 0 (calls-made p)))
          (is (= [{:pos {:x 3 :y 64 :z 0} :at [3 71 0] :block "stone"}] (warns seen :prepare.cramped))))))))

;; ------------------------------------------------------------------ access

(def stone-plan {:id "other" :parts [{:id "o" :cells [[4 64 0]] :want "stone"}]})

(deftest a-zone-or-another-plans-footprint-refuses-the-cell-and-the-rest-is-prepared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones plans cleared planted reason]
                [[[] {"forest" two-cells "other" stone-plan} 1 1 :footprint]]]
          (let [{:keys [p result seen]} (await (with-zones zones #(outcome (world {"3,64,0" "short_grass" "4,64,0" "short_grass"} (item "oak_sapling" 2)) plans)))]
            (is (= (expect {:cleared cleared :planted planted :refused [{:pos [4 64 0] :reason reason}]}) result) (str reason))
            (is (= [[3 64 0 "oak_sapling"]] (places p)) (str reason))
            (is (= [{:pos {:x 4 :y 64 :z 0} :reason reason}] (warns seen :prepare.refused)) (str reason))))))))

(deftest a-zone-over-a-plan-cell-is-no-obstacle-the-plan-is-the-permission
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [allow [#{} #{:dig}]]
          (let [{:keys [p result]} (await (with-zones [{:name "vault" :min [4 64 0] :max [4 64 0] :allow allow}]
                                            #(outcome (world {"3,64,0" "short_grass" "4,64,0" "short_grass"} (item "oak_sapling" 2)) {"forest" two-cells})))]
            (is (= (expect {:cleared 2 :planted 2}) result) (pr-str allow))
            (is (= [[3 64 0 "oak_sapling"] [4 64 0 "oak_sapling"]] (places p)) (pr-str allow))))))))

(deftest a-zone-over-the-ground-under-a-cell-refuses-the-soil-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result]} (await (with-zones [{:name "vault" :min [3 63 0] :max [3 63 0] :allow #{}}]
                                          #(outcome (world {"3,63,0" "stone" "3,64,0" "short_grass"} (item "dirt" 1) (item "oak_sapling" 1)) {"forest" one-cell})))]
          (is (= [[3 64 0]] (digs p)) "the cell is cleared, the ground is not touched")
          (is (= [] (places p)))
          (is (= (expect {:cleared 1 :refused [{:pos [3 64 0] :reason :zone}]}) result)))))))

(deftest cells-outside-the-plan-are-never-touched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (outcome (world {"3,64,0" "short_grass" "5,64,0" "short_grass" "3,65,1" "short_grass" "2,63,0" "stone"}
                                                 (item "oak_sapling" 3) (item "dirt" 3))
                                          {"forest" one-cell}))]
          (is (= [[3 64 0]] (digs p)))
          (is (= [[3 64 0 "oak_sapling"]] (places p))))))))

(deftest only-the-asked-part-is-prepared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[3 64 0]] "oak"] ["b" [[4 64 0]] "oak"])
              {:keys [p]} (await (outcome (world {"3,64,0" "short_grass" "4,64,0" "short_grass"} (item "oak_sapling" 2))
                                          {"forest" plan} {:plan "forest" :part "b"}))]
          (is (= [[4 64 0]] (digs p)))
          (is (= [[4 64 0 "oak_sapling"]] (places p))))))))

(deftest the-access-rules-are-asked-again-right-before-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {"3,64,0" "short_grass" "4,64,0" "short_grass"} (item "oak_sapling" 2)) {"forest" two-cells})]
          (.override (.-world (:p s)) "dig"
                     (fn ^:async f [token args impl]
                       (world/set-data! (:w s) {"forest" two-cells "other" stone-plan} {})
                       (await (impl token args))))
          (core/submit! (:eng s) (list job {:plan "forest"}) {})
          (await (ticks (:eng s) 40))
          (is (= [[3 64 0]] (digs (:p s))) "the first dig goes, then the other plan's claim stops the second")
          (is (= [{:pos {:x 4 :y 64 :z 0} :reason :footprint}] (warns (:seen s) :prepare.refused))))))))

;; ------------------------------------------------------------------ walking

(def far-plan (oak-plan [9 64 0]))

(defn far-world [blocks & inventory]
  {:blocks (merge (ground [[3 0] [9 0]]) blocks) :inventory (vec inventory)})

(deftest a-claim-made-while-the-body-walks-stops-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [claim {:id "other" :parts [{:id "o" :cells [[9 64 0]] :want "stone"}]}
              s (start (far-world {"9,64,0" "short_grass"} (item "oak_sapling" 1)) {"forest" far-plan})]
          (.override (.-world (:p s)) "steer"
                     (fn ^:async f [token args impl]
                       (world/set-data! (:w s) {"forest" far-plan "other" claim} {})
                       (await (impl token args))))
          (core/submit! (:eng s) (list job {:plan "forest"}) {})
          (await (ticks (:eng s) 30))
          (is (= [] (digs (:p s))) "asked again after the walk, right before the dig")
          (is (= [{:pos {:x 9 :y 64 :z 0} :reason :footprint}] (warns (:seen s) :prepare.refused))))))))

(deftest a-refused-cell-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [claim {:id "other" :parts [{:id "o" :cells [[9 64 0]] :want "stone"}]}
              {:keys [seen eng]} (await (run (far-world {"9,64,0" "short_grass"} (item "oak_sapling" 1)) {"forest" far-plan "other" claim} {:plan "forest"} 20))]
          (is (= [] (tu/walked-to eng)) "asked when the cell is chosen, before the walk")
          (is (= [{:pos {:x 9 :y 64 :z 0} :reason :footprint}] (warns seen :prepare.refused))))))))

(deftest the-body-steps-off-a-cell-before-soiling-and-planting-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result]} (await (outcome (far-world {"9,63,0" "stone"} (item "dirt" 1) (item "oak_sapling" 1)) {"forest" far-plan}))]
          (is (= [[9 63 0]] (digs p)))
          (is (= [[9 63 0 "dirt"] [9 64 0 "oak_sapling"]] (places p)))
          (is (= (expect {:soiled 1 :planted 1}) result)))))))

(deftest a-cell-that-cannot-be-reached-is-refused-after-three-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[9 64 0] [3 64 0]] "oak"])
              {:keys [p result seen eng]} (await (outcome (assoc (far-world {"9,64,0" "short_grass" "3,64,0" "short_grass"} (item "oak_sapling" 2)) :unreachable ["9,64,0"])
                                                          {"forest" plan}))]
          (is (= 3 (count (filter #(= 9 (:x %)) (tu/walked-to eng)))) "three walks toward the cell at x 9")
          (is (= [[3 64 0]] (digs p)))
          (is (= (expect {:cleared 1 :planted 1 :refused [{:pos [9 64 0] :reason :unreachable}]}) result))
          (is (= 1 (count (warns seen :prepare.refused)))))))))

(deftest a-species-without-a-table-entry-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[3 64 0]] "mangrove"] ["b" [[4 64 0]] "oak"])
              {:keys [p result]} (await (outcome (world {"3,64,0" "short_grass" "4,64,0" "short_grass"} (item "oak_sapling" 2) (item "mangrove_propagule" 1))
                                                 {"forest" plan}))]
          (is (= [[4 64 0]] (digs p)))
          (is (= (expect {:cleared 1 :planted 1 :refused [{:pos [3 64 0] :reason :unsupported-species}]}) result)))))))

;; ------------------------------------------------------------------ declining, not spinning

(deftest the-check-declines-a-plan-it-cannot-work-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [crops {:id "crops" :parts [{:id "c" :cells [[3 64 0]] :want {:crop "wheat"}}]}]
          (doseq [[plans args reason] [[{} {:plan "nope"} "no such plan"]
                                       [{"crops" crops} {:plan "crops"} "no tree cells"]
                                       [{"forest" one-cell} {:plan "forest" :part "zz"} "no tree cells"]]]
            (let [s (start (world {"3,64,0" "short_grass"} (item "oak_sapling" 1)) plans)]
              (core/submit! (:eng s) (list job args) {})
              (await (ticks (:eng s) 5))
              (is (= [0 [[(:plan args) reason]]]
                     [(calls-made (:p s)) (mapv (juxt :plan :reason) (h/events-of (:seen s) :prepare.declined))])
                  reason))))))))

(deftest a-broken-plan-declines-naming-the-error
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen w eng]} (start (world {"3,64,0" "short_grass"} (item "oak_sapling" 1)) {})]
          (reset! (:state w) (assoc @(:state w) :plans {"forest" {:error "unreadable EDN: eof"}}))
          (core/submit! eng (list job {:plan "forest"}) {})
          (await (ticks eng 4))
          (is (= 0 (calls-made p)))
          (is (= ["the plan cannot be read: unreadable EDN: eof"] (mapv :reason (h/events-of seen :prepare.declined)))))))))

(deftest without-a-zone-list-the-job-declines-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (with-zones nil #(run (world {"3,64,0" "short_grass"} (item "oak_sapling" 1)) {"forest" one-cell} {:plan "forest"} 10)))]
          (is (= 0 (calls-made p)))
          (is (= ["no zone list"] (mapv :reason (h/events-of seen :prepare.declined)))))))))

(deftest a-field-nothing-can-improve-is-left-alone-with-one-note-per-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (oak-plan [3 64 0] [4 64 0] [5 64 0])
              spec {:blocks (merge (ground [[3 0] [4 0] [5 0]]) {"3,64,0" "birch_sapling" "4,63,0" "stone" "5,70,0" "stone"})
                    :inventory [(item "oak_sapling" 3)]}
              {:keys [p eng seen]} (await (run spec {"forest" plan} {:plan "forest"} 40))]
          (is (= 0 (calls-made p)) "40 ticks, not one act")
          (is (listed? eng) "waiting")
          (is (= 1 (count (warns seen :prepare.wrong))))
          (is (= 1 (count (warns seen :prepare.no-soil))))
          (is (= 1 (count (warns seen :prepare.cramped)))))))))

(deftest the-job-wakes-when-what-it-lacked-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng seen]} (await (run (world {"3,64,0" "oak_sapling" "4,63,0" "stone"}) {"forest" two-cells} {:plan "forest"} 20))]
          (is (= 0 (calls-made p)))
          (give! p "dirt" 1)
          (give! p "oak_sapling" 1)
          (await (ticks eng 30))
          (is (= [[4 63 0]] (digs p)))
          (is (= [[4 63 0 "dirt"] [4 64 0 "oak_sapling"]] (places p))))))))

(deftest a-second-run-over-a-prepared-field-does-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng result]} (await (outcome (world {"3,64,0" "short_grass" "4,63,0" "stone"} (item "dirt" 1) (item "oak_sapling" 2)) {"forest" two-cells}))
              before (calls-made p)]
          (is (= (expect {:cleared 1 :soiled 1 :planted 2}) result))
          (core/submit! eng (list job {:plan "forest"}) {})
          (await (ticks eng 40))
          (is (= before (calls-made p)) "no act in 40 ticks")
          (is (listed? eng)))))))

(deftest a-plan-deleted-while-the-job-runs-stops-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {"3,64,0" "short_grass" "4,64,0" "short_grass"} (item "oak_sapling" 2)) {"forest" two-cells})]
          (.override (.-world (:p s)) "dig"
                     (fn ^:async f [token args impl]
                       (world/set-data! (:w s) {} {})
                       (await (impl token args))))
          (core/submit! (:eng s) (list job {:plan "forest"}) {})
          (await (ticks (:eng s) 40))
          (is (= [[3 64 0]] (digs (:p s))) "the dig under way ends, nothing after it")
          (is (= [] (places (:p s))))
          (is (= ["no such plan"] (mapv :reason (h/events-of (:seen s) :prepare.declined)))))))))

(deftest the-saplings-a-dug-leaf-drops-are-collected-and-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result]} (await (outcome (assoc (world {"3,64,0" "oak_leaves"}) :drops {"oak_leaves" "oak_sapling"}) {"forest" one-cell}))]
          (is (= [[3 64 0 "oak_sapling"]] (places p)))
          (is (= (expect {:cleared 1 :planted 1}) result)))))))

(deftest a-dug-hole-is-filled-after-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec (world {"3,63,0" "stone"} (item "dirt" 1) (item "oak_sapling" 1))
              dir (tu/tmp-dir)
              p (tu/fake-on-floor spec)
              first-run (start spec {"forest" one-cell} dir p)]
          (core/submit! (:eng first-run) (list job {:plan "forest"}) {})
          (loop [i 0]
            (when (< i 30)
              (let [{:keys [eng]} (start spec {"forest" one-cell} dir p)]
                (await (ticks eng 1))
                (recur (inc i)))))
          (is (= [[3 63 0]] (digs p)))
          (is (= [[3 63 0 "dirt"] [3 64 0 "oak_sapling"]] (places p))))))))

;; ------------------------------------------------------------------ water in the cell

(def sapling-and-dirt [(item "dirt" 1) (item "oak_sapling" 1)])

(deftest a-source-in-the-cell-is-filled-with-dirt-dug-out-and-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result]} (await (outcome (apply wet-world {"3,64,0" 0} sapling-and-dirt) {"forest" one-cell}))]
          (is (= [[3 64 0 "dirt"] [3 64 0 "oak_sapling"]] (places p)))
          (is (= [[3 64 0]] (digs p)))
          (is (= "oak_sapling" (h/block-at p 3 64 0)))
          (is (= (expect {:dammed 1 :cleared 1 :planted 1}) result)))))))

(deftest a-source-cell-with-source-neighbours-dams-them-before-it-is-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng seen]} (await (run (assoc (update (wet-world {"3,64,0" 0 "2,64,0" 0 "4,64,0" 0} (item "dirt" 3) (item "oak_sapling" 1)) :blocks merge (ground [[1 0] [2 0] [5 0]])) :self {:pos {:x 1 :y 64 :z 0}})
                                               {"forest" one-cell} {:plan "forest"} 60))]
          (is (= [[4 64 0 "dirt"] [2 64 0 "dirt"] [3 64 0 "dirt"] [3 64 0 "oak_sapling"]] (places p)))
          (is (= [[3 64 0]] (digs p)))
          (is (= [{:dammed 3 :cleared 1 :planted 1}] (mapv #(select-keys % [:dammed :cleared :planted]) (h/events-of seen :prepare.done)))))))))

(deftest a-source-beside-the-cell-is-dammed-and-the-flow-given-time-to-recede
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng seen]} (await (run (apply wet-world {"3,64,0" 1 "2,64,0" 0} sapling-and-dirt) {"forest" one-cell} {:plan "forest"} 6))
              before (calls-made p)]
          (is (= [[2 64 0 "dirt"]] (places p)))
          (is (= "dirt" (h/block-at p 2 64 0)))
          (await (ticks eng 4))
          (is (= before (calls-made p)) "the flow is waited out, nothing is placed in it")
          (is (= [] (h/events-of seen :prepare.done)))
          (is (listed? eng))
          (drain! p "3,64,0")
          (await (ticks eng 30))
          (is (= [[2 64 0 "dirt"] [3 64 0 "oak_sapling"]] (places p)))
          (is (= [{:dammed 1 :planted 1}] (mapv #(select-keys % [:dammed :planted]) (h/events-of seen :prepare.done)))))))))

(deftest a-stream-is-followed-to-its-source-and-only-that-is-dammed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run (assoc (apply wet-world {"0,64,0" 0 "1,64,0" 1 "2,64,0" 2 "3,64,0" 3} sapling-and-dirt) :self {:pos {:x 1 :y 64 :z 0}})
                                      {"forest" one-cell} {:plan "forest"} 60))]
          (is (= [[0 64 0 "dirt"]] (places p))))))))

(deftest a-falling-column-is-followed-up-to-its-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run (apply wet-world {"3,64,0" 8 "3,65,0" 8 "3,66,0" 0} sapling-and-dirt)
                                      {"forest" one-cell} {:plan "forest"} 60))]
          (is (= [[3 66 0 "dirt"]] (places p))))))))

(def stray-and-sapling {"4,64,0" "short_grass"})

(deftest a-source-the-rules-refuse-leaves-the-cell-wet-with-one-note
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result seen]} (await (with-zones [{:name "vault" :min [2 64 0] :max [2 64 0] :allow #{}}]
                                                #(outcome (-> (apply wet-world {"3,64,0" 1 "2,64,0" 0} sapling-and-dirt)
                                                              (update :blocks merge stray-and-sapling))
                                                          {"forest" two-cells})))]
          (is (= [[4 64 0 "oak_sapling"]] (places p)) "only the dry cell is worked")
          (is (= [[4 64 0]] (digs p)))
          (is (= (expect {:cleared 1 :planted 1 :wet [{:pos [3 64 0] :why :zone :source [2 64 0]}]}) result))
          (is (= [{:pos {:x 3 :y 64 :z 0} :why :zone :water-source [2 64 0]}] (warns seen :prepare.wet)))
          (is (= [] (warns seen :prepare.refused)))
          (is (= [] (warns seen :prepare.wrong))))))))

(deftest without-dirt-a-wet-cell-is-left-and-reported-with-its-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result seen]} (await (outcome (-> (wet-world {"3,64,0" 0} (item "oak_sapling" 1))
                                                          (update :blocks merge stray-and-sapling))
                                                      {"forest" two-cells}))]
          (is (= [[4 64 0 "oak_sapling"]] (places p)) "nothing is placed in the water")
          (is (= [[4 64 0]] (digs p)))
          (is (= (expect {:cleared 1 :planted 1 :wet [{:pos [3 64 0] :why :no-dirt :source [3 64 0]}]}) result))
          (is (= 1 (count (warns seen :prepare.wet)))))))))

(deftest water-that-cannot-be-traced-is-left-and-reported-without-a-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label water] [["level 3 and nothing around" (wet-world {"3,64,0" 3} (item "dirt" 1) (item "oak_sapling" 1))]
                                  ["water without a level" (assoc (world {"3,64,0" "water"} (item "dirt" 1) (item "oak_sapling" 1)) :states {})]]]
          (let [{:keys [p result seen]} (await (outcome (update water :blocks merge stray-and-sapling) {"forest" two-cells}))]
            (is (= [[4 64 0 "oak_sapling"]] (places p)) label)
            (is (= (expect {:cleared 1 :planted 1 :wet [{:pos [3 64 0] :why :untraced}]}) result))
            (is (= [{:pos {:x 3 :y 64 :z 0} :why :untraced}] (warns seen :prepare.wet)))))))))

(deftest lava-in-the-cell-is-wrong-and-never-touched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p result]} (await (outcome (far-world {"3,64,0" "lava" "9,64,0" "short_grass"} (item "dirt" 1) (item "oak_sapling" 2)) {"forest" (oak-plan [3 64 0] [9 64 0])}))]
          (is (= [[9 64 0]] (digs p)))
          (is (= [[9 64 0 "oak_sapling"]] (places p)))
          (is (= "lava" (h/block-at p 3 64 0)))
          (is (= (expect {:cleared 1 :planted 1 :wrong [{:pos [3 64 0] :found "lava" :species "oak"}]}) result)))))))
