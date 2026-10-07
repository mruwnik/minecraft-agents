(ns engine.forest-maintain-test
  "jobs.forestry.maintain (a forest plan's tree cells) and fell-tree's :at, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.events :as events]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.forestry.maintain :as maintain]
            [jobs.forestry.trees :as trees]
            [jobs.lib.pace :as pace]
            [plan.shape :as shape]))

(def job 'jobs.forestry.maintain)

(defn k [x y z] (str x "," y "," z))

(defn ground
  "Dirt at y 63 under every [x z]."
  [cells]
  (into {} (map (fn [[x z]] [(k x 63 z) "dirt"])) cells))

(defn forest-plan
  "A plan whose parts are given as [id cells species]."
  [& parts]
  {:id "forest"
   :parts (mapv (fn [[id cells species]] {:id id :cells cells :want {:tree species}}) parts)})

(def oak-cell (forest-plan ["a" [[3 64 0]] "oak"]))

(defn item [name n] {:name name :count n})

(def test-zones
  "The zone list of the worlds start makes (nil: never readable)."
  (atom []))

(defn start
  "An engine over the fake world spec with the plans {id plan} as its world data and the zones in test-zones, on dir
  when given."
  ([spec plans] (start spec plans (tu/tmp-dir)))
  ([spec plans dir] (start spec plans dir (tu/seeing-all (tu/fake-on-floor spec))))
  ([spec plans dir p]
   (let [[seen sink] (tu/legacy-capture-sink)
         w (world/of-data plans {} @test-zones)
         eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now #(deref h/clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                           :world w})]
     {:eng eng :p p :seen seen :w w :dir dir})))

(defn ^:async ticks
  "Tick the engine n times, 700 ms apart."
  [eng n]
  (dotimes [_ n]
    (swap! h/clock + 700)
    (await (core/tick! eng))))

(defn ^:async run
  "Submit the job with args in a world over plans and tick it n times: the start map."
  [spec plans args n]
  (let [s (start spec plans)]
    (core/submit! (:eng s) (list job args) {})
    (await (ticks (:eng s) n))
    s))

(defn pos-xyz [a] (let [p (.-pos (.-args a))] [(.-x p) (.-y p) (.-z p)]))
(defn digs [p] (mapv pos-xyz (h/calls p "dig")))
(defn places [p] (mapv #(let [a (.-args %) q (.-pos a)] [(.-x q) (.-y q) (.-z q) (.-item a)]) (h/calls p "place")))
(defn listed? [eng] (boolean (seq (:list (core/state eng)))))
(defn warns [seen kind] (mapv #(dissoc % :seq :t :body :source :job :cause :level :attention :text :log :logs :round :kind :chain) (h/events-of seen kind)))
(defn set-block! [p x y z name] (fake/set-block! p [x y z] name))
(defn give! [p name n] (fake/add-item! p name n))

(def oak-world
  "An oak at x 3 (4 logs) on a planned cell, an oak at x 9 outside the plan, one sapling carried."
  {:blocks (merge (ground [[3 0] [9 0]]) (lt/tree 3 0 "oak" 4) (lt/tree 9 0 "oak" 4))
   :inventory [(item "oak_sapling" 1)]})

;; ------------------------------------------------------------------ pure

(deftest cells-read-the-tree-cells-of-a-plan-answer
  (let [answer {:cells [{:pos [1 64 1] :want {:tree "oak"} :part "a"} {:pos [2 64 1] :want {:tree "birch"} :part "b"}
                        {:pos [3 64 1] :want "torch" :part "b"} {:pos [4 64 1] :want {:crop "wheat"} :part "c"}]}]
    (is (= {{:x 1 :y 64 :z 1} "oak" {:x 2 :y 64 :z 1} "birch"} (maintain/tree-cells answer nil)))
    (is (= {{:x 2 :y 64 :z 1} "birch"} (maintain/tree-cells answer "b")))
    (is (= {} (maintain/tree-cells answer "c")))))

(deftest a-large-tree-is-four-cells
  (let [answer {:cells (mapv (fn [pos] {:pos pos :want {:tree "dark_oak"} :part "d"})
                             [[2 64 0] [3 64 0] [2 64 1] [3 64 1]])}]
    (is (= 4 (count (maintain/tree-cells answer nil))))))

(deftest a-cell-is-classified-by-what-stands-on-it
  (are [here below above expected]
       (= expected (maintain/classify (tu/fake-on-floor {:blocks {"3,64,0" here "3,63,0" below "3,65,0" above}}) {:x 3 :y 64 :z 0} "oak"))
    "oak_log" "dirt" "air" :ripe
    "oak_sapling" "dirt" "air" :growing
    "birch_sapling" "dirt" "air" :foreign
    "birch_log" "dirt" "air" :foreign
    "stone" "dirt" "air" :foreign
    "oak_leaves" "dirt" "air" :foreign
    "air" "dirt" "air" :bare
    "air" "grass_block" "air" :bare
    "air" "podzol" "oak_leaves" :bare
    "air" "stone" "air" :no-ground
    "air" "air" "air" :no-ground
    "air" "dirt" "oak_log" :cramped))

(deftest an-unloaded-cell-is-unloaded
  (is (= :unloaded (maintain/classify (tu/fake-on-floor {:unloaded ["3,64,0"]}) {:x 3 :y 64 :z 0} "oak"))))

;; ------------------------------------------------------------------ fell-tree :at

(defn ^:async fell-at
  "Run fell-tree with args as a child; the digs' xs."
  [spec args]
  (let [{:keys [eng p]} (start spec {})]
    (await (tu/child-outcome eng 'jobs.forestry.fell-tree args 60))
    (mapv first (digs p))))

(deftest fell-tree-at-fells-that-column-not-the-nearest-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks (merge (lt/tree 3 0 "oak" 3) (lt/tree 6 0 "oak" 3))}]
          (is (= [3 3 3] (await (fell-at spec {:radius 10}))) "without :at the nearest tree")
          (is (= [6 6 6] (await (fell-at spec {:radius 10 :at {:x 6 :y 64 :z 0}}))))
          (is (= [6 6 6] (await (fell-at (assoc spec :self {:pos {:x 40 :y 64 :z 0}}) {:radius 10 :at {:x 6 :y 64 :z 0}})))
              "also from beyond the radius"))))))

(deftest fell-tree-walks-to-the-foot-of-the-column-and-digs-high-logs-from-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (lt/tree 6 0 "oak" 5)} {})]
          (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:radius 10} 60))
          (is (= [[6 64 0] [6 65 0] [6 66 0] [6 67 0] [6 68 0]] (digs p)))
          (is (= [[6 64 0]] (mapv (juxt :x :y :z) (tu/walked-to eng))) "one walk, to the column's foot, none for the high logs"))))))

(deftest fell-tree-tries-a-wider-stand-when-the-foot-of-the-column-has-no-path
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (lt/tree 6 0 "oak" 5)} {})
              n (atom 0)]
          (.override (.-world p) "steer"
                     (fn ^:async f [token args impl]
                       (if (= 1 (swap! n inc))
                         #js {:status "failed" :reason "test: the first walk goes nowhere"}
                         (await (impl token args)))))
          (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:radius 10} 60))
          (is (= 5 (count (digs p))))
          (is (= ["blocked" "arrived"] (take 2 (mapv :status (mapv :data (mem/entries (mem/view (:store eng)) :moved)))))
              "the walk to within 2 of the foot is blocked, the one to within 3 arrives"))))))

(deftest fell-tree-at-does-nothing-where-no-log-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [] (await (fell-at {:blocks (lt/tree 3 0 "oak" 3)} {:radius 10 :at {:x 5 :y 64 :z 0}}))))))))

;; ------------------------------------------------------------------ what the body has seen

(defn seen-below
  "p whose perception has seen only the cells below y-limit and answers the rest as unknown, with no properties."
  [p y-limit]
  (let [at (.-seenBlockAt p)]
    (aset p "seenBlockAt" (fn [pos] (if (< (.-y pos) y-limit) (at pos) #js {:unknown true :pos pos})))
    p))

(deftest logs-at-counts-only-the-logs-the-body-has-seen
  (let [p (seen-below (tu/seeing-all (tu/fake-on-floor {:blocks (lt/tree 3 0 "oak" 5)})) 66)]
    (is (= [[3 64 0] [3 65 0]] (mapv (comp (juxt :x :y :z) :pos) (trees/logs-at p {:x 3 :y 64 :z 0} "oak")))
        "the real logs above the seen ones are not read")))

(deftest tree-at-needs-a-seen-log
  (let [p (seen-below (tu/seeing-all (tu/fake-on-floor {:blocks (lt/tree 3 0 "oak" 5)})) 64)]
    (is (nil? (trees/tree-at p {:x 3 :y 64 :z 0})) "a log in the real world the body has not seen is no tree")))

(deftest tree-at-copes-with-a-seen-block-without-properties
  (let [p (tu/seeing-all (tu/fake-on-floor {:blocks (lt/tree 3 0 "oak" 5)}))
        at (.-seenBlockAt p)]
    (aset p "seenBlockAt" (fn [pos] (let [b (at pos)] #js {:name (.-name b) :pos pos})))
    (is (= "oak" (:species (trees/tree-at p {:x 3 :y 64 :z 0}))))))

;; ------------------------------------------------------------------ the job

(deftest a-grown-planned-tree-is-felled-collected-and-replanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start oak-world {"forest" oak-cell})
              result (await (tu/child-outcome eng job {:plan "forest"} 200))]
          (is (= [[3 64 0] [3 65 0] [3 66 0] [3 67 0]] (digs p)) "its four logs, lowest first, nothing else")
          (is (= [[3 64 0 "oak_sapling"]] (places p)))
          (is (= "oak_sapling" (h/block-at p 3 64 0)))
          (is (= 4 (get (h/inv p) "oak_log")) "the drops are collected")
          (is (= (repeat 4 "oak_log") (mapv #(h/block-at p 9 % 0) (range 64 68))) "the tree outside the plan stands")
          (is (= {:felled 1 :planted 1 :left [] :bare []} result))
          (is (= [] (lt/debts eng)) "fell-tree's replant debt is cleared by the planting")
          (is (= 1 (count (h/events-of seen :forest.done)))))))))

(deftest a-large-planned-tree-is-felled-and-replanted-cell-by-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (for [x [2 3] z [0 1]] [x z])
              plan {:id "forest" :parts [{:id "d" :box [[2 64 0] [3 64 1]] :want {:tree "dark_oak"}}]}
              {:keys [eng p]} (start {:blocks (merge (ground cells) (into {} (map (fn [[x z]] (lt/tree x z "dark_oak" 3))) cells))
                                      :inventory [(item "dark_oak_sapling" 4)]}
                                     {"forest" plan})
              result (await (tu/child-outcome eng job {:plan "forest"} 300))]
          (is (= 12 (count (digs p))))
          (is (= (set (map (fn [[x z]] [x 64 z "dark_oak_sapling"]) cells)) (set (places p))))
          (is (= {:felled 4 :planted 4 :left [] :bare []} result)))))))

(defn ^:async cell-case
  "Run the job over the oak cell at 3 64 0 in a world of blocks and inventory: [digs places events of kind]."
  [blocks inventory kinds]
  (let [{:keys [p seen eng]} (await (run {:blocks (merge (ground [[3 0]]) blocks) :inventory inventory} {"forest" oak-cell}
                                         {:plan "forest"} 12))]
    [(digs p) (places p) (into {} (map (fn [kind] [kind (warns seen kind)])) kinds) (listed? eng)]))

(deftest a-bare-planned-cell-gets-the-planned-sapling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [[] [[3 64 0 "oak_sapling"]] {:forest.no-sapling []} false]
               (await (cell-case {} [(item "oak_sapling" 2)] [:forest.no-sapling]))))))))

(deftest a-bare-cell-with-another-species-carried-is-skipped-with-one-note
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [[] [] {:forest.no-sapling [{:species "oak" :cells [{:x 3 :y 64 :z 0}]}]} true]
               (await (cell-case {} [(item "birch_sapling" 2)] [:forest.no-sapling]))))))))

(deftest cells-holding-something-else-are-left-and-reported-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[blocks warns] [[{"3,64,0" "oak_sapling"} []]
                                [{"3,64,0" "birch_sapling"} [{:pos {:x 3 :y 64 :z 0} :found "birch_sapling" :wanted "oak"}]]
                                [(lt/tree 3 0 "birch" 4) [{:pos {:x 3 :y 64 :z 0} :found "birch_log" :wanted "oak"}]]
                                [{"3,64,0" "stone"} [{:pos {:x 3 :y 64 :z 0} :found "stone" :wanted "oak"}]]]]
          (is (= [[] [] {:forest.foreign warns} true]
                 (await (cell-case blocks [(item "oak_sapling" 3)] [:forest.foreign])))
              (pr-str blocks)))))))

(deftest a-plan-edited-while-listed-drops-a-tree-cell-from-the-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[3 64 0]] "oak"] ["b" [[9 64 0]] "oak"])
              s (start oak-world {"forest" plan})]
          (.override (.-world (:p s)) "dig"
                     (fn ^:async f [token args impl]
                       (world/set-data! (:w s) {"forest" oak-cell} {})
                       (await (impl token args))))
          (await (tu/child-outcome (:eng s) job {:plan "forest"} 200))
          (is (= #{3} (set (map first (digs (:p s))))) "the tree whose cell left the plan is not felled")
          (is (= (repeat 4 "oak_log") (mapv #(h/block-at (:p s) 9 % 0) (range 64 68)))))))))

(deftest a-tree-cut-just-before-its-cell-left-the-plan-still-gets-its-replant
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[3 64 0]] "oak"] ["b" [[9 64 0]] "birch"])
              s (start (update oak-world :blocks assoc "9,64,0" "birch_sapling") {"forest" plan})
              dug (atom 0)]
          (.override (.-world (:p s)) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (when (= 2 (swap! dug inc))
                           (world/set-data! (:w s) {"forest" (forest-plan ["b" [[9 64 0]] "birch"])} {}))
                         r)))
          (await (tu/child-outcome (:eng s) job {:plan "forest"} 200))
          (is (= [[3 64 0 "oak_sapling"]] (places (:p s))) "the cut cell is replanted though the plan dropped it")
          (is (= 4 (count (digs (:p s)))) "the felling is one call: the tree begun is felled whole"))))))

(deftest the-check-declines-a-plan-it-cannot-work-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [crops {:id "crops" :parts [{:id "c" :cells [[3 64 0]] :want {:crop "wheat"}}]}]
          (doseq [[plans args reason] [[{} {:plan "nope"} "no such plan"]
                                       [{"crops" crops} {:plan "crops"} "no tree cells"]
                                       [{"forest" oak-cell} {:plan "forest" :part "zz"} "no tree cells"]]]
            (let [{:keys [p seen]} (await (run oak-world plans args 5))]
              (is (= [[] [] [[(:plan args) reason]]]
                     [(digs p) (places p) (mapv (juxt :plan :reason) (h/events-of seen :forest.declined))])
                  reason))))))))

(deftest a-broken-plan-declines-naming-the-error
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen w eng]} (start oak-world {})]
          (reset! (:state w) (assoc @(:state w) :plans {"forest" {:error "unreadable EDN: eof"}}))
          (core/submit! eng (list job {:plan "forest"}) {})
          (await (ticks eng 4))
          (is (= [] (digs p)))
          (is (= ["the plan cannot be read: unreadable EDN: eof"] (mapv :reason (h/events-of seen :forest.declined)))))))))

;; ------------------------------------------------------------------ not spinning

(deftest with-nothing-ripe-and-nothing-to-plant-the-job-does-nothing-until-something-changes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[3 64 0] [5 64 0] [7 64 0]] "oak"])
              {:keys [p eng seen]} (await (run {:blocks (merge (ground [[3 0] [5 0] [7 0]]) {"3,64,0" "oak_sapling" "5,64,0" "stone"})}
                                               {"forest" plan} {:plan "forest"} 40))]
          (is (= [] (vec (.-calls (.-world p)))) "40 ticks, not one act")
          (is (listed? eng) "still listed, waiting")
          (set-block! p 3 64 0 "oak_log")
          (set-block! p 3 65 0 "oak_log")
          (set-block! p 3 66 0 "oak_leaves")
          (await (ticks eng 20))
          (is (= [[3 64 0] [3 65 0]] (digs p)) "a tree that ripened is felled")
          (is (= 1 (count (warns seen :forest.foreign)))))))))

(deftest a-bare-cell-waits-for-a-sapling-and-is-planted-once-one-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng seen]} (await (run {:blocks (ground [[3 0]])} {"forest" oak-cell} {:plan "forest"} 40))]
          (is (= [] (vec (.-calls (.-world p)))))
          (is (= 1 (count (h/events-of seen :forest.no-sapling))) "one note")
          (give! p "oak_sapling" 1)
          (await (ticks eng 20))
          (is (= [[3 64 0 "oak_sapling"]] (places p)))
          (is (= 1 (count (h/events-of seen :forest.no-sapling)))))))))

(deftest a-tree-too-tall-is-left-with-one-note-and-not-retried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng seen]} (await (run {:blocks (merge (ground [[3 0]]) (lt/tree 3 0 "oak" 8)) :inventory [(item "oak_sapling" 1)]}
                                               {"forest" oak-cell} {:plan "forest" :pillar? false} 60))]
          (is (= [] (vec (.-calls (.-world p)))))
          (is (= [{:pos {:x 3 :y 64 :z 0} :reason :too-tall}] (warns seen :forest.left)))
          (is (not (listed? eng)) "the run ends")
          (core/submit! eng (list job {:plan "forest" :pillar? false}) {})
          (await (ticks eng 40))
          (is (= [] (vec (.-calls (.-world p)))) "a second run does not retry the tree")
          (is (= 1 (count (warns seen :forest.left))))
          (is (listed? eng) "it waits instead"))))))

(deftest a-tree-the-felling-gave-up-on-is-left-with-one-note
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (run {:blocks (merge (ground [[8 0]]) (lt/tree 8 0 "oak" 4)) :unreachable ["8,64,0"]}
                                           {"forest" (forest-plan ["a" [[8 64 0]] "oak"])} {:plan "forest"} 80))]
          (is (= [] (places p)))
          (is (= [{:pos {:x 8 :y 64 :z 0} :reason :unreachable}] (warns seen :forest.left))))))))

;; ------------------------------------------------------------------ access

(defn ^:async with-zones
  "Run the async thunk f with zones as the zone list of the worlds start makes; restored after."
  [zones f]
  (reset! test-zones zones)
  (try (await (f))
       (finally (reset! test-zones []))))

(def other-plan
  "Another active plan claiming the cell over the second log of the tree at x 3."
  {:id "other" :parts [{:id "o" :cells [[3 66 0]] :want "stone"}]})

(deftest a-log-over-another-plans-footprint-or-a-zone-leaves-the-whole-tree-standing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones plans reason] [[[] {"forest" oak-cell "other" other-plan} :footprint]
                                      [[] {"forest" oak-cell "other" (shape/with-author other-plan "Fake")} :footprint]
                                      [[{:name "vault" :min [3 65 0] :max [3 65 0] :allow #{:place}}] {"forest" oak-cell} :zone]]]
          (let [{:keys [p seen]} (await (with-zones zones #(run oak-world plans {:plan "forest"} 20)))]
            (is (= [] (digs p)) (str reason))
            (is (= [{:pos {:x 3 :y 64 :z 0} :reason :refused :why reason}] (warns seen :forest.left)))))))))

(deftest the-plans-own-footprint-does-not-refuse-its-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (assoc-in oak-cell [:parts 0 :cells] [[3 64 0]])
              {:keys [p]} (await (run oak-world {"forest" plan} {:plan "forest"} 40))]
          (is (= 4 (count (digs p)))))))))

(deftest a-zone-that-forbids-placing-is-no-obstacle-to-replanting-a-plan-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (with-zones [{:name "vault" :min [3 64 0] :max [3 64 0] :allow #{:dig}}]
                                        #(run {:blocks (ground [[3 0]]) :inventory [(item "oak_sapling" 1)]}
                                              {"forest" oak-cell} {:plan "forest"} 20)))]
          (is (= [[3 64 0 "oak_sapling"]] (places p)))
          (is (= [] (warns seen :forest.left))))))))

(deftest without-a-zone-list-the-job-declines-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (with-zones nil #(run oak-world {"forest" oak-cell} {:plan "forest"} 10)))]
          (is (= [] (vec (.-calls (.-world p)))))
          (is (= ["no zone list"] (mapv :reason (h/events-of seen :forest.declined)))))))))

(deftest a-hazard-the-job-accepts-does-not-stop-the-felling-and-one-it-does-not-accept-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (update oak-world :blocks assoc "4,65,0" "water")
              {:keys [p]} (await (run world {"forest" oak-cell} {:plan "forest"} 40))
              {p2 :p seen :seen} (await (run world {"forest" oak-cell} {:plan "forest" :accept #{}} 40))]
          (is (= 4 (count (digs p))) "fluid beside a log is accepted by default")
          (is (= [] (digs p2)))
          (is (= [{:pos {:x 3 :y 64 :z 0} :reason :refused :why :fluid-adjacent}] (warns seen :forest.left))))))))

(deftest a-log-that-turns-refused-during-the-felling-stops-the-felling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start oak-world {"forest" oak-cell})]
          (.override (.-world (:p s)) "dig"
                     (fn ^:async f [token args impl]
                       (world/set-data! (:w s) {"forest" oak-cell "other" other-plan} {})
                       (await (impl token args))))
          (core/submit! (:eng s) (list job {:plan "forest"}) {})
          (await (ticks (:eng s) 30))
          (is (= [[3 64 0] [3 65 0]] (digs (:p s))) "the logs below the refused third are dug, then the felling stops")
          (is (= [{:pos {:x 3 :y 64 :z 0} :reason :refused :why :footprint}] (warns (:seen s) :forest.left)) "the tree is left standing"))))))

(deftest a-tree-begun-and-dropped-from-the-plan-before-its-first-dig-is-left-and-its-cell-not-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (forest-plan ["a" [[3 64 0]] "oak"] ["b" [[9 64 0]] "birch"])
              {:keys [p eng w]} (start (update oak-world :blocks assoc "9,64,0" "birch_sapling") {"forest" plan})
              dropped (atom false)]
          (with-redefs [pace/pace! (fn []
                                     (when-not @dropped
                                       (reset! dropped true)
                                       (world/set-data! w {"forest" (forest-plan ["b" [[9 64 0]] "birch"])} {}))
                                     (js/Promise.resolve nil))]
            (core/submit! eng (list job {:plan "forest"}) {})
            (await (ticks eng 30)))
          (is @dropped)
          (is (= [] (vec (.-calls (.-world p)))) "no dig, no place on the cell holding the tree")
          (is (= "oak_log" (h/block-at p 3 64 0))))))))

(deftest a-place-that-finds-no-item-or-an-occupied-cell-is-tried-three-times-then-the-cell-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [status ["no-item" "occupied"]]
          (let [s (start {:blocks (ground [[3 0]]) :inventory [(item "oak_sapling" 1)]} {"forest" oak-cell})]
            (.override (.-world (:p s)) "place" (fn ^:async f [_ _ _] #js {:status status}))
            (core/submit! (:eng s) (list job {:plan "forest"}) {})
            (await (ticks (:eng s) 6))
            (is (= 3 (count (places (:p s)))) status)
            (is (= [{:pos {:x 3 :y 64 :z 0} :reason (keyword status)}] (warns (:seen s) :forest.left)) status)))))))

;; ------------------------------------------------------------------ restart

(deftest a-restart-before-every-tick-fells-once-and-replants-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              p (tu/seeing-all (tu/fake-on-floor oak-world))
              first-run (start oak-world {"forest" oak-cell} dir p)]
          (core/submit! (:eng first-run) (list job {:plan "forest"}) {})
          (loop [i 0]
            (when (< i 30)
              (let [{:keys [eng]} (start oak-world {"forest" oak-cell} dir p)]
                (await (ticks eng 1))
                (recur (inc i)))))
          (is (= [[3 64 0] [3 65 0] [3 66 0] [3 67 0]] (digs p)))
          (is (= [[3 64 0 "oak_sapling"]] (places p)) "no double replant"))))))


(deftest fell-tree-spares-logs-of-the-bodys-own-plan-unless-told-not-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks (lt/tree 3 0 "oak" 3)}
              own (shape/with-author (forest-plan ["a" [[3 64 0] [3 65 0] [3 66 0]] "oak"]) "Fake")]
          (doseq [[args n] [[{:radius 10} 0] [{:radius 10 :spare-own-builds false} 3]]]
            (let [{:keys [eng p]} (start spec {"forest" own})]
              (await (tu/child-outcome eng 'jobs.forestry.fell-tree args 60))
              (is (= n (count (digs p))) (pr-str args)))))))))

;; ------------------------------------------------------------------ :accept reaches the fell child

(deftest fell-tree-digs-with-the-accept-it-was-given
  (is (= #{} (:accept (fell-tree/log-dig-args {:args {:accept #{}}} {:x 0 :y 64 :z 0}))))
  (is (= #{:fluid-adjacent} (:accept (fell-tree/log-dig-args {:args {:accept #{:fluid-adjacent}}} {:x 0 :y 64 :z 0})))))

(deftest maintain-hands-its-accept-to-the-fell-child
  (is (= #{:falling-block}
         (:accept (maintain/fell-args {:args {:accept #{:falling-block}}} {:x 3 :y 64 :z 0} "oak")))))

(deftest maintain-fells-a-tall-tree-from-a-pillar-unless-told-not-to
  (is (false? (maintain/too-tall? {:args {:max-logs 6 :pillar? true}} (range 9))))
  (is (true? (maintain/too-tall? {:args {:max-logs 6 :pillar? false}} (range 9))))
  (is (false? (maintain/too-tall? {:args {:max-logs 6 :pillar? false}} (range 6))))
  (are [on] (= on (:pillar? (maintain/fell-args {:args {:pillar? on}} {:x 3 :y 64 :z 0} "oak")))
    true false))

(deftest a-declined-dig-hazard-is-refused-not-unreachable
  (are [waits out] (= out (fell-tree/outcome :declined nil waits))
    {:reason :not-allowed} :refused
    {:reason :hazard} :refused
    {:reason :unreachable} :unreachable
    {:reason :no-tool} :unreachable))
