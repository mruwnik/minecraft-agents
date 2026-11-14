(ns engine.part-cut-test
  "Part-cut trunks: jobs.lib.part-cut reads them, fell-tree :part-cut? fells only them and writes the replant debt on the
  ground under the hanging trunk, and the :part-cut-tree and :replant-owed triggers hold for them."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.forest-maintain-test :as fm]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.part-cut :as part-cut]
            [jobs.lib.trees :as trees]
            [jobs.forestry.plant-sapling :as plant-sapling]
            [jobs.lib.world-files :as ew]
            [triggers.forestry.part-cut-tree :as pct]
            [triggers.forestry.replant-owed :as ro]))

(def sapling {:name "oak_sapling" :count 2})

(defn part-cut
  "An oak of 4 logs at x whose lowest log was cut: three logs float over the air, leaves on top."
  [x]
  (apply dissoc (lt/tree x 0 "oak" 4) (map #(fm/k x % 0) [64])))

(defn branched
  "A living tree at x 6: a trunk of 4 logs on the ground and one branch log at bx, by, connected to it, leaves on top."
  [bx by]
  (merge (lt/tree 6 0 "oak" 4) {(fm/k bx by 0) "oak_log" (fm/k bx (inc by) 0) "oak_leaves"}))

(def wall (into {} (for [x (range 4 9)] [(fm/k x 66 0) "oak_log"])))

(defn ray-sight
  "p seeing only the blocks a ray from above or the side enters: a block whose top and four sides hold blocks (the
  ground under a trunk's foot, the block under a stump) stays unseen."
  [p]
  (let [blocks (.-seenBlocks p) block-at (.-seenBlockAt p)
        shut? (fn [x y z] (not (contains? bs/air (.-name (block-at #js {:x x :y y :z z})))))
        hid? (fn [pos]
               (let [[x y z] (map #(js/Math.floor (aget pos %)) ["x" "y" "z"])]
                 (every? (fn [[dx dy dz]] (shut? (+ x dx) (+ y dy) (+ z dz))) [[0 0 0] [0 1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])))]
    (aset p "seenBlocks" (fn [q] (.filter (blocks q) #(not (hid? (.-pos %))))))
    (aset p "seenBlockAt" (fn [pos] (if (hid? pos) #js {:unknown true :pos pos} (block-at pos))))
    p))

(defn trunks
  ([blocks] (trunks blocks #{[6 0]}))
  ([blocks owed] (mapv :base (part-cut/part-cut-trees (ray-sight (:p (lt/setup {:blocks blocks}))) owed 12 nil))))

(deftest only-an-owed-raised-trunk-is-part-cut
  (is (= [{:x 6 :y 65 :z 0}] (trunks (merge (fm/ground [[6 0]]) (part-cut 6)))))
  (is (= [] (trunks (merge (fm/ground [[6 0]]) (part-cut 6)) #{})) "a hanging trunk the body did not cut: a stump left by others")
  (is (= [] (trunks (merge (fm/ground [[6 0]]) (part-cut 6)) #{[9 9]})) "another column is owed")
  (is (= [] (trunks (merge (fm/ground [[6 0] [7 0]]) (branched 7 66)))) "a branch of a living oak: its trunk column is not hanging")
  (is (= [] (trunks (merge (fm/ground [[6 0]]) (lt/tree 6 0 "oak" 4)))) "an owed column grown whole again")
  (is (= [] (trunks wall)) "a log wall has no leaves"))

(deftest the-ground-under-a-raised-log-is-the-first-seen-solid-block
  (let [p (:p (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))}))]
    (is (= {:x 6 :y 64 :z 0} (part-cut/ground-cell p {:x 6 :y 65 :z 0})))
    (is (nil? (part-cut/ground-cell (:p (lt/setup {:blocks (part-cut 6)})) {:x 6 :y 65 :z 0})) "no ground seen")))

(deftest the-walk-to-a-raised-base-aims-at-the-ground-under-it
  (let [p (:p (lt/setup {:blocks (merge (fm/ground [[6 0] [9 0]]) (part-cut 6) (lt/tree 9 0 "oak" 4))}))
        trees (into {} (map (juxt (comp :x :base) identity)) (trees/trees-near p 12 nil #{}))]
    (is (= {:x 6 :y 64 :z 0} (fell-tree/walk-target p (trees 6))) "the hanging trunk: the ground under it")
    (is (= (:base (trees 9)) (fell-tree/walk-target p (trees 9))) "a standing trunk: its own base")))

(defn owe! [eng] (mem/write! (:store eng) trees/part-cut-kind {:column [6 0]} trees/part-cut-policy))

(defn ^:async fell
  "Run fell-tree :part-cut? over the zones in a world of spec, column [6 0] owed; {:p :eng :out}."
  [spec zones args]
  (let [{:keys [eng p]} (update (lt/setup (assoc spec :zones zones)) :p ray-sight)]
    (owe! eng)
    {:eng eng :p p :out (await (tu/child-outcome eng 'jobs.forestry.fell-tree (merge {:part-cut? true :radius 12 :ways #{:carried}} args) 150))}))

(deftest fell-tree-part-cut-fells-the-hanging-trunk-and-owes-the-sapling-on-the-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (fell {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))} [] {}))]
          (is (= #{65 66 67} (into #{} (map second) (fm/digs p))) "the three floating logs go")
          (is (<= 3 (get (lt/inv p) "oak_log" 0)) "its own logs are picked up")
          (is (= [{:pos {:x 6 :y 64 :z 0} :species "oak"}] (lt/debts eng)) "the debt is the ground cell, not the hanging log")
          (is (empty? (mem/entries (mem/view (:store eng)) trees/part-cut-kind)) "a finished tree is no longer owed"))))))

(deftest fell-tree-does-not-fell-a-hanging-trunk-it-did-not-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))})]
          (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:part-cut? true :radius 12 :ways #{:carried}} 150))
          (is (= [] (fm/digs p))))))))

(deftest fell-tree-leaving-a-tree-half-cut-owes-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (merge (fm/ground [[6 0]]) (lt/tree 6 0 "oak" 9)
                                                (into {} (for [y (range 64 73)] [(fm/k 7 y 0) "oak_log"])))
                                         :zones [{:name "vault" :min [5 68 -1] :max [7 80 1] :allow #{:place}}]})
              out (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:radius 12 :ways #{:carried}} 600))]
          (is (not= :done (:status out)) "the felling ended without the tree down")
          (is (seq (fm/digs p)) "some logs were cut")
          (is (= (set (map (fn [[x _ z]] [x z]) (fm/digs p)))
                 (set (map (comp vec :column :data) (mem/entries (mem/view (:store eng)) trees/part-cut-kind))))
              "every column logs were cut in is owed, the neighbouring one too"))))))

(def stumped
  "An oak of 5 logs at x 6 cut at its second log: the stump on the ground, three logs over the gap, leaves on top."
  (merge (fm/ground [[6 0]]) (dissoc (lt/tree 6 0 "oak" 5) (fm/k 6 65 0))))

(deftest a-trunk-cut-above-its-stump-is-part-cut-and-fell-tree-takes-stump-and-trunk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [{:x 6 :y 64 :z 0}] (trunks stumped)))
        (let [{:keys [p eng]} (await (fell {:blocks stumped} [] {}))]
          (is (= #{64 66 67 68} (into #{} (map second) (fm/digs p))))
          (is (= [{:pos {:x 6 :y 64 :z 0} :species "oak"}] (lt/debts eng)) "the sapling goes where the stump stood"))))))

(deftest fell-tree-part-cut-leaves-whole-trees-and-refused-ones
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (fell {:blocks (merge (fm/ground [[6 0]]) (lt/tree 6 0 "oak" 4))} [] {}))]
          (is (= [] (fm/digs p))))
        (let [zones [{:name "vault" :min [5 64 -1] :max [7 80 1] :allow #{:place}}]
              {:keys [p]} (await (fell {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))} zones {}))]
          (is (= [] (fm/digs p))))))))

(deftest fell-tree-part-cut-leaves-a-column-it-gave-up-on-lately
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))})]
          (owe! eng)
          (mem/write! (:store eng) trees/unreachable-kind {:column [6 0]} trees/unreachable-policy)
          (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:part-cut? true :radius 12 :ways #{:carried}} 150))
          (is (= [] (fm/digs p))))))))

(deftest fell-tree-part-cut-ending-a-need-keeps-the-column-and-what-it-lacked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out]} (await (fell {:blocks (merge (fm/ground [[6 0]]) (apply dissoc (lt/tree 6 0 "oak" 9) (map #(fm/k 6 % 0) [64 65 66 67 68])))} [] {:ways #{:carried :pillar}}))
              entries (mem/entries (mem/view (:store eng)) trees/need-kind)]
          (is (= :need (:reason out)))
          (is (= [{:column [6 0] :any-of ["dirt"] :count 3}] (map :data entries)))
          (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:part-cut? true :radius 12 :ways #{:carried :pillar}} 150))
          (is (= 1 (count (mem/entries (mem/view (:store eng)) trees/need-kind))) "a later need on the column replaces the first"))))))

(defn view [entries & [now]]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly (or now 1000000))})]
    (doseq [[kind data] (cons [trees/part-cut-kind {:column [6 0]}] entries)] (mem/write! s kind data {:cap 8 :ttl :forever}))
    (mem/view s)))

(defn holds [t blocks & [entries inventory zones unloaded]]
  (let [p (ray-sight (:p (lt/setup (cond-> {:blocks blocks :inventory inventory} unloaded (assoc :unloaded unloaded)))))]
    (t p (view entries) {} (ew/of-data {} {} (or zones [])) nil)))

(deftest the-part-cut-tree-trigger-holds-for-a-hanging-trunk-only
  (is (true? (holds pct/part-cut-tree (merge (fm/ground [[6 0]]) (part-cut 6)))))
  (is (false? (holds pct/part-cut-tree (merge (fm/ground [[6 0]]) (lt/tree 6 0 "oak" 4)))))
  (is (false? (holds pct/part-cut-tree wall)))
  (let [w (ew/of-data {} {} [])
        owed? #(pct/part-cut-tree (ray-sight (:p (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))}))) (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000000)})]
                                      (when % (mem/write! s trees/part-cut-kind {:column [6 0]} trees/part-cut-policy))
                                      (mem/view s)) {} w nil)]
    (is (true? (owed? true)))
    (is (false? (owed? false)) "a hanging trunk the body did not cut never holds it")))

(deftest the-part-cut-tree-trigger-waits-out-the-dark
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))
        at (fn [time] (pct/part-cut-tree (tu/with-light-data (:p (lt/setup {:blocks blocks :time time}))) (view []) {} (ew/of-data {} {} []) nil))]
    (is (true? (at 6000)))
    (is (false? (at 14000)) "in the dark the felling walk would leave the night's shelter")))

(deftest the-part-cut-tree-trigger-ignores-a-trunk-in-another-player-s-zone
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))
        zone (fn [owner] [{:name "apiary" :owner owner :min [5 60 -1] :max [7 80 1] :allow #{}}])
        self (.-username (.self (:p (lt/setup {:blocks blocks}))))]
    (is (false? (holds pct/part-cut-tree blocks [] nil (zone "Someone"))) "refused up front, so no walk and no refire")
    (is (true? (holds pct/part-cut-tree blocks [] nil (zone self))) "own zone")))

(deftest the-part-cut-tree-trigger-ignores-a-trunk-in-another-player-s-claim-until-it-runs-out
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))
        claim {:id "c1" :owner "Someone" :status :active :until 100 :min [5 60 -1] :max [7 80 1]}
        at (fn [now] (let [w (ew/of-data {} {} [])]
                       (ew/set-area-claims! w [claim])
                       (pct/part-cut-tree (ray-sight (:p (lt/setup {:blocks blocks}))) (view [] now) {} w nil)))]
    (is (false? (at 50)) "a live claim refuses the trunk")
    (is (true? (at 200)) "the claim has run out")))

(deftest the-part-cut-tree-trigger-leaves-a-column-fell-tree-gave-up-on
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))]
    (is (false? (holds pct/part-cut-tree blocks [[trees/unreachable-kind {:column [6 0]}]])) "no refire on the failed column")
    (is (true? (holds pct/part-cut-tree blocks [[trees/unreachable-kind {:column [9 9]}]])) "another column")))

(deftest the-part-cut-tree-trigger-waits-for-the-items-a-need-ending-named
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))
        need [[trees/need-kind {:column [6 0] :any-of ["dirt"] :count 5}]]]
    (is (false? (holds pct/part-cut-tree blocks need)) "dirt still lacking")
    (is (false? (holds pct/part-cut-tree blocks need [{:name "dirt" :count 4}])) "too few")
    (is (true? (holds pct/part-cut-tree blocks need [{:name "dirt" :count 5}])) "carried now")
    (is (true? (holds pct/part-cut-tree blocks [[trees/need-kind {:column [9 9] :any-of ["dirt"] :count 5}]])) "another column")
    (is (false? (holds pct/part-cut-tree blocks [[trees/need-kind {:column :any :any-of ["dirt"] :count 5 :trees [[6 0]]}]])) "a need before any column was chosen holds the trunks it was written with")
    (is (true? (holds pct/part-cut-tree blocks [[trees/need-kind {:column :any :any-of ["dirt"] :count 5 :trees [[9 9]]}]])) "a trunk it was not written with lets the trigger fire again")))

(deftest fell-tree-ending-otherwise-drops-an-any-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))})]
          (owe! eng)
          (mem/write! (:store eng) trees/need-kind {:column :any :any-of ["dirt"] :count 5 :trees []} trees/need-policy)
          (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:part-cut? true :radius 12 :ways #{:carried}} 150))
          (is (empty? (mem/entries (mem/view (:store eng)) trees/need-kind))))))))

(deftest the-part-cut-tree-trigger-is-a-bounded-chore
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))
        home (fn [x] [[:home {:pos {:x x :y 64 :z 0}}]])
        logs (fn [n] [{:name "dark_oak_log" :count n}])]
    (is (true? (holds pct/part-cut-tree blocks (home 20))) "near home")
    (is (false? (holds pct/part-cut-tree blocks (home 100))) "a trunk far from home is not the body's to finish")
    (is (true? (holds pct/part-cut-tree blocks [] (logs 63))))
    (is (false? (holds pct/part-cut-tree blocks [] (logs 64))) "a stack of logs is enough")
    (is (false? (holds pct/part-cut-tree blocks [] [{:name "oak_log" :count 40} {:name "birch_log" :count 24}])) "all species count")))

(deftest the-replant-owed-trigger-holds-while-a-debt-s-sapling-is-carried
  (let [debt [[:forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"}]]]
    (is (true? (holds ro/replant-owed (fm/ground [[6 0]]) debt [sapling])))
    (is (false? (holds ro/replant-owed (assoc (fm/ground [[6 0]]) (fm/k 6 66 0) "oak_log") debt [sapling]))
        "a log over the cell leaves no room: plant-sapling would refuse it at start")
    (is (false? (holds ro/replant-owed {} debt)) "no sapling carried: the trigger never fetches")
    (is (false? (holds ro/replant-owed {} [] [sapling])) "nothing owed")
    (is (false? (holds ro/replant-owed {} debt [{:name "birch_sapling" :count 1}])) "another species")
    (is (false? (holds ro/replant-owed {} debt [sapling] nil ["6,64,0"]))
        "the debt's cell is not loaded: the walk would end :not-loaded and refire")
    (is (false? (holds ro/replant-owed {} (conj debt [trees/stalled-kind {:pos {:x 6 :y 64 :z 0}}]) [sapling]))
        "plant-sapling failed there lately: no refire")
    (is (true? (holds ro/replant-owed (fm/ground [[6 0]]) (conj debt [trees/stalled-kind {:pos {:x 9 :y 64 :z 0}}]) [sapling]))
        "another cell's note leaves the debt owed")
    (is (false? (holds ro/replant-owed (assoc (fm/ground [[6 0]]) (fm/k 6 64 0) "crafting_table") debt [sapling]))
        "a block the body put on the stump cell: nothing to plant there")
    (is (true? (holds ro/replant-owed (assoc (fm/ground [[6 0]]) (fm/k 6 64 0) "short_grass") debt [sapling])) "a plant there is replaced")
    (is (false? (holds ro/replant-owed {} [[:forestry/replant {:pos {:x 9000 :y 64 :z 0} :species "oak"}]] [sapling]))
        "a debt far from the body waits until the body is near")))

(deftest plant-sapling-drops-a-debt-whose-cell-another-block-fills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (assoc (fm/ground [[6 0] [9 0]]) (fm/k 6 64 0) "furnace") :inventory [sapling]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (mem/write! (:store eng) :forestry/replant {:pos {:x 9 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (await (tu/child-outcome eng 'jobs.forestry.plant-sapling {:ways #{:carried}} 100))
          (is (some #(= [9 64 0 "oak_sapling"] %) (fm/places p)))
          (is (= [] (lt/debts eng)) "the filled cell's debt is gone, not retried"))))))

(deftest the-replant-owed-trigger-leaves-the-dusk-to-the-walk-home
  (let [debt [:forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"}]
        at (fn [time home-x] (ro/replant-owed (:p (lt/setup {:blocks (fm/ground [[6 0]]) :inventory [sapling] :time time}))
                                              (view [debt [:home {:pos {:x home-x :y 64 :z 0}}]]) {} (ew/of-data {} {} []) nil))]
    (is (true? (at 6000 0)))
    (is (false? (at 11800 0)) "at the debt the body would be due home: dusk-home would call it back at once")
    (is (true? (at 11800 4)) "a debt beside home")))

(deftest the-replant-owed-trigger-follows-the-retuned-dusk-home-timing
  (let [debt [:forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"}]
        at (fn [] (ro/replant-owed (:p (lt/setup {:blocks (fm/ground [[6 0]]) :inventory [sapling] :time 11800}))
                                       (view [debt [:home {:pos {:x 30 :y 64 :z 0}}]]) {} (ew/of-data {} {} []) nil))]
    (is (false? (at)) "10 s of day left, inside the default margin: due home")
    (settings/with-settings {:jobs.lib.shelter/home-due {:margin-s 0 :slack 0 :arrive 4}}
      #(is (true? (at)) "a retuned timing leaves time to plant"))))

(deftest plant-sapling-notes-a-failed-debt-cell-so-the-trigger-stays-quiet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (fm/ground [[6 0]])})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (await (tu/child-outcome eng 'jobs.forestry.plant-sapling {} 50))
          (is (= [{:pos {:x 6 :y 64 :z 0}}] (mapv :data (mem/entries (mem/view (:store eng)) trees/stalled-kind)))))))))

(deftest plant-sapling-takes-the-debt-whose-sapling-is-carried-before-an-older-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (fm/ground [[6 0] [9 0]]) :inventory [{:name "birch_sapling" :count 1}]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (mem/write! (:store eng) :forestry/replant {:pos {:x 9 :y 64 :z 0} :species "birch"} {:cap 50 :ttl :forever})
          (await (tu/child-outcome eng 'jobs.forestry.plant-sapling {:ways #{:carried}} 100))
          (is (some #(= [9 64 0 "birch_sapling"] %) (fm/places p)))
          (is (= [{:pos {:x 6 :y 64 :z 0} :species "oak"}] (lt/debts eng))))))))

(deftest plant-sapling-takes-a-loaded-debt-before-an-older-unloaded-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (fm/ground [[9 0]]) :inventory [sapling] :unloaded ["6,64,0"]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (mem/write! (:store eng) :forestry/replant {:pos {:x 9 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (await (tu/child-outcome eng 'jobs.forestry.plant-sapling {:ways #{:carried}} 100))
          (is (some #(= [9 64 0 "oak_sapling"] %) (fm/places p)) "the trigger held for this one, so the job plants it, not the older unloaded debt"))))))
