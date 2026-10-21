(ns engine.part-cut-test
  "Part-cut trunks: jobs.lib.part-cut reads them, fell-tree :part-cut? fells only them and writes the replant debt on the
  ground under the hanging trunk, and the :part-cut-tree and :replant-owed triggers hold for them."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.forest-maintain-test :as fm]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.forestry.fell-tree :as fell-tree]
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

(defn trunks [blocks] (mapv :base (part-cut/part-cut-trees (:p (lt/setup {:blocks blocks})) 12 nil)))

(deftest only-a-raised-trunk-not-joined-to-a-standing-tree-is-part-cut
  (is (= [{:x 6 :y 65 :z 0}] (trunks (merge (fm/ground [[6 0]]) (part-cut 6)))))
  (is (= [] (trunks (merge (fm/ground [[6 0]]) (lt/tree 6 0 "oak" 4)))) "a tree on the ground")
  (is (= [] (trunks wall)) "a log wall has no leaves")
  (is (= [] (trunks (merge (fm/ground [[6 0] [7 0]]) (branched 7 67)))) "a branch of a living acacia")
  (is (= [] (trunks (merge (fm/ground [[6 0] [7 0]]) (branched 7 66)))) "a side branch of a living fancy oak"))

(deftest the-ground-under-a-raised-log-is-the-first-seen-solid-block
  (let [p (:p (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))}))]
    (is (= {:x 6 :y 64 :z 0} (part-cut/ground-cell p {:x 6 :y 65 :z 0})))
    (is (nil? (part-cut/ground-cell (:p (lt/setup {:blocks (part-cut 6)})) {:x 6 :y 65 :z 0})) "no ground seen")))

(deftest the-walk-to-a-raised-base-aims-at-the-ground-under-it
  (let [p (:p (lt/setup {:blocks (merge (fm/ground [[6 0] [9 0]]) (part-cut 6) (lt/tree 9 0 "oak" 4))}))
        trees (into {} (map (juxt (comp :x :base) identity)) (trees/trees-near p 12 nil #{}))]
    (is (= {:x 6 :y 64 :z 0} (fell-tree/walk-target p (trees 6))) "the hanging trunk: the ground under it")
    (is (= (:base (trees 9)) (fell-tree/walk-target p (trees 9))) "a standing trunk: its own base")))

(defn ^:async fell
  "Run fell-tree :part-cut? over the zones in a world of spec; {:p :eng :out}."
  [spec zones args]
  (let [{:keys [eng p]} (lt/setup (assoc spec :zones zones))]
    {:eng eng :p p :out (await (tu/child-outcome eng 'jobs.forestry.fell-tree (merge {:part-cut? true :radius 12 :ways #{:carried}} args) 150))}))

(deftest fell-tree-part-cut-fells-the-hanging-trunk-and-owes-the-sapling-on-the-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (fell {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))} [] {}))]
          (is (= #{65 66 67} (into #{} (map second) (fm/digs p))) "the three floating logs go")
          (is (<= 3 (get (lt/inv p) "oak_log" 0)) "its own logs are picked up")
          (is (= [{:pos {:x 6 :y 64 :z 0} :species "oak"}] (lt/debts eng)) "the debt is the ground cell, not the hanging log"))))))

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
          (mem/write! (:store eng) trees/unreachable-kind {:column [6 0]} trees/unreachable-policy)
          (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:part-cut? true :radius 12 :ways #{:carried}} 150))
          (is (= [] (fm/digs p))))))))

(defn view [entries]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000000)})]
    (doseq [[kind data] entries] (mem/write! s kind data {:cap 8 :ttl :forever}))
    (mem/view s)))

(defn holds [t blocks & [entries inventory zones unloaded]]
  (let [p (:p (lt/setup (cond-> {:blocks blocks :inventory inventory} unloaded (assoc :unloaded unloaded))))]
    (t p (view entries) {} (ew/of-data {} {} (or zones [])) nil)))

(deftest the-part-cut-tree-trigger-holds-for-a-hanging-trunk-only
  (is (true? (holds pct/part-cut-tree (merge (fm/ground [[6 0]]) (part-cut 6)))))
  (is (false? (holds pct/part-cut-tree (merge (fm/ground [[6 0]]) (lt/tree 6 0 "oak" 4)))))
  (is (false? (holds pct/part-cut-tree wall))))

(deftest the-part-cut-tree-trigger-ignores-a-trunk-in-another-player-s-zone
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))
        zone (fn [owner] [{:name "apiary" :owner owner :min [5 60 -1] :max [7 80 1] :allow #{}}])
        self (.-username (.self (:p (lt/setup {:blocks blocks}))))]
    (is (false? (holds pct/part-cut-tree blocks [] nil (zone "Someone"))) "refused up front, so no walk and no refire")
    (is (true? (holds pct/part-cut-tree blocks [] nil (zone self))) "own zone")))

(deftest the-part-cut-tree-trigger-leaves-a-column-fell-tree-gave-up-on
  (let [blocks (merge (fm/ground [[6 0]]) (part-cut 6))]
    (is (false? (holds pct/part-cut-tree blocks [[trees/unreachable-kind {:column [6 0]}]])) "no refire on the failed column")
    (is (true? (holds pct/part-cut-tree blocks [[trees/unreachable-kind {:column [9 9]}]])) "another column")))

(deftest the-replant-owed-trigger-holds-while-a-debt-s-sapling-is-carried
  (let [debt [[:forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"}]]]
    (is (true? (holds ro/replant-owed {} debt [sapling])))
    (is (false? (holds ro/replant-owed {} debt)) "no sapling carried: the trigger never fetches")
    (is (false? (holds ro/replant-owed {} [] [sapling])) "nothing owed")
    (is (false? (holds ro/replant-owed {} debt [{:name "birch_sapling" :count 1}])) "another species")
    (is (false? (holds ro/replant-owed {} debt [sapling] nil ["6,64,0"]))
        "the debt's cell is not loaded: the walk would end :not-loaded and refire")
    (is (false? (holds ro/replant-owed {} (conj debt [trees/stalled-kind {:pos {:x 6 :y 64 :z 0}}]) [sapling]))
        "plant-sapling failed there lately: no refire")
    (is (true? (holds ro/replant-owed {} (conj debt [trees/stalled-kind {:pos {:x 9 :y 64 :z 0}}]) [sapling]))
        "another cell's note leaves the debt owed")))

(deftest plant-sapling-notes-a-failed-debt-cell-so-the-trigger-stays-quiet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (fm/ground [[6 0]]) :inventory [sapling] :unloaded ["6,64,0"]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 6 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (await (tu/child-outcome eng 'jobs.forestry.plant-sapling {:ways #{:carried}} 50))
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
