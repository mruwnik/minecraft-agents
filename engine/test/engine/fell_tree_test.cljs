(ns engine.fell-tree-test
  "jobs.forestry.fell-tree from a pillar: a trunk too tall to reach from the ground is felled from a pillar that is
  taken back (jobs.access.pillar, jobs.access.cleanup) before the job ends."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.fake :as fake]
            [engine.forest-maintain-test :as fm]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [engine.registry :as registry]
            [jobs.lib.access.approach :as approach]
            [jobs.items.obtain :as obtain]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.trees :refer [axe-fetch-limits]]
            [jobs.lib.ledger :as ledger]
            [engine.memory :as mem]))

(def tall {:blocks (lt/tree 6 0 "oak" 8) :inventory [{:name "dirt" :count 20}]})

(defn dug-ys
  "The heights of the digs in the trunk's column."
  [p]
  (into [] (comp (filter #(= 6 (first %))) (map second)) (fm/digs p)))

(defn pillar-cells
  "The cells of dirt standing in the fake world."
  [p]
  (vec (keep (fn [[k v]] (when (= "dirt" v) k)) (:blocks @(fake/state p)))))

(def eye-height 1.62)

(defn eye-reach!
  "The fake measures a dig from the feet to a cell corner; the real body from the eye to the centre. Digs here run with
  the body lifted to its eye height, as the planner counts reach. Cut logs fall to the ground at once, or with lose-logs?
  vanish (none to carry)."
  ([p] (eye-reach! p false))
  ([p lose-logs?]
  (.override (.-world p) "dig"
             (fn ^:async f [token args impl]
               (let [st (fake/state p)]
                 (swap! st update-in [:self :pos 1] + eye-height)
                 (let [r (await (impl token args))]
                   (swap! st update-in [:self :pos 1] - eye-height)
                   (swap! st update :entities (fn [es] (if lose-logs?
                                                         (vec (remove #(and (= "item" (:kind %)) (= "oak_log" (get-in % [:item :name]))) es))
                                                         (mapv #(if (and (= "item" (:kind %)) (= "oak_log" (get-in % [:item :name])) (= 6 (first (:pos %)))) (assoc-in % [:pos 1] 64) %) es))))
                   r))))))

(defn ^:async fell [spec args n]
  (let [s (fm/start spec {})
        _ (eye-reach! (:p s) (:lose-logs? spec))
        out (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree args n))]
    (assoc s :out out)))

(deftest a-tall-trunk-is-felled-from-a-pillar-and-the-pillar-is-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out]} (await (fell tall {:radius 10} 200))]
          (is (= {:base {:x 6 :y 64 :z 0} :logs 8 :axe :none-found} out))
          (is (= (set (range 64 72)) (set (dug-ys p))) "every log dug")
          (is (pos? (count (h/calls p "jumpPlace"))) "a pillar was built")
          (is (= [] (ledger/open-entries (mem/view (:store eng)))) "ledger empty")
          (is (= [] (pillar-cells p)) "no pillar block left"))))))

(deftest logs-above-the-ground-reach-are-dug-top-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (fell tall {:radius 10} 200))
              ys (dug-ys p)]
          (is (= 8 (count ys)))
          (is (apply < (take 5 ys)) "the low logs go bottom-up from the ground")
          (is (apply > (drop 5 ys)) "the high ones top-down from the pillar"))))))

(def tall-9 {:blocks (lt/tree 6 0 "oak" 9) :inventory []})

(defn log-pillar-cells
  "Oak logs standing in the fake world outside the trunk's column."
  [p]
  (vec (keep (fn [[k v]] (when (and (= "oak_log" v) (not= 6 (first k))) k)) (:blocks @(fake/state p)))))

(deftest a-nine-log-tree-is-felled-whole-pillaring-on-its-own-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out]} (await (fell tall-9 {:radius 10 :fetch false} 300))]
          (is (= {:base {:x 6 :y 64 :z 0} :logs 9 :axe :skipped-fetch-off} out))
          (is (= (set (range 64 73)) (set (dug-ys p))) "every log dug")
          (is (pos? (count (h/calls p "jumpPlace"))) "a pillar was built")
          (is (= [] (ledger/open-entries (mem/view (:store eng)))) "ledger empty")
          (is (= [] (log-pillar-cells p)) "no pillar log left"))))))

(deftest no-logs-or-blocks-carried-with-fetch-false-waits-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (fell (assoc tall :inventory [] :lose-logs? true) {:radius 10 :fetch false} 60))]
          (is (= :not-done out))
          (is (= [] (h/calls p "jumpPlace")))
          (is (= [] (pillar-cells p))))))))

(deftest a-tree-in-a-zone-that-bars-placing-is-declined-without-a-pillar
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "grove" :min [-6 64 -9] :max [16 80 9] :allow #{:dig}}]
              {:keys [p]} (await (fm/with-zones zones #(fell tall {:radius 10} 80)))]
          (is (= [] (h/calls p "jumpPlace")))
          (is (< (count (dug-ys p)) 8) "the high logs stay"))))))

(defn kinds [{:keys [seen]} kind] (h/events-of seen kind))

(deftest a-cleanup-that-leaves-the-pillar-open-ends-the-job-with-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cleanup (assoc (get registry/jobs 'jobs.access.cleanup) :round (fn ^:async f [_] :done))
              s (with-redefs [registry/jobs (assoc registry/jobs 'jobs.access.cleanup cleanup)]
                  (await (fell tall {:radius 10} 200)))]
          (is (not= :not-done (:out s)) "the job ends instead of cleaning for ever")
          (is (= 1 (count (kinds s :tree_blocked)))))))))

(deftest a-pillar-with-no-log-in-reach-marks-the-tree-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              s (with-redefs [approach/plan (fn [_] (swap! calls inc) {:stand [[5 64 0]]})]
                  (await (fell tall {:radius 10} 200)))]
          (is (not= :not-done (:out s)))
          (is (<= @calls 2) "the plan is not made again and again")
          (is (= 1 (count (kinds s :tree_blocked)))))))))

(deftest a-planner-refusal-other-than-a-zone-warns-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (with-redefs [approach/plan (fn [_] {:reason :no-base})]
                  (await (fell tall {:radius 10} 120)))
              w (first (kinds s :fell-tree.declined))]
          (is (= :no-base (:reason w)))
          (is (= 1 (count (kinds s :fell-tree.declined)))))))))

(def bare "tall without the side leaf: no stand to plan on the canopy." (update tall :blocks dissoc "7,71,0"))

(defn looked-low?
  "A look at a cell below the trunk foot's level (y 64) was made: what sense/decide! does for an unseen floor."
  [p]
  (boolean (some #(and (= "look" (.-name %)) (some-> (.-args %) .-pos .-y (< 64))) (.-calls (.-world p)))))

(defn ^:async fell-floor-unseen
  "fell over a world whose cells at y 63 and below read as never seen until (seen-now? p) says so."
  [spec args n seen-now?]
  (let [s (fm/start spec {})
        p (:p s)
        omniscient (.-sensedAt p)]
    (eye-reach! p)
    (aset p "sensedAt" (fn [pos] (if (or (> (.-y pos) 63) (seen-now? p)) (omniscient pos) #js {:unknown true})))
    (assoc s :out (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree args n)))))

(deftest an-unseen-floor-by-the-trunk-is-looked-at-before-the-pillar-is-planned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (fell-floor-unseen bare {:radius 10} 300 looked-low?))]
          (is (= [] (kinds s :fell-tree.declined)) "not declined as no way to reach")
          (is (= 0 (count (kinds s :tree_blocked))) "the tree is not dropped")
          (is (= (set (range 64 72)) (set (dug-ys p))) "every log dug"))))))

(defn ^:async fell-ring-floor-unseen
  "fell over a world with a zone ring round the trunk (x 3..9, z -3..3) whose floor cells at y 63 and below outside the ring
  read as never seen until a look at one is made."
  [spec args n]
  (let [s (fm/start spec {})
        p (:p s)
        omniscient (.-sensedAt p)
        outside? (fn [pos] (not (and (<= 3 (.-x pos) 9) (<= -3 (.-z pos) 3))))]
    (eye-reach! p)
    (aset p "sensedAt" (fn [pos] (if (or (> (.-y pos) 63) (not (outside? pos)) (looked-low? p)) (omniscient pos) #js {:unknown true})))
    (assoc s :out (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree args n)))))

(deftest an-unseen-floor-beyond-a-zone-ring-is-looked-at-before-the-pillar-is-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ring [{:name "ring" :min [3 63 -3] :max [9 80 3] :allow #{:dig}}]
              s (await (fm/with-zones ring #(fell-ring-floor-unseen bare {:radius 10} 400)))]
          (is (= [] (kinds s :fell-tree.declined)) "the zone ring is not the whole story: the floor beyond it was unseen")
          (is (= 1 (count (kinds s :pillar.done))) "a pillar was built from outside the ring"))))))

(deftest a-floor-that-stays-unseen-after-the-looks-declines-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell-floor-unseen bare {:radius 10} 200 (constantly false)))
              w (first (kinds s :fell-tree.declined))]
          (is (not= :not-done (:out s)))
          (is (contains? #{:no-base :no-stand} (:reason w)))
          (is (= 1 (count (kinds s :fell-tree.declined)))))))))

(def chest-at "-2,64,3")

(defn chest-world [stock]
  (assoc tall :inventory [] :blocks (assoc (:blocks tall) chest-at "chest") :containers {chest-at stock}))

(defn dirt-in-chest [{:keys [p]}]
  (some #(when (= "dirt" (:name %)) (:count %)) (get-in @(fake/state p) [:containers [-2 64 3]])))

(deftest a-fetch-plan-for-a-need-asks-for-the-count
  (let [o {:what #{:item} :how #{:chest} :depth 2}]
    (is (= {:any-of ["dirt" "cobblestone"] :count 4} (:args (fetch/plan-for {:reason :need :any-of ["dirt" "cobblestone"] :count 4} o))))
    (is (= {:item "dirt" :count 3} (:args (fetch/plan-for {:reason :need :item "dirt" :count 3} o))))))

(deftest cut-logs-are-the-pillars-blocks-so-a-seen-chest-of-dirt-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (chest-world [{:name "dirt" :count 20}]) {:radius 10} 300))
              {:keys [p out]} s]
          (is (= {:base {:x 6 :y 64 :z 0} :logs 8 :axe :none-found} out))
          (is (= (set (range 64 72)) (set (dug-ys p))) "every log dug")
          (is (= 20 (dirt-in-chest s)) "no dirt fetched while logs are carried")
          (is (= [] (log-pillar-cells p)) "no pillar block left"))))))

(deftest no-logs-carried-the-pillars-height-of-dirt-is-fetched-from-a-seen-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (assoc (chest-world [{:name "dirt" :count 20}]) :lose-logs? true) {:radius 10} 300))
              {:keys [p out]} s]
          (is (= {:base {:x 6 :y 64 :z 0} :logs 8 :axe :none-found} out))
          (is (= (set (range 64 72)) (set (dug-ys p))) "every log dug")
          (is (< 1 (- 20 (dirt-in-chest s))) "the chest gave the pillar's height, not one block")
          (is (= [] (pillar-cells p)) "no pillar block left"))))))

(deftest a-shortfall-of-logs-asks-only-for-the-missing-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (chest-world [{:name "dirt" :count 20}]) :lose-logs? true)
              none (await (fell world {:radius 10} 300))
              one (await (fell (assoc world :inventory [{:name "oak_log" :count 1}]) {:radius 10} 300))
              given #(- 20 (dirt-in-chest %))]
          (is (pos? (given one)) "dirt was fetched for the shortfall")
          (is (= 1 (- (given none) (given one))) "one carried log cuts the fetch by one"))))))

(deftest another-players-fresh-log-near-the-tree-is-not-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ann {:id 99 :name "item" :kind "item" :visible true :pos [8.5 64.02 0.5] :item {:name "oak_log" :count 1}
                   :spawn {:at (+ (js/Date.now) 1000000) :pos [8.5 64.02 0.5] :player {:name "Ann" :pos [8.5 62.7 0.5]}}}
              {:keys [p out]} (await (fell (assoc tall :entities [ann]) {:radius 10} 300))]
          (is (= 8 (:logs out)))
          (is (some #(= 99 (:id %)) (:entities @(fake/state p))) "her log is still on the ground"))))))

(deftest a-log-over-hidden-lava-is-dug-and-the-lava-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start {:blocks (assoc (lt/tree 6 0 "oak" 3) "6,63,0" "lava")
                           :inventory [{:name "cobblestone" :count 2}]} {})
              p (:p s)]
          (bd/hiding p #(= [6 63 0] %))
          (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree {:radius 10} 200))
          (is (= "air" (h/block-at p 6 64 0)) "the bottom log is dug")
          (is (= "cobblestone" (h/block-at p 6 63 0)) "the lava under it is sealed"))))))

(defn ^:async fell-top-unseen
  "fell over a world where the top log (6,71,0) reads as never seen until the body looks at it."
  [spec args n]
  (let [s (fm/start spec {})
        p (:p s)
        sensed (.-sensedAt p)
        block-at (.-seenBlockAt p)
        top? (fn [pos] (and (= 6 (.-x pos)) (= 71 (.-y pos)) (= 0 (.-z pos))))
        looked? #(boolean (some (fn [call] (and (= "look" (.-name call)) (= 71 (js/Math.floor (.-y (.-pos (.-args call))))))) (.-calls (.-world p))))
        hidden? (fn [pos] (and (top? pos) (not (looked?))))]
    (eye-reach! p)
    (aset p "sensedAt" (fn [pos] (if (hidden? pos) #js {:unknown true} (.call sensed p pos))))
    (aset p "seenBlockAt" (fn [pos] (if (hidden? pos) #js {:unknown true :pos pos} (.call block-at p pos))))
    (assoc s :out (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree args n)))))

(deftest a-top-log-never-seen-is-looked-at-before-the-tree-counts-as-felled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (fell-top-unseen tall {:radius 10} 300))]
          (is (contains? (set (dug-ys p)) 71) "the top log was dug"))))))

(deftest a-lower-log-seen-after-the-base-was-chosen-becomes-the-base-and-the-debt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks (assoc (lt/tree 6 0 "oak" 3) "6,64,0" "air")}
              s (fm/start spec {})
              p (:p s)
              shown (atom false)]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (when-not @shown
                         (reset! shown true)
                         (fake/set-block! p [6 64 0] "oak_log"))
                       (await (impl token args))))
          (let [out (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree {:radius 10} 100))]
            (is (= {:base {:x 6 :y 64 :z 0} :logs 3 :axe :skipped-small-tree} out) "the stump log is the base")
            (is (= [{:pos {:x 6 :y 64 :z 0} :species "oak"}] (lt/debts (:eng s))) "the replant is on the ground, not on the log above it")))))))

(deftest a-body-left-up-in-the-leaves-after-the-felling-comes-down-to-the-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [leaves (into {} (for [x (range 4 8) z [0 1]] [(str x ",69," z) "oak_leaves"]))
              s (fm/start {:blocks (merge (lt/tree 3 0 "oak" 3) leaves)} {})
              p (:p s)
              n (atom 0)]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (when (= 3 (swap! n inc))
                           (swap! (fake/state p) assoc-in [:self :pos] [5.5 70 0.5]))
                         r)))
          (let [out (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree {:radius 10} 100))
                y (get-in @(fake/state p) [:self :pos 1])]
            (is (= 3 (:logs out)))
            (is (<= y 65) "the body ends on the ground, not up on the leaves")))))))

;; ------------------------------------------------------------------ an axe is sourced when none is carried

(defn axe-world
  "The tall tree, dirt carried, and a chest holding stock."
  [carried stock]
  (assoc tall :inventory (into [{:name "dirt" :count 20}] carried)
         :blocks (assoc (:blocks tall) chest-at "chest") :containers {chest-at stock}))

(defn chest-axes [{:keys [p]}]
  (->> (get-in @(fake/state p) [:containers [-2 64 3]]) (filter #(= "iron_axe" (:name %))) (map :count) (reduce + 0)))

(deftest a-seen-chest-axe-is-fetched-before-a-big-tree-is-felled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (axe-world [] [{:name "iron_axe" :count 1}]) {:radius 10} 400))]
          (is (= 8 (:logs (:out s))) "the tree is felled")
          (is (= 0 (chest-axes s)) "the chest gave its axe")
          (is (= 1 (get (lt/inv (:p s)) "iron_axe"))))))))

(deftest fetch-false-chops-by-hand-and-leaves-the-axe-in-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (axe-world [] [{:name "iron_axe" :count 1}]) {:radius 10 :fetch false} 400))]
          (is (= 8 (:logs (:out s))))
          (is (= 1 (chest-axes s))))))))

(deftest a-carried-axe-is-not-fetched-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (axe-world [{:name "stone_axe" :count 1}] [{:name "iron_axe" :count 1}]) {:radius 10} 400))]
          (is (= 8 (:logs (:out s))))
          (is (= 1 (chest-axes s)) "the chest was left alone"))))))

(deftest a-fetch-kinds-set-without-tools-chops-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (axe-world [] [{:name "iron_axe" :count 1}]) {:radius 10 :fetch #{:item}} 400))]
          (is (= 8 (:logs (:out s))))
          (is (= 1 (chest-axes s))))))))

(deftest a-failed-axe-fetch-still-fells-the-tree-by-hand-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (axe-world [] []) {:radius 10} 400))]
          (is (= 8 (:logs (:out s))) "felled without an axe")
          (is (= 1 (count (kinds s :fetch.failed)))))))))

(deftest a-small-tree-is-chopped-by-hand-without-a-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (axe-world [] [{:name "iron_axe" :count 1}]) :blocks (assoc (lt/tree 6 0 "oak" 2) chest-at "chest"))
              s (await (fell w {:radius 10} 300))]
          (is (= 2 (:logs (:out s))))
          (is (= 1 (chest-axes s))))))))

(deftest a-wait-limits-only-tighten-the-callers
  (is (= {:how #{:chest :craft} :minutes 0.2} (select-keys (fetch/tighten {:how #{:chest :craft} :minutes 0.2} {:how #{:chest :craft} :minutes 1}) [:how :minutes])) "shorter minutes win")
  (is (= {:how #{:chest} :minutes 1} (select-keys (fetch/tighten {:how #{:chest} :minutes 10} {:how #{:chest :craft} :minutes 1}) [:how :minutes])) "the caller's sources stay a subset")
  (is (= #{:chest} (:how (fetch/tighten {:how #{:chest :craft :gather}} {:how #{:chest}}))) "the wait's sources cut the caller's")
  (is (= #{:chest} (:how (fetch/tighten {:how #{:chest}} {:how #{:chest :craft}}))) "a wait never adds a source")
  (is (= 3 (:minutes (fetch/tighten {:minutes 3} {:how #{:chest}}))) "keys the wait lacks are the caller's"))

(deftest a-gathered-log-need-fetches-no-axe
  (let [need (first (obtain/gather-needs {"oak_log" 3}))]
    (is (= 'jobs.forestry.harvest-wood (:job need)))
    (is (= #{:item} (:fetch (:args need))) "an axe fetch inside a gather would gather logs again")))

(deftest the-axe-wait-plans-a-chest-or-craft-get-tool-and-no-gather
  (let [lim (fetch/tighten (merge fetch/built-in {}) (axe-fetch-limits))
        pl (fetch/plan-for {:reason :no-tool :kind "axe"} lim)]
    (is (= 'jobs.items.get-tool (:job pl)))
    (is (= "axe" (:kind (:args pl))))
    (is (= #{:chest :craft} (:how lim)) "no :gather: felling logs for the axe would come back to fell-tree")
    (is (= 1 (:minutes lim)))))

(deftest the-result-says-why-no-axe-was-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [off (await (fell (axe-world [] [{:name "iron_axe" :count 1}]) {:radius 10 :fetch false} 400))
              none (await (fell (axe-world [] []) {:radius 10} 400))
              small (await (fell (assoc (axe-world [] []) :blocks (lt/tree 6 0 "oak" 2)) {:radius 10} 300))]
          (is (= :skipped-fetch-off (:axe (:out off))))
          (is (= :none-found (:axe (:out none))))
          (is (= :skipped-small-tree (:axe (:out small)))))))))
