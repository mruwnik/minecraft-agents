(ns engine.obtain-iron-test
  "Iron as a source of items.obtain: raw ores mined from their seen ore blocks, smelt products made by a smelt child."
  (:require [cljs.test :refer [deftest is async]]
            [engine.settings :as settings]
            [jobs.items.obtain.gather :as gather]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.fetch-test :as f]
            [engine.game :as game]
            [engine.perception :as perception]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.items.obtain :as obtain]
            [jobs.items.obtain.base :as base]
            [jobs.items.obtain.table :as table]
            [jobs.items.obtain.why :as why]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.items.recipes :as recipes]))

(defn ore-world
  "Ore blocks south of the body (where it faces, so the first sight pass sees them), carrying inv."
  [blocks inv]
  (-> (f/bare inv)
      (update :blocks merge blocks)
      (assoc :drops {"iron_ore" "raw_iron" "deepslate_iron_ore" "raw_iron" "stone" "cobblestone"})))

(defn ^:async obtain! [s args n]
  (core/submit! (:eng s) (list 'jobs.items.obtain args) {})
  (await (f/run-ticks s n)))

(defn mine-events [s] (filterv #(re-find #"^mine\." (name (:kind %))) @(:seen s)))

(deftest the-plan-for-an-iron-chestplate-from-nothing-gathers-raw-iron-and-smelts
  (let [pl (recipes/plan game/default-version {} "iron_chestplate" 1 (assoc (table/source-opts) :gather? true))]
    (is (= 8 (get-in pl [:gather "raw_iron"])))
    (is (some #(and (= :smelt (:op %)) (= "iron_ingot" (:item %))) (:steps pl)))))

(deftest the-plan-for-glass-panes-from-nothing-gathers-sand-and-smelts
  (let [pl (recipes/plan game/default-version {} "glass_pane" 16 (assoc (table/source-opts) :gather? true))]
    (is (= 6 (get-in pl [:gather "sand"])))
    (is (some #(and (= :smelt (:op %)) (= "glass" (:item %))) (:steps pl)))))

(deftest the-plan-for-a-white-bed-gathers-wool-or-string
  (let [pl (recipes/plan game/default-version {"oak_planks" 3} "white_bed" 1 (assoc (table/source-opts) :gather? true))]
    (is (= 3 (get-in pl [:gather "white_wool"])) (pr-str pl)))
  (let [pl (recipes/plan game/default-version {"oak_planks" 3 "string" 12} "white_bed" 1 (assoc (table/source-opts) :gather? true))]
    (is (= 0 (get-in pl [:gather "white_wool"] 0)) (pr-str pl))))

(deftest raw-iron-is-mined-from-a-seen-iron-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,4" "iron_ore" "2,64,4" "iron_ore"} [{:name "stone_pickaxe" :count 1}]) [])]
          (await (obtain! s {:item "raw_iron" :count 2} 80))
          (is (empty? (f/listed s)))
          (is (= 2 (get (f/inv s) "raw_iron"))))))))

(deftest raw-iron-is-mined-from-a-seen-deepslate-iron-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,4" "deepslate_iron_ore"} [{:name "stone_pickaxe" :count 1}]) [])]
          (await (obtain! s {:item "raw_iron"} 80))
          (is (empty? (f/listed s)))
          (is (= 1 (get (f/inv s) "raw_iron"))))))))

(deftest raw-iron-with-only-a-wooden-pickaxe-gets-a-stone-one-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone (into {} (for [x [-1 0] z [4 5]] [(str x ",64," z) "stone"]))
              s (f/start (ore-world (merge stone {"2,64,4" "iron_ore"})
                                    [{:name "wooden_pickaxe" :count 1} {:name "stick" :count 2} {:name "crafting_table" :count 1}])
                         [])]
          (await (obtain! s {:item "raw_iron"} 240))
          (is (empty? (f/listed s)))
          (is (= 1 (get (f/inv s) "stone_pickaxe")))
          (is (= 1 (get (f/inv s) "raw_iron")))
          (let [es (f/events-of s :obtain.pickaxe)]
            (is (= 1 (count es)) "one event for the pickaxe step")
            (is (= "raw_iron" (:item (first es))) "names the item it is for")
            (is (= :info (:level (first es))))))))))

(deftest a-pickaxe-that-breaks-mid-mine-is-replaced-with-wood-and-stone-gathered-and-mining-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trees (for [x [-4 0 4]] (merge (into {} (for [y [64 65 66]] [(str x "," y ",5") "oak_log"]))
                                             {(str x ",67,5") "oak_leaves" (str x ",63,5") "grass_block"}))
              blocks (merge (apply merge trees)
                            (into {} (for [x (range -2 4) z [12 13] y [63 64]] [(str x "," y "," z) "stone"]))
                            (into {} (for [x (range -2 3)] [(str x ",64,17") "iron_ore"]))
                            (into {} (for [x (range -2 3)] [(str x ",63,17") "stone"])))
              s (f/start (-> (ore-world blocks [{:name "stone_pickaxe" :count 1} {:name "crafting_table" :count 1}])
                             (assoc :floor [-30 -10 40 30])
                             (update :drops assoc "oak_log" "oak_log"))
                         [] (assoc f/sight-opts :radius 24))
              p (:p s)
              breaks (atom 0)]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (when (and (< @breaks 3) (some (comp #{"stone_pickaxe"} :name) (:inventory @(fake/state p))))
                           (swap! breaks inc)
                           (swap! (fake/state p) update :inventory #(vec (remove (comp #{"stone_pickaxe"} :name) %))))
                         r)))
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "raw_iron" :count 4 :explore true}) {})
          (loop [i 0]
            (await (f/run-ticks s 1))
            (.advance (.-world p) 40)
            (when (and (< i 3000) (seq (f/listed s))) (recur (inc i))))
          (is (= 3 @breaks))
          (is (<= 4 (get (f/inv s) "raw_iron" 0))))))))

(deftest raw-iron-with-no-ore-seen-waits-no-source-naming-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} [{:name "stone_pickaxe" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "raw_iron" :ways #{:craft :smelt :gather} :explore false} 6))
          (let [w (first (filter #(= :no-source (:reason %)) (f/events-of s :waiting)))]
            (is (some? w))
            (is (re-find #"raw_iron \(from deepslate_iron_ore/iron_ore\)" (str (:text w)))))
          (is (empty? (f/calls s "dig"))))))))

(deftest raw-iron-unseen-with-explore-goes-to-the-mine-not-a-surface-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (ore-world {} [{:name "stone_pickaxe" :count 1}]) :floor [-30 -30 40 30]) [f/own-zone])]
          (await (obtain! s {:item "raw_iron" :ways #{:craft :smelt :gather} :explore true} 4))
          (is (empty? (f/events-of s :obtain.exploring)) "no surface search")
          (is (seq (mine-events s)) "the mine child ran"))))))

(deftest an-iron-pickaxe-with-no-ore-seen-goes-to-the-mine-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (ore-world {} [{:name "stone_pickaxe" :count 1} {:name "stick" :count 2} {:name "crafting_table" :count 1}]) :floor [-30 -30 40 30]) [f/own-zone])]
          (await (obtain! s {:item "iron_pickaxe"} 4))
          (is (empty? (f/events-of s :waiting)) "no silent wait")
          (is (seq (mine-events s)) "the mine child ran"))))))

(deftest raw-iron-unseen-at-night-with-torches-waits-or-digs-not-none-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (ore-world {} [{:name "stone_pickaxe" :count 1} {:name "torch" :count 16}])
                                :floor [-30 -30 40 30] :time st/night) [f/own-zone])]
          (await (obtain! s {:item "raw_iron" :ways #{:craft :smelt :gather} :explore true} 6))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))) "no source is not claimed")
          (is (empty? (filter #(= :stopped (:kind %)) @(:seen s)))))))))

(deftest an-unseen-ore-gets-torches-before-the-mine-goes-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (ore-world {} [{:name "stone_pickaxe" :count 1} {:name "stick" :count 2} {:name "coal" :count 2} {:name "crafting_table" :count 1}])
                                :floor [-30 -30 40 30]) [f/own-zone])]
          (await (obtain! s {:item "raw_iron" :ways #{:craft :smelt :gather} :explore true} 60))
          (is (seq (f/events-of s :torch.stocked)) "the torches were fetched by obtain, since the mine's ways cannot gather")
          (is (< 0 (get (f/inv s) "torch" 0))))))))

(deftest a-smelted-product-without-its-input-names-the-smelt-not-a-block-recipe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} [{:name "stone_pickaxe" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "iron_ingot" :count 31 :ways #{:craft :smelt :gather} :explore false} 6))
          (let [w (str (:text (first (filter #(= :no-source (:reason %)) (f/events-of s :waiting)))))]
            (is (re-find #"smelting iron_ingot needs raw_iron, have 0" w))
            (is (not (re-find #"iron_block" w)))))))))

(deftest cobblestone-unseen-with-explore-goes-to-the-mine-not-a-surface-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (ore-world {} [{:name "wooden_pickaxe" :count 1}]) :floor [-30 -30 40 30]) [f/own-zone])]
          (await (obtain! s {:item "cobblestone" :count 3 :ways #{:gather} :explore true} 6))
          (is (empty? (f/events-of s :obtain.exploring)) "no surface search")
          (is (seq (mine-events s)) "the mine child ran"))))))

(deftest a-badly-hurt-body-does-not-walk-off-to-look-for-an-unseen-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc-in (ore-world {} []) [:self :health] 0.5) [f/own-zone])]
          (await (obtain! s {:item "oak_log" :count 2 :ways #{:gather} :explore true} 8))
          (is (empty? (tu/walk-calls (:p s))) "no search walk at 0.5 hp")
          (is (= [:low-health] (distinct (map :reason (f/events-of s :waiting))))))))))

(def st-pit
  "A one-cell pit round the body at the origin: stone on its four sides at feet and head height and over it."
  (into {"0,66,0" "stone"} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "stone"])))

(defn sheltered-night-start
  "A body sealed in its night pit, the tree (and for stone, a stone) seen from inside."
  [blocks health]
  (let [s (f/start (assoc (ore-world (merge f/tree-blocks blocks) []) :time st/night) [f/own-zone])]
    (swap! (fake/state (:p s)) assoc-in [:self :health] health)
    (doseq [pitch [-80 80 0]]
      (swap! (fake/state (:p s)) assoc :pitch pitch)
      (perception/pass! (aget (:p s) "perception")))
    (is (some? (sh-inside/night-sheltered (:p s) nil)) "sheltered at the start")
    s))

(deftest a-tree-seen-from-a-night-shelter-is-walked-to-when-the-trip-is-cheap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (sheltered-night-start st-pit 20)]
          (await (obtain! s {:any-of ["oak_log" "spruce_log" "birch_log"] :count 2 :ways #{:gather} :explore true} 8))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))) "the tree is seen")
          (is (seq (tu/walk-calls (:p s))) "go-to prices its own route; there is no flat night hold"))))))

(deftest a-tree-seen-from-a-night-shelter-waits-when-the-trip-is-dear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (sheltered-night-start st-pit 7)]
          (await (settings/with-settings {:jobs.lib.price/risk-share 0} #(obtain! s {:any-of ["oak_log" "spruce_log" "birch_log"] :count 2 :ways #{:gather} :explore true} 8)))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))) "the tree is seen")
          (is (empty? (tu/walk-calls (:p s))) "no walk out")
          (is (some #(= :risky (:reason %)) (f/events-of s :waiting)) "the priced route risk is over what the body can afford"))))))

(deftest a-pickaxe-whose-logs-are-outside-a-night-shelter-walks-out-for-them-not-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (sheltered-night-start (assoc st-pit "1,64,4" "stone") 20)]
          (await (obtain! s {:item "cobblestone" :count 3 :ways #{:gather :craft}} 8))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))) "the stone and the logs are seen")
          (is (seq (tu/walk-calls (:p s))) "the logs are walked to under go-to's own pricing"))))))

(deftest a-craft-the-dark-prices-too-high-makes-the-obtain-wait-not-end-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (ore-world {} [{:name "oak_log" :count 6} {:name "cobblestone" :count 5}]) :time st/night) [f/own-zone])]
          (swap! (fake/state (:p s)) assoc-in [:self :health] 1)
          (doseq [pitch [-80 80 0]]
            (swap! (fake/state (:p s)) assoc :pitch pitch)
            (perception/pass! (aget (:p s) "perception")))
          (await (settings/with-settings {:jobs.lib.price/risk-share 0} #(obtain! s {:item "stone_pickaxe" :count 1 :ways #{:craft}} 8)))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))) "logs are carried: not no-source")
          (is (some #(#{:dark :exposed} (:reason %)) (f/events-of s :waiting))))))))

(deftest a-table-fetch-whose-logs-wait-out-the-night-waits-once-and-is-not-restarted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (ore-world (merge f/tree-blocks {"0,66,0" "stone"}) []) :time st/night) [f/own-zone])]
          (doseq [pitch [-80 80 0]]
            (swap! (fake/state (:p s)) assoc :pitch pitch)
            (perception/pass! (aget (:p s) "perception")))
          (core/submit! (:eng s) (list 'jobs.items.craft {:item "stone_pickaxe"}) {})
          (await (f/run-ticks s 30))
          (is (= [:exposed] (distinct (map :reason (f/events-of s :waiting)))) "harvest-wood judges the tree it walks to, not the room")
          (is (= 1 (count (f/events-of s :waiting))) "one wait, not one per round")
          (is (empty? (filter #(= :stopped (:kind %)) @(:seen s)))))))))

(deftest an-iron-ingot-with-raw-iron-carried-is-smelted-by-a-smelt-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,2" "furnace"} [{:name "raw_iron" :count 3} {:name "coal" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "iron_ingot" :count 3 :ways #{:craft :smelt :gather}} 6))
          (is (seq (f/calls s "furnace")) "the furnace was used")
          (is (and (zero? (get (f/inv s) "iron_ingot" 0)) (zero? (get (f/inv s) "raw_iron" 0))) "the raw iron is in the furnace, no ingot yet")
          (.advance (.-world (:p s)) 2000)
          (await (f/run-ticks s 200))
          (is (= 3 (get (f/inv s) "iron_ingot")))
          (is (empty? (f/listed s))))))))

(deftest an-iron-pickaxe-from-seen-ore-is-mined-smelted-and-crafted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ore (into {} (for [x [1 2 3]] [(str x ",64,4") "iron_ore"]))
              s (f/start (ore-world (merge ore {"1,64,2" "furnace"})
                                    [{:name "stone_pickaxe" :count 1} {:name "stick" :count 2} {:name "coal" :count 1}
                                     {:name "crafting_table" :count 1}])
                         [])
              _ (swap! (fake/state (:p s)) assoc-in [:recipes "iron_pickaxe"] {:count 1 :needs {"iron_ingot" 3 "stick" 2} :table true})]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "iron_pickaxe" :spend {:job-time 60} :explore false}) {})
          (loop [i 0]
            (await (f/run-ticks s 20))
            (.advance (.-world (:p s)) 2000)
            (when (and (< i 30) (zero? (get (f/inv s) "iron_pickaxe" 0)))
              (recur (inc i))))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (empty? (filter #(= :timeout (:reason %)) (f/events-of s :stopped))))
          (is (empty? (f/listed s))))))))

(deftest a-coal-wanted-with-none-seen-falls-back-to-charcoal-from-seen-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [logs f/tree-blocks
              s (f/start (ore-world (merge logs {"1,64,2" "furnace"}) []) [f/own-zone])]
          (await (obtain! s {:any-of ["coal" "charcoal"] :ways #{:craft :smelt :gather}} 60))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))))
          (is (seq (f/calls s "dig")) "a log was felled for the charcoal"))))))

(deftest a-torch-with-no-coal-seen-is-made-from-charcoal-of-seen-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world (merge f/tree-blocks {"1,64,2" "furnace"}) [{:name "stick" :count 2}]) [f/own-zone])]
          (await (obtain! s {:item "torch" :count 4 :ways #{:craft :smelt :gather}} 60))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))))
          (is (seq (f/calls s "dig")) "a log was felled for the charcoal")
          (is (seq (f/events-of s :smelt.loaded)) "the logs went in the furnace"))))))

(defn coal-below-world
  "A coal ore 2 below the body, 9 south, at the bottom of a cut in the floor so the body's sight reaches it."
  [inv]
  (ore-world (merge f/tree-blocks {"1,64,2" "furnace" "1,62,9" "coal_ore"}
                    (into {} (for [x (range 0 3) z (range 5 10)] [(str x ",63," z) "air"])))
             inv))

(defn coal-seen? [s] (boolean (seq (look/seen-blocks (:p s) {:match #(= "coal_ore" %) :radius 32 :max 4}))))

(defn ^:async torches-with-coal-below!
  "Obtain 4 torches with a coal ore below, after one sight pass so it is seen when the plan is made."
  [light inv]
  (let [s (f/start (coal-below-world inv) [f/own-zone])]
    (swap! (fake/state (:p s)) assoc :light (into {} (for [x (range 0 3) y (range 60 67) z (range 6 11)] [[x y z] light])))
    (await (f/run-ticks s 2))
    (let [seen (coal-seen? s)] (is seen "the coal below is seen before the plan is made"))
    (await (obtain! s {:item "torch" :count 4 :ways #{:craft :smelt :gather}} 60))
    s))

(deftest a-torch-with-dark-coal-seen-below-and-no-torch-is-made-from-charcoal-of-seen-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (torches-with-coal-below! [0 0] [{:name "stick" :count 2} {:name "wooden_pickaxe" :count 1}]))]
          (is (empty? (mine-events s)) "the dark way down to the coal is not taken")
          (is (seq (f/events-of s :smelt.loaded)) "the logs went in the furnace"))))))

(deftest a-torch-with-lit-coal-seen-below-is-mined-not-made-from-charcoal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (torches-with-coal-below! [0 15] [{:name "stick" :count 2} {:name "wooden_pickaxe" :count 1}]))]
          (is (seq (mine-events s)) "a coal seen in the light is mined")
          (is (empty? (f/events-of s :smelt.loaded)) "no logs were smelted"))))))

(deftest a-charcoal-smelt-burns-the-planks-kind-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,2" "furnace"} [{:name "oak_log" :count 1} {:name "spruce_planks" :count 4}]) [f/own-zone])]
          (await (obtain! s {:item "charcoal" :ways #{:craft :smelt :gather}} 6))
          (is (= ["spruce_planks"] (keep :fuel (f/events-of s :smelt.loaded))) "the carried planks kind is the fuel, not coal or the first wood")
          (.advance (.-world (:p s)) 2000)
          (await (f/run-ticks s 200))
          (is (= 1 (get (f/inv s) "charcoal")) "the cook finished and its output was collected"))))))

(deftest a-charcoal-smelt-of-the-only-logs-carried-burns-some-of-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,2" "furnace"} [{:name "spruce_log" :count 3}]) [f/own-zone])]
          (await (obtain! s {:item "charcoal" :ways #{:craft :smelt :gather}} 6))
          (is (empty? (f/events-of s :obtain.exploring)) "no walk to look for oak")
          (is (= ["spruce_log"] (keep :fuel (f/events-of s :smelt.loaded))) "the carried logs burn"))))))

(deftest nuggets-carried-are-combined-into-an-ingot-with-ways-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (f/bare [{:name "iron_nugget" :count 27} {:name "oak_log" :count 2}])
                                  :recipes {"iron_ingot" {:count 1 :needs {"iron_nugget" 9} :table true}}) [])]
          (await (obtain! s {:item "iron_ingot" :count 1 :ways #{:craft :smelt}} 80))
          (is (empty? (f/listed s)))
          (is (= 1 (get (f/inv s) "iron_ingot"))))))))

(deftest a-failed-gather-is-named-in-the-no-source-text
  (is (nil? (why/tried-text {:craft :no-plan})))
  (is (= "; gathering was tried and ended :gave-up" (why/tried-text {:gather :gave-up})))
  (is (re-find #"searched" (why/tried-text {:gather {:searched "coal" :result :not-found}}))))

(deftest the-bad-args-text-names-the-arg-and-its-value
  (is (re-find #":count.*\"16\"" (base/args-error {:item "torch" :count "16"})))
  (is (re-find #":item.*nil" (base/args-error {:count 4}))))

(deftest ore-in-water-is-left-for-the-next-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pool (into {} (for [x (range 0 5) y [64 65 66] z [2 3 4 5 6]] [(str x "," y "," z) "water"]))
              blocks (merge pool {"1,64,4" "iron_ore" "3,64,5" "iron_ore" "0,64,6" "iron_ore" "4,64,3" "iron_ore"})
              s (f/start (ore-world blocks [{:name "stone_pickaxe" :count 1}]) [])]
          ;; barren-ore 0: once the ore is all left, the tunnels after it are not this test's
          (await (obtain! s {:item "raw_iron" :count 1 :settings {:jobs.items.obtain.base/barren-ore 0}} 200))
          (is (every? (set (map :at (filter (comp #{:obtain.gathering} :kind) @(:seen s))))
                      [{:x 1 :y 64 :z 4} {:x 3 :y 64 :z 5} {:x 0 :y 64 :z 6} {:x 4 :y 64 :z 3}])
              "each flooded ore is left for the next, none counted fruitless"))))))

(deftest ore-in-a-dark-hall-the-mine-refuses-is-left-for-the-next-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks {"1,64,4" "iron_ore" "3,64,5" "iron_ore" "0,64,6" "iron_ore"}
              s (f/start (ore-world blocks [{:name "stone_pickaxe" :count 1}]) [])
              eng (assoc-in (:eng s) [:jobs 'jobs.gather.mine :round]
                            (fn ^:async f [c] (ctx/end c :impossible {:reason :exposed :text "dark hall"})))
              s (assoc s :eng eng)]
          (await (obtain! s {:item "raw_iron" :count 1} 200))
          (is (every? (set (map :at (filter (comp #{:obtain.gathering} :kind) @(:seen s))))
                      [{:x 1 :y 64 :z 4} {:x 3 :y 64 :z 5} {:x 0 :y 64 :z 6}])
              "each refused ore is left for the next, none counted as nothing seen")
          (is (not-any? #(= :none-seen (:reason %)) (f/events-of s :waiting))))))))

(deftest an-unseen-ore-is-mined-on-while-each-run-cuts-new-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (loop [[[y dug runs why] & more] [[64 {:tunnel {:steps 32}} 5 "a run that tunnelled looked at new rock: the gather mines on"]
                                          [64 {:legs [{:length 20} {:length 12}] :tunnel {:steps 12}} 5 "so did one that turned"]
                                          [64 {} 3 "a run that cut nothing is fruitless: three end the gather"]
                                          [16 {:tunnel {:steps 200}} 4 "at y 16 the second 200 blocks with no iron make the ground barren: three more runs end it"]]]
          (let [s (f/start (-> (ore-world {} [{:name "stone_pickaxe" :count 1} {:name "torch" :count 16}]) (assoc :floor [-30 -30 40 30]) (assoc-in [:self :pos :y] y)) [f/own-zone])
                calls (atom 0)
                eng (assoc-in (:eng s) [:jobs 'jobs.gather.mine :round]
                              (fn ^:async f [c]
                                ;; a run takes a round at least, as a real dig does
                                (if-not (:dug (ctx/mem c))
                                  (do (swap! calls inc) (ctx/update-mem! c assoc :dug true) :continue)
                                  (ctx/end c :nothing-possible {:reason :tunnel-length} (merge {:got 0 :reason :tunnel-length} dug)))))]
            (await (obtain! (assoc s :eng eng) {:item "raw_iron" :ways #{:craft :smelt :gather} :explore true} 60))
            (is (= runs (min 5 @calls)) why))
          (when more (recur more)))))))

(deftest the-dug-count-follows-the-site-not-the-ore
  (let [at {:x 0 :y 16 :z 32}
        mem {:gather {:dug {"iron_ore" {:n0 200 :at at}}}}]
    (is (= 232 (gather/dug-total mem "iron_ore" {:x 0 :y 16 :z 60} 32)) "the run went on from where the last ended")
    (is (= 32 (gather/dug-total mem "iron_ore" {:x 300 :y 16 :z 0} 32)) "site B, far from where A's count ended, starts at its own run")
    (is (= 32 (gather/dug-total {} "iron_ore" at 32)))))

(deftest the-blocks-dug-without-a-find-do-not-outlive-the-gather
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (-> (ore-world {} [{:name "stone_pickaxe" :count 1} {:name "torch" :count 16}]) (assoc :floor [-30 -30 40 30])) [f/own-zone])
              runs (atom 0)
              eng (assoc-in (:eng s) [:jobs 'jobs.gather.mine :round]
                            (fn ^:async f [c]
                              (if-not (:dug (ctx/mem c))
                                (do (ctx/update-mem! c assoc :dug true) :continue)
                                (ctx/end c :nothing-possible {:reason :tunnel-length}
                                         (merge {:got 0 :reason :tunnel-length} (when (= 1 (swap! runs inc)) {:tunnel {:steps 32}}))))))]
          (await (obtain! (assoc s :eng eng) {:item "raw_iron" :ways #{:craft :smelt :gather} :explore true} 60))
          (is (map? (core/job-memory (:eng s) "j1")) "the job memory is read")
          (is (nil? (get-in (core/job-memory (:eng s) "j1") [:gather :dug])) "the count of a gather that ended fruitless is gone"))))))

(defn ^:async hall-refusal-run
  "Obtain one raw_iron from a seen ore while the mine refuses a dark hall at cell until light-area has run, the refusal
  given by refuse (the mine def, lit atom -> def); [the light-area cells asked, the raw_iron carried]."
  [refuse]
  (let [s (f/start (ore-world {"1,64,4" "iron_ore"} [{:name "stone_pickaxe" :count 1}]) [])
        lit (atom [])
        eng (-> (:eng s)
                (update-in [:jobs 'jobs.gather.mine] refuse lit)
                (assoc-in [:jobs 'jobs.build.light-area :round]
                          (fn ^:async f [c] (swap! lit conj (:cells (:args c))) (ctx/end c :done nil {}))))]
    (await (obtain! (assoc s :eng eng) {:item "raw_iron" :count 1} 120))
    [@lit (get (f/inv s) "raw_iron")]))

(def hall-cause {:reason :exposed :text "dark hall" :lowered-by #{:torch} :at {:x 0 :y 64 :z 0}})

(deftest a-mine-refused-for-a-dark-hall-a-torch-ends-is-lit-first-then-mined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (loop [[[how refuse] & more] [["its start check"
                                        (fn [def lit] (update def :passed-status (fn [ps] (fn [c] (if (seq @lit) (ps c) [:never "dark hall" hall-cause])))))]
                                       ["its ending"
                                        (fn [def lit] (update def :round (fn [r] (fn [c] (if (seq @lit) (r c) (ctx/end c :impossible hall-cause))))))]]]
          (is (= [[[{:x 0 :y 64 :z 0}]] 1] (await (hall-refusal-run refuse))) (str "refused by " how ": the hall cell is lit once, then the ore mined"))
          (when more (recur more)))))))

(deftest a-body-standing-in-water-walks-to-dry-land-before-it-mines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pool (into {} (for [x (range -2 3) y [64 65 66] z (range -2 3)] [(str x "," y "," z) "water"]))
              s (f/start (ore-world (merge pool {"0,64,8" "iron_ore"}) [{:name "stone_pickaxe" :count 1}]) [])]
          (await (obtain! s {:item "raw_iron" :count 1} 100))
          (is (seq (f/events-of s :obtain.leave-water)) "the body left the pool before the first mine"))))))

(deftest a-cell-a-mine-gave-up-on-in-two-runs-is-left-out-of-the-third
  (let [once (gather/note-skipped {} [[1 2 3] [4 5 6]])
        twice (gather/note-skipped once [[1 2 3]])]
    (is (empty? (:skip-cells once)))
    (is (= #{[1 2 3]} (:skip-cells twice)))))
