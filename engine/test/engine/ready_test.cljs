(ns engine.ready-test
  "jobs.lib.ready: the safe? and fit? checkers a job binds, as statuses over the shelter, the light and the mobs."
  (:require [cljs.test :refer [deftest is are]]
            [engine.hut-shelter-test :as hut]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.core.ending :as ending]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.explore.look :as look]
            [jobs.forestry.plant-sapling :as plant-sapling]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.explore.search :as search]
            [jobs.combat.attack :as attack]
            [jobs.food.fish :as fish]
            [jobs.movement.look-around :as look-around]
            [jobs.access.stair :as stair]
            [jobs.build.light-area :as light-area]
            [jobs.access.toggle :as toggle]
            [jobs.blocks.dig :as dig]
            [jobs.blocks.fill-cells :as fill-cells]
            [jobs.blocks.place :as place]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.world :as known]
            [jobs.strip.site :as strip-site]
            [jobs.movement.boat-drive :as boat-drive]
            [jobs.movement.stay-near :as stay-near]
            [jobs.movement.mount :as mount]
            [jobs.movement.leave-vehicle :as leave-vehicle]
            [jobs.movement.boat-recover :as boat-recover]
            [jobs.items.collect-drops :as collect-drops]
            [jobs.items.kitchen :as kitchen]
            [jobs.items.get-kit :as get-kit]
            [jobs.items.obtain :as obtain]
            [jobs.gather.mine :as mine]
            [jobs.gather.dig-for :as dig-for]
            [jobs.gather.strip-tunnel :as strip-tunnel]
            [jobs.items.pick-up-loose :as pick-up-loose]
            [jobs.items.smelt :as smelt]
            [jobs.storage.deposit :as deposit]
            [jobs.storage.withdraw :as withdraw]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.danger :as danger]
            [jobs.maintenance.shut :as shut]
            [jobs.survival.eat :as eat]
            [jobs.survival.get-food :as get-food]
            [jobs.survival.log-out :as log-out]
            [jobs.survival.recover-drops :as recover-drops]
            [jobs.survival.set-spawn :as set-spawn]
            [jobs.survival.restore-broken :as restore-broken]
            [engine.settings :as settings]
            [jobs.time.noop :as noop]
            [jobs.time.wait-for-light :as wait-for-light]))

(defn ctx-of
  "A job's ctx over a fake body built from world (a fresh job id: the check cache is per job), staying at its place long enough that no exposure is affordable (jobs.lib.price)."
  [world]
  (let [p (cond-> (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world))) (not (:light world)) sky/with-sky)]
    {::price/stay-s 1e6 :id (str (random-uuid)) :slots [] :engine {:warned (atom #{}) :jobs registry/jobs} :primitives p :view (fn [] {:now 0 :data {}})}))

(def inside {:x 5 :y 64 :z 0})
(def outside {:x 15 :y 64 :z 0})
(def barn-roof (tu/box 14 67 -2 16 67 2 "oak_planks"))
(def night-hut (merge (hut/hut-world inside {}) {:time st/night}))
(def open-night {:time st/night :self {:pos {:x 0 :y 64 :z 0}}})
(def tunnel
  "A torch-lit 1x2 tunnel x 0..6 sealed at both ends, the body at its west end."
  {:time st/night :self {:pos {:x 0 :y 64 :z 0}}
   :blocks (merge (into {} (remove (fn [[k _]] (let [[x y z] (map js/Number (.split k ","))] (and (<= 0 x 6) (<= 64 y 65) (= z 0)))))
                        (tu/box 0 63 -1 6 66 1 "stone"))
                  {"-1,64,0" "stone" "-1,65,0" "stone" "7,64,0" "stone" "7,65,0" "stone"})
   :light (into {} (for [x (range 0 7) y [64 65]] [[x y 0] [0 14]]))})

(defn lit
  "(f) with every cell's light read as block and sky (the fake world has no light data)."
  [block sky f]
  (with-redefs [sh-sense/block-light-at (constantly block) sh-sense/sky-light-at (constantly sky)] (f)))

(defn kind [check world] (first (check (ctx-of world))))
(defn reason [check world] (:reason (nth (check (ctx-of world)) 2)))

(deftest safe-at-open-ground-at-night-waits-for-dawn
  (let [check (ready/safe-at (constantly outside))
        s (lit 0 15 #(check (ctx-of open-night)))]
    (is (= :later (first s)))
    (is (= #{:light} (:lowered-by (nth s 2))))
    (is (pos? (:recheck-after-ms (nth s 2))) "a trigger held on it waits for the light")
    (is (re-find #"15 64 0" (second s)) "names the place")
    (is (re-find #"until light" (second s)) "and what helps")))

(deftest safe-if-lit-ignores-a-stair-step-with-stone-over-it-though-sky-light-reaches-it
  (let [roofed (assoc open-night :blocks (tu/box -1 65 -1 1 70 1 "stone"))]
    (is (= [:later :ok] [(lit 0 11 #(kind ready/safe-if-lit open-night)) (lit 0 11 #(kind ready/safe-if-lit roofed))]))))

(deftest safe-if-lit-treats-an-unseen-overhead-as-open-sky-at-any-sky-light
  (let [seen-name u/seen-name
        look-down (fn [sky] (with-redefs [u/seen-name (fn [p pos] (when (<= (:y pos) 66) (seen-name p pos)))]
                              (lit 0 sky #(kind ready/safe-if-lit open-night))))]
    (is (= [:later :later] [(look-down 14) (look-down 15)]))))

(deftest safe-if-lit-looks-up-at-an-unseen-overhead-before-it-waits
  (let [roof [0 65 0]
        status (fn [unseen looked] (with-redefs [sense/unknown? (fn [_ cell] (contains? unseen cell))
                                                 sense/round-looked (constantly looked)]
                                     (lit 0 15 #(let [s (ready/safe-if-lit (ctx-of open-night))] [(first s) (:look-at (nth s 2))]))))]
    (is (= [:look [roof]] (status #{roof} #{})) "the unseen roof is looked at")
    (is (= [:later nil] (status #{roof} #{roof})) "a look that did not show it leaves the wait")
    (is (= [:later nil] (status #{} #{})) "nothing unseen over the feet: the sky is open")))

(deftest an-exposed-place-is-ok-while-the-stay-there-is-affordable
  (let [check (ready/safe-at (constantly outside))
        status (fn [stay-s] (lit 0 15 #(check (assoc (ctx-of open-night) ::price/stay-s stay-s))))]
    (is (= :ok (first (status 2))))
    (is (= :later (first (status 1e6))))))

(deftest safe-at-waits-for-light-only-within-the-light-patience
  (let [check (ready/safe-at (constantly outside))
        status (fn [patience] (settings/with-settings {:jobs.lib.ready/light-patience-s patience} #(lit 0 15 (fn [] (check (ctx-of open-night))))))]
    (is (= :later (first (status 100000))))
    (is (= :never (first (status 10))) "light further off than the patience is not waited for")
    (is (re-find #"s away" (second (status 10))))))

(deftest safe-at-a-sheltered-place-is-ok-and-a-walk-out-of-the-hut-is-not
  (are [world to expected] (= expected (kind (ready/safe-at (constantly to)) world))
    night-hut {:x 6 :y 64 :z 0} :ok
    night-hut outside :later
    (assoc night-hut :blocks (merge (:blocks night-hut) barn-roof)) outside :later
    tunnel {:x 4 :y 64 :z 0} :ok
    (assoc open-night :time st/noon) {:x 4 :y 64 :z 0} :look))

(deftest a-body-in-a-hut-with-a-wall-never-seen-looks-at-it-before-it-judges-a-walk-out
  (let [seen-block u/seen-block
        hide (fn [pos] (fn [p cell] (when-not (= pos [(:x cell) (:y cell) (:z cell)]) (seen-block p cell))))
        status #((ready/safe-at (constantly outside)) (ctx-of night-hut))]
    (is (= :later (first (status))))
    (with-redefs [u/seen-block (hide [3 64 0])]
      (let [s (status)]
        (is (= :look (first s)))
        (is (= [[3 64 0]] (:look-at (nth s 2))))))))

(deftest a-wall-a-look-round-did-not-show-counts-as-open-not-looked-at-again
  (let [seen-block u/seen-block
        hide (fn [pos] (fn [p cell] (when-not (= pos [(:x cell) (:y cell) (:z cell)]) (seen-block p cell))))]
    (with-redefs [u/seen-block (hide [3 64 0])
                  sense/round-looked (constantly #{[3 64 0]})]
      (is (not= :look (first ((ready/safe-at (constantly outside)) (ctx-of night-hut))))))))

(deftest safe-at-open-ground-by-day-is-ok-where-the-light-is-seen
  (is (= :ok (lit 0 15 #(kind (ready/safe-at (constantly outside)) (assoc open-night :time st/noon))))))

(deftest dark-at-reads-the-light-of-the-cell-not-the-clock
  (are [block sky world dark?] (= dark? (lit block sky #((complement sh-sense/lit-here?) (:primitives (ctx-of world)) (sh-sense/feet (:primitives (ctx-of world))))))
    0 15 open-night true
    8 15 open-night false
    0 0 (assoc open-night :time st/noon) true
    0 15 (assoc open-night :time st/noon) false))

(deftest safe-here-by-night-is-ok-in-the-hut
  (is (= :ok (kind ready/safe-here-by-night night-hut))))

(deftest safe-here-by-night-outside-at-night-is-later
  (is (= :later (lit 0 15 #(kind ready/safe-here-by-night open-night)))))

(deftest priced-stay-goes-ahead-when-its-expected-damage-is-affordable
  (are [stay-s expected] (= expected (lit 0 15 #(kind (price/stays ready/safe-here-by-night (constantly stay-s)) open-night)))
    2 :ok
    1e6 :later))

(deftest a-long-stay-in-the-dark-open-is-refused-where-a-click-is-accepted
  (let [bare (fn [] (dissoc (ctx-of open-night) ::price/stay-s))
        status (fn [category] (lit 0 15 #(first ((price/staying category (ready/safe-at (constantly outside))) (bare)))))]
    (is (= [:ok :later :later] (mapv status [:click :work :long])))))

(deftest a-dark-furnace-is-exposed-for-a-whole-stack-but-not-for-one-item
  (let [world (assoc open-night :inventory [{:name "furnace" :count 1}])
        exposed? (fn [n] (lit 0 15 #(smelt/exposed-furnace? (assoc (ctx-of world) :args {:item "raw_iron" :count n}) outside)))]
    (is (= [false true] (mapv exposed? [1 64])))))

(deftest safe-if-lit-waits-at-night-above-ground
  (are [world expected] (= expected (kind ready/safe-if-lit world))
    open-night :later
    tunnel :ok
    (assoc open-night :time st/noon) :ok))

(deftest safe-if-lit-does-not-wait-for-a-day-that-cannot-light-the-cell
  (are [sky time expected] (= expected (lit 0 sky #(kind ready/safe-if-lit (assoc open-night :time time))))
    15 st/night :later
    5 st/noon :ok
    5 st/night :ok))

(deftest a-job-that-goes-to-its-place-binds-safe-at-that-place
  (are [check args] (= :later (lit 0 15 #(first (check (assoc (ctx-of open-night) :args args)))))
    smelt/safe? {:furnace outside}
    kitchen/safe? {:furnace {:x 40 :y 64 :z 0}}
    pick-up-loose/safe? {}))

(deftest a-short-visit-in-the-open-night-goes-ahead-priced-by-its-exposure
  (are [check args] (= :ok (lit 0 15 #(first (check (assoc (ctx-of open-night) :args args)))))
    deposit/safe? {:chest outside}
    withdraw/safe? {:chest outside}
    set-spawn/safe? {:bed [15 64 0]}))

(deftest a-smelt-puts-its-own-furnace-down-instead-of-walking-to-a-chosen-dark-one
  (let [world (assoc night-hut :inventory [{:name "cobblestone" :count 8}])
        chosen {:furnace outside}
        verdict (fn [world mem] (with-redefs [ctx/mem (constantly mem)] (first (smelt/safe? (ctx-of world)))))]
    (is (= :ok (lit 0 15 #(verdict world chosen))) "an unloaded choice 10 blocks out in the dark is dropped")
    (is (= :later (lit 0 15 #(verdict world (assoc chosen :owed {:count 4})))) "a load in it still calls for the walk")
    (is (= :later (lit 0 15 #(verdict (assoc night-hut :inventory []) chosen))) "no cobblestone to make one: the walk stays")))

;; plain safe-at counts darkness by day too (it broke set-spawn); the debug probe wraps it, so it is not caught
(deftest no-job-binds-plain-safe-at
  (is (empty? (for [[j d] registry/jobs :when (::ready/plain (meta (:safe? d)))] j))))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})
(def wall (tu/box 2 64 -2 2 66 2 "stone"))

(deftest threatened-is-later-for-a-reachable-zombie-and-ok-behind-a-wall
  (are [world expected] (= expected (kind ready/threatened world))
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]} :later
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie] :blocks (merge wall (tu/box 2 67 -2 4 67 2 "stone") (tu/box 3 64 -2 4 66 -2 "stone") (tu/box 3 64 2 4 66 2 "stone") (tu/box 4 64 -2 4 66 2 "stone"))} :ok
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}} :ok))

(deftest a-look-reads-without-moving-and-does-not-wait-for-a-zombie
  (is (= :ok (first (look/safe? (ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))))

(deftest a-look-around-only-turns-and-does-not-wait-for-a-zombie
  (is (= :ok (first (look-around/safe? (ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))))

(deftest pick-up-loose-waits-while-a-zombie-is-near
  (is (= :later (first (pick-up-loose/safe? (ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))))

(def skeleton {:id 8 :name "skeleton" :kind "hostile" :pos {:x 12 :y 64 :z 0}})
(def at-origin {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}})

(def sword [{:name "iron_sword" :count 1}])

(deftest fit-armed-waits-while-unarmed-or-hurt
  (are [world expected] (= expected (let [s (ready/fit-armed (ctx-of (merge at-origin world)))] [(first s) (:reason (nth s 2))]))
    {:inventory sword} [:ok :healthy]
    {:inventory []} [:later :unarmed]
    {:inventory sword :self {:pos {:x 0 :y 64 :z 0} :health 3}} [:later :low-health]))

(defn zombies-at [& xs] (vec (map-indexed (fn [i x] {:id (+ 20 i) :name "zombie" :kind "hostile" :pos {:x x :y 64 :z 0}}) xs)))
(def iron-worn (into {} (for [[part item] {"head" "iron_helmet" "torso" "iron_chestplate" "legs" "iron_leggings" "feet" "iron_boots"}] [part {:name item :count 1}])))

(deftest outmatched-waits-while-the-hostiles-known-past-the-trigger-radius-outfight-the-kit
  (are [world expected] (= expected (let [s (ready/outmatched (ctx-of (merge at-origin world)))] [(first s) (:reason (nth s 2))]))
    {:inventory [{:name "wooden_sword" :count 1}] :entities (zombies-at 10 11)} [:later :outmatched]
    {:inventory sword :equipment iron-worn :entities (zombies-at 10 11)} [:ok :matched]
    {:inventory [{:name "wooden_sword" :count 1}] :entities (zombies-at 10)} [:ok :matched]
    {:inventory [{:name "wooden_sword" :count 1}] :entities (zombies-at 20 21)} [:ok :matched])
  (is (= :ok (first (ready/threatened (ctx-of (merge at-origin {:entities (zombies-at 10 11)}))))) "past the hostile-near radius"))

(deftest the-long-dig-jobs-wait-while-outmatched
  (are [safe] (= [:later :ok] (for [kit [{:inventory [{:name "wooden_sword" :count 1}]} {:inventory sword :equipment iron-worn}]]
                                (first (safe (ctx-of (merge at-origin kit {:entities (zombies-at 10 11)}))))))
    mine/safe? dig-for/safe? strip-tunnel/safe?))

(def far-husk {:id 9 :name "husk" :kind "hostile" :pos {:x 30 :y 64 :z 0}})

(def walled-in-husk
  (let [box (fn [x0 x1] (merge (tu/box x0 63 -2 x1 63 2 "stone") (tu/box x0 67 -2 x1 67 2 "stone") (tu/box x0 64 -2 x1 66 -2 "stone")
                              (tu/box x0 64 2 x1 66 2 "stone") (tu/box x0 64 -2 x0 66 2 "stone") (tu/box x1 64 -2 x1 66 2 "stone")))]
    {:entities [far-husk] :blocks (box 28 32)}))

(deftest an-unarmed-body-waits-while-a-hostile-is-known-within-the-dawn-radius
  (are [world expected] (= expected (first (ready/unarmed-threatened (ctx-of (merge at-origin world)))))
    {:inventory [] :entities [far-husk]} :later
    (assoc walled-in-husk :inventory []) :ok
    {:inventory sword :entities [far-husk]} :ok
    {:inventory []} :ok))

(deftest get-kit-binds-the-unarmed-hostile-wait
  (is (= :later (first (get-kit/safe? (ctx-of (merge at-origin {:inventory [] :entities [far-husk]})))))))

(deftest get-kit-makes-its-sword-while-a-hostile-is-known
  (are [items inv expected] (= expected (first (get-kit/safe? (assoc (ctx-of (merge at-origin {:inventory inv :entities [far-husk]})) :args {:items items}))))
    ["iron_pickaxe"] [{:name "oak_planks" :count 3} {:name "stick" :count 2}] :ok
    ["iron_pickaxe"] [{:name "cobblestone" :count 2}] :ok
    ["iron_pickaxe"] [{:name "dirt" :count 2}] :later
    ["wooden_pickaxe"] [{:name "oak_planks" :count 3} {:name "stick" :count 2}] :later))

(deftest a-stair-up-is-not-held-for-health-a-stair-down-is
  (are [dir expected] (= expected (first (stair/fit? (assoc (ctx-of (assoc at-origin :self {:pos {:x 0 :y 64 :z 0} :health 3})) :args {:dir dir}))))
    :up :ok
    :down :later))

(deftest a-job-whose-checks-default-to-armed-and-healthy-runs-them-from-the-declared-forms
  (are [world expected] (= expected (first (ending/run-status (get registry/jobs 'jobs.access.cleanup) (ctx-of (merge at-origin world)))))
    {:inventory sword} :ok
    {:inventory []} :later))

(defn with-entry
  "ctx-of world, its engine holding a hostile-near register entry with args."
  [world args]
  (assoc-in (ctx-of world) [:engine :state] (atom {:register [{:id :hostile-near :trigger :hostile-near :args args}]})))

(deftest threatened-reads-the-hostile-near-entrys-args-and-the-triggers-options
  (are [world args expected] (= expected (first (ready/threatened (with-entry (merge at-origin world) args))))
    {:entities [zombie]} {} :later
    {:entities [zombie]} {:radius 2} :ok
    {:entities [(assoc skeleton :pos {:x 0 :y 64 :z 20})]} {} :ok
    {:entities [skeleton]} {} :later
    {:entities [skeleton]} {:ranged-radius 5} :ok))

(deftest threatened-keeps-the-triggers-visible-only-option
  (let [far (assoc zombie :pos {:x 5 :y 64 :z 0})
        behind (tu/box 2 64 0 3 66 0 "stone")]
    (are [args expected] (= expected (first (ready/threatened (with-entry (merge at-origin {:entities [far] :blocks behind}) args))))
      {} :ok
      {:visible-only false} :later)))

(deftest threatened-is-read-once-per-window
  (let [c (ctx-of (merge at-origin {:entities [zombie]}))
        asked (atom 0)]
    (with-redefs [danger/hostile-near? (fn [& _] (swap! asked inc) true)]
      (ready/threatened c)
      (ready/threatened c))
    (is (= 1 @asked))))

(deftest a-walk-held-by-a-near-hostile-goes-on-only-when-it-leads-away
  (let [walk (ready/safe-at (fn [_] nil) true)]
    (are [goal expected] (= expected (first (ready/threatened-walking (ctx-of (merge at-origin {:entities [zombie]})) goal)))
      {:x -8 :y 64 :z 0} :ok
      {:x 8 :y 64 :z 0} :later
      {:x 4 :y 64 :z 2} :later
      {:x 0 :y 64 :z 6} :later
      {:x -4 :y 64 :z 6} :ok
      nil :later)
    (is (= :later (first (walk (ctx-of (merge at-origin {:time st/noon :entities [zombie]}))))) "no goal: the plain threat")))

(deftest a-hostile-at-the-heels-is-no-walk-away
  (let [close (assoc zombie :pos {:x 2 :y 64 :z 0})
        [kind-of text] (ready/threatened-walking (ctx-of (merge at-origin {:entities [close]})) {:x -8 :y 64 :z 0})]
    (is (= :later kind-of))
    (is (re-find #"zombie seen, 2 blocks off" text) "the refusal names the hostile")))

(deftest safe-at-adds-the-threat-to-the-exposure
  (is (= :later (kind (ready/safe-at (constantly {:x 1 :y 64 :z 0}))
                      {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))

(deftest fit-is-later-at-5-hp
  (are [hp expected] (= expected (kind ready/fit? {:self {:pos {:x 0 :y 64 :z 0} :health hp}}))
    5 :later
    20 :ok))

(deftest ok-is-always-ok
  (is (= :ok (kind ready/ok open-night))))

(deftest an-exposure-over-the-limit-is-judged-by-what-lowers-it
  (are [block sky world expected] (= expected (with-redefs [sh-sense/block-light-at (fn [_ c] (if (= outside (select-keys c [:x :y :z])) block 0))
                                                            sh-sense/sky-light-at (constantly sky)]
                                              (kind (ready/safe-at (constantly outside)) world)))
    8 15 open-night :ok
    0 15 open-night :later))


(deftest a-roofed-dark-cell-is-exposed-in-a-dark-hall-and-free-in-a-small-dark-place
  (are [hall? expected] (= expected (with-redefs [sh-sense/block-light-at (constantly 0)
                                                  sh-sense/sky-light-at (constantly 0)
                                                  sh-inside/dark-hall-at? (constantly hall?)]
                                      ((juxt first #(:reason (nth % 2)))
                                       (ready/at-status (ctx-of (assoc open-night :time st/noon)) outside))))
    true [:never :exposed]
    false [:ok :sealed]))

(deftest a-way-out-of-a-dark-hall-underground-is-no-exposure-but-other-work-there-is
  (with-redefs [ready/hall-status (fn [_ _] [:never "hall" {:reason :exposed}])]
    (are [check expected] (= expected (kind check (assoc tunnel :time st/noon)))
      ready/safe-at-from-out :ok
      ready/safe-at-from :never)))

(deftest an-unseen-cell-asks-for-a-look-at-it
  (let [s ((ready/safe-at (constantly outside)) (ctx-of open-night))]
    (is (= :look (first s)))
    (is (= [[15 64 0]] (:look-at (nth s 2))))))

(deftest an-unseen-cell-a-look-round-could-not-show-is-judged-where-a-buried-body-stands
  (are [looked expected] (= expected (with-redefs [sh-sense/seen-cell? (fn [_ c] (not= outside (select-keys c [:x :y :z])))
                                                   sense/round-looked (constantly looked)]
                                       (kind (ready/safe-at (constantly outside)) (assoc tunnel :time st/noon))))
    #{} :look
    #{[15 64 0]} :ok))

(deftest an-unseen-cell-well-below-the-feet-under-a-seen-roof-is-underground-and-asks-no-look-or-dawn
  (are [to expected] (= expected (with-redefs [sh-sense/seen-cell? (constantly false)
                                               sh-sense/solid-at? (fn [_ c] (= 66 (:y c)))]
                                   (kind (ready/safe-at (constantly to)) (assoc open-night :floor [-70 -70 70 70]))))
    {:x 15 :y 40 :z 0} :ok
    {:x 15 :y 62 :z 0} :ok
    {:x 15 :y 63 :z 0} :look
    {:x 15 :y 64 :z 0} :look))

(deftest an-unseen-cell-below-the-feet-with-no-seen-roof-is-a-valley-and-asks-a-look
  (are [y] (= :look (with-redefs [sh-sense/seen-cell? (constantly false)
                                  sh-sense/solid-at? (constantly false)]
                      (kind (ready/safe-at (constantly {:x 15 :y y :z 0})) open-night)))
    62 40))

(def escapes
  '[respond-to-hostile block-arrow-gap unwedge breathe extinguish night dig-pocket shut-in])

(def healers '[bed-down sleep go-home])

(deftest escapes-bind-ok-and-the-night-walkers-bind-threatened
  (doseq [j escapes :let [d (get registry/jobs (symbol (str "jobs.survival." j)))]]
    (is (= [ready/ok ready/ok] ((juxt :safe? :fit?) d)) (str j)))
  (doseq [j healers :let [d (get registry/jobs (symbol (str "jobs.survival." j)))]]
    (is (= [ready/threatened ready/ok] ((juxt :safe? :fit?) d)) (str j))))

(defn bound
  "The kind of a job's checker over world with args."
  [checker world args]
  (first (checker (assoc (ctx-of world) :args args))))

(deftest sweep-c-jobs-bind-the-checker-their-audit-row-names
  (are [checker world args expected] (= expected (lit 0 15 #(bound checker world args)))
    search/safe? open-night {} :ok
    stay-near/safe? open-night {:player "Steve"} :ok
    boat-drive/safe? open-night {:pos [15 64 0]} :later
    boat-drive/safe? (assoc open-night :time st/noon) {:pos [15 64 0]} :ok
    stay-near/safe? open-night {:pos [15 64 0]} :later
    mount/safe? open-night {} :ok
    leave-vehicle/safe? open-night {} :ok
    boat-recover/safe? open-night {} :ok
    search/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} {} :later
    stay-near/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} {:player "Steve"} :later
    mount/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} {} :later))

(deftest a-hurt-body-that-cannot-heal-may-search-for-food-only
  (let [hurt {:pos {:x 0 :y 64 :z 0} :health 5}]
    (are [self inventory target expected] (= expected (bound search/fit? {:self (assoc hurt :food self) :inventory inventory} {:target target}))
      8 [] ["cow" "carrots"] :ok
      8 [{:name "rotten_flesh" :count 3}] ["cow"] :ok
      8 [{:name "bread" :count 1}] ["cow"] :later
      20 [] ["cow"] :later
      8 [] ["cow" "iron_ore"] :later)))

(deftest reserved-food-does-not-let-a-hurt-body-heal
  (let [view (fn [entries] (let [d (reduce #(mem/add-entry %1 :reserve {:t 0 :data %2} reserve/policy) mem/empty-data entries)]
                             (fn [] {:data d :now 1000})))
        carrots [{:name "carrot" :count 3}]]
    (are [food entries expected]
         (= expected (ready/cannot-heal? (assoc (ctx-of {:self {:pos {:x 0 :y 64 :z 0} :health 5 :food food} :inventory carrots})
                                                :view (view entries))))
      12 [{:item "carrot"}] true
      12 [] false
      2 [{:item "carrot"}] false
      20 [{:item "carrot"}] false)))

(deftest a-hurt-foodless-body-does-not-roam-for-wool-or-wood
  (are [health food inventory args expected]
       (= expected (bound search/fit? {:self {:pos {:x 0 :y 64 :z 0} :health health :food food} :inventory inventory} args))
    8.2 5 [] {:target ["sheep"] :forage false} :later
    8.2 5 [] {:target ["oak_log"]} :later
    8.2 5 [] {:target ["sheep"]} :ok
    8.2 5 [{:name "bread" :count 1}] {:target ["oak_log"]} :ok
    8.2 20 [] {:target ["oak_log"]} :ok
    12 5 [] {:target ["oak_log"]} :ok))

(deftest a-starving-body-works-its-field-in-the-dark-and-a-fed-one-waits
  (let [safe? (:safe? (get registry/jobs 'jobs.farm.harvest))]
    (are [food health inventory expected]
         (= expected (lit 0 15 #(kind safe? (assoc open-night :self {:pos {:x 0 :y 64 :z 0} :food food :health health} :inventory inventory))))
      6 3.8 [] :ok
      6 3.8 [{:name "bread" :count 1}] :later
      20 3.8 [] :later
      6 15 [] :later
      0 15 [] :ok)))

(deftest a-hurt-body-that-cannot-heal-fishes-and-walks-for-food-over-its-health-budget
  (let [hurt {:pos {:x 0 :y 64 :z 0} :health 5}
        walk {:pos {:x 3 :y 64 :z 0}}]
    (are [food inventory expected-fit expected-walk]
         (let [c (assoc (ctx-of {:self (assoc hurt :food food) :inventory inventory}) :args {})]
           (= [expected-fit expected-walk] [(first (fish/fit? c)) (ready/forage-walk c walk)]))
      8 [] :ok (assoc walk :floor {:health 0})
      8 [{:name "bread" :count 1}] :later walk
      20 [] :later walk)))

(deftest a-hurt-body-waits-to-start-the-access-and-boat-jobs
  (doseq [j '[jobs.access.tunnel jobs.access.stair jobs.access.pillar jobs.access.clear-path jobs.access.bridge jobs.blocks.clear-cells jobs.build.path
              jobs.movement.boat-launch jobs.movement.boat-recover jobs.movement.mount]]
    (is (= :later (kind (:fit? (get registry/jobs j)) {:self {:pos {:x 0 :y 64 :z 0} :health 5}})) (str j))))

(deftest a-hurt-body-afloat-still-lands-and-drives
  (doseq [j '[jobs.movement.boat-land jobs.movement.boat-drive jobs.access.toggle]]
    (is (= :ok (kind (:fit? (get registry/jobs j)) {:self {:pos {:x 0 :y 64 :z 0} :health 5}})) (str j))))

(deftest the-field-jobs-work-at-low-health-where-risky-jobs-wait
  (doseq [j '[jobs.farm.fertilize jobs.farm.plant jobs.farm.harvest jobs.farm.till jobs.farm.tend jobs.farm.compost jobs.farm.tidy jobs.farm.maintain
             jobs.forestry.plant-sapling jobs.forestry.prepare]]
    (is (= :ok (kind (:fit? (get registry/jobs j)) {:self {:pos {:x 0 :y 64 :z 0} :health 2}})) (str j))))

(deftest safe-at-by-night-waits-for-an-unseen-place-only-at-night
  (are [world expected] (= expected (kind (ready/safe-at-by-night (constantly outside)) world))
    (assoc open-night :time st/noon) :ok
    night-hut :later))

(deftest safe-at-by-night-judges-the-goal-of-a-buried-body
  (are [goal expected] (= expected (kind (ready/safe-at-by-night (constantly goal)) tunnel))
    {:x 4 :y 64 :z 0} :ok
    nil :ok
    outside :later))

(deftest safe-at-by-night-takes-an-unseen-goal-no-higher-than-a-buried-body-as-underground-when-roofed
  (are [goal roofed? expected] (= expected (with-redefs [sh-sense/seen-cell? (constantly false)
                                                        ready/roof-over? (constantly roofed?)]
                                             (kind (ready/safe-at-by-night (constantly goal)) tunnel)))
    {:x 40 :y 64 :z 0} true :ok
    {:x 40 :y 50 :z 0} true :ok
    {:x 40 :y 70 :z 0} true :later
    {:x 40 :y 64 :z 0} false :later))

(deftest safe-at-by-night-dug-takes-an-unseen-goal-no-higher-than-a-buried-body-as-underground-with-no-seen-roof
  (are [goal expected] (= expected (with-redefs [sh-sense/seen-cell? (constantly false)
                                                ready/roof-over? (constantly false)]
                                     (kind (ready/safe-at-by-night (constantly goal) false true) tunnel)))
    {:x 40 :y 64 :z 0} :ok
    {:x 40 :y 70 :z 0} :later))

(defn kind-in-shelter [check world shelter]
  (let [c (assoc (ctx-of world) :view (fn [] {:now 0 :data {:entries {:shelter [{:t 0 :data shelter}]}}}))]
    (first (check c))))

(def dug-in-entry {:pos {:x 0 :y 64 :z 0}})

(deftest safe-at-by-night-waits-for-day-for-a-place-outside-the-night-shelter-at-any-distance
  (are [shelter goal expected] (= expected (kind-in-shelter (ready/safe-at-by-night (constantly goal)) tunnel shelter))
    dug-in-entry {:x 8 :y 64 :z 0} :later
    dug-in-entry {:x 20 :y 64 :z 0} :later
    dug-in-entry {:x 4 :y 64 :z 0} :ok)
  (is (= :later (lit 15 0 #(kind-in-shelter (ready/safe-at-by-night (constantly {:x 20 :y 64 :z 0})) tunnel dug-in-entry)))
      "a lit place too: the night keeps the seal"))

(deftest felling-a-tree-outside-the-night-shelter-waits-for-day-instead-of-walking-out
  (let [check (fn [at] (let [c (assoc (ctx-of tunnel) :args {:at at} :view (fn [] {:now 0 :data {:entries {:shelter [{:t 0 :data dug-in-entry}]}}}))]
                         (first (fell-tree/safe? c))))]
    (is (= :later (check {:x 20 :y 64 :z 0})))
    (is (= :ok (check {:x 4 :y 64 :z 0})))))

(deftest explore-search-underground-at-night-is-ok
  (is (= :ok (bound search/safe? tunnel {}))))

(deftest the-rest-of-survival-binds-its-checkers
  (let [by-day-hostile {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}]
    (are [check world expected] (= expected (kind check world))
      restore-broken/safe? open-night :later
      restore-broken/safe? (assoc open-night :time st/noon) :ok
      restore-broken/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} :later
      get-food/safe? by-day-hostile :ok
      get-food/safe? open-night :ok
      recover-drops/safe? by-day-hostile :ok
      recover-drops/safe? open-night :ok
      get-food/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} :ok
      shut/safe? by-day-hostile :later
      shut/safe? open-night :ok)))

(deftest jobs-with-nothing-to-check-are-ok-at-night-with-a-hostile-and-hurt
  (let [world (assoc open-night :entities [zombie] :self {:pos {:x 0 :y 64 :z 0} :health 5})]
    (doseq [check [log-out/safe? log-out/fit? eat/safe? eat/fit? noop/safe? noop/fit? wait-for-light/safe? wait-for-light/fit?
                   shut/fit?]]
      (is (= :ok (kind check world))))))

(deftest the-work-jobs-bind-their-place-by-night
  (are [world job args expected] (= expected (lit 0 15 #(first ((:safe? job) (assoc (ctx-of world) :args args)))))
    open-night {:safe? place/safe?} {:pos [40 64 0]} :later
    open-night {:safe? place/safe?} {:pos [15 64 0]} :ok
    open-night {:safe? place/safe?} {:pos [1 64 0]} :ok
    night-hut {:safe? place/safe?} {:pos [6 64 0]} :ok
    open-night {:safe? light-area/safe?} {:cells [[40 64 0]]} :later
    open-night {:safe? light-area/safe?} {:cells [[15 64 0]]} :ok
    night-hut {:safe? light-area/safe?} {:cells [[6 64 0]]} :ok
    open-night {:safe? fill-cells/safe?} {:cells [[40 64 0]]} :later
    open-night {:safe? fill-cells/safe?} {:cells [[15 64 0]]} :ok
    open-night {:safe? toggle/safe?} {:pos [15 64 0]} :ok
    open-night {:safe? toggle/safe?} {:pos [40 64 0]} :later
    open-night {:safe? dig/safe?} {:pos [15 64 0]} :ok
    open-night {:safe? dig/safe?} {:pos [40 64 0]} :later
    night-hut {:safe? fill-cells/safe?} {:cells [[6 64 0]]} :ok
    open-night {:safe? stair/safe?} {:dir :up :submitted-at {:x 0 :y 64 :z 0}} :later
    open-night {:safe? stair/safe?} {:dir :down :submitted-at {:x 0 :y 64 :z 0}} :ok
    night-hut {:safe? stair/safe?} {} :ok))

(deftest collect-drops-at-an-open-death-pile-does-not-wait-for-light
  (let [died (fn [x] (-> mem/empty-data (mem/add-entry :died {:t 0 :data {:pos {:x x :y 64 :z 0}}} nil)))
        status (fn [data near]
                 (lit 0 15 #(first (collect-drops/safe? (-> (ctx-of open-night) (assoc :args {:near near :search 4}
                                                                                              :view (fn [] {:now 1000 :data data})))))))]
    (is (= :ok (status (died 40) {:x 38 :y 64 :z 0})) "the pile lies in the dark 40 blocks out")
    (is (= :ok (status (died 40) [38 64 0])) "a vector :near too")
    (is (= :later (status mem/empty-data {:x 38 :y 64 :z 0})) "no death there: a far walk in the dark waits")
    (is (= :later (status (died 80) {:x 38 :y 64 :z 0})) "a death elsewhere is not this spot's pile")))

(defn jobs-under
  "The registry's jobs in the namespaces under the prefixes, but find-spot and grow (which roam)."
  [& prefixes]
  (for [j (sort (keys registry/jobs))
        :when (some #(.startsWith (str j) %) prefixes)
        :when (not (#{'jobs.farm.find-spot 'jobs.farm.grow} j))]
    j))

(def work-jobs (jobs-under "jobs.farm." "jobs.build."))

(defn safe-of [job] (:safe? (get registry/jobs job)))

(def plans
  {"far" {:id "far" :cells [{:pos [15 64 0] :part "p"}]}
   "near" {:id "near" :cells [{:pos [6 64 0] :part "p"}]}
   "pen-far" {:id "pen-far" :cells [] :plan {:parts [{:id "p" :box [[14 64 -1] [16 64 1]] :herd {:mob "cow"}}]}}
   "pen-near" {:id "pen-near" :cells [] :plan {:parts [{:id "p" :box [[5 64 -1] [7 64 1]] :herd {:mob "cow"}}]}}})

(def box-at (fn [x] {:min {:x (dec x) :y 64 :z -1} :max {:x (inc x) :y 64 :z 1}}))
(def corners-at (fn [x] {:min {:x (dec x) :y 64 :z -1} :max {:x (inc x) :y 64 :z 1}}))

(deftest work-jobs-bind-the-place-their-args-name-by-night
  (with-redefs [known/plan (fn [_ id] (get plans id))
                strip-site/site (constantly {:at [15 64 0]})]
    (are [job args world expected] (= expected (lit 0 15 #(bound (safe-of job) world args)))
      'jobs.animals.cull {:box (box-at 15)} night-hut :later
      'jobs.animals.cull {:box (box-at 5)} night-hut :ok
      'jobs.animals.cull {:center {:x 15 :z 0}} night-hut :later
      'jobs.animals.maintain {:box (box-at 15)} night-hut :later
      'jobs.animals.maintain {:box (box-at 5)} night-hut :ok
      'jobs.animals.maintain {:plan "pen-far" :part "p"} night-hut :later
      'jobs.animals.maintain {:plan "pen-near" :part "p"} night-hut :ok
      'jobs.animals.pen-check {:box (box-at 15)} night-hut :later
      'jobs.animals.pen-check {:plan "far"} night-hut :later
      'jobs.animals.pen-check {:plan "near"} night-hut :ok
      'jobs.animals.pen-check {:at [15 64 0]} night-hut :later
      'jobs.animals.pen-check {:at [6 64 0] :box (box-at 15)} night-hut :ok
      'jobs.apiary.guard {:box (corners-at 15)} night-hut :later
      'jobs.apiary.maintain {:box (corners-at 5)} night-hut :ok
      'jobs.apiary.harvest {:center [15 64 0]} night-hut :later
      'jobs.strip.site {:site "t" :base "b"} night-hut :later
      'jobs.strip.all {:site "t" :base "b"} night-hut :later
      'jobs.strip.base {:base "far"} night-hut :later
      'jobs.strip.smelt {:base "near"} night-hut :ok
      'jobs.strip.haul {:base "far"} night-hut :later
      'jobs.food.fish {:pos [15 64 0]} night-hut :later
      'jobs.food.fish {:pos [6 64 0]} night-hut :ok)))

(deftest farm-and-build-jobs-bind-the-place-of-their-args
  (doseq [job (remove '#{jobs.build.light-area jobs.build.from-plan jobs.farm.irrigate} work-jobs)]
    (are [args world expected] (= expected (lit 0 15 #(bound (safe-of job) world args)))
      {:at [40 64 0]} night-hut :later
      {:at [6 64 0]} night-hut :ok)))

(deftest irrigate-binds-its-field-once-a-field-cell-is-seen
  (let [seen (fn [x] (assoc-in night-hut [:blocks (str x ",64,0")] "dirt"))
        at (fn [x] {:area {:pos {:x x :y 64 :z 0} :radius 0}})]
    (are [world args expected] (= expected (lit 0 15 #(bound (safe-of 'jobs.farm.irrigate) world args)))
      (seen 15) (at 15) :later
      (seen 6) (at 6) :ok
      night-hut (at 15) :ok)))

(deftest roaming-jobs-wait-for-dawn-above-ground
  (are [job args] (= :later (lit 0 15 #(bound (safe-of job) open-night args)))
    'jobs.food.fish {}))

(deftest a-find-spot-scan-needs-no-light
  (is (= :ok (lit 0 15 #(bound (safe-of 'jobs.farm.find-spot) night-hut {})))))

(deftest jobs-that-go-to-a-place-keep-its-exposure-for-exposed-checks
  (doseq [job (concat work-jobs (jobs-under "jobs.animals." "jobs.apiary." "jobs.village." "jobs.strip." "jobs.food.")
                      '[jobs.farm.find-spot jobs.farm.grow])]
    (is (contains? (meta (safe-of job)) ::ready/exposure) (str job))))

(deftest near-unseen-here-judges-a-hidden-cell-within-reach-where-the-body-stands
  (are [goal expected] (= expected (with-redefs [sh-sense/seen-cell? (constantly false)]
                                     (kind (ready/safe-at-by-night (ready/near-unseen-here (constantly goal))) tunnel)))
    {:x 4 :y 68 :z 0} :ok
    {:x 40 :y 70 :z 0} :later))

(deftest the-near-unseen-reach-is-a-setting
  (is (= :later (with-redefs [sh-sense/seen-cell? (constantly false)]
                  (settings/with-settings {:jobs.lib.ready/unseen-judged-here-within 2}
                    #(kind (ready/safe-at-by-night (ready/near-unseen-here (constantly {:x 4 :y 68 :z 0}))) tunnel))))))

(deftest an-attack-walk-to-an-animal-spends-hp-only-while-the-body-cannot-heal
  (let [hurt {:pos {:x 0 :y 64 :z 0} :health 5}]
    (are [food targets expected]
         (= expected (select-keys (attack/walk-opts (assoc (ctx-of {:self (assoc hurt :food food) :inventory []}) :args {:targets targets}) #js {:id 1 :name "cow" :pos #js {:x 3 :y 64 :z 0}}) [:floor :leg-s]))
      8 ["cow"] {:floor {:health 0} :leg-s 30}
      20 ["cow"] {:leg-s 5}
      8 :hostiles {:leg-s 10})))

(deftest a-dark-build-place-within-the-no-spawn-radius-is-no-wait-for-light
  (are [to expected] (= expected (lit 0 15 #(kind (ready/safe-at-build (constantly to)) open-night)))
    {:x 15 :y 64 :z 0} :ok
    {:x 40 :y 64 :z 0} :later))

(deftest a-light-block-around-the-body-is-placed-in-the-dark
  (are [item pos expected] (= expected (lit 0 15 #(bound place/safe? open-night {:item item :pos pos})))
    "torch" [15 64 0] :ok
    "lantern" [15 64 0] :ok
    "cobblestone" [40 64 0] :later
    "torch" [40 64 0] :later))

(deftest from-plan-does-not-wait-for-light-at-a-site-in-the-no-spawn-radius
  (are [args expected] (= expected (lit 0 15 #(bound (safe-of 'jobs.build.from-plan) open-night args)))
    {:at [15 64 0]} :ok
    {:at [40 64 0]} :later))

(deftest cleanup-takes-scaffold-back-from-a-dark-cell-near-the-body
  (is (= :ok (lit 0 15 #(bound (safe-of 'jobs.access.cleanup) open-night {})))))

(deftest a-dark-closed-room-is-no-wait-by-day-or-night-in-or-out
  (are [t self] (= :ok (lit 0 0 #(kind (ready/safe-at (constantly {:x 5 :y 64 :z 0}))
                                       (-> (hut/hut-world self {}) (dissoc :light) (assoc :time t)))))
    st/noon {:x 5 :y 64 :z 5}
    st/noon {:x 5 :y 64 :z 0}
    st/night {:x 5 :y 64 :z 5}
    st/night {:x 5 :y 64 :z 0}))

(deftest plant-sapling-prices-the-debt-spot-not-the-feet
  (let [debt {:pos outside :species "oak"}]
    (with-redefs [plant-sapling/current-target (constantly debt)]
      (is (= :later (kind plant-sapling/safe? night-hut)) "sealed in at night, the debt outside in the dark waits"))
    (with-redefs [plant-sapling/current-target (constantly {:pos {:x 6 :y 64 :z 0} :species "oak"})]
      (is (= :ok (kind plant-sapling/safe? night-hut)) "a debt inside the hut is planted"))))
