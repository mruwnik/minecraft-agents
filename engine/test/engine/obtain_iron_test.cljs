(ns engine.obtain-iron-test
  "Iron as a source of items.obtain: raw ores mined from their seen ore blocks, smelt products made by a smelt child."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.fetch-test :as f]
            [engine.game :as game]
            [engine.test-util :as tu]
            [jobs.items.obtain :as obtain]
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
  (let [pl (recipes/plan game/default-version {} "iron_chestplate" 1 (assoc (obtain/source-opts) :gather? true))]
    (is (= 8 (get-in pl [:gather "raw_iron"])))
    (is (some #(and (= :smelt (:op %)) (= "iron_ingot" (:item %))) (:steps pl)))))

(deftest raw-iron-is-mined-from-a-seen-iron-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,4" "iron_ore" "2,64,4" "iron_ore"} [{:name "stone_pickaxe" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "raw_iron" :count 2} 80))
          (is (empty? (f/listed s)))
          (is (= 2 (get (f/inv s) "raw_iron"))))))))

(deftest raw-iron-is-mined-from-a-seen-deepslate-iron-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,4" "deepslate_iron_ore"} [{:name "stone_pickaxe" :count 1}]) [f/own-zone])]
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
                         [f/own-zone])]
          (await (obtain! s {:item "raw_iron"} 240))
          (is (empty? (f/listed s)))
          (is (= 1 (get (f/inv s) "stone_pickaxe")))
          (is (= 1 (get (f/inv s) "raw_iron"))))))))

(deftest raw-iron-with-no-ore-seen-waits-no-source-naming-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} [{:name "stone_pickaxe" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "raw_iron" :how #{:craft :gather}} 6))
          (let [w (first (filter #(= :no-source (:reason %)) (f/events-of s :waiting)))]
            (is (some? w))
            (is (re-find #"raw_iron \(from deepslate_iron_ore/iron_ore\)" (str (:why w)))))
          (is (empty? (f/calls s "dig"))))))))

(deftest raw-iron-unseen-with-explore-goes-to-the-mine-not-a-surface-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} [{:name "stone_pickaxe" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "raw_iron" :how #{:craft :gather} :explore true} 4))
          (is (empty? (f/events-of s :obtain.exploring)) "no surface search")
          (is (seq (mine-events s)) "the mine child ran"))))))

(deftest cobblestone-unseen-with-explore-goes-to-the-mine-not-a-surface-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} [{:name "wooden_pickaxe" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "cobblestone" :count 3 :how #{:gather} :explore true} 6))
          (is (empty? (f/events-of s :obtain.exploring)) "no surface search")
          (is (seq (mine-events s)) "the mine child ran"))))))

(deftest a-badly-hurt-body-does-not-walk-off-to-look-for-an-unseen-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc-in (ore-world {} []) [:self :health] 0.5) [f/own-zone])]
          (await (obtain! s {:item "oak_log" :count 2 :how #{:gather} :explore true} 8))
          (is (empty? (f/events-of s :obtain.exploring)) "no search walk at 0.5 hp")
          (is (some #(= :low-health (:reason %)) (f/events-of s :waiting))))))))

(deftest an-iron-ingot-with-raw-iron-carried-is-smelted-by-a-smelt-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {"1,64,2" "furnace"} [{:name "raw_iron" :count 3} {:name "coal" :count 1}]) [f/own-zone])]
          (await (obtain! s {:item "iron_ingot" :count 3 :how #{:craft :gather}} 6))
          (is (seq (f/calls s "furnace")) "the furnace was used")
          (is (= [:cooking] (mapv :reason (filter #(= :cooking (:reason %)) (f/events-of s :waiting)))) "obtain waits while it cooks")
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
                         [f/own-zone])
              _ (swap! (fake/state (:p s)) assoc-in [:recipes "iron_pickaxe"] {:count 1 :needs {"iron_ingot" 3 "stick" 2} :table true})]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "iron_pickaxe" :minutes 1}) {})
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
          (await (obtain! s {:any-of ["coal" "charcoal"] :how #{:craft :gather}} 60))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))))
          (is (seq (f/calls s "dig")) "a log was felled for the charcoal"))))))

(deftest the-time-limit-counts-from-the-last-change-of-what-is-carried
  (is (= {:inv {"a" 1} :t 5} (obtain/progress nil {"a" 1} 5)))
  (is (= {:inv {"a" 1} :t 5} (obtain/progress {:inv {"a" 1} :t 5} {"a" 1} 9)))
  (is (= {:inv {"a" 2} :t 9} (obtain/progress {:inv {"a" 1} :t 5} {"a" 2} 9))))
