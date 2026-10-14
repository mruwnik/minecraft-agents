(ns engine.forestry-contract-test
  "jobs.forestry.harvest-wood, forestry.tidy and gather.make-obsidian as contract jobs: they export the contract and end
  with a kind and a cause (0 of N is never done); a child's failed ending is read, not taken for success."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.forest-maintain-test :as fm]
            [engine.library-test :as lt]
            [engine.obtain-obsidian-test :as ot]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn kc [e] {:kind (:kind e) :cause (contract/cause-key e)})

(def jobs '[jobs.forestry.harvest-wood jobs.forestry.tidy jobs.gather.make-obsidian])

(deftest the-three-jobs-export-the-contract-and-quote-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [eng (core/create {:primitives (tu/fake {}) :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now (constantly 0)
                                :events (events/make {:body "Fake" :sinks [] :now (constantly 0)})})]
          (doseq [sym jobs
                  :let [job (get registry/jobs sym)]]
            (is (vector? (:conditions job)) (str sym " has conditions"))
            (is (nil? (:check job)) (str sym " has no old check"))
            (is (vector? (:target job)) (str sym " has a target"))
            (is (fn? (:done? job)) (str sym " has done?"))
            (is (nil? (contract/quote-problem (:quote (await (event-api/quote-view eng (str sym) "{:radius 10}")))))
                (str sym))))))))

;; ------------------------------------------------------------------ harvest-wood

(def harvest 'jobs.forestry.harvest-wood)
(def oak {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"})})

(defn ^:async harvest-end
  ([spec args n] (harvest-end spec args n nil))
  ([spec args n wrap-fell]
   (let [{:keys [eng] :as s} (lt/setup spec)
         eng (cond-> eng wrap-fell (assoc-in [:jobs 'jobs.forestry.fell-tree :round] (wrap-fell (get-in eng [:jobs 'jobs.forestry.fell-tree :round]))))]
     (assoc s :end (await (ending-of {:eng eng} harvest args n))))))

(deftest a-felled-tree-is-done-and-the-replant-owed-is-data
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (harvest-end oak {:species "oak" :radius 10} 40))]
          (is (= :done (:kind end)))
          (is (= {:replant-owed 1} (:data end))))
        (let [{:keys [end]} (await (harvest-end (assoc oak :inventory [{:name "oak_sapling" :count 1}]) {:species "oak" :radius 10} 40))]
          (is (= :done (:kind end)))
          (is (nil? (:data end))))))))

(deftest count-short-of-the-trees-is-condition-failed-no-tree-with-the-logs-got
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (harvest-end oak {:species "oak" :radius 10 :count 6} 80))]
          (is (= {:kind :condition-failed :cause :no-tree} (kc end)))
          (is (= 3 (get-in end [:data :got])))
          (is (= 1 (get-in end [:data :replant-owed]))))))))

(defn fail [kind cause] (fn [_real] (fn ^:async f [c] (ctx/end c kind cause {:columns [[3 0]] :why :walk}))))

(deftest a-fell-ending-that-is-not-done-or-no-tree-ends-the-job-with-that-kind-and-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end p]} (await (harvest-end oak {:species "oak" :radius 10} 40 (fail :nothing-possible :tree-unreachable)))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)) "not taken for a felled tree")
          (is (= [] (lt/calls p "dig")) "nothing collected or planted after it")
          (is (= [[3 0]] (get-in end [:data :columns]))))
        (let [{:keys [end]} (await (harvest-end oak {:species "oak" :radius 10} 40 (fail :condition-failed {:reason :need :any-of ["dirt"] :count 4})))]
          (is (= {:kind :condition-failed :cause :need} (kc end))))))))

(deftest nothing-collected-after-a-felling-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clear (fn [real] (fn ^:async f [c]
                                 (let [e (await (real c))]
                                   (when (and (map? e) (= :done (:kind e)))
                                     (doseq [x (fake/entities (:primitives c))]
                                       (swap! (fake/state (:primitives c)) update :entities (fn [es] (remove #(= (:id x) (:id %)) es)))))
                                   e)))
              {:keys [end]} (await (harvest-end oak {:species "oak" :radius 10} 40 clear))]
          (is (= {:kind :nothing-possible :cause :nothing-collected} (kc end))))))))

;; ------------------------------------------------------------------ tidy

(def tidy 'jobs.forestry.tidy)
(def sapling {:name "oak_sapling" :count 2})
(defn part-cut [x] (apply dissoc (lt/tree x 0 "oak" 4) (map #(fm/k x % 0) [64])))

(deftest tidy-ends-done-with-the-count-and-nothing-to-tidy-is-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6)) :inventory [sapling]})
              e (await (ending-of s tidy {:radius 12} 150))]
          (is (= :done (:kind e)))
          (is (= {:tidied 1} (:data e))))
        (let [s (lt/setup {:blocks (fm/ground [[6 0]])})
              e (await (ending-of s tidy {:radius 12} 150))]
          (is (= {:kind :condition-failed :cause :nothing-to-tidy} (kc e)))
          (is (nil? (:data e)) "a failed condition, not a round's ending"))))))

(deftest tidy-with-no-axe-and-no-fetch-ends-with-the-fell-ending-not-forever
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6)) :inventory [sapling]})
              eng (assoc-in eng [:jobs 'jobs.forestry.fell-tree :round]
                            (fn ^:async f [c] (ctx/end c :condition-failed {:reason :need :any-of ["dirt"] :count 4} nil)))
              e (await (ending-of {:eng eng} tidy {:radius 12} 60))]
          (is (= {:kind :condition-failed :cause :need} (kc e))))))))

;; ------------------------------------------------------------------ make-obsidian

(def obsidian 'jobs.gather.make-obsidian)

(defn ^:async obsidian-end [w args]
  (let [s (lt/setup w)]
    (assoc s :end (await (ending-of s obsidian args 200)))))

(deftest obsidian-made-is-done-and-the-missing-bucket-or-lava-are-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end p]} (await (obsidian-end (ot/world ot/lava-lake [ot/pick {:name "water_bucket" :count 1}]) {:count 1}))]
          (is (= :done (:kind end)))
          (is (= {:got 1} (:data end)))
          (is (= 1 (ot/carried p "obsidian"))))
        (let [{:keys [end]} (await (obsidian-end (ot/world ot/lava-lake [ot/pick]) {:count 1}))]
          (is (= {:kind :condition-failed :cause :no-bucket} (kc end))))
        (let [{:keys [end]} (await (obsidian-end (ot/world ot/ground [ot/pick {:name "water_bucket" :count 1}]) {:count 1}))]
          (is (= {:kind :condition-failed :cause :no-lava} (kc end))))))))
