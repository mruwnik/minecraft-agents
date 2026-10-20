(ns engine.forestry-tidy-test
  "jobs.forestry.tidy against the fake world: it fells the part-cut trunks (natural, raised off the ground) it has seen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.forest-maintain-test :as fm]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]))

(def job 'jobs.forestry.tidy)

(def sapling {:name "oak_sapling" :count 2})

(defn part-cut
  "An oak of 4 logs at x whose lowest log was cut: three logs float over the air, leaves on top."
  [x]
  (apply dissoc (lt/tree x 0 "oak" 4) (map #(fm/k x % 0) [64])))

(defn ^:async tidy
  "Run tidy with args in a world of spec over the zones; the start map after 150 ticks."
  [spec zones args]
  (await (fm/with-zones zones
           (fn ^:async f []
             (let [s (fm/start (merge {:inventory [sapling]} spec) {})]
               (core/submit! (:eng s) (list job args) {})
               (await (fm/ticks (:eng s) 150))
               s)))))

(defn dug-ys [p x] (into #{} (comp (filter #(= x (first %))) (map second)) (fm/digs p)))

(deftest a-part-cut-trunk-is-felled-and-replanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (tidy {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))} [] {:radius 12}))]
          (is (= #{65 66 67} (dug-ys p 6)) "the three floating logs go")
          (is (some #(= [6 64 0 "oak_sapling"] %) (fm/places p)) "a sapling goes on the ground"))))))

(deftest a-whole-tree-standing-on-the-ground-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (tidy {:blocks (merge (fm/ground [[6 0]]) (lt/tree 6 0 "oak" 4))} [] {:radius 12}))]
          (is (= [] (fm/digs p))))))))

(deftest a-log-wall-in-a-build-is-never-touched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [x (range 4 9)] [(fm/k x 66 0) "oak_log"]))
              {:keys [p]} (await (tidy {:blocks wall} [] {:radius 12}))]
          (is (= [] (fm/digs p))))))))

(deftest a-trunk-in-another-bodys-zone-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "vault" :min [5 64 -1] :max [7 80 1] :allow #{:place}}]
              {:keys [p]} (await (tidy {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))} zones {:radius 12}))]
          (is (= [] (fm/digs p))))))))

(defn ^:async outcome
  "The result of tidy with args in a world of spec (child-outcome), with the world's calls: {:out :p}."
  [spec args]
  (let [{:keys [eng p]} (lt/setup spec)]
    {:out (await (tu/child-outcome eng job args 150)) :p p}))

(defn branched
  "A living tree at x 6: a trunk of 4 logs on the ground and one branch log at bx, by, connected to it, leaves on top."
  [bx by]
  (merge (lt/tree 6 0 "oak" 4) {(fm/k bx by 0) "oak_log" (fm/k bx (inc by) 0) "oak_leaves"}))

(deftest the-branch-of-a-living-acacia-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (outcome {:blocks (merge (fm/ground [[6 0] [7 0]]) (branched 7 67))} {:radius 12}))]
          (is (= [] (fm/digs p))))))))

(deftest the-side-branch-of-a-living-fancy-oak-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (outcome {:blocks (merge (fm/ground [[6 0] [7 0]]) (branched 7 66))} {:radius 12}))]
          (is (= [] (fm/digs p))))))))

(deftest nothing-to-tidy-is-done-with-none-tidied
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (outcome {:blocks (fm/ground [[6 0]])} {:radius 12}))]
          (is (= {:status :done :tidied 0} (select-keys out [:status :tidied]))))))))

(deftest the-logs-of-a-felled-trunk-are-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (outcome {:blocks (merge (fm/ground [[6 0]]) (part-cut 6)) :inventory [sapling]} {:radius 12}))]
          (is (= {:status :done :tidied 1} out))
          (is (<= 3 (get (lt/inv p) "oak_log" 0))))))))

(deftest max-trees-ends-the-run-after-that-many-trunks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (settings/with-settings {:jobs.forestry.tidy/max-trees 1} #(outcome {:blocks (merge (fm/ground [[6 0] [10 0]]) (part-cut 6) (part-cut 10))
                                               :inventory [sapling]}
                                              {:radius 12})))]
          (is (= {:status :done :tidied 1} out))
          (is (= 1 (count (filter seq [(dug-ys p 6) (dug-ys p 10)])))))))))

(deftest a-replant-that-cannot-be-done-is-owed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (outcome {:blocks (merge (fm/ground [[6 0]]) (part-cut 6))} {:radius 12 :ways #{:carried}}))]
          (is (= #{65 66 67} (dug-ys p 6)))
          (is (= {:status :done :tidied 1 :replant-owed 1} out)))))))

(deftest a-trunk-with-no-ground-seen-under-it-is-owed-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (outcome {:blocks (part-cut 6) :inventory [sapling]} {:radius 12}))]
          (is (= [] (fm/places p)))
          (is (= {:status :done :tidied 1 :replant-owed 1} out)))))))

(deftest a-hungry-body-with-no-food-ends-food-short-and-skips-no-trunk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (merge (fm/ground [[6 0]]) (part-cut 6)) :self {:food 2}})
              _ (mem/write! (:store eng) :hungry {:food 2} {:cap 10 :ttl 3600000})
              out (await (tu/child-outcome eng job {:radius 12} 150))]
          (is (= [:stopped :food-short :condition-failed] [(:status out) (:reason out) (:kind (:ending out))]))
          (is (= [] (fm/digs p))))))))

(deftest a-light-block-under-a-raised-log-is-not-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (outcome {:blocks (merge (fm/ground [[6 0]]) (part-cut 6) {"6,64,0" "light"})
                                               :inventory [sapling]}
                                              {:radius 12}))]
          (is (= #{65 66 67} (dug-ys p 6)))
          (is (= :done (:status out))))))))
