(ns engine.tidy-test
  "jobs.forestry.tidy against the fake world: it fells the part-cut trunks (natural, raised off the ground) it has seen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.forest-maintain-test :as fm]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
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
