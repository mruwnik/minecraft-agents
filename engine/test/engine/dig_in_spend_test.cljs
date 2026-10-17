(ns engine.dig-in-spend-test
  "dig-in keeps valuable blocks (logs, planks: jobs.lib.cost item-value) for tools: it digs a pit instead of walling with
  them, walls with them only when no pit can be dug, and says which blocks it spent."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.shelter-support :as st]
            [engine.test-util :as tu :refer [run-until-empty]]))

(def pick {:name "iron_pickaxe" :count 1})
(def logs {:name "oak_log" :count 12})
(def roofed (assoc st/floor "0,66,0" "stone"))

(defn placed [p] (frequencies (map #(.-item (.-args %)) (st/calls p "place"))))

(defn ^:async run-dig! [world & [args]]
  (let [{:keys [eng p seen]} (st/setup (assoc world :time st/night))]
    (core/submit! eng (list 'jobs.survival.dig-in (or args {})) {})
    (await (run-until-empty eng 20))
    {:p p :seen seen}))

(deftest logs-are-not-walled-in-when-a-pit-can-be-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-dig! {:inventory [pick logs] :blocks st/floor}))]
          (is (nil? (get (placed p) "oak_log")) (pr-str (placed p)))
          (is (seq (st/calls p "dig"))))))))

(deftest cheap-blocks-short-of-the-walls-do-not-make-logs-the-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-dig! {:inventory [pick {:name "dirt" :count 4} logs] :blocks st/floor}))]
          (is (nil? (get (placed p) "oak_log")) (pr-str (placed p))))))))

(deftest dirt-walls-say-what-they-spent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (run-dig! {:inventory [{:name "dirt" :count 12} logs] :blocks st/floor}))
              ev (first (st/emitted seen :dig-in.sealed))]
          (is (nil? (get (placed p) "oak_log")))
          (is (= {"dirt" 10} (:spent ev)))
          (is (re-find #"10 dirt" (:text ev))))))))

(deftest logs-wall-the-body-in-when-no-pit-is-wanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (run-dig! {:inventory [{:name "oak_log" :count 20}] :blocks roofed} {:enclose true}))
              ev (first (st/emitted seen :dig-in.sealed))]
          (is (pos? (get (placed p) "oak_log" 0)))
          (is (= {"oak_log" (get (placed p) "oak_log")} (:spent ev)))
          (is (re-find #"oak_log" (:text ev))))))))

(deftest a-body-with-only-logs-and-no-tool-walls-in-at-once-when-the-pit-takes-longer-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-dig! {:inventory [logs] :blocks st/floor}))]
          (is (empty? (st/calls p "dig")))
          (is (pos? (get (placed p) "oak_log" 0)) (pr-str (placed p))))))))

(deftest the-night-handler-walls-in-with-logs-it-takes-back-when-no-tool-digs-slower
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time st/night :inventory [logs] :blocks st/floor})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 40))
          (is (empty? (st/calls p "dig")))
          (is (pos? (get (placed p) "oak_log" 0)) (pr-str (placed p))))))))
