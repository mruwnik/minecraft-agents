(ns engine.obtain-explore-test
  "items.obtain's long looks: logs in a foreign zone or cut off from the body, ore seen from far. Split from engine.obtain-iron-test, whose ray casts over a wide sight made one namespace outlast the test runner's limit."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fetch-test :as f]
            [engine.obtain-iron-test :refer [ore-world obtain! mine-events]]
            [engine.test-util :as tu]))

(def village-trunk
  "A trunk at x 3 inside a foreign zone."
  {:blocks (into {} (for [y [64 65 66]] [(str "3," y ",4") "oak_log"]))
   :zones [f/own-zone {:name "village" :min [2 60 3] :max [4 70 5] :owner "Miles"}]})

(deftest logs-seen-only-in-a-foreign-zone-are-explored-past-and-then-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world (:blocks village-trunk) []) (:zones village-trunk))]
          (await (obtain! s {:any-of ["oak_log" "birch_log"] :count 2 :ways #{:gather} :explore true} 12))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for logs it may take")
          (await (f/run-ticks s 600))
          (is (re-find #"seen but not taken.*zone" (str (:why (first (f/events-of s :stopped))))) "when exploring fails the stop names the refusal"))))))

(deftest a-tree-it-may-take-is-preferred-to-logs-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [free (into {"30,68,4" "oak_leaves" "31,67,4" "oak_leaves"} (for [y [64 65 66 67]] [(str "30," y ",4") "oak_log"]))
              s (f/start (ore-world (merge (:blocks village-trunk) free) []) (:zones village-trunk))]
          (await (obtain! s {:any-of ["oak_log"] :count 2 :ways #{:gather} :explore true} 400))
          (is (<= 2 (get (f/inv s) "oak_log" 0)) "logs were got from the tree outside the zone"))))))

(deftest a-tree-cut-off-from-the-body-is-explored-past-with-explore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [free (into {"30,68,4" "oak_leaves" "31,67,4" "oak_leaves"} (for [y [64 65 66 67]] [(str "30," y ",4") "oak_log"]))
              s (f/start (assoc (ore-world (merge f/tree-blocks free) []) :unreachable ["0,64,4" "0,65,4" "0,66,4" "0,67,4"]) [f/own-zone])]
          (await (obtain! s {:any-of ["oak_log"] :count 2 :ways #{:gather} :explore true} 600))
          (is (<= 2 (get (f/inv s) "oak_log" 0)) "logs were got from the reachable tree"))))))

(deftest raw-iron-seen-38-blocks-away-sends-the-mine-not-a-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world (into {} (for [x [0 1 2] y [63 64 65]] [(str x "," y ",38") "iron_ore"])) [{:name "stone_pickaxe" :count 1}]) [] (assoc f/sight-opts :radius 48 :ray-deg 4))]
          (await (obtain! s {:item "raw_iron" :ways #{:craft :smelt :gather}} 200))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))))
          (is (empty? (f/events-of s :obtain.exploring)))
          (is (seq (mine-events s)) "the mine went for the ore"))))))

(deftest logs-only-on-a-bedrock-pillar-are-explored-for-before-obtain-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pillar (into {} (concat (for [y (range 63 67)] [(str "0," y ",4") "bedrock"])
                                      (for [y (range 67 73)] [(str "0," y ",4") "oak_log"])))
              s (f/start (ore-world pillar []) [f/own-zone])]
          (await (obtain! s {:item "oak_log" :count 2 :explore true} 60))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for other logs")
          (is (empty? (filter #(= :no-tree (:reason %)) (f/events-of s :stopped))) "not stopped no-tree before that"))))))
