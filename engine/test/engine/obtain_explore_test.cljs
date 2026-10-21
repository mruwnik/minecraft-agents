(ns engine.obtain-explore-test
  "items.obtain's long looks: logs in a foreign zone or cut off from the body, ore seen from far. Split from engine.obtain-iron-test, whose ray casts over a wide sight made one namespace outlast the test runner's limit."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fetch-test :as f]
            [engine.obtain-iron-test :refer [ore-world obtain!]]
            [engine.fake :as fake]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.items.obtain.gather :as gather]
            [jobs.items.obtain.seen :as seen]
            [jobs.lib.access :as access]
            [jobs.lib.look :as look]))

(def village-trunk
  "A trunk at x 3 inside a foreign zone."
  {:blocks (into {} (for [y [64 65 66]] [(str "3," y ",4") "oak_log"]))
   :zones [f/own-zone {:name "village" :min [2 60 3] :max [4 70 5] :owner "Miles"}]})

(def small-explore
  {:jobs.items.obtain.base/explore {:max-distance 16 :max-legs 2 :spend {:job-time 60}}
   :jobs.items.obtain.base/explore-far {:max-distance 32 :max-legs 2 :spend {:job-time 60}}
   :jobs.items.obtain.base/explore-rounds 2})

(defn ^:async obtain-small! [s args n]
  (await (settings/with-settings small-explore #(obtain! s args n))))

(defn free-trees
  "A tree 30 blocks off on each side: the search starts the way its legs sort first, which the test must not depend on."
  []
  (into {"30,68,4" "oak_leaves" "31,67,4" "oak_leaves" "-30,68,4" "oak_leaves" "-31,67,4" "oak_leaves"}
        (for [x [30 -30] y [64 65 66 67]] [(str x "," y ",4") "oak_log"])))

(deftest logs-seen-only-in-a-foreign-zone-are-explored-past-and-then-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world (:blocks village-trunk) []) (:zones village-trunk))]
          (await (obtain-small! s {:any-of ["oak_log" "birch_log"] :count 2 :ways #{:gather} :explore true} 12))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for logs it may take")
          (await (settings/with-settings small-explore #(f/run-ticks s 600)))
          (is (re-find #"seen but not taken.*zone" (str (:why (first (f/events-of s :stopped))))) "when exploring fails the stop names the refusal"))))))

(deftest a-tree-it-may-take-is-preferred-to-logs-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [free (free-trees)
              s (f/start (ore-world (merge (:blocks village-trunk) free) []) (:zones village-trunk))]
          (await (obtain! s {:any-of ["oak_log"] :count 2 :ways #{:gather} :explore true} 400))
          (is (<= 2 (get (f/inv s) "oak_log" 0)) "logs were got from the tree outside the zone"))))))

(def arity1 "access/rules-input is called through its 1-arity property" "cljs$core$IFn$_invoke$arity$1")

(deftest the-seen-log-to-take-is-found-beyond-any-number-of-nearer-barred-logs
  (let [barred (for [i (range 3000)] {:name "oak_log" :pos {:x i :y 64 :z 0}})
        free {:name "oak_log" :pos {:x 150 :y 64 :z 0}}
        real (aget access/rules-input arity1)]
    (aset access/rules-input arity1 (constantly {}))
    (try
      (with-redefs [look/seen-blocks (fn [_ {:keys [max]}] (vec (take max (concat barred [free]))))
                    access/may? (fn [_ _ pos] {:ok (= 150 (:x pos))})
                    seen/cut-off? (constantly false)]
        (is (= free (first (seen/seen-blocks-of {:primitives nil} {:seen #{"oak_log"}} false)))
            "every nearer log is barred, the free one lies past them"))
      (finally (aset access/rules-input arity1 real)))))

(deftest a-tree-cut-off-from-the-body-is-explored-past-with-explore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [free (free-trees)
              s (f/start (assoc (ore-world (merge f/tree-blocks free) []) :unreachable ["0,64,4" "0,65,4" "0,66,4" "0,67,4"]) [f/own-zone])]
          (await (obtain! s {:any-of ["oak_log"] :count 2 :ways #{:gather} :explore true} 600))
          (is (<= 2 (get (f/inv s) "oak_log" 0)) "logs were got from the reachable tree"))))))

(deftest raw-iron-seen-38-blocks-away-sends-the-mine-not-a-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world (into (into {} (for [x (range -2 5) z (range 0 38)] [(str x ",63," z) "stone"])) (for [x [0 1 2] y [63 64 65]] [(str x "," y ",38") "iron_ore"])) [{:name "stone_pickaxe" :count 1}]) [] (assoc f/sight-opts :radius 48 :ray-deg 4))]
          (await (obtain! s {:item "raw_iron" :ways #{:craft :smelt :gather}} 200))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))))
          (is (empty? (f/events-of s :obtain.exploring)))
          (is (<= 1 (get (f/inv s) "raw_iron" 0)) "the mine went for the ore and got it"))))))

(deftest logs-only-on-a-bedrock-pillar-are-explored-for-before-obtain-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pillar (into {} (concat (for [y (range 63 67)] [(str "0," y ",4") "bedrock"])
                                      (for [y (range 67 73)] [(str "0," y ",4") "oak_log"])))
              s (f/start (ore-world pillar []) [f/own-zone])]
          (await (obtain-small! s {:item "oak_log" :count 2 :explore true} 60))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for other logs")
          (is (empty? (filter #(= :no-tree (:reason %)) (f/events-of s :stopped))) "not stopped no-tree before that"))))))

(def sealed-cave
  "Stone to y 70 over x -22..48, z -22..22, with a hollow 5x2x5 inside it where the body stands: no way out but through rock. A tree stands on the stone 40 blocks off."
  (merge (into {} (for [x (range -22 49) z (range -22 23) y (range 63 71)
                         :when (not (and (<= -2 x 2) (<= -2 z 2) (<= 64 y 65)))]
                    [(str x "," y "," z) (if (= y 63) "andesite" "stone")]))
         (into {} (for [y [71 72 73 74]] [(str "24," y ",0") "oak_log"]))
         {"24,75,0" "oak_leaves"}))

(deftest a-body-sealed-in-a-cave-stairs-up-to-the-sky-when-its-search-finds-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world sealed-cave []) [f/own-zone])]
          (swap! (fake/state (:p s)) assoc :light (into {} (for [x (range -20 21) z (range -20 21) y (range 64 71)] [[x y z] [0 0]])))
          (await (obtain! s {:item "oak_log" :count 1 :explore true} 900))
          (is (seq (f/events-of s :stair.step)) "a stair was cut up through the roof")
          (is (<= 1 (get (f/inv s) "oak_log" 0)) "and the tree outside was felled"))))))

(deftest far-searches-run-on-and-then-turn-so-eight-take-eight-ways
  (is (= :north (gather/far-heading [0 64 0] [3 64 2] 1)) "still at the start: north")
  (is (= :east (gather/far-heading [0 64 0] [60 64 -2] 1)) "away from the start")
  (is (= :south-west (gather/far-heading [0 64 0] [-50 64 50] 1)))
  (is (= 8 (count (set (map #(gather/far-heading [0 64 0] [0 64 -40] %) (range 1 9))))) "eight searches, eight headings"))

(deftest a-search-that-finds-nothing-is-followed-by-one-outward-along-a-heading-until-the-rounds-are-spent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} []) [f/own-zone])]
          (await (obtain-small! s {:any-of ["oak_log"] :count 1 :ways #{:gather} :explore true} 200))
          (is (= [nil :north] (mapv :heading (f/events-of s :obtain.exploring))) "rings first, then outward")
          (is (= :no-source (:reason (first (f/events-of s :stopped))))))))))

(deftest planks-of-any-species-search-for-any-log
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} []) [f/own-zone])]
          (await (obtain-small! s {:any-of ["oak_planks" "spruce_planks" "birch_planks"] :count 2 :ways #{:gather :craft} :explore true} 200))
          (is (seq (f/events-of s :obtain.exploring)))
          (is (every? #(< 1 (count (:for %))) (f/events-of s :obtain.exploring)) "searches for logs of every species, not oak alone"))))))

(deftest searches-that-keep-running-out-of-time-still-end-the-gather
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} []) [f/own-zone])
              short {:jobs.items.obtain.base/explore {:max-distance 16 :max-legs 2 :spend {:job-time 0.01}}
                     :jobs.items.obtain.base/explore-far {:max-distance 32 :max-legs 2 :spend {:job-time 0.01}}
                     :jobs.items.obtain.base/explore-rounds 2}]
          (await (settings/with-settings short #(obtain! s {:any-of ["oak_log"] :count 1 :ways #{:gather} :explore true} 400)))
          (is (<= 2 (count (f/events-of s :obtain.exploring)) 10) "a bounded number of searches")
          (is (= :no-source (:reason (first (f/events-of s :stopped))))))))))

(deftest water-out-of-sight-is-explored-for-and-the-bucket-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (update (f/bare [{:name "bucket" :count 1}]) :blocks merge {"30,63,4" "water"}) [f/own-zone])]
          (await (obtain! s {:item "water_bucket" :explore true} 600))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for water")
          (is (= 1 (get (f/inv s) "water_bucket" 0))))))))
