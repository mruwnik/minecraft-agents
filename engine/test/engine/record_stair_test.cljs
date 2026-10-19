(ns engine.record-stair-test
  "jobs.access.record-stair: the run of stands through the body's cell, and the :stair-way entry it writes."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.access.record-stair :as rs]
            [jobs.lib.world-files :as world]))

(def job 'jobs.access.record-stair)

(defn stair-cells
  "The cells cut for a stair down east from the top stand [0 65 0], n steps: a stand, its head and the cell over it."
  [n]
  (for [i (range (inc n)) dy [0 1 2]] [i (+ (- 65 i) dy) 0]))

(defn block-at-of
  "Stone below y 65, air above, the cells named carved to air."
  [carved]
  (let [air (set carved)]
    (fn [[_ y _ :as cell]] (if (or (air cell) (>= y 65)) "air" "stone"))))

(deftest the-run-through-a-stand-is-found-both-ways-top-first
  (let [block-at (block-at-of (stair-cells 4))
        stands (fn [cell] (:stands (rs/best-run (rs/lines-through block-at cell [:east :south] 50) 2)))
        run [[0 65 0] [1 64 0] [2 63 0] [3 62 0] [4 61 0]]]
    (are [cell] (= run (stands cell))
      [0 65 0] [2 63 0] [4 61 0])))

(deftest a-hop-a-flat-floor-and-a-cut-without-headroom-are-no-stair
  (let [min-steps 2]
    (is (nil? (rs/best-run (rs/lines-through (block-at-of (stair-cells 1)) [0 65 0] [:east :south] 50) min-steps)) "one step")
    (is (nil? (rs/best-run (rs/lines-through (block-at-of []) [3 65 0] [:east :south] 50) min-steps)) "open ground")
    (is (nil? (rs/best-run (rs/lines-through (fn [[x y _ :as c]] (if (= [3 62 0] c) "stone" ((block-at-of (stair-cells 4)) c))) [3 62 0] [:east :south] 50) 2))
        "a stand whose head is stone")
    (is (nil? (rs/best-run (rs/lines-through (fn [c] (when-not (= [2 63 0] c) ((block-at-of (stair-cells 4)) c))) [0 65 0] [:east :south] 50) 3))
        "an unseen cell ends the run: two steps only")))

(def blocks
  (merge (into {} (for [x (range -3 9) y (range 55 65) z (range -2 3)] [(str x "," y "," z) "stone"]))
         (into {} (for [[x y z] (stair-cells 4)] [(str x "," y "," z) "air"]))))

(defn ^:async record!
  "Run the job over the stair world with the body at at and memory seeded with the :stair-way entries olds; the engine."
  [at args olds]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {:self {:pos {:x (first at) :y (second at) :z (nth at 2)}} :blocks blocks})
                          :jobs registry/jobs :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:sinks [sink] :now #(deref clock)})})]
    (doseq [floors olds] (mem/write! (:store eng) :stair-way {:dim "overworld" :floors floors}))
    (core/submit! eng (list job args) {})
    (loop [i 0]
      (when (and (< i 30) (not-any? #(#{:completed :stopped} (:kind %)) @seen))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    {:eng eng :end (first (filter #(#{:completed :stopped} (:kind %)) @seen))}))

(defn ways [{:keys [eng]}] (map (comp :floors :data) (mem/entries (mem/view (:store eng)) :stair-way)))

(deftest a-walked-stair-is-remembered-as-one-way-joined-with-the-single-floors-it-covers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (record! [2.5 63 0.5] {} [[[1 63 0]] [[9 9 9]]]))
              floors [[0 64 0] [1 63 0] [2 62 0] [3 61 0] [4 60 0]]]
          (is (= :completed (:kind (:end r))))
          (is (= {:steps 4 :top [0 65 0] :bottom [4 61 0] :floors 5} (select-keys (:data (:end r)) [:steps :top :bottom :floors])))
          (is (= #{floors [[9 9 9]]} (set (ways r))) "the single floor is taken in, the unrelated way kept"))))))

(deftest standing-off-a-stair-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (record! [6.5 65 0.5] {} []))]
          (is (= [:stopped :no-stair] [(:kind (:end r)) (:reason (:data (:end r)))]))
          (is (empty? (ways r))))))))
