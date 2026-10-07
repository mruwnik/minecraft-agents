(ns engine.respond-gap-test
  "respond-to-hostile calls block-arrow-gap for a ranged mob with a line of fire when the body holds cover."
  (:require [cljs.test :refer [deftest is async]]
            [engine.block-arrow-gap-test :as g]
            [engine.core :as core]
            [engine.test-util :as tu]
            [jobs.lib.reach :as reach]
            [jobs.survival.respond-to-hostile :as respond]))

(def pickaxe {:name "iron_pickaxe" :count 1})
(def cobble8 {:name "cobblestone" :count 8})
(def zombie {:id 5 :name "zombie" :kind "hostile" :visible true :pos {:x 0.5 :y 64 :z 6.5}})

(defn ^:async respond!
  "Run respond-to-hostile for a few rounds over the gap test's doorway cell with the given spec."
  [spec]
  (let [{:keys [eng] :as s} (g/setup [] (merge {:entities [g/pit-skeleton] :blocks (merge g/ground g/pit g/shell)} spec))]
    (core/submit! eng '(jobs.survival.respond-to-hostile) {})
    (loop [i 0]
      (when (< i 6)
        (await (core/tick! eng))
        (recur (inc i))))
    s))

(defn placed? [{:keys [seen]}] (contains? (g/kinds-seen seen) :block-arrow-gap.closed))

(deftest cover-blocks-and-a-pickaxe-stop-the-arrows-by-placing-not-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (respond! {:inventory [cobble8 pickaxe]}))]
          (is (placed? s))
          (is (= 7 (g/cobble p)) "one block ended the line of fire")
          (is (empty? (g/shot-at p))))))))

(defn wanted?
  "gap-wanted? for the body of a doorway cell setup with the spec, the dangers it sees."
  [spec tried?]
  (let [{:keys [p]} (g/setup [] (merge {:entities [g/pit-skeleton] :blocks (merge g/ground g/pit g/shell)} spec))
        hs (reach/dangers p 16 {:ranged-radius 16} {})]
    (respond/gap-wanted? {:primitives p} hs tried?)))

(deftest the-gap-is-wanted-only-with-cover-blocks-a-digging-tool-and-ranged-mobs-alone
  (is (wanted? {:inventory [cobble8 pickaxe]} false))
  (is (not (wanted? {:inventory [cobble8]} false)) "no digging tool: a fill could shut the body in")
  (is (not (wanted? {:inventory [pickaxe]} false)) "no blocks")
  (is (not (wanted? {:inventory [cobble8 pickaxe]} true)) "tried this call")
  (is (not (wanted? {:inventory [cobble8 pickaxe] :entities [g/pit-skeleton zombie]} false)) "a melee mob is near")
  (is (not (wanted? {:inventory [cobble8 pickaxe] :blocks g/ground
                     :entities [(assoc g/skeleton :pos {:x 0.5 :y 64 :z 8.5})]} false))
      "open ground: no roof"))
