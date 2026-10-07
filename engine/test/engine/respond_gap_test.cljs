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

(def wide-ground
  "Ground to flee over: a retreat runs until it is out of line."
  (g/blocks "dirt" [-40 40] [60 63] [-40 40]))

(defn ^:async tick-or-hold!
  "One tick; :held when it has not returned after a few seconds (a body hiding sealed in holds while a danger stays), the
  job then cancelled."
  [eng]
  (let [timer (atom nil)
        held (js/Promise. (fn [resolve] (reset! timer (js/setTimeout #(resolve :held) 4000))))
        r (await (js/Promise.race [(core/tick! eng) held]))]
    (js/clearTimeout @timer)
    (when (= :held r) (core/cancel! eng "j1"))
    r))

(defn ^:async respond!
  "Run respond-to-hostile for a few rounds over the gap test's doorway cell with the given spec; stops at a hold."
  [spec]
  (let [{:keys [eng] :as s} (g/setup (:zones spec []) (merge {:entities [g/pit-skeleton] :blocks (merge wide-ground g/pit g/shell) :act-ms 1000} (dissoc spec :zones)))]
    (core/submit! eng '(jobs.survival.respond-to-hostile) {})
    (loop [i 0]
      (when (and (< i 6) (not= :held (await (tick-or-hold! eng))))
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

(def canopy
  "A roof over the body with open sides all round: an overhang in open ground."
  {"0,66,0" "oak_planks"})

(deftest a-roof-with-open-sides-is-no-cover
  (is (not (wanted? {:inventory [cobble8 pickaxe] :blocks (merge g/ground g/pit canopy)} false))))

(defn ^:async no-gap-run
  "Respond for a few rounds with the spec: it ends (no hang) and the gap job placed nothing."
  [spec]
  (let [s (await (respond! spec))]
    (is (not (placed? s)))
    s))

(deftest no-pickaxe-no-gap-placed
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [cobble8]}))))))

(deftest no-blocks-no-gap-placed
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [pickaxe]}))))))

(deftest a-melee-mob-as-well-no-gap-placed
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [cobble8 pickaxe] :entities [g/pit-skeleton zombie]}))))))

(deftest open-ground-no-gap-placed
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [cobble8 pickaxe] :blocks (merge wide-ground g/pit canopy)}))))))

(deftest a-gap-that-fails-refused-falls-back-to-the-usual-response
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (no-gap-run {:inventory [cobble8 pickaxe]
                                                 :zones [{:name "keep" :owner "Miles" :min [-4 60 -4] :max [4 70 4]}]}))]
          (is (= :refused (:reason (g/failed seen))) "the gap job ran and placed nothing"))))))
