(ns jobs.build.light-area
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.torch :as torch]
            [jobs.lib.torch-plan :as tp]
            [jobs.lib.util :as u]))

(def doc
  "Light a room, corridor or box so no cell a monster could spawn in stays dark: the fewest floor torches
  (jobs.lib.torch-plan, light spreads 13 cells through open cells). Area: :boxes (each two opposite corners) and/or :cells. The
  cells that matter are those the body has seen as spawn ground (sense/spawnable?: air, air, solid floor) that it perceives
  dark (jobs.lib.torch/perceived-dark?); torches already standing count through that light read, the ones this call hung
  count at once. An area cell not seen yet that touches a seen open cell is walked to first (once) to look. Each step hangs one torch with a jobs.blocks.place child (walks in reach; a missing torch is fetched
  by it), at the spot lighting most of the dark cells still owed. A spot whose place fails is not tried again.

  Needs: the area seen, torches (carried or fetchable). Ends done {:torches [cells hung this call] :uncovered []} when no
  spawnable cell of the area is dark (at once, with none hung, when none is); nothing-possible :uncovered when dark cells
  remain that no spot can light or whose torch could not be placed (data :uncovered, :torches); condition-failed :need
  when the torch is missing and cannot be had, :refused when a zone or claim refuses a spot. With :level those cells must reach that block
  light (not just 1), and with :beside? the torches hang round the area, not in it. Impossible :bad-args with no area.")

(a/defargs args
  {:cells {:doc "cells of the area, each [x y z] or {:x :y :z}" :spec (a/coll-of a/position?) :default []}
   :boxes {:doc "boxes of the area, each two opposite corners [[x y z] [x y z]]; their cells come first, x then y then z" :spec (a/coll-of (a/coll-of a/position?)) :default []}
   :near {:doc "also the cells within this many blocks of the body (its square round the feet, one below to two above), read where the body stands now" :spec (a/int-in 0 16) :default 0}
   :level {:doc "the block light every area cell must reach, 1 to 14 (1: no spawn ground stays dark; a crop needs 9); a torch lights 14 less its steps" :spec (a/int-in 1 14) :default 1}
   :beside? {:doc "hang torches only in the cells round the area, not in it (a field keeps its cells for crops)" :spec boolean? :default false}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def target [:cells :boxes])
(def defaults {:spend {:job-time ::job-time-s}})

(defn box-cells [box]
  (let [[lo hi] (map (comp (juxt :x :y :z) a/cell) box)
        span (fn [i] (range (min (lo i) (hi i)) (inc (max (lo i) (hi i)))))]
    (for [x (span 0) y (span 1) z (span 2)] [x y z])))

(defn near-cells
  "The cells [x y z] within :near blocks of the body, none with :near 0."
  [c]
  (let [n (:near (:args c))
        {:keys [x y z]} (look/cell-of (u/self-pos c))]
    (when (pos? n)
      (for [dx (range (- n) (inc n)) dz (range (- n) (inc n)) dy [-1 0 1 2]] [(+ x dx) (+ y dy) (+ z dz)]))))

(defn area
  "The area's cells as [x y z]: the boxes', then :cells, then the cells :near the body, no repeats."
  [c]
  (let [{:keys [cells boxes]} (:args c)]
    (vec (distinct (concat (mapcat box-cells boxes) (map (comp (juxt :x :y :z) a/cell) cells) (near-cells c))))))

(defn bad-args [c]
  (cond
    (not-every? #(= 2 (count %)) (:boxes (:args c))) (contract/never :bad-args "light-area :boxes take two corners each")
    (empty? (area c)) (contract/never :bad-args "light-area needs :cells, :boxes or :near")
    :else (contract/ok :args "an area to light")))

(def can-start? bad-args)

(defn ring-cells
  "The cells [x y z] touching the area's cells sideways, not in the area."
  [c]
  (let [in? (set (area c))]
    (vec (distinct (for [[x y z] in? dx [-1 0 1] dz [-1 0 1]
                         :let [cell [(+ x dx) y (+ z dz)]]
                         :when (not (in? cell))]
                     cell)))))

(defn spawn-cells
  "The cells of the area the body has seen as spawn ground."
  [c]
  (filterv #(sh-sense/spawnable? (:primitives c) (zipmap [:x :y :z] %)) (area c)))

(defn lighting
  "{:spots [cell ..] :uncovered [cell ..]} (jobs.lib.torch-plan/plan) for what the area still owes: spots nearest the body
  first, those a place failed at left out."
  [c]
  (let [p (:primitives c)
        block-at (access/seen-at p)
        here (u/self-pos c)
        {:keys [torches failed]} (ctx/mem c)
        {:keys [level beside?]} (:args c)
        cells (spawn-cells c)
        hangs (if beside? (filterv #(sh-sense/spawnable? p (zipmap [:x :y :z] %)) (ring-cells c)) cells)
        spots (->> hangs (remove (set failed))
                   (sort-by #(u/dist here (zipmap [:x :y :z] %))))]
    (tp/plan {:cells cells :spread (- 14 (or level 1))
              :dark? (if (> (or level 1) 1)
                       #(< (or (sh-sense/light-at p (zipmap [:x :y :z] %)) 0) level)
                       #(torch/perceived-dark? p %))
              :open? #(let [n (block-at %)] (and n (not (solid/solid? n))))
              :spots spots :sources torches})))

(defn half-seen?
  "An open cell whose floor or head cell the body has not seen yet: whether it is spawn ground is undecided."
  [p [x y z]]
  (let [open? (some-> (u/seen-name p {:x x :y y :z z}) solid/solid? not)]
    (boolean (and open? (some #(nil? (u/seen-name p {:x x :y (+ y %) :z z})) [-1 1])))))

(defn frontier
  "The nearest cell of the area not walked to yet whose light is unread (a seen spawnable cell), or that is unseen and touches
  a seen open cell, or is open with its floor or head unseen: where the room goes on out of sight. Cells inside rock never qualify."
  [c]
  (let [p (:primitives c)
        block-at (access/seen-at p)
        looked (set (:looked (ctx/mem c)))
        here (u/self-pos c)
        open? #(let [n (block-at %)] (and n (not (solid/solid? n))))]
    (->> (area c)
         (filter #(and (not (looked %))
                       (or (half-seen? p %)
                           (and (nil? (block-at %)) (some (fn [d] (open? (mapv + % d))) tp/neighbours))
                           (and (sh-sense/spawnable? p (zipmap [:x :y :z] %))
                                (nil? (sh-sense/light-at p (zipmap [:x :y :z] %)))))))
         (sort-by #(u/dist here (zipmap [:x :y :z] %)))
         first)))

(defn ^:async look-at!
  "Walk next to the unseen cell (a dark cell shows from close by) and look round there, once: :again, or :continue while the walk waits."
  [c cell]
  (let [r (await (near/go-near! c (zipmap [:x :y :z] cell) 1 {}))]
    (if (= :partial r)
      :continue
      (do (await (look/look-around! c))
          (ctx/update-mem! c update :looked (fnil conj []) cell)
          :again))))

(defn ^:async step! [c]
  (let [{:keys [spots uncovered]} (lighting c)
        spot (first spots)
        unseen (frontier c)]
    (cond
      unseen (await (look-at! c unseen))
      (and (empty? spots) (empty? uncovered)) (ctx/end c :done nil {:torches (:torches (ctx/mem c) []) :uncovered []})
      (nil? spot) (ctx/end c :nothing-possible {:reason :uncovered} {:torches (:torches (ctx/mem c) []) :uncovered uncovered})
      :else
      (let [o (b/place-outcome (waits/waiting-out c (await (ctx/call c :place 'jobs.blocks.place
                                                (merge (select-keys (:args c) [:ignore-zones?])
                                                       {:pos spot :item "torch"})))))]
        (case o
          :continue :continue
          (:placed :already) (do (ctx/update-mem! c update :torches (fnil conj []) spot) :again)
          :need (ctx/end c :condition-failed {:reason :need :item "torch"}
                         {:torches (:torches (ctx/mem c) []) :uncovered (:uncovered (lighting c))})
          :refused (do (ctx/update-mem! c update :failed (fnil conj []) spot)
                       (if (empty? (:spots (lighting c)))
                         (ctx/end c :condition-failed {:reason :refused :cell spot}
                                  {:torches (:torches (ctx/mem c) []) :uncovered (:uncovered (lighting c))})
                         :again))
          (do (ctx/update-mem! c update :failed (fnil conj []) spot) :again))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))

(def safe? (ready/safe-at-build (fn [c] (some->> (area c) first (zipmap [:x :y :z])))))

(def fit? ready/ok)

(defn done?
  "Ok when no spawnable cell of the area is dark and none is left to look at; else later with the torches hung."
  [c]
  (let [{:keys [spots uncovered]} (lighting c)
        hung (count (:torches (ctx/mem c)))]
    (if (and (empty? spots) (empty? uncovered) (nil? (frontier c)))
      (contract/ok :lit "no spawnable cell of the area is dark" :units :torches :done hung
                   :result {:torches (:torches (ctx/mem c) []) :uncovered []})
      (contract/later :dark (str (count spots) " torches to hang") :units :torches :done hung))))

(defn quote-of [c]
  (let [n (count (:spots (lighting c)))]
    {:uses {:job-time [0 (* 8 n) (* 20 (max 1 n))] :items {"torch" [0 n n]}}}))
