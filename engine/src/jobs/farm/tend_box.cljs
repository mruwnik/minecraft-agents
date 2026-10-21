(ns jobs.farm.tend-box
  "Box mode of jobs.farm.tend: the facts and census read from the box."
  (:require [jobs.lib.access :as access]
            [jobs.lib.gate :as gate]
            [jobs.lib.field-area :as fa]
            [jobs.lib.util :as u]
            [jobs.lib.crops :as crops]
            [jobs.farm.field :as fld]
            [jobs.farm.irrigate :as irrigate]
            [jobs.lib.farm :as farm]
            [jobs.farm.tend-ground :as gr]
            [engine.ctx :as ctx]))

(def seed-items crops/field-seeds)

(def crop-names (set (keys crops/ripe-age)))

;; ------------------------------------------------------------------ pure helpers

;; ------------------------------------------------------------------ facts

(defn ground-layer
  "[{:pos :name :above}] for the cells of the lowest layer of the box."
  [p {lo :min hi :max}]
  (vec (for [x (range (:x lo) (inc (:x hi))) z (range (:z lo) (inc (:z hi)))
             :let [pos {:x x :y (:y lo) :z z}]]
         {:pos pos :name (u/seen-name p pos) :above (u/seen-name p (update pos :y inc))})))

(defn unripe-in-box
  "The unripe crop cells of the box, read around its centre."
  [p box mid R]
  (->> (crops/seen-crops p (keys crops/ripe-age) (+ R (u/dist (u/pos-of (.-pos (.self p))) mid)) 4096)
       (filter #(some-> (:age %) (< (crops/ripe-age (:name %)))))
       (map :pos)
       (filter #(fa/contains-pos? box %))
       vec))

(defn box-permitted
  "The poss of the box that zones, claims and footprints do not refuse for action, quietly (the child warns when it declines)."
  [c action poss]
  (let [in (access/rules-input c)]
    (filterv #(not (gate/refused? (access/may? in action %))) poss)))

(defn refusers
  "Who refuses the hoe on the poss: {:zones :claims :plans :owners} of the refusals, nil when nothing is refused. It names
  the plans so a wait says when it is the body's own plan."
  [c poss]
  (let [in (access/rules-input c)
        vs (filter gate/refused? (map #(access/may? in :dig %) poss))]
    (when (seq vs)
      {:zones (vec (distinct (keep :zone vs))) :claims (vec (distinct (keep :claim vs)))
       :plans (vec (distinct (keep :plan vs))) :owners (vec (distinct (keep :owner vs)))})))

(defn ground-box
  "The box's ground layer as the box irrigate and its need work over."
  [{lo :min hi :max}]
  {:min lo :max (assoc hi :y (:y lo))})

(defn irrigate-need
  "How many ground cells of the box (a ground-box) are seen dry, 0 when no spot would wet any (irrigate would end :no-spot)."
  [c {lo :min hi :max}]
  (let [view (assoc c :args {:area {:min lo :max hi} :for-plan (fa/plan c)})
        dry (irrigate/dry-cells view)]
    (if (irrigate/spot view dry) (count dry) 0)))

(defn box-facts
  "What the decisions are made from for a box, read live."
  [c]
  (let [p (:primitives c)
        fertilize (:fertilize (:args c))
        box (fa/arg-box c)
        mid (fa/centre box)
        R (fa/cover-radius box mid)
        inventory (u/inventory p)
        me (u/self-pos c)
        tried (:till-tried (ctx/mem c) #{})
        ground (ground-layer p box)
        dry (if (:irrigate (:args c)) (irrigate-need c (ground-box box)) 0)
        raw-untilled (->> ground (filter gr/untilled?) (map :pos) (remove tried))
        bare (count (box-permitted c :sow (farm/bare-cells p box [])))]
    {:field (:area (:args c))
     :ripe (count (box-permitted c :harvest (filter #(fa/contains-pos? box %) (fld/ripe-crops p {:radius R :crops nil} mid []))))
     :dry dry
     :ground-box (ground-box box)
     :hoe (some? (farm/hoe-of p))
     :untilled (->> ground
                    (filter gr/untilled?)
                    (map :pos)
                    (remove tried)
                    (box-permitted c :dig)
                    (sort-by #(u/dist me %))
                    vec)
     :refused (- (count raw-untilled) (count (box-permitted c :dig raw-untilled)))
     :refusers (refusers c raw-untilled)
     :covered (count (filter gr/covered? ground))
     :bare bare
     :seeds (transduce (map #(get (farm/sowable c) % 0)) + 0 seed-items)
     :seed (some? (farm/pick-seed nil c))
     :meal (boolean (farm/has-meal? c))
     :bones (boolean (some #(= "bone" (:name %)) inventory))
     :unripe (if fertilize (count (unripe-in-box p box mid R)) 0)}))

(defn box-census
  "The live {:crops :bare :untilled} of the box."
  [c]
  (let [p (:primitives c)
        {lo :min hi :max :as box} (fa/arg-box c)
        above (inc (:y lo))]
    {:crops (count (for [x (range (:x lo) (inc (:x hi))) z (range (:z lo) (inc (:z hi)))
                         :when (crop-names (u/seen-name p {:x x :y above :z z}))]
                     1))
     :bare (count (farm/bare-cells p box []))
     :untilled (count (filter gr/untilled? (ground-layer p box)))}))
