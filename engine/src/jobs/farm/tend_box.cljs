(ns jobs.farm.tend-box
  "Box mode of jobs.farm.tend: the box's geometry, the sowing reserve, and the facts and census read from the box."
  (:require [jobs.lib.access :as access]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.crops :as crops]
            [jobs.farm.field :as fld]
            [jobs.farm.irrigate :as irrigate]
            [jobs.lib.farm :as farm]
            [jobs.lib.reserve :as rsv]
            [jobs.farm.tend-stock :as stock]
            [engine.ctx :as ctx]))

(def seed-items crops/field-seeds)

(def crop-names (set (keys crops/ripe-age)))

;; ------------------------------------------------------------------ pure helpers

(defn box-cells
  "How many cells the box covers."
  [{lo :min hi :max}]
  (apply * (map #(inc (- (% hi) (% lo))) [:x :y :z])))

(defn in-box? [{lo :min hi :max} {:keys [x y z]}]
  (and (<= (:x lo) x (:x hi)) (<= (:y lo) y (:y hi)) (<= (:z lo) z (:z hi))))

(defn centre
  "The box centre rounded to cells."
  [{lo :min hi :max}]
  (into {} (map (fn [k] [k (js/Math.round (/ (+ (k lo) (k hi)) 2))])) [:x :y :z]))

(defn radius
  "How far a sphere around mid must reach to cover the box: the farthest corner, rounded up, plus 1."
  [{lo :min hi :max} mid]
  (inc (js/Math.ceil (apply js/Math.max (for [x [(:x lo) (:x hi)] y [(:y lo) (:y hi)] z [(:z lo) (:z hi)]]
                                          (u/dist mid {:x x :y y :z z}))))))

(defn reserve
  "{seed count} to keep: twice the beds in all, taken one at a time from each carried seed that has more than is reserved already."
  [beds inventory]
  (let [have (storage/carried-totals inventory)]
    (loop [budget (* 2 beds) res {}]
      (let [open (filterv #(> (get have % 0) (get res % 0)) seed-items)]
        (if (or (zero? budget) (empty? open))
          res
          (let [[res budget] (reduce (fn [[r b] item]
                                       (if (zero? b)
                                         (reduced [r b])
                                         [(update r item (fnil inc 0)) (dec b)]))
                                     [res budget]
                                     open)]
            (recur budget res)))))))

(defn beds
  "How many ground cells are farmland or untilled."
  [ground]
  (count (filter #(or (= "farmland" (:name %)) (stock/untilled? %)) ground)))

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
       (filter #(in-box? box %))
       vec))

(defn box-permitted
  "The poss of the box that zones, claims and footprints do not refuse for action, quietly (the child warns when it declines)."
  [c action poss]
  (let [in (access/rules-input c)]
    (filterv #(not (gate/refused? (access/may? in action %))) poss)))

(defn ground-box
  "The box's ground layer as the box irrigate and its need work over."
  [{lo :min hi :max}]
  {:min lo :max (assoc hi :y (:y lo))})

(defn irrigate-need
  "How many ground cells of the box (a ground-box) are seen dry, 0 when no spot would wet any (irrigate would end :no-spot)."
  [c {lo :min hi :max}]
  (let [view (assoc c :args {:from lo :to hi :for-plan (:plan (:args c))})
        dry (irrigate/dry-cells view)]
    (if (irrigate/spot view dry) (count dry) 0)))

(defn box-facts
  "What the decisions are made from for a box, read live."
  [c]
  (let [p (:primitives c)
        {:keys [box fertilize keep]} (:args c)
        mid (centre box)
        R (radius box mid)
        inventory (u/inventory p)
        me (u/self-pos c)
        tried (:till-tried (ctx/mem c) #{})
        ground (ground-layer p box)
        dry (if (:irrigate (:args c)) (irrigate-need c (ground-box box)) 0)
        raw-untilled (->> ground (filter stock/untilled?) (map :pos) (remove tried))
        bare (count (box-permitted c :sow (farm/bare-cells p box [])))
        keep-of (stock/keeps (reserve (beds ground) inventory) (rsv/held c :tend) keep)]
    {:mid mid
     :radius R
     :ripe (count (box-permitted c :harvest (filter #(in-box? box %) (fld/ripe-crops p {:radius R :crops nil} mid []))))
     :dry dry
     :ground-box (ground-box box)
     :hoe (some? (farm/hoe-of p))
     :untilled (->> ground
                    (filter stock/untilled?)
                    (map :pos)
                    (remove tried)
                    (box-permitted c :dig)
                    (sort-by #(u/dist me %))
                    vec)
     :refused (- (count raw-untilled) (count (box-permitted c :dig raw-untilled)))
     :covered (count (filter stock/covered? ground))
     :bare bare
     :seeds (transduce (map #(get (farm/sowable c) % 0)) + 0 seed-items)
     :seed (some? (farm/pick-seed nil c))
     :meal (boolean (farm/has-meal? c))
     :bones (boolean (some #(= "bone" (:name %)) inventory))
     :unripe (if fertilize (count (unripe-in-box p box mid R)) 0)
     :keep keep-of
     :waste (stock/surplus inventory keep-of stock/waste-seeds)
     :stored (stock/surplus inventory keep-of stock/farm-goods)}))

(defn box-census
  "The live {:crops :bare :untilled} of the box."
  [c]
  (let [p (:primitives c)
        {lo :min hi :max :as box} (:box (:args c))
        above (inc (:y lo))]
    {:crops (count (for [x (range (:x lo) (inc (:x hi))) z (range (:z lo) (inc (:z hi)))
                         :when (crop-names (u/seen-name p {:x x :y above :z z}))]
                     1))
     :bare (count (farm/bare-cells p box []))
     :untilled (count (filter stock/untilled? (ground-layer p box)))}))
