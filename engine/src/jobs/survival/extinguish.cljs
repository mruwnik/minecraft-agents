(ns jobs.survival.extinguish
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.triggers.burning :as burning]))

(def doc
  "Put the body out: it is on fire or in lava. Each round, with a water bucket
  carried and the body on land, pours the water at the feet. Otherwise it makes
  one short walk: when in lava or no water is within :water-radius, to the
  best nearby cell (:step blocks around the body) that is passable, stands on
  something solid and is not fire, lava, magma or a campfire, scored by
  distance from those hazards (up to 4 blocks), a bonus for each block of
  height gained, and a small cost per block walked; when on fire with water
  within :water-radius, into the nearest water. On fire (not in lava) with no
  bucket use, no water within :water-radius and no fire, lava, magma or
  campfire within 1.5 blocks of the feet cell, there is nothing useful to do:
  it stands still, emits info :extinguish_wait and is done (the trigger fires
  it again after its cooldown while the body still burns). Lava seen within :scan-radius
  is written to memory as :hazard entries (cap 50, 6 hours) for retreat logic.
  If there is no safe cell, or the walk is blocked while still burning, that
  is a failed round; after three the job warns :extinguish_stuck once and
  gives up. Each round writes an :extinguish entry with position and cause (cap 20,
  1 hour). Done once the body is neither on fire nor in lava.")

(def args
  {:water-radius {:doc "on fire, water within this many blocks is walked into" :default 6}
   :step {:doc "candidate cells lie within this many blocks (horizontally) of the body" :default 4}
   :scan-radius {:doc "fire, lava and magma within this many blocks count as hazards" :default 8}})

(def hazards #{"lava" "fire" "soul_fire" "magma_block" "campfire" "soul_campfire"})
(def passable #{"air" "cave_air" "water" "short_grass" "tall_grass" "grass" "snow"})
(def unsafe-floor
  #{"air" "cave_air" "void_air" "water" "lava" "fire" "soul_fire" "magma_block" "campfire" "soul_campfire"
    "cactus" "sweet_berry_bush" "powder_snow"})

(def extinguish-policy {:cap 20 :ttl (* 60 60 1000)})
(def hazard-policy {:cap 50 :ttl (* 6 60 60 1000)})
(def max-hazards-per-round 16)
(def hazard-reach 4)
(def height-bonus 2)
(def walk-cost 0.5)
(def hazard-touch 1.5)

(defn check [c] (burning/burning? (.self (:primitives c))))

(defn floor-cell [pos] (into {} (map (fn [[k v]] [k (js/Math.floor v)])) pos))

(defn offset [pos dx dy dz] {:x (+ (:x pos) dx) :y (+ (:y pos) dy) :z (+ (:z pos) dz)})


(defn scan [p radius names max]
  (mapv (fn [b] {:name (.-name b) :pos (u/pos-of (.-pos b))})
        (array-seq (.blocks p #js {:radius radius :names (clj->js names) :max max}))))

(defn standable?
  "Feet cell and head cell passable, and the cell below solid, or the feet in water."
  [p pos]
  (let [feet (u/block-name p pos)
        head (u/block-name p (offset pos 0 1 0))
        below (u/block-name p (offset pos 0 -1 0))]
    (and (contains? passable feet)
         (contains? passable head)
         (or (= "water" feet)
             (and (some? below) (not (contains? unsafe-floor below)))))))

(defn score
  "Higher is better: away from hazards, up, and close."
  [from hazard-poss cell]
  (let [near (transduce (map #(u/dist cell %)) min hazard-reach hazard-poss)]
    (- (+ near (* height-bonus (- (:y cell) (:y from))))
       (* walk-cost (u/dist from cell)))))

(defn best-cell
  "The standable cell within step blocks of from with the best score, never
  from itself or a cell within a block of it."
  [p from step hazard-poss]
  (let [cells (for [dx (range (- step) (inc step))
                    dz (range (- step) (inc step))
                    dy (range 0 3)
                    :let [cell (offset from dx dy dz)]
                    :when (>= (u/dist from cell) 2)
                    :when (standable? p cell)]
                cell)]
    (when (seq cells)
      (apply max-key #(score from hazard-poss %) cells))))

(defn remember-hazards!
  "Write the lava among scanned hazards that memory does not know yet."
  [c scanned]
  (let [known (into #{} (map (comp :pos :data)) (ctx/entries c :hazard))
        fresh (->> scanned
                   (filter #(= "lava" (:name %)))
                   (remove #(contains? known (:pos %)))
                   (take max-hazards-per-round))]
    (doseq [{:keys [pos]} fresh]
      (ctx/remember! c :hazard {:kind :lava :pos pos} hazard-policy))))

(defn hazard-near?
  "Whether any scanned hazard lies within hazard-touch blocks of the feet cell."
  [pos scanned]
  (boolean (some #(<= (u/dist pos (:pos %)) hazard-touch) scanned)))

(defn clear? [c] (not (check c)))

(defn finish
  "After acting: done when the body is out, else continue."
  [c]
  (if (clear? c) :done :continue))

(defn ^:async pour-water!
  "Pour a carried water bucket at the feet. True when the water was placed."
  [c pos]
  (let [r (await (ctx/act c :place (clj->js {:pos pos :item "water_bucket"})))]
    (= "placed" (.-status r))))

(defn has-bucket? [p] (boolean (some #(= "water_bucket" (:name %)) (u/inventory p))))

(defn ^:async round [c]
  (let [p (:primitives c)
        me (.self p)]
    (if-not (burning/burning? me)
      :done
      (let [{:keys [water-radius step scan-radius]} (:args c)
            pos (floor-cell (u/pos-of (.-pos me)))
            lava? (boolean (.-inLava me))
            scanned (scan p scan-radius hazards 128)
            water (when-not lava? (first (scan p water-radius ["water"] 1)))]
        (ctx/remember! c :extinguish {:pos pos :cause (if lava? :lava :fire)} extinguish-policy)
        (remember-hazards! c scanned)
        (cond
          (and (not lava?) (has-bucket? p) (await (pour-water! c pos)))
          (finish c)

          water
          (do (await (ctx/act c :moveTo (clj->js {:pos (:pos water) :range 0})))
              (finish c))

          (and (not lava?) (not (hazard-near? pos scanned)))
          (do (ctx/emit! c :extinguish_wait :info {:text "no water near; waiting for the fire to go out"})
              :done)

          :else
          (let [target (best-cell p pos step (map :pos scanned))]
            (if-not target
              (u/fail! c :extinguish_stuck "no safe cell within reach")
              (let [r (await (ctx/act c :moveTo (clj->js {:pos target :range 0})))]
                (cond
                  (clear? c) :done
                  (= "blocked" (.-status r)) (u/fail! c :extinguish_stuck "the way to a safe cell is blocked")
                  :else :continue)))))))))
