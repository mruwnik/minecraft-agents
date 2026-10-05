(ns jobs.survival.extinguish
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.util :as u]
            [engine.triggers.burning :as burning]))

(def doc
  "Put the body out when it is on fire or in lava.
  Each round, in this order:
  1. With a water bucket carried and the body not in lava, pours water at the feet and notes the cell as :poured.
     A cell in another's zone or claim is skipped while another way exists. As a last resort it pours there anyway,
     with one extinguish.trespass-last-resort warning.
  2. Once the fire is out, scoops the water back up with the empty bucket so no source block stays.
     It waits up to 10 rounds for the poured cell to read as water, and up to 8 rounds while still burning.
     A scoop that fails warns extinguish.scoop_failed with the cell.
  3. With no bucket, on fire and with water within :water-radius, walks into the nearest water.
  4. In lava, or with no water in reach, walks to the best nearby cell within :step blocks.
     It must be passable, solid underfoot and not fire, lava, magma or a campfire.
     Scoring favours distance from those hazards (up to 4 blocks) and height, and charges a small cost per block walked.
  5. On fire, not in lava, with no water in reach and no hazard within 1.5 blocks, there is nothing useful to do.
     It stands still, emits info extinguish_wait and ends. The trigger fires it again after its cooldown.
  Ends when the body is neither burning nor in lava.
  Three failed rounds (no safe cell, or the walk blocked while burning) give an extinguish_stuck warning, then it gives up.
  Memory: writes :extinguish {:pos :cause} each round (cap 20, one hour),
  and lava seen within :scan-radius as :hazard entries (cap 50, six hours) for retreat logic.")

(def backoff
  "Off: a danger reflex (rule: no cooldown and no backoff while the danger lasts); fruitless rounds while no water or safe cell is
  in reach must not mute it."
  false)

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

(def max-pour-waits 8)
(def max-water-waits 10)

(defn body-burning? [c] (burning/burning? (.self (:primitives c))))

(defn check [c] (boolean (or (body-burning? c) (:poured (ctx/mem c)))))

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

(defn clear? [c] (not (body-burning? c)))

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

(defn clear-pour! [c]
  (ctx/update-mem! c dissoc :poured :pour-waits :water-waits))

(defn ^:async scoop-round
  "The water was poured at poured. Wait while burning (up to max-pour-waits
  rounds); once out scoop the water back up and finish."
  [c poured]
  (let [p (:primitives c)]
    (cond
      (body-burning? c)
      (let [waits (inc (:pour-waits (ctx/mem c) 0))]
        (if (> waits max-pour-waits)
          (do (clear-pour! c) nil)
          (do (ctx/update-mem! c assoc :pour-waits waits) :continue)))

      (not= "water" (u/block-name p poured))
      (let [waits (inc (:water-waits (ctx/mem c) 0))]
        (if (> waits max-water-waits)
          (do (clear-pour! c) :done)
          (do (ctx/update-mem! c assoc :water-waits waits) :continue)))

      :else
      (let [r (await (ctx/act c :place (clj->js {:pos poured :item "bucket"})))
            status (.-status r)]
        (clear-pour! c)
        (when-not (= "placed" status)
          (ctx/emit! c :extinguish.scoop_failed :warn
                     {:text (str "could not scoop the poured water at " (pr-str poured) ": " status)
                      :pos poured :status status}))
        :done))))

(defn ^:async pour-last-resort!
  "No permitted way out is left and the feet cell is refused: pour there anyway, with the warn. :continue when poured,
  else nil."
  [c pos refusal]
  (access/trespass! c "extinguish" refusal)
  (when (await (pour-water! c pos))
    (ctx/update-mem! c assoc :poured pos)
    :continue))

(defn ^:async round [c]
  (let [p (:primitives c)
        me (.self p)
        poured (:poured (ctx/mem c))
        scooped (when poured (await (scoop-round c poured)))]
    (cond
      scooped scooped

      (and (not poured) (not (burning/burning? me)))
      :done

      :else
      (let [{:keys [water-radius step scan-radius]} (:args c)
            pos (floor-cell (u/pos-of (.-pos me)))
            lava? (boolean (.-inLava me))
            scanned (scan p scan-radius hazards 128)
            water (when-not lava? (first (scan p water-radius ["water"] 1)))
            pour? (and (not lava?) (has-bucket? p))
            refusal (when pour? (access/trespass-refusal c :place pos))]
        (ctx/remember! c :extinguish {:pos pos :cause (if lava? :lava :fire)} extinguish-policy)
        (remember-hazards! c scanned)
        (cond
          (and pour? (nil? refusal) (await (pour-water! c pos)))
          (do (ctx/update-mem! c assoc :poured pos)
              :continue)

          water
          ;; raw moveTo kept: an emergency step into water or out of fire (range 0), a few blocks, no time for a plan.
          (do (await (ctx/act c :moveTo (clj->js {:pos (:pos water) :range 0})))
              (finish c))

          (and (not lava?) (not (hazard-near? pos scanned)))
          (or (when refusal (await (pour-last-resort! c pos refusal)))
              (do (ctx/emit! c :extinguish_wait :info {:text "no water near; waiting for the fire to go out"})
                  :done))

          :else
          (let [target (best-cell p pos step (map :pos scanned))]
            (if-not target
              (or (when refusal (await (pour-last-resort! c pos refusal)))
                  (u/fail! c :extinguish_stuck "no safe cell within reach"))
              ;; raw moveTo kept: an emergency step into water or out of fire (range 0), a few blocks, no time for a plan.
              (let [r (await (ctx/act c :moveTo (clj->js {:pos target :range 0})))]
                (cond
                  (clear? c) :done
                  (= "blocked" (.-status r)) (u/fail! c :extinguish_stuck "the way to a safe cell is blocked")
                  :else :continue)))))))))
