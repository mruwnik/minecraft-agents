(ns jobs.survival.extinguish
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.util :as u]
            [engine.triggers.burning :as burning]))

(def doc
  "Put the body out: it is on fire or in lava. Each round, with a water bucket
  carried and the body on land, pours the water at the feet, remembers the cell
  as :poured and, once the fire is out, scoops that water back up with the
  empty bucket so no source block is left behind. The block read can lag the
  pour, so a poured cell not yet read as water is waited for (up to 10 rounds)
  before the job gives it up; a scoop that is not placed warns
  extinguish.scoop_failed naming the cell; if still burning after 8 waiting rounds it stops
  waiting and acts normally). A cell in another's zone or claim is not poured over while water or a safe cell is
  within reach; with nothing else to do it pours there anyway, as a last resort, with one
  extinguish.trespass-last-resort warn (a missing zone list changes nothing). Otherwise it makes
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
