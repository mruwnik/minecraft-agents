(ns jobs.survival.extinguish
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.util :as u]
            [jobs.survival.eat :as eat]
            [triggers.survival.burning :as burning]))

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
  Water also counts powder snow. Not in lava, with a cover block carried (cobblestone, stone, dirt, ...) it first
  places one on a lava cell next to the feet (side or below) with open air above it, so the body does not step past it. Never sand, gravel or
  other falling blocks, never in another's zone or claim, and at most 3 covers per run.
  5. On fire, not in lava, with no water in reach and no hazard within 1.5 blocks, there is nothing useful to do.
     It stands still: emits info extinguish_wait once, eats when food is under 18 and food is carried (to keep
     regenerating), and waits a second per round for up to 20 rounds. The job stays alive meanwhile, so an
     interrupted walk does not resume into more hazards; it ends when the fire is out or after the 20 rounds.
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

(def max-stand-waits 20)
(def stand-wait-ms 1000)
(def eat-below 18)
(def water-like ["water" "powder_snow"])
(def max-covers "Lava cells covered per run." 3)
;; solid blocks that do not fall (sand, gravel and concrete powder would drop into the lava)
(def cover-blocks ["cobblestone" "stone" "dirt" "netherrack" "cobbled_deepslate" "deepslate" "andesite" "diorite" "granite"])

(def max-pour-waits 8)
(def max-water-waits 10)

(defn body-burning? [c] (burning/burning? (.self (:primitives c))))

(defn check [c] (boolean (or (body-burning? c) (:poured (ctx/mem c)) (:stand-waits (ctx/mem c)))))

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

(defn adjacent-lava
  "A scanned lava cell next to the feet cell (side, or the cell below), or nil."
  [pos scanned]
  (->> scanned
       (filter #(= "lava" (:name %)))
       (filter (fn [{p :pos}]
                 (and (<= (js/Math.abs (- (:x p) (:x pos))) 1)
                      (<= (js/Math.abs (- (:z p) (:z pos))) 1)
                      (<= 0 (- (:y pos) (:y p)) 1)
                      (not= p pos)
                      (<= (+ (js/Math.abs (- (:x p) (:x pos))) (js/Math.abs (- (:z p) (:z pos)))) 1))))
       first))

(defn exposed-lava
  "The scanned lava cells without a block over them; lava below the feet level under a block, such as an earlier
  cover, is no candidate. Lava at feet level is one even under an overhang: the body stands beside it."
  [p feet scanned]
  (remove (fn [{:keys [name pos]}]
            (and (= "lava" name)
                 (< (:y pos) (:y feet))
                 (let [above (u/block-name p (offset pos 0 1 0))]
                   (not (or (nil? above) (contains? passable above) (= "lava" above))))))
          scanned))

(defn cover-item [p] (some (fn [n] (when (some #(= n (:name %)) (u/inventory p)) n)) cover-blocks))

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

(defn ^:async stand-round!
  "On fire, nothing to do but wait it out off the fire: eat to keep regenerating, tell the agent once, wait a second.
  The job stays alive (so an interrupted walk does not resume into more hazards) up to max-stand-waits rounds."
  [c]
  (let [p (:primitives c)
        waits (inc (:stand-waits (ctx/mem c) 0))
        food (eat/carried-best c)]
    (ctx/update-mem! c assoc :stand-waits waits)
    (when (= 1 waits)
      (ctx/emit! c :extinguish_wait :info {:text "no water near; standing still off the fire until it goes out"}))
    (when (and food (< (.-food (.self p)) eat-below))
      (await (ctx/act c :equip #js {:item food}))
      (await (ctx/act c :eat #js {:item food})))
    (await (ctx/act c :wait (clj->js {:ms stand-wait-ms})))
    (if (or (clear? c) (>= waits max-stand-waits))
      :done
      :continue)))

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
            water (when-not lava? (first (scan p water-radius water-like 1)))
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

          (and (not lava?) (< (:covers (ctx/mem c) 0) max-covers) (cover-item p)
               (let [lava (:pos (adjacent-lava pos (exposed-lava p pos scanned)))]
                 (and lava
                      (nil? (access/trespass-refusal c :place lava))
                      (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos lava :item (cover-item p)}))))))))
          (do (ctx/update-mem! c update :covers (fnil inc 0))
              :continue)

          (and (not lava?) (not (hazard-near? pos scanned)))
          (or (when refusal (await (pour-last-resort! c pos refusal)))
              (await (stand-round! c)))

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
