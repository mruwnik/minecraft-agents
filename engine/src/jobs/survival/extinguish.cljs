(ns jobs.survival.extinguish
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.survival.eat :as eat]
            [jobs.lib.body :as body]))

(def doc
  "Put the body out when it is on fire or in lava. One run, re-reading the world before each pass, until the body is
  neither burning nor in lava (:done). Each pass, in this order:
  1. With a water bucket carried and the body not in lava, pours water at the feet and notes the cell in body memory
     (:extinguish-pour, ten minutes), so a run cut before the scoop leaves it for the next run.
     A cell in another's zone or claim is skipped while another way exists. As a last resort it pours there anyway,
     with one extinguish.trespass-last-resort warning.
  2. Once the fire is out, scoops the water back up with the empty bucket so no source block stays.
     It waits (a declared :burning-wait hold) up to 10 quarter seconds for the poured cell to read as water, and up to
     8 while still burning. A pour left more than 4 blocks away is walked back to (one go-to) when within 16 blocks, else dropped.
     A scoop that fails, or a pour dropped as too far, warns extinguish.scoop_failed with the cell (the entry is dropped).
  3. With no bucket, on fire and with water within :water-radius, walks into the nearest water.
  4. In lava, or with no water in reach, walks to the best nearby cell within :step blocks.
     It must be passable, solid underfoot and not fire, lava, magma or a campfire.
     Scoring favours distance from those hazards (up to 4 blocks) and height, and charges a small cost per block walked.
  Water also counts powder snow. Not in lava (or in lava once a walk was blocked), with a cover block carried (cobblestone, stone, dirt, ...) it first
  places one on a lava cell next to the feet (side or below) with open air above it, so the body does not step past it. Never sand, gravel or
  other falling blocks, never in another's zone or claim, and at most 3 covers per run.
  Before each decision from a new cell it looks round (level and down) so the fire at the feet and the lava beside them are seen.
  5. On fire, not in lava, with no water in reach and no hazard within 1.5 blocks, there is nothing useful to do.
     It holds still (:burning-wait): emits info extinguish_wait once, eats when food is under 18 and food is carried
     (to keep regenerating), and waits a second per pass; still burning after 20 seconds ends stopped :still-burning.
  A walk that is blocked (boxed in, perhaps by its own covers) goes through a go-to from then on (it pillars, stairs or digs
  out when shut in); a go-to still working is not a failure. Three failures (no safe cell, or a go-to that fails, with its reason) give an extinguish_stuck warning and end stopped :stuck.
  Still burning after max-passes passes ends stopped :still-burning. Never :continue.
  Memory: writes :extinguish {:pos :cause} each pass (cap 20, one hour),
  and lava seen within :scan-radius as :hazard entries (cap 50, six hours) for retreat logic.")

(def args
  {:water-radius {:doc "on fire, water within this many blocks is walked into" :default 6}
   :step {:doc "candidate cells lie within this many blocks (horizontally) of the body" :default 4}
   :scan-radius {:doc "fire, lava and magma within this many blocks count as hazards" :default 8}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

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

(def max-stand-waits "One-second waits standing still before the run stops :still-burning." 20)
(def stand-wait-ms 1000)
(def eat-below 18)
(def water-like ["water" "powder_snow"])
(def max-covers "Lava cells covered per run." 3)
;; solid blocks that do not fall (sand, gravel and concrete powder would drop into the lava)
(def cover-blocks ["cobblestone" "stone" "dirt" "netherrack" "cobbled_deepslate" "deepslate" "andesite" "diorite" "granite"])

(def max-return-distance "A pour left farther than this is dropped with a warn, not walked back to." 16)
(def max-scoop-return-range "go-to range for the walk back to a pour." 3)
(def max-pour-waits 8)
(def max-water-waits 10)

(defn body-burning? [c] (body/burning? (.self (:primitives c))))

(def pour-policy "Body memory of the cell poured: it outlives a cut run so the next run scoops it." {:cap 1 :ttl (* 10 60 1000)})

(defn poured
  "The cell a run poured water at and has not scooped yet, or nil."
  [c]
  (some-> (ctx/latest c :extinguish-pour) :data :pos))

(defn check [c]
  (or (boolean (or (body-burning? c) (poured c)))
      (ctx/wait c {:reason :not-burning})))

(defn floor-cell [pos] (into {} (map (fn [[k v]] [k (js/Math.floor v)])) pos))

(defn offset [pos dx dy dz] {:x (+ (:x pos) dx) :y (+ (:y pos) dy) :z (+ (:z pos) dz)})


(defn scan
  "The blocks of names the body has seen within radius, nearest first, as {:name :pos}."
  [p radius names max]
  (mapv #(select-keys % [:name :pos]) (look/seen-blocks p {:radius radius :names names :max max})))

(defn feel-feet
  "scanned plus the fire at the feet cell when the scan lacks it: the body feels the flames it stands in, in view or not."
  [p pos scanned]
  (let [feet (u/feel-name p pos)]
    (if (and (contains? hazards feet) (not-any? #(= pos (:pos %)) scanned))
      (conj scanned {:name feet :pos pos})
      scanned)))

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

(defn better-cell?
  "Whether cell scores higher than staying at from."
  [from cell hazard-poss]
  (> (score from hazard-poss cell) (score from hazard-poss from)))

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

(def pour-wait-ms "A wait for the server to put the fire out or show the poured water." 250)
(def max-passes "Passes in one run before it stops :still-burning." 60)

(defn ^:async pour-water!
  "Pour a carried water bucket at the feet. True when the water was placed (then noted in body memory)."
  [c pos]
  (ctx/hold-still! c nil)
  (let [placed (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos pos :item "water_bucket"})))))]
    (when placed (ctx/remember! c :extinguish-pour {:pos pos} pour-policy))
    placed))

(defn has-bucket? [p] (boolean (some #(= "water_bucket" (:name %)) (u/inventory p))))

(defn clear-pour! [c]
  (ctx/forget-where! c :extinguish-pour (constantly true))
  (ctx/update-mem! c dissoc :pour-waits :water-waits :returned))

(defn ^:async burning-wait!
  "Hold still on purpose for ms."
  [c ms]
  (ctx/hold-still! c :burning-wait)
  (await (ctx/act c :wait (clj->js {:ms ms}))))

(defn ^:async scoop-pass
  "The water was poured at cell. Wait while burning (up to max-pour-waits), nil after that so the pass tries
  another way; once out, wait for the water to show (up to max-water-waits), scoop it back up and end."
  [c cell]
  (let [p (:primitives c)]
    (cond
      (body-burning? c)
      (let [waits (inc (:pour-waits (ctx/mem c) 0))]
        (if (> waits max-pour-waits)
          (do (clear-pour! c) nil)
          (do (ctx/update-mem! c assoc :pour-waits waits)
              (await (burning-wait! c pour-wait-ms))
              :again)))

      (not= "water" (u/block-name p cell))
      (let [waits (inc (:water-waits (ctx/mem c) 0))]
        (if (> waits max-water-waits)
          (do (clear-pour! c) :done)
          (do (ctx/update-mem! c assoc :water-waits waits)
              (await (burning-wait! c pour-wait-ms))
              :again)))

      (and (> (u/eye-dist (u/self-pos c) cell) u/bucket-reach)
           (<= (u/dist (u/self-pos c) cell) max-return-distance)
           (not (:returned (ctx/mem c))))
      (do (ctx/update-mem! c assoc :returned true)
          (await (ctx/call-child c :go 'jobs.movement.go-to {:pos cell :range max-scoop-return-range :escalate false}))
          :again)

      (> (u/eye-dist (u/self-pos c) cell) u/bucket-reach)
      (do (clear-pour! c)
          (ctx/emit! c :extinguish.scoop_failed :warn
                     {:text (str "left the poured water at " (pr-str cell) ": too far to scoop") :pos cell :status "far"})
          :done)

      :else
      (do (ctx/hold-still! c nil)
          (let [status (.-status (await (ctx/act c :place (clj->js {:pos cell :item "bucket"}))))]
            (clear-pour! c)
            (when-not (= "placed" status)
              (ctx/emit! c :extinguish.scoop_failed :warn
                         {:text (str "could not scoop the poured water at " (pr-str cell) ": " status)
                          :pos cell :status status}))
            :done)))))

(defn ^:async pour-last-resort!
  "No permitted way out is left and the feet cell is refused: pour there anyway, with the warn. :again when poured,
  else nil."
  [c pos refusal]
  (access/trespass! c "extinguish" refusal)
  (when (await (pour-water! c pos))
    :again))

(defn ^:async stand-pass!
  "On fire, nothing to do but wait it out off the fire: eat to keep regenerating, tell the agent once, wait a second.
  Stopped :still-burning after max-stand-waits."
  [c]
  (let [p (:primitives c)
        waits (inc (:stand-waits (ctx/mem c) 0))
        food (eat/carried-best c)]
    (ctx/update-mem! c assoc :stand-waits waits)
    (when (= 1 waits)
      (ctx/emit! c :extinguish_wait :info {:text "no water near; standing still off the fire until it goes out"}))
    (ctx/hold-still! c :burning-wait)
    (when (and food (< (.-food (.self p)) eat-below))
      (await (ctx/act c :equip #js {:item food}))
      (await (ctx/act c :eat #js {:item food})))
    (await (ctx/act c :wait (clj->js {:ms stand-wait-ms})))
    (cond
      (clear? c) :done
      (>= waits max-stand-waits) (result/stop! c :still-burning "still burning after standing still off the fire")
      :else :again)))

(defn stuck!
  "One failed walk: :again until u/max-failures in a row, then a warn and stopped :stuck."
  [c text]
  (if-not (u/count-fail! c)
    :again
    (do (ctx/emit! c :extinguish_stuck :warn {:tries (u/max-failures) :text text})
        (result/stop! c :stuck text))))

(defn ^:async move!
  "An emergency step to pos (range 0). The walk's status."
  [c pos]
  (ctx/hold-still! c nil)
  ;; raw moveTo kept: an emergency step into water or out of fire (range 0), a few blocks, no time for a plan.
  (.-status (await (ctx/act c :moveTo (clj->js {:pos pos :range 0})))))

(defn ^:async escape!
  "The emergency step is blocked (boxed in, perhaps by own covers): one go-to to pos, which plans the way and, when
  shut in, pillars, stairs or digs out. :again while it works or when it arrives; a go-to that fails or is declined
  counts toward stuck!, which carries its reason; an arrival resets that count."
  [c pos]
  (ctx/hold-still! c nil)
  (ctx/update-mem! c assoc :blocked true)
  (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos pos :range 0}))
        res (ctx/child-result c :go)]
    (cond
      (clear? c) :done
      (= :continue r) :again
      (and (= :done r) (:arrived res)) (do (u/progress! c) :again)
      :else (stuck! c (str "the way to a safe cell is blocked" (some->> (:text res) (str ": ")))))))

(defn ^:async pass!
  "One try at putting the body out. :again for another pass, else :done (perhaps stopped)."
  [c]
  (let [p (:primitives c)
        me (.self p)
        cell (poured c)
        scooped (when cell (await (scoop-pass c cell)))]
    (cond
      scooped scooped

      (not (body/burning? me))
      :done

      :else
      (let [{:keys [water-radius step scan-radius]} (:args c)
            pos (floor-cell (u/pos-of (.-pos me)))
            lava? (boolean (.-inLava me))
            _ (when-not (look/looked-here? c) (await (look/look-around! c)))
            scanned (feel-feet p pos (scan p scan-radius hazards 128))
            water (when-not lava? (first (scan p water-radius water-like 1)))
            pour? (and (not lava?) (has-bucket? p))
            refusal (when pour? (access/trespass-refusal c :place pos))]
        (ctx/remember! c :extinguish {:pos pos :cause (if lava? :lava :fire)} extinguish-policy)
        (remember-hazards! c scanned)
        (cond
          (and pour? (nil? refusal) (await (pour-water! c pos)))
          :again

          water
          (do (await (move! c (:pos water))) :again)

          (and (or (not lava?) (:blocked (ctx/mem c))) (< (:covers (ctx/mem c) 0) max-covers) (cover-item p)
               (let [lava (:pos (adjacent-lava pos (exposed-lava p pos scanned)))]
                 (and lava
                      (nil? (access/trespass-refusal c :place lava))
                      (do (ctx/hold-still! c nil)
                          (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos lava :item (cover-item p)})))))))))
          (do (ctx/update-mem! c update :covers (fnil inc 0))
              :again)

          (and (not lava?) (not (hazard-near? pos scanned)))
          (or (when refusal (await (pour-last-resort! c pos refusal)))
              (await (stand-pass! c)))

          :else
          (let [hazard-poss (map :pos scanned)
                target (best-cell p pos step hazard-poss)]
            (cond
              (not target)
              (or (when refusal (await (pour-last-resort! c pos refusal)))
                  (stuck! c "no safe cell within reach"))

              ;; out of the lava a move is a must; otherwise a cell that is no better is not worth the walk back and forth
              (and (not lava?) (not (better-cell? pos target hazard-poss)))
              (await (stand-pass! c))

              :else
              (let [blocked? (:blocked (ctx/mem c))
                    status (if blocked? "blocked" (await (move! c target)))]
                (cond
                  (clear? c) :done
                  ;; in lava the first blocked step covers the lava around first (next passes): the go-to needs a step
                  (and lava? (not blocked?) (= "blocked" status)) (do (ctx/update-mem! c assoc :blocked true) :again)
                  (= "blocked" status) (await (escape! c target))
                  :else :again)))))))))

(defn ^:async round [c]
  (loop [i 0]
    (cond
      (not (ctx/alive? c)) :done
      (<= max-passes i) (result/stop! c :still-burning "still burning after many tries")
      :else (let [r (await (pass! c))]
              (if (= :again r)
                (do (await (pace/pace!)) (recur (inc i)))
                r)))))
