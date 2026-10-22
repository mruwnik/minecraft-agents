(ns jobs.access.cleanup.step
  "The pure step decision of jobs.access.cleanup: which ledger cell to dig, walk to, retreat from, or give up."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.ledger :refer [centre distance submerged?]]))

(defn hazard-reason
  "The :accept key of a dig hazard: lava beside is :lava-adjacent, kept apart from water."
  [{:keys [reason fluid]}]
  (if (and (= :fluid-adjacent reason) (= "lava" fluid)) :lava-adjacent reason))

(defn open? [block-at pos] (let [n (block-at pos)] (boolean (and n (bs/replaceable n)))))

(defn clear?
  "Open, or leaves: a player enclosed by a canopy digs through it, so leaves are no wall."
  [block-at pos]
  (or (open? block-at pos) (boolean (some->> (block-at pos) (re-find #"_leaves$")))))

(defn exit-after-dig?
  "True when a body standing on cell, dropping onto the block below it, has a way on: a side cell at its new feet
  level that is clear with clear head room (walked off), or solid with two clear cells above it (a step up)."
  [block-at cell]
  (boolean
   (some (fn [[dx dz]]
           (let [side (rules/offset cell dx 0 dz)]
             (or (and (clear? block-at side) (clear? block-at (rules/offset side 0 1 0)))
                 (and (not (clear? block-at side)) (block-at side)
                      (clear? block-at (rules/offset side 0 1 0)) (clear? block-at (rules/offset side 0 2 0))))))
         [[1 0] [-1 0] [0 1] [0 -1]])))

(defn row-way
  "The cells from the floor under the feet along the ledger row to firm ground, the ground last, or nil: the floor cell
  under feet must be a ledger cell, the row is the ledger cells joined at that height (the cell without ignored), the
  ground a neighbour that is no ledger cell, solid floor and open above it (a body can step on)."
  [{:keys [feet block-at ledger]} without]
  (let [[fx fy fz] feet
        floor [fx (dec fy) fz]
        in-row? #(and (contains? ledger %) (not= % without))
        around (fn [[x y z]] (map (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]))
        ground? #(and (not (contains? ledger %)) (rules/solid-floor? block-at %)
                      (open? block-at (rules/offset % 0 1 0)) (open? block-at (rules/offset % 0 2 0)))]
    (when (contains? ledger floor)
      (loop [todo [floor] came {floor nil}]
        (when-let [cell (first todo)]
          (let [ns (around cell)]
            (if-let [ground (first (filter ground? ns))]
              (conj (vec (reverse (take-while some? (iterate came cell)))) ground)
              (let [fresh (remove #(contains? came %) (filter in-row? ns))]
                (recur (into (vec (rest todo)) fresh) (into came (map (fn [n] [n cell])) fresh))))))))))

(defn row-exit
  "The stand cell of firm ground at the end of row-way, or nil."
  [in without]
  (some-> (row-way in without) last (update 1 inc)))

(defn stranding?
  "True when digging cell would cut the body off from firm ground it can reach over its row now."
  [{:keys [feet] :as in} cell]
  (let [[fx fy fz] feet]
    (and (not= cell [fx (dec fy) fz])
         (some? (row-exit in nil))
         (nil? (row-exit in cell)))))

(def shelter-purposes #{:shelter :retreat-plug})

(defn plan-cells
  "The cells of entries l that lie on the footprint of a plan this body made (own-plans), when spare-own-builds holds."
  [l footprints own-plans spare-own-builds]
  (if-not spare-own-builds
    #{}
    (into #{} (comp (map :cell) (filter #(contains? own-plans (get footprints %)))) l)))

(defn blocker
  "Why entry e cannot be dug now, as {:reason ...detail}, or nil. in: {:feet :block-at :zones :footprints :ledger
  :accept :floor-at (block-at over what was seen, no guess: the cell under the body's floor must be seen solid) :can-clear? (block name -> whether the body can clear it now, by hand or a carried tool; absent: always)
  :hostile-near? (a known hostile near) :opens-dark? (fn of a cell: it opens onto a dark cell, so a shelter's block stays)
  :bucket? (an empty bucket is carried: a poured water source is scooped, not dug)
  :lava-near? (fn of a cell: the risk of going there, for seen lava near it, is over the block's worth; such a block is left unless :lava-adjacent is accepted)}."
  [{:keys [feet block-at floor-at accept can-clear? hostile-near? opens-dark? keep-at-night? bucket? lava-near?] :as in} {:keys [cell purpose item]}]
  (let [[fx fy fz] feet
        [x y z] cell
        below (rules/offset cell 0 -1 0)]
    (cond
      (nil? (block-at cell)) {:reason :not-loaded}
      (= "water" item) (when-not bucket? {:reason :no-bucket})
      (and hostile-near? (shelter-purposes purpose)) {:reason :hostile-near}
      (and opens-dark? (not (false? keep-at-night?)) (shelter-purposes purpose) (opens-dark? cell)) {:reason :night}
      (and can-clear? (not (can-clear? (block-at cell)))) {:reason :no-tool :block (block-at cell)}
      (and lava-near? (not (accept :lava-adjacent)) (lava-near? cell)) {:reason :lava-near}
      (and (= [x z] [fx fz]) (< y (dec fy))) {:reason :under-body}
      (and (= cell [fx (dec fy) fz]) (not (rules/solid-floor? (or floor-at block-at) below)))
      {:reason :no-floor-below :block (block-at below)}
      (and (= cell [fx (dec fy) fz]) (not (exit-after-dig? block-at cell)))
      {:reason :no-exit}
      (stranding? in cell) {:reason :stranding}
      :else
      (let [v (rules/may-dig? (-> (select-keys in [:block-at :feet :zones :footprints :shafts :claims :self :now :ignore-zones? :ledger])
                                  (update :footprints #(if (:spare-own-builds in) % (into {} (remove (comp (:own-plans in #{}) val)) %)))
                                  (assoc :cell cell)))
            hazards (mapv hazard-reason (:hazards v))]
        (cond
          (not (:ok v)) (select-keys v [:reason :zone :claim :plan :block])
          (not-every? accept hazards) {:reason :hazard :hazards hazards})))))

(defn step-back
  "The stand cell on the row cell (or ground) next toward the ground, when the body's floor cell is a ledger cell it cannot dig from
  under its feet (no floor below it, no way on): from there the floor cell is dug and the row is taken back end first."
  [{:keys [feet] :as in} entries]
  (let [[fx fy fz] feet
        floor [fx (dec fy) fz]
        way (row-way in nil)]
    (when (and (some? way)
               (some (fn [e] (and (= floor (:cell e)) (#{:no-floor-below :no-exit} (:reason (blocker in e))))) entries))
      (update (second way) 1 inc))))

(defn next-step
  "The next step over entries (each holding its item, or unloaded), from {:feet :eye [x y z] :block-at :entries
  :ledger #{cells} :zones :footprints :accept :reach :held {cell {:reason ...}} :walk-blocker (fn of an entry: why a walk to it is not worth, or nil)}: {:step :dig :cell :item},
  {:step :walk :cell}, {:step :retreat :to stand} (only :stranding cells are left, and :retreats is under 3) or
  {:step :finish :open [{:cell :item :reason ...}]}. Held cells are given up for this run."
  [{:keys [entries eye reach held retreats] :as in}]
  (let [d #(distance eye (centre (:cell %)))
        far? #(> (d %) reach)
        judged (map (fn [e] [e (or (held (:cell e)) (blocker in e)
                                   (when (and (far? e) (submerged? (:block-at in) (:cell e))) {:reason :submerged})
                                   (when-let [why (and (far? e) (some-> (:walk-blocker in) (apply [e])))] (assoc why :walk-blocked true)))])
                    entries)
        ready (map first (remove second judged))
        near (filter #(<= (d %) reach) ready)
        far (remove #(<= (d %) reach) ready)]
    (cond
      (seq near) (let [e (first (sort-by (juxt #(- (get (:cell %) 1)) d) near))]
                   {:step :dig :cell (:cell e) :item (:item e)})
      (seq far) {:step :walk :cell (:cell (first (sort-by d far)))}
      (and (< (or retreats 0) 16) (step-back in entries)) {:step :retreat :to (step-back in entries)}
      (and (< (or retreats 0) 3) (some (comp #{:stranding} :reason second) judged) (row-exit in nil))
      {:step :retreat :to (row-exit in nil)}
      :else {:step :finish :open (mapv (fn [[e why]] (merge {:cell (:cell e) :item (:item e)} why)) judged)})))

;; ------------------------------------------------------------------ reading the world
