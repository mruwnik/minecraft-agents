(ns jobs.access.cleanup.step
  "The pure step decision of jobs.access.cleanup: which ledger cell to dig, walk to, retreat from, or give up."
  (:require [jobs.lib.access.rules :as rules]))

(defn centre [[x y z]] [(+ x 0.5) (+ y 0.5) (+ z 0.5)])

(defn distance [a b] (apply js/Math.hypot (map - a b)))

(defn hazard-reason
  "The :accept key of a dig hazard: lava beside is :lava-adjacent, kept apart from water."
  [{:keys [reason fluid]}]
  (if (and (= :fluid-adjacent reason) (= "lava" fluid)) :lava-adjacent reason))

(defn open? [block-at pos] (let [n (block-at pos)] (boolean (and n (rules/replaceable n)))))

(defn exit-after-dig?
  "True when a body standing on cell, dropping onto the block below it, has a way on: a side cell at its new feet
  level that is open with open head room (walked off), or solid with two open cells above it (a step up)."
  [block-at cell]
  (boolean
   (some (fn [[dx dz]]
           (let [side (rules/offset cell dx 0 dz)]
             (or (and (open? block-at side) (open? block-at (rules/offset side 0 1 0)))
                 (and (not (open? block-at side)) (block-at side)
                      (open? block-at (rules/offset side 0 1 0)) (open? block-at (rules/offset side 0 2 0))))))
         [[1 0] [-1 0] [0 1] [0 -1]])))

(defn row-exit
  "The stand cell of firm ground beside the ledger row the body's floor belongs to, or nil: the floor cell under feet
  must be a ledger cell, the row is the ledger cells joined at that height (the cell without ignored), the ground a
  neighbour that is no ledger cell and solid floor."
  [{:keys [feet block-at ledger]} without]
  (let [[fx fy fz] feet
        floor [fx (dec fy) fz]
        in-row? #(and (contains? ledger %) (not= % without))
        around (fn [[x y z]] (map (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]))]
    (when (contains? ledger floor)
      (loop [todo [floor] seen #{floor}]
        (when-let [cell (first todo)]
          (let [ns (around cell)
                ground (first (filter #(and (not (contains? ledger %)) (rules/solid-floor? block-at %)) ns))
                fresh (remove seen (filter in-row? ns))]
            (if ground
              (update ground 1 inc)
              (recur (into (rest todo) fresh) (into seen fresh)))))))))

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
  :accept :can-clear? (block name -> whether the body can clear it now, by hand or a carried tool; absent: always)
  :hostile-near? (a known hostile near) :night?: a shelter's blocks stay}."
  [{:keys [feet block-at accept can-clear? hostile-near? night? keep-at-night?] :as in} {:keys [cell purpose]}]
  (let [[fx fy fz] feet
        [x y z] cell
        below (rules/offset cell 0 -1 0)]
    (cond
      (nil? (block-at cell)) {:reason :not-loaded}
      (and hostile-near? (shelter-purposes purpose)) {:reason :hostile-near}
      (and night? (not (false? keep-at-night?)) (shelter-purposes purpose)) {:reason :night}
      (and can-clear? (not (can-clear? (block-at cell)))) {:reason :no-tool :block (block-at cell)}
      (and (= [x z] [fx fz]) (< y (dec fy))) {:reason :under-body}
      (and (= cell [fx (dec fy) fz]) (not (rules/solid-floor? block-at below)))
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

(defn roofed?
  "Whether a solid block that is not a ledger cell lies within roof-height above cell."
  [{:keys [block-at ledger roof-height]} cell]
  (boolean (some (fn [dy] (let [c (rules/offset cell 0 dy 0)
                                n (block-at c)]
                            (and n (not (rules/replaceable n)) (not (contains? ledger c)))))
                 (range 1 (inc roof-height)))))

(defn walk-blocker
  "Why the body should not walk to the entry at cell, as {:reason ...}, or nil: :far-below (more than max-below under
  the feet), :died-near (died-near? of the cell), :roofed (under cover where the body is not)."
  [{:keys [feet max-below died-near?] :as in} {:keys [cell]}]
  (cond
    (> (- (get feet 1) (get cell 1)) max-below) {:reason :far-below}
    (and died-near? (died-near? cell)) {:reason :died-near}
    (and (roofed? in cell) (not (roofed? in (update feet 1 dec)))) {:reason :roofed}))

(defn next-step
  "The next step over entries (each holding its item, or unloaded), from {:feet :eye [x y z] :block-at :entries
  :ledger #{cells} :zones :footprints :accept :reach :held {cell {:reason ...}}}: {:step :dig :cell :item},
  {:step :walk :cell}, {:step :retreat :to stand} (only :stranding cells are left, and :retreats is under 3) or
  {:step :finish :open [{:cell :item :reason ...}]}. Held cells are given up for this run."
  [{:keys [entries eye reach held retreats guard-walks?] :as in}]
  (let [d #(distance eye (centre (:cell %)))
        judged (map (fn [e] [e (or (held (:cell e)) (blocker in e) (when-not (or (<= (d e) reach) (not guard-walks?)) (walk-blocker in e)))]) entries)
        ready (map first (remove second judged))
        near (filter #(<= (d %) reach) ready)
        far (remove #(<= (d %) reach) ready)]
    (cond
      (seq near) (let [e (first (sort-by (juxt #(- (get (:cell %) 1)) d) near))]
                   {:step :dig :cell (:cell e) :item (:item e)})
      (seq far) {:step :walk :cell (:cell (first (sort-by d far)))}
      (and (< (or retreats 0) 3) (some (comp #{:stranding} :reason second) judged) (row-exit in nil))
      {:step :retreat :to (row-exit in nil)}
      :else {:step :finish :open (mapv (fn [[e why]] (merge {:cell (:cell e) :item (:item e)} why)) judged)})))

;; ------------------------------------------------------------------ reading the world
