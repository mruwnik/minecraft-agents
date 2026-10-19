(ns jobs.lib.stair-cells
  "The geometry and the stop rules of a stair step, pure: where its cells are, which of them may be cut, what a dig
  laid open. Shared by jobs.access.stair and jobs.access.tunnel."
  (:require [clojure.string :as str]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [plan.shape :as shape]))

(def rises (assoc shape/rises :level 0))

(defn delta
  "One step's move [dx dy dz]."
  [dir heading]
  (let [[dx dz] (shape/headings heading)] [dx (rises dir) dz]))

(defn step-cells
  "plan.shape/step-cells, and for :level the run step: {:next :cut (head first) :floor :under}."
  [feet dir heading]
  (if (= :level dir)
    (let [n (shape/add feet (delta dir heading))]
      {:next n :cut [(shape/add n [0 1 0]) n] :floor (shape/add n [0 -1 0]) :under (shape/add n [0 -2 0])})
    (shape/step-cells feet dir heading)))

(defn stair-index
  "i when feet is the stair's cell after i steps from origin, 0 <= i <= steps; else nil."
  [origin feet dir heading steps]
  (let [[dx dy dz] (delta dir heading)
        i (+ (* dx (- (feet 0) (origin 0))) (* dz (- (feet 2) (origin 2))))]
    (when (and (<= 0 i steps) (= feet (shape/add origin [(* i dx) (* i dy) (* i dz)])))
      i)))

(def crop-names
  "Blocks a stair never cuts: a farm's crops, stems and the farmland they stand on (the farm is another's work even
  when no zone says so)."
  #{"wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "attached_melon_stem" "attached_pumpkin_stem"
    "nether_wart" "sweet_berry_bush" "torchflower_crop" "pitcher_crop" "farmland"})

(defn stair-hazards
  "The hazard the stair adds to the rules' for cell: a falling block over it, unless that cell is cut too (the rules
  see one only over the body's own column; the body walks into the next one)."
  [block-at cell cut]
  (let [above (shape/add cell [0 1 0])
        n (block-at above)]
    (when (and (rules/falling? n) (not (some #{above} cut)))
      [{:reason :falling-block :block n :at above}])))

(defn cell-verdict
  "The dig verdict of cell: the rules' refusal, else {:ok true :hazards [...]} with the stair's hazards added."
  [in cut]
  (let [v (rules/may-dig? in)]
    (if-not (:ok v)
      v
      (let [[fx _ fz] (:feet in)
            hs (vec (for [h (distinct (concat (:hazards v) (stair-hazards (:block-at in) (:cell in) cut)))
                          :let [[x _ z] (:at h)]]
                      (cond-> h (and (= :falling-block (:reason h)) (= [x z] [fx fz])) (assoc :on-body true))))]
        (cond-> {:ok true} (seq hs) (assoc :hazards hs))))))

(defn way-floors
  "The floors of the steps a stair walked (not the start's own ground), from its origin, direction and steps reached."
  [origin dir heading steps]
  (let [d (delta dir heading)]
    (vec (for [i (range 1 (inc (or steps 0)))] (shape/add (shape/add origin (mapv #(* i %) d)) [0 -1 0])))))

(defn joined-way
  "floors (of a run that began standing on the cell ground) as one way with the earlier ways (olds, the floors of each)
  it goes on from or covers, those that hold ground or one of floors: {:floors [theirs, then floors] :gone #{the olds
  taken in}}."
  [olds ground floors]
  (let [touched (conj (set floors) ground)
        gone (set (filter #(some touched %) olds))]
    {:floors (vec (concat (remove (set floors) (distinct (mapcat identity gone))) floors)) :gone gone}))

(defn hazards-why
  "The hazards in words: \"falling-block gravel at [x y z]; ...\"."
  [hazards]
  (str/join "; " (map (fn [h] (str (name (:reason h)) (some->> (:block h) (str " ")) (some->> (:at h) pr-str (str " at ")))) hazards)))

(defn known-floor
  "The first cut cell that is a floor of a stair the body made earlier (ways: the set of those floors) and is still
  solid as the body sees it now (a floor dug away since, or put back as air, no longer carries a way)."
  [block-at cut ways]
  (first (filter #(and (contains? ways %) (rules/solid-floor? block-at %)) cut)))

(defn stop-of
  "Why the step cannot go on, as {:reason ...detail}, or nil when every cell may be cut.
  in: the rules' input without :cell; its :unseen? (fn cell -> true when never sensed) makes a floor the body has not
  seen a stop :floor-unseen once the cut is open, not a guess. cells :bridged? true: the floor was placed by the stair, what is under it is not judged; :filler? true: air under a solid floor is no stop (the next floor is bridged); :up? true (a stair up or level): it never is (those dig no floor)."
  [{:keys [block-at plugs] :as in} {:keys [cut floor under bridged? up? filler? ways]} accept]
  (let [plug-cell (first (filter #(and (contains? plugs %) (not (rules/air (block-at %)))) cut))
        all-ways (into (set (:ways in)) ways)
        floor-cell (known-floor block-at cut all-ways)
        over-stand (when (and (contains? (set (map (fn [[x y z]] [x (inc y) z]) all-ways)) floor) (not (rules/solid-floor? block-at floor))) floor)
        fluid-cell (first (filter #(rules/fluids (block-at %)) cut))
        crop-cell (first (filter #(crop-names (block-at %)) cut))
        unseen-cell (when-let [unseen? (:unseen? in)]
                      (when (and (every? #(rules/air (block-at %)) cut) (unseen? floor)) floor))]
    (cond
      (some #(nil? (block-at %)) (conj cut floor under))
      {:reason :not-loaded :cell (first (filter #(nil? (block-at %)) (conj cut floor under)))}
      fluid-cell {:reason :fluid-in-cut :cell fluid-cell :fluid (block-at fluid-cell)}
      crop-cell {:reason :crop :cell crop-cell :block (block-at crop-cell)}
      plug-cell {:reason :plugged :cell plug-cell :block (block-at plug-cell)
                 :why "the cell was filled against water by this body; the water is behind it (:dig-plugs true digs it)"}
      floor-cell {:reason :undercuts-way :cell floor-cell :block (block-at floor-cell)
                  :why "the cell is the floor of a stair this body cut earlier; cutting it breaks the way"}
      over-stand {:reason :undercuts-way :cell over-stand :block (block-at over-stand)
                  :why "the floor of the step is a step of a stair this body cut earlier; flooring it would block the way"}
      unseen-cell {:reason :floor-unseen :cell unseen-cell}
      (not (rules/solid-floor? block-at floor)) {:reason :no-floor :cell floor :block (block-at floor)}
      (and (not bridged?) (not up?) (let [n (block-at under)] (or (and (rules/air n) (not filler?)) (rules/fluids n))))
      {:reason :cave-below :cell under :block (block-at under)}
      :else
      (some (fn [cell]
              (when-not (rules/air (block-at cell))
                (let [v (cell-verdict (assoc in :cell cell) cut)]
                  (cond
                    (not (:ok v)) (assoc (dissoc v :ok) :cell cell)
                    (not-every? (comp accept access/stair-hazard-key) (:hazards v))
                    {:reason :hazard :cell cell :hazards (:hazards v) :why (hazards-why (:hazards v))}))))
            cut))))

(defn exposed-lava
  "The lava cells a step lays open: lava beside the feet, the head or a cut cell that is open (air or fluid) now,
  then lava in one of those; sources behind first."
  [block-at feet cut]
  (let [open (into [feet (shape/add feet [0 1 0])] (filter #(let [n (block-at %)] (or (rules/air n) (rules/fluids n))) cut))
        open? (set open)
        lava? #(= "lava" (block-at %))
        beside (for [o open d rules/neighbour-deltas :let [n (shape/add o d)] :when (and (not (open? n)) (lava? n))] n)]
    (distinct (concat beside (filter lava? open)))))

(defn exposed-water
  "The cells this stair dug (memory :dug) among the cut that are open now and touch seen water, each with the water
  cell: [cell water], water in the cell itself counted. Empty when :fluid-adjacent is accepted."
  [block-at dug accept cut]
  (if (accept :fluid-adjacent)
    []
    (let [dug? (set (map :cell dug))]
      (vec (for [cell cut :when (dug? cell)
                 :let [n (block-at cell)]
                 :when (or (rules/air n) (rules/fluids n))
                 w (cons cell (map #(shape/add cell %) rules/neighbour-deltas))
                 :when (= "water" (block-at w))]
             [cell w])))))

(defn target-steps
  "Steps to cut from the args and the start feet, or {:error text}; :here marks an error only the feet cause."
  [{:keys [dir heading steps y]} [_ fy _]]
  (cond
    (not (rises dir)) {:error "dir must be :down, :up or :level"}
    (not (shape/headings heading)) {:error "heading must be :north :east :south or :west"}
    (and (int? steps) (pos? steps)) steps
    (and (int? y) (pos? (* (rises dir) (- y fy)))) (js/Math.abs (- y fy))
    (and (int? y) (not (zero? (rises dir)))) {:error "give :steps > 0, or :y beyond the feet in :dir" :here true}
    :else {:error "give :steps > 0, or :y beyond the feet in :dir"}))

(defn record-dug
  "Memory after a dig intent: the cell is recorded as dug when it no longer holds its block."
  [m block-at]
  (if-let [{:keys [cell block]} (:digging m)]
    (cond-> (dissoc m :digging)
      (not= block (block-at cell)) (update :dug (fnil conj []) {:cell cell :block block}))
    m))
