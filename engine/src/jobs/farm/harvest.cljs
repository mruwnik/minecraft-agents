(ns jobs.farm.harvest
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.gate :as gate]
            [engine.jobs.util :as u]))

(def doc
  "Cut the ripe crops within :radius of a centre (the body's position when the job first runs,
  kept in memory) and replant them. Each round takes the first step that applies: (1) replant:
  every cell in the replant debt that is still bare farmland is seeded from what is carried
  (within :reach, else walk to the nearest); (2) cut the ripe crops within :reach, else walk
  to the nearest, giving up on a crop that cannot be reached and, after :give-up of those,
  on cutting at all (a harvest.gave-up warn; replanting and collecting go on); (3) collect
  the drops once; (4) finish with a result {:cut :replanted :bare :gave-up}, cells that
  could not be replanted named in a harvest.bare warn. The debt {:pos :seed} of a cell is
  written to job memory before its dig, so a cut, a reflex or a restart never loses which
  cells owe a seed (the world only shows bare farmland, not which seed was there).

  With :plan (and optionally :part) the field is that plan's crop cells instead (wants {:crop c} of a crop
  harvest knows), read afresh every round and check: a cell is cut only when it is ripe and holds the crop
  the plan wants there, crops outside the plan are left standing, and every planned cell standing bare
  (air over farmland) owes the planned crop's seed (the plan is the debt; memory only keeps the counts of
  refused places). The check declines, with one harvest.declined warn naming the plan and the reason, while
  the plan is missing, unreadable or holds no crop cells; :assign in the plan is not read.

  Zones and claims are a rule the job consults: a crop (and a bare cell to replant) in a zone or claim of another
  owner, or in another plan's footprint, is not a candidate, left standing. One harvest.declined warn per job names
  the zones, claims and plans ({:reason :refused :zones :claims :plans}); without a zone list (zones.edn missing or
  never valid) it declines with {:reason :no-zones}. A job whose every crop is refused ends like one with none to
  cut. :ignore-zones? acts regardless.")

(def args
  {:radius {:doc "how far around the centre to harvest, in blocks" :default 12}
   :center {:doc "centre of the field; the body's position when the job first runs when nil" :default nil}
   :replant {:doc "replant what was cut" :default true}
   :crops {:doc "crop block names to cut; all known crops when nil" :default nil}
   :give-up {:doc "unreachable crops after which cutting stops" :default 4}
   :reach {:doc "cells whose centre is this close to the eye (dig and place accept 4.5) are worked without walking, in blocks" :default 4.2}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field (then :radius and :center are not used)" :default nil}
   :part {:doc "with :plan, only the cells of this part" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def ripe-age {"wheat" 7 "carrots" 7 "potatoes" 7 "beetroots" 3})

(def seed-of {"wheat" "wheat_seeds" "carrots" "carrot" "potatoes" "potato" "beetroots" "beetroot_seeds"})

(def collect-items ["wheat" "wheat_seeds" "carrot" "potato" "poisonous_potato" "beetroot" "beetroot_seeds"])

;; ------------------------------------------------------------------ pure helpers

(defn cell-pos [[x y z]] {:x x :y y :z z})

(defn crop-cells
  "{pos crop} of the cells of a plan answer (of part, when given) that want a crop harvest knows."
  [answer part]
  (into {} (keep (fn [{:keys [pos want] :as cell}]
                   (when (and (map? want) (ripe-age (:crop want)) (or (nil? part) (= part (:part cell))))
                     [(cell-pos pos) (:crop want)])))
        (:cells answer)))

(defn plan-trouble
  "Why a plan answer with these crop cells cannot be worked, or nil."
  [answer cells]
  (cond
    (nil? answer) "no such plan"
    (:broken answer) (str "the plan cannot be read: " (:broken answer))
    (empty? cells) "no crop cells"))

(defn ripe-at?
  "Whether p shows crop ripe at pos (nil, an unloaded cell, is not)."
  [p pos crop]
  (let [b (.blockAt p (clj->js pos))]
    (boolean (and b (= crop (.-name b)) (some-> (.-age b) (>= (ripe-age crop)))))))

(defn planned-ripe
  "The planned cells (cells {pos crop}) ripe with the crop the plan wants there, of a wanted crop, not in skipped,
  nearest to the body first."
  [p args cells skipped]
  (let [crops (set (or (:crops args) (keys ripe-age)))
        skipped (set skipped)
        here (u/pos-of (.-pos (.self p)))]
    (->> cells
         (filter (fn [[pos crop]] (and (crops crop) (not (skipped pos)) (ripe-at? p pos crop))))
         (map key)
         (sort-by #(u/dist here %)))))

(defn planned-bare
  "Debts {:pos :seed} of the planned cells standing bare: air over farmland."
  [p cells]
  (keep (fn [[pos crop]]
          (when (and (= "air" (u/block-name p pos)) (= "farmland" (u/block-name p (update pos :y dec))))
            {:pos pos :seed (seed-of crop)}))
        cells))

(defn sync-debts
  "m with :replant made the debts of bare (those given up as :bare excepted), each keeping the counts of its old debt,
  followed by the debts of cells this job cut (:cut) that bare no longer names: what it cut it still owes, also when
  the plan dropped the cell meanwhile."
  [m bare]
  (let [old (into {} (map (juxt :pos identity)) (:replant m))
        given-up (set (:bare m))
        planned (set (map :pos bare))]
    (assoc m :replant (vec (concat (for [{:keys [pos] :as debt} bare :when (not (given-up pos))]
                                     (merge (dissoc (old pos) :seed) debt))
                                   (filter #(and (:cut %) (not (planned (:pos %)))) (:replant m)))))))

(defn plan-field
  "[center radius] around cells {pos crop}: their mean and the farthest cell's distance from it, plus 2."
  [cells]
  (let [ps (keys cells)
        mean (fn [k] (js/Math.round (/ (reduce + (map k ps)) (count ps))))
        center {:x (mean :x) :y (mean :y) :z (mean :z)}]
    [center (+ 2 (js/Math.ceil (reduce max 0 (map #(u/dist center %) ps))))]))

(defn ripe-crops
  "Ripe crop cells of a wanted crop within :radius of center, not in skipped, nearest to the body first."
  [p args center skipped]
  (let [crops (or (:crops args) (keys ripe-age))
        skipped (set skipped)
        here (u/pos-of (.-pos (.self p)))
        radius (+ (:radius args) (u/dist here center))]
    (->> (array-seq (.blocks p #js {:radius radius :names (clj->js crops) :max 256}))
         (filter #(some-> (.-age %) (>= (ripe-age (.-name %)))))
         (map #(u/pos-of (.-pos %)))
         (filter #(<= (u/dist % center) (:radius args)))
         (remove skipped))))

(defn center-of
  "The centre: the :center arg, else the remembered one, else the body's position."
  [c]
  (or (:center (:args c)) (:center (ctx/mem c)) (u/self-pos c)))

(defn below [pos] (update pos :y dec))

(defn drop-debt [m pos]
  (update m :replant (fn [debts] (vec (remove #(= pos (:pos %)) debts)))))

(defn bare-debt [m pos]
  (-> (drop-debt m pos)
      (update :bare (fn [cells] (vec (distinct (conj (vec cells) pos)))))))

(defn skip-crop [m pos]
  (-> (drop-debt m pos)
      (update :skipped (fnil conj []) pos)
      (update :unreachable (fnil inc 0))))

(def max-place-fails 3)

(defn fail-debt
  "Count a refused place on the debt at pos; the cell is bare once it has failed max-place-fails times."
  [m pos]
  (let [fails (inc (or (:fails (first (filter #(= pos (:pos %)) (:replant m)))) 0))]
    (if (>= fails max-place-fails)
      (bare-debt m pos)
      (update m :replant (fn [debts] (mapv #(if (= pos (:pos %)) (assoc % :fails fails) %) debts))))))

(def eye-height 1.62)

(defn eye-dist
  "Distance from the eye of a body at feet position here to the centre of cell, the measure dig and place accept up to 4.5."
  [here cell]
  (u/dist {:x (:x here) :y (+ (:y here) eye-height) :z (:z here)}
          {:x (+ (:x cell) 0.5) :y (+ (:y cell) 0.5) :z (+ (:z cell) 0.5)}))

(def failed-reach 3.0)

(defn debt-reach
  "The eye-to-centre distance within which debt is placed without walking: a cell that was refused once needs the body closer."
  [debt reach]
  (if (pos? (:fails debt 0)) failed-reach reach))

(defn walk-range
  "The range walked to when out of reach: 3, or 2 for a debt that was refused."
  [debt]
  (if (pos? (:fails debt 0)) 2 3))

(def max-walk-fails 3)

(defn walk-fail-crop
  "Count a blocked walk to the crop at pos in :walk-fails [{:pos :n}]; the crop is skipped at the max-walk-fails-th."
  [m pos]
  (let [n (inc (or (:n (first (filter #(= pos (:pos %)) (:walk-fails m)))) 0))]
    (if (>= n max-walk-fails)
      (skip-crop m pos)
      (update m :walk-fails (fn [entries] (conj (vec (remove #(= pos (:pos %)) entries)) {:pos pos :n n}))))))

(defn walk-fail-debt
  "Count a blocked walk to the debt at pos; the cell is bare at the max-walk-fails-th."
  [m pos]
  (let [n (inc (or (:walk-fails (first (filter #(= pos (:pos %)) (:replant m)))) 0))]
    (if (>= n max-walk-fails)
      (bare-debt m pos)
      (update m :replant (fn [debts] (mapv #(if (= pos (:pos %)) (assoc % :walk-fails n) %) debts))))))

(defn inc-in [m k] (update m k (fnil inc 0)))

(defn sort-debts
  "{:owed [debts to plant] :gone [pos no longer to plant] :bare [pos with no farmland]} for the debts, read off p."
  [p debts]
  (reduce
   (fn [acc {:keys [pos] :as debt}]
     (let [here (u/block-name p pos)]
       (cond
         (nil? here) (update acc :owed conj debt)
         (not= "air" here) (update acc :gone conj pos)
         (not= "farmland" (u/block-name p (below pos))) (update acc :bare conj pos)
         :else (update acc :owed conj debt))))
   {:owed [] :gone [] :bare []}
   debts))

(defn ^:async walk!
  "Walk until within range of pos: :there, :partial (closer, call again), :no-path (the pathfinder
  found none) or :blocked (no progress, maybe only this time)."
  [c pos range]
  (if (u/within? (u/self-pos c) pos range)
    :there
    (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))
          no-path? (= "noPath" (.-reason r))]
      (case (.-status r)
        "arrived" :there
        "partial" (if no-path? :no-path :partial)
        "blocked" (if no-path? :no-path :blocked)
        :blocked))))

(defn carried-names [p] (set (map :name (u/inventory p))))

(defn nearest [p cells]
  (let [here (u/pos-of (.-pos (.self p)))]
    (first (sort-by #(u/dist here %) cells))))

(defn permitted
  "The poss the job may act on with action: zones, claims and the footprints of plans but its own (see engine.jobs.gate)."
  [c action poss]
  (gate/allowed c :harvest.declined "harvest" action poss {:except (:plan (:args c))}))

(defn cut-action
  "The zone action asked for cutting a crop: :harvest in box mode; with :plan :dig, the action the plan jobs (farm.tend)
  ask for a cut."
  [c]
  (if (:plan (:args c)) :dig :harvest))

(defn ripe-of
  "The ripe crops to cut: the planned ones with :plan, else those within :radius of the centre; none that zones,
  claims or other plans' footprints refuse."
  [c skipped]
  (permitted c (cut-action c)
             (if-let [cells (:plan-cells c)]
               (planned-ripe (:primitives c) (:args c) cells skipped)
               (ripe-crops (:primitives c) (:args c) (center-of c) skipped))))

(defn planned
  "Without :plan nil; else {:cells {pos crop}}, or {:trouble text} (warned once per reason) when the plan
  cannot be worked."
  [c]
  (when-let [id (:plan (:args c))]
    (let [part (:part (:args c))
          answer (ctx/plan c id)
          cells (crop-cells answer part)
          trouble (plan-trouble answer cells)]
      (if-not trouble
        {:cells cells}
        (do (ctx/warn-once! c [id trouble] :harvest.declined
                            {:plan id :part part :reason trouble
                             :text (str "harvest declines plan " id (when part (str " part " part)) ": " trouble)})
            {:trouble trouble})))))

(defn with-field
  "c over the field of planned (nil: unchanged): :plan-cells set, :center and :radius of the args around them."
  [c field]
  (if-let [cells (:cells field)]
    (let [[center radius] (plan-field cells)]
      (-> c
          (assoc :plan-cells cells)
          (update :args assoc :center center :radius radius)))
    c))

(defn permitted-debts
  "The debts whose cell the job may sow."
  [c debts]
  (let [ok (set (permitted c :sow (map :pos debts)))]
    (filterv #(ok (:pos %)) debts)))

(defn sowable
  "The planned bare cells whose seed is carried, unless replanting is off or the cell was given up."
  [c]
  (when (:replant (:args c))
    (let [have (carried-names (:primitives c))
          given-up (set (:bare (ctx/mem c)))]
      (->> (planned-bare (:primitives c) (:plan-cells c))
           (filter #(and (have (:seed %)) (not (given-up (:pos %)))))
           (permitted-debts c)))))

(defn gave-up? [c]
  (>= (:unreachable (ctx/mem c) 0) (:give-up (:args c))))

(defn cutting? [c]
  (not (gave-up? c)))

(defn check
  "A debt is owed (with :plan: a planned bare cell whose seed is carried), a collect sweep is owed, or there
  is a ripe crop and cutting has not been given up. A job that has begun (its :center is in memory) always
  passes, so that the round that finishes it runs; with :plan only while the plan can be worked."
  [c]
  (let [field (planned c)
        c (with-field c field)
        m (ctx/mem c)]
    (boolean
     (and (not (:trouble field))
          (or (:center m)
              (if field (seq (sowable c)) (seq (:replant m)))
              (:collect m)
              (and (cutting? c) (seq (ripe-of c (:skipped m)))))))))

;; ------------------------------------------------------------------ steps

(defn drop-settled!
  "Forget the debts whose cell is no longer bare air, move the farmland-less ones to :bare; the owed debts."
  [c]
  (let [{:keys [owed gone bare]} (sort-debts (:primitives c) (:replant (ctx/mem c)))]
    (when (or (seq gone) (seq bare))
      (ctx/update-mem! c (fn [m] (reduce bare-debt (reduce drop-debt m gone) bare))))
    owed))

(defn ^:async plant-one!
  "Place the seed of debt at its cell and settle the debt by the status."
  [c {:keys [pos seed]}]
  (if-not (seq (permitted c :sow [pos]))
    (ctx/update-mem! c bare-debt pos)
    (let [r (await (ctx/act c :place (clj->js {:pos pos :item seed})))]
      (case (.-status r)
        ("placed" "occupied") (ctx/update-mem! c (fn [m] (-> (drop-debt m pos)
                                                              (inc-in :replanted)
                                                              (update :planted (fn [cells] (vec (distinct (conj (vec cells) pos))))))))
        ("no-item" "unreachable") nil
        (ctx/update-mem! c fail-debt pos)))))

(defn ^:async replant!
  "Step 1: :continue when a seed was planted or a walk made, else nil."
  [c]
  (let [p (:primitives c)
        owed (drop-settled! c)
        have (carried-names p)
        plantable (permitted-debts c (filterv #(have (:seed %)) owed))]
    (when (seq plantable)
      (let [here (u/self-pos c)
            reach (:reach (:args c))
            near (filterv #(<= (eye-dist here (:pos %)) (debt-reach % reach)) plantable)
            nearest-debt (first (sort-by #(u/dist here (:pos %)) plantable))
            walked (when (empty? near) (await (walk! c (:pos nearest-debt) (walk-range nearest-debt))))
            targets (if (seq near) near [nearest-debt])]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c bare-debt (:pos (first targets))) :continue)
          :blocked (do (ctx/update-mem! c walk-fail-debt (:pos (first targets))) :continue)
          (loop [todo targets]
            (if (empty? todo)
              :continue
              (do (await (plant-one! c (first todo)))
                  (recur (rest todo))))))))))

(defn ^:async cut-cell!
  "Write the debt of the crop at pos, dig it, and settle by the status."
  [c pos]
  (when (seq (permitted c (cut-action c) [pos]))
    (let [seed (get seed-of (u/block-name (:primitives c) pos))]
      (when (and (:replant (:args c)) seed)
        (ctx/update-mem! c update :replant (fnil conj []) {:pos pos :seed seed :cut true}))
      (let [r (await (ctx/act c :dig (clj->js {:pos pos})))]
        (case (.-status r)
          "dug" (ctx/update-mem! c inc-in :cut)
          "missing" nil
          (ctx/update-mem! c skip-crop pos))))))

(defn warn-gave-up! [c]
  (let [m (ctx/mem c)]
    (when (and (gave-up? c) (not (:warned m)))
      (ctx/update-mem! c assoc :warned true)
      (ctx/emit! c :harvest.gave-up :warn
                 {:unreachable (:unreachable m) :skipped (:skipped m)
                  :text (str "harvest gave up cutting after " (:unreachable m) " unreachable crops: "
                             (str/join ", " (map pr-str (:skipped m))))}))))

(defn still-planned
  "The targets the plan, read again now (a walk may have taken a while), still wants with the crop it wanted when the
  round began; all of them without :plan."
  [c targets]
  (if-let [was (:plan-cells c)]
    (let [now (:cells (planned c))]
      (filterv #(= (get was %) (get now %)) targets))
    targets))

(defn ^:async cut!
  "Step 2: :continue when it cut or walked, else nil."
  [c]
  (let [p (:primitives c)
        m (ctx/mem c)
        ripe (when (cutting? c) (ripe-of c (:skipped m)))]
    (when (seq ripe)
      (let [here (u/self-pos c)
            near (filterv #(<= (eye-dist here %) (:reach (:args c))) ripe)
            walked (when (empty? near) (await (walk! c (first ripe) 3)))
            targets (if (seq near) near [(first ripe)])]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c skip-crop (first ripe))
                       (warn-gave-up! c)
                       :continue)
          :blocked (do (ctx/update-mem! c walk-fail-crop (first ripe))
                       (warn-gave-up! c)
                       :continue)
          (do (ctx/update-mem! c assoc :collect true)
              (loop [todo (still-planned c targets)]
                (when (seq todo)
                  (await (cut-cell! c (first todo)))
                  (recur (rest todo))))
              (warn-gave-up! c)
              :continue))))))

(def max-home-fails 3)

(defn ^:async home!
  "Step 2b: a body farther than :radius from the centre with debts or a sweep owed walks back
  (range radius/2): :continue, or :finish after max-home-fails walks that went nowhere; nil when
  there is nothing to do."
  [c]
  (let [m (ctx/mem c)
        center (center-of c)
        radius (:radius (:args c))]
    (when (and (or (seq (:replant m)) (:collect m))
               (> (u/dist (u/self-pos c) center) radius))
      (if (>= (:home-fails m 0) max-home-fails)
        :finish
        (case (await (walk! c center (quot radius 2)))
          (:there :partial) :continue
          (do (ctx/update-mem! c inc-in :home-fails) :continue))))))

(defn ^:async collect!
  "Step 3: one collect-drops round while a sweep is owed."
  [c]
  (when (:collect (ctx/mem c))
    (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                   {:radius (+ (:radius (:args c)) 4) :filter collect-items}))]
      (when (= :done r) (ctx/update-mem! c dissoc :collect))
      :continue)))

(defn lost-cells
  "The planted cells whose block is no longer a crop (nil, an unloaded cell, is not counted)."
  [p planted]
  (filterv #(let [n (u/block-name p %)] (and n (not (ripe-age n)))) planted))

(defn finish!
  "Step 4: the debts still owed and the planted cells that are no longer crops are bare; warn, report and be done."
  [c]
  (let [m (ctx/mem c)
        lost (lost-cells (:primitives c) (:planted m))
        bare (vec (distinct (concat (:bare m) (map :pos (:replant m)) lost)))
        result {:cut (:cut m 0) :replanted (:replanted m 0) :bare bare :lost lost :gave-up (gave-up? c)}]
    (when (seq bare)
      (ctx/emit! c :harvest.bare :warn
                 {:cells bare :lost lost
                  :text (str "harvest could not replant " (count bare) " cells: " (str/join ", " (map pr-str bare))
                             (when (seq lost) (str "; lost after replanting: " (str/join ", " (map pr-str lost)))))}))
    (ctx/emit! c :harvest.done :info
               {:cut (:cut result) :replanted (:replanted result) :bare-count (count bare) :gave-up (:gave-up result)
                :text (str "harvest done: cut " (:cut result) ", replanted " (:replanted result) ", bare " (count bare))})
    (ctx/result! c result)
    :done))

(defn sync-plan-debts!
  "With :plan and replanting, the debts are the planned cells standing bare (written only when they change)."
  [c]
  (when (and (:plan-cells c) (:replant (:args c)))
    (let [m (ctx/mem c)
          synced (sync-debts m (planned-bare (:primitives c) (:plan-cells c)))]
      (when (not= (:replant m) (:replant synced))
        (ctx/update-mem! c assoc :replant (:replant synced))))))

(defn ^:async work
  "One round over the field of c."
  [c]
  (when-not (:center (ctx/mem c))
    (ctx/update-mem! c assoc :center (center-of c)))
  (sync-plan-debts! c)
  (or (await (replant! c))
      (await (cut! c))
      (let [home (await (home! c))]
        (case home
          :finish (finish! c)
          :continue :continue
          (or (await (collect! c))
              (finish! c))))))

(defn ^:async round [c]
  (let [field (planned c)]
    (if (:trouble field)
      :declined
      (await (work (with-field c field))))))
