(ns jobs.farm.harvest
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.gate :as gate]
            [jobs.lib.crops :as crops]
            [jobs.lib.look :as look]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.world :as known]))

(def doc
  "Cut the ripe crops within :radius of a centre and replant them, then collect the drops.
  The centre is :center, or the body's position when the job first runs (kept in memory).
  One call is the whole run: it does the first step that applies, again and again, until it finishes:
  1. Replant: seed the bare farmland of cells it cut, from the seeds carried.
  2. Cut the ripe crops, walking to the nearest when out of :reach. A crop it cannot reach is skipped.
     After :give-up such crops it stops cutting (warn harvest.gave-up); replanting and collecting go on.
  3. Collect the drops.
  4. Finish.
  With no crop seen at all (ripe or not) it looks around once from where it stands first; if still none it ends
  :stopped :no-crop-seen (warn harvest.no-crop-seen).
  Result: {:cut :replanted :bare :lost :gave-up}. Cells that could not be replanted, or stopped being a crop
  afterwards, are named in a harvest.bare warn.
  Each cell it cuts is written to memory with its seed before the dig, because bare farmland does not show
  which seed was there. A restart or reflex in between loses nothing.
  :plan (optionally :part) makes the plan's crop cells the field instead (:radius and :center are then unused).
  The plan is read again every step. A cell is cut only when ripe and holding the crop the plan wants there.
  Crops outside the plan are left standing. A planned cell standing bare is sown unless :replant-bare is false,
  food seeds only above the food reserve. A cell it cut is always replanted.
  The job declines (one harvest.declined warn naming the plan and the reason) while the plan is missing,
  unreadable or has no crop cells.
  Zones: a crop or bare cell in another owner's zone or claim, or in another plan's footprint, is left alone.
  The job warns harvest.declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.")

(def args
  {:radius {:doc "how far around the centre to harvest, in blocks" :default 12}
   :center {:doc "centre of the field; the body's position when the job first runs when nil" :type :pos :default nil}
   :replant {:doc "replant what was cut" :default true}
   :replant-bare {:doc "with :plan, also sow the planned cells that stood bare before; false: only the cells this run cut" :default true}
   :crops {:doc "crop block names to cut; all known crops when nil" :default nil}
   :give-up {:doc "unreachable crops after which cutting stops" :default 4}
   :reach {:doc "cells whose centre is this close to the eye (dig and place accept 4.5) are worked without walking, in blocks" :default u/eye-reach}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field (then :radius and :center are not used)" :default nil}
   :part {:doc "with :plan, only the cells of this part" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def ripe-age crops/ripe-age)

(def seed-of crops/seed-of)

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
  (let [b (u/block-at p pos)]
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

(defn sowable-counts
  "{name count} of the carried items that may be sown: food crops only above the food reserve, the rest in full."
  [inventory]
  (let [kept (cost/food-reserve inventory)
        totals (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} inventory)]
    (reduce-kv (fn [acc name n] (assoc acc name (- n (get kept name 0)))) {} totals)))

(defn above-reserve
  "The debts of bare cells without those whose seed is carried but held back by the food reserve."
  [inventory debts]
  (let [carried (into {} (map (juxt :name :count)) inventory)
        left (volatile! (sowable-counts inventory))]
    (filterv (fn [{:keys [seed]}]
               (or (not (pos? (get carried seed 0)))
                   (when (pos? (get @left seed 0))
                     (vswap! left update seed dec)
                     true)))
             debts)))

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
    (->> (crops/seen-crops p crops radius 4096)
         (filter #(some-> (:age %) (>= (ripe-age (:name %)))))
         (map :pos)
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
  (crops/count-debt m :fails pos max-place-fails bare-debt))

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
  (crops/count-entry m :walk-fails pos max-walk-fails skip-crop))

(defn walk-fail-debt
  "Count a blocked walk to the debt at pos; the cell is bare at the max-walk-fails-th."
  [m pos]
  (crops/count-debt m :walk-fails pos max-walk-fails bare-debt))

(def inc-in crops/inc-in)

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
  "Walk until within range of pos (near/go-near!, doors :shut): :there, :partial (closer or an escalation waiting, call
  again) or :blocked (it gave up, maybe only this time, or no pathWorld sensing)."
  [c pos range]
  (near/go-near! c pos range {:tolls (tc/walk-tolls c (near/cell-of pos))}))

(defn carried-names [p] (set (map :name (u/inventory p))))

(defn nearest [p cells]
  (let [here (u/pos-of (.-pos (.self p)))]
    (first (sort-by #(u/dist here %) cells))))

(defn permitted
  "The poss the job may act on with action: zones, claims and the footprints of plans but its own (see jobs.lib.gate)."
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
          answer (known/plan c id)
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
  (when (and (:replant (:args c)) (:replant-bare (:args c)))
    (let [have (carried-names (:primitives c))
          given-up (set (:bare (ctx/mem c)))]
      (->> (planned-bare (:primitives c) (:plan-cells c))
           (filter #(and (have (:seed %)) (not (given-up (:pos %)))))
           (above-reserve (u/inventory (:primitives c)))
           (permitted-debts c)))))

(defn gave-up? [c]
  (>= (:unreachable (ctx/mem c) 0) (:give-up (:args c))))

(defn cutting? [c]
  (not (gave-up? c)))

(defn crop-seen?
  "Whether any crop of a wanted name within :radius of the centre has been seen, ripe or not (box mode)."
  [c]
  (let [{:keys [crops radius]} (:args c)]
    (boolean (seq (crops/seen-crops (:primitives c) (or crops (keys ripe-age)) (+ radius (u/dist (u/self-pos c) (center-of c))) 1)))))

(defn blind?
  "Box mode with no crop seen yet and no look from this cell: the round must look around before it can tell."
  [c]
  (and (cutting? c) (not (:plan-cells c)) (not (look/looked-here? c)) (not (crop-seen? c))))

(defn check
  "A debt is owed (with :plan: a planned bare cell whose seed is carried), a collect sweep is owed, or there
  is a ripe crop and cutting has not been given up. A job that has begun (its :center is in memory) always
  passes, so that the round that finishes it runs; with :plan only while the plan can be worked."
  [c]
  (let [field (planned c)
        c (with-field c field)
        m (ctx/mem c)]
    (cond
      (:trouble field) (ctx/wait c {:reason :plan-trouble :why (:trouble field)})
      (or (:center m)
          (if field (or (seq (sowable c)) (seq (:replant m))) (seq (:replant m)))
          (:collect m)
          (and (cutting? c) (seq (ripe-of c (:skipped m))))
          (and (not field) (blind? c))) true
      :else (gate/wait-unless-set c {:reason :nothing-to-do}))))

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
        (ctx/update-mem! c fail-debt pos)))))

(def max-per-round
  "Most crops one step cuts or plants; the rest wait for the next step."
  8)

(defn ^:async replant!
  "Step 1: :again when a seed was planted or a walk made, else nil."
  [c]
  (let [p (:primitives c)
        owed (drop-settled! c)
        have (carried-names p)
        plantable (permitted-debts c (filterv #(have (:seed %)) owed))]
    (when (seq plantable)
      (let [here (u/self-pos c)
            reach (:reach (:args c))
            near (filterv #(<= (u/eye-dist here (:pos %)) (debt-reach % reach)) plantable)
            nearest-debt (first (sort-by #(u/dist here (:pos %)) plantable))
            walked (when (empty? near) (await (walk! c (:pos nearest-debt) (walk-range nearest-debt))))
            targets (if (seq near) near [nearest-debt])]
        (case walked
          :partial :continue
          :blocked (do (ctx/update-mem! c walk-fail-debt (:pos (first targets))) :again)
          (loop [todo (take max-per-round targets)]
            (if (empty? todo)
              :again
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
  "Step 2: :again when it cut or walked, else nil."
  [c]
  (let [p (:primitives c)
        m (ctx/mem c)
        _ (when (and (cutting? c) (empty? (ripe-of c (:skipped m))) (not (look/looked-here? c)))
            (await (look/look-around! c)))
        ripe (when (cutting? c) (ripe-of c (:skipped m)))]
    (when (seq ripe)
      (let [here (u/self-pos c)
            near (filterv #(<= (u/eye-dist here %) (:reach (:args c))) ripe)
            walked (when (empty? near) (await (walk! c (first ripe) 3)))
            targets (if (seq near) near [(first ripe)])]
        (case walked
          :partial :continue
          :blocked (do (ctx/update-mem! c walk-fail-crop (first ripe))
                       (warn-gave-up! c)
                       :again)
          (do (ctx/update-mem! c assoc :collect true)
              (loop [todo (take max-per-round (still-planned c targets))]
                (when (seq todo)
                  (await (cut-cell! c (first todo)))
                  (recur (rest todo))))
              (warn-gave-up! c)
              :again))))))

(def max-home-fails 3)

(defn ^:async home!
  "Step 2b: a body farther than :radius from the centre with debts or a sweep owed walks back
  (range radius/2): :again, or :finish after max-home-fails walks that went nowhere; nil when
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
          :there :again
          :partial :continue
          (do (ctx/update-mem! c inc-in :home-fails) :again))))))

(defn ^:async collect!
  "Step 3: one collect-drops round while a sweep is owed."
  [c]
  (when (:collect (ctx/mem c))
    (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                   {:radius (+ (:radius (:args c)) 4) :filter collect-items}))]
      (when (#{:done :declined} r) (ctx/update-mem! c dissoc :collect))
      (if (= :continue r) :continue :again))))

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
        no-crop? (and (not (:plan-cells c)) (zero? (:cut m 0)) (empty? (:replant m)) (not (crop-seen? c)))
        result {:cut (:cut m 0) :replanted (:replanted m 0) :bare bare :lost lost :gave-up (gave-up? c)}]
    (when no-crop?
      (ctx/emit! c :harvest.no-crop-seen :warn
                 {:text (str "harvest saw no crop within " (:radius (:args c)) " of " (pr-str (center-of c)) " after looking around")}))
    (when (seq bare)
      (ctx/emit! c :harvest.bare :warn
                 {:cells bare :lost lost
                  :text (str "harvest could not replant " (count bare) " cells: " (str/join ", " (map pr-str bare))
                             (when (seq lost) (str "; lost after replanting: " (str/join ", " (map pr-str lost)))))}))
    (when-not no-crop?
      (ctx/emit! c :harvest.done :info
               {:cut (:cut result) :replanted (:replanted result) :bare-count (count bare) :gave-up (:gave-up result)
                :text (str "harvest done: cut " (:cut result) ", replanted " (:replanted result) ", bare " (count bare))}))
    (if no-crop?
      (result/stop! c :no-crop-seen (str "harvest saw no crop within " (:radius (:args c)) " of " (pr-str (center-of c)))
                    :cut 0 :replanted 0 :bare bare :lost lost :gave-up (:gave-up result))
      (do (ctx/result! c result)
          :done))))

(defn sync-plan-debts!
  "With :plan and replanting, the debts are the planned cells standing bare (written only when they change)."
  [c]
  (when (and (:plan-cells c) (:replant (:args c)))
    (let [m (ctx/mem c)
          bare (when (:replant-bare (:args c))
                 (above-reserve (u/inventory (:primitives c)) (planned-bare (:primitives c) (:plan-cells c))))
          synced (sync-debts m bare)]
      (when (not= (:replant m) (:replant synced))
        (ctx/update-mem! c assoc :replant (:replant synced))))))

(defn ^:async work
  "One step over the field of c: :again after a unit of work, :continue while a walk waits on the world."
  [c]
  (when-not (:center (ctx/mem c))
    (ctx/update-mem! c assoc :center (center-of c)))
  (sync-plan-debts! c)
  (or (await (replant! c))
      (await (cut! c))
      (let [home (await (home! c))]
        (case home
          :finish (finish! c)
          (:again :continue) home
          (or (await (collect! c))
              (finish! c))))))

(defn ^:async step [c]
  (let [field (planned c)]
    (if (:trouble field)
      :declined
      (await (work (with-field c field))))))

(defn ^:async round
  "The whole attempt: loop the steps (replant, cut, walk home, sweep) until the field is done; :continue only while a
  walk or the sweep waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
