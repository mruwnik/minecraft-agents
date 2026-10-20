(ns jobs.farm.plant
  (:require [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [jobs.lib.job-common :as common]
            [jobs.farm.permit :as permit]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.farm.field :as fld]
            [jobs.farm.till :as till]
            [jobs.lib.farm :refer [bare-cells pick-seed sowable]]
            [jobs.lib.plan-field :as pf]
            [jobs.lib.world :as known]))

(def doc
  "Sow the bare farmland of a :box (a cell is bare when (x, min.y, z) is farmland and the block above is air).
  The seed is :seed, else the carried seed with the largest stack, kept while carried (one run sows one crop). Carrots
  and potatoes are sown only above the food reserve (jobs.lib.cost/food-reserve); harvest replants whatever the reserve.
  A missing seed is fetched before the run starts (jobs.lib.need, :fetch; a failed fetch ends the call with its ending as
  the :because), unless :fetch is false.
  One call plants the bare cells within the reach setting (eye reach), walking to the nearest (a go-to child) when none is. A cell whose place is
  refused or unreachable, or whose walk is blocked, three times is skipped.
  :plan (optionally :part) makes the plan's crop cells the field instead (:box and :seed are then unused); each bare
  planned cell is sown with the seed of the crop the plan wants there, when carried. Zones and other active plans'
  footprints are asked before choosing a cell and again before the place; a cell they refuse is left bare (one
  plant.declined warn, :reason :refused), :ignore-zones? true skips the check.
  Refused before it starts: impossible {:reason :bad-args :why}; failed conditions {:reason :no-zones}, {:reason
  :plan-trouble :why} (the plan is missing or unreadable: a wait; it has no crop cells: impossible, also later),
  :refused (every bare cell is refused), :food-reserve (a food-crop seed is carried but the reserve keeps it; nothing is
  fetched), {:reason :need ..} (no seed, :fetch false) and :nothing-to-do (no bare cell; dried-back dirt names :untilled n).
  Ends with data {:planted n :skipped [cells] :untilled [cells]} (plan mode: :refused [{:pos :reason}] and :short [seeds
  not carried] instead of :untilled). Done when every bare cell was sown, or some were and the number meets :least (cells,
  default 1); else condition-failed :need, :refused or :food-reserve for cells left without a seed or permission,
  nothing-possible :gave-up for skipped cells; none planted and nothing left, :nothing-to-do. done? counts the cells sown and the bare cells left.")

(a/defargs args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; the ground layer is y = (:y :min); required without :plan" :spec a/box? :default nil}
   :seed {:doc "item name to plant; the carried seed with the largest count when nil" :spec a/item? :default nil}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field (then :box and :seed are not used)" :spec a/name? :default nil}
   :part {:doc "with :plan, only the cells of this part" :spec a/name? :default nil}
   :fetch {:doc "get a missing seed (jobs.lib.need): true, a set of kinds or a map of limits; false refuses the call :need" :spec need/option? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::reach {:default u/eye-reach :doc "Cells whose centre is this close to the eye (place accepts 4.5) are planted without walking, in blocks." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::max-fails {:default 3 :doc "Failures before the job gives up." :spec (a/int-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-fails [] (settings/get settings ::max-fails))

;; ------------------------------------------------------------------ pure helpers

(defn untilled-cells
  "The cells of box whose ground layer is dirt, grass or a path (farmland that dried back, or never tilled), in x then z order."
  [p box]
  (let [{:keys [min max]} box
        y (:y min)]
    (vec (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))
               :let [pos {:x x :y y :z z}]
               :when (contains? till/tillable (u/seen-name p pos))]
           pos))))

(defn held-back
  "{:seed :held :kept :reserve-points} when no seed may be sown but a food-crop seed (seed, else any) is carried and the
  food reserve keeps it; else nil."
  [seed inventory]
  (when-not (pick-seed seed inventory)
    (let [carried (into {} (map (juxt :name :count)) inventory)
          kept (cost/food-reserve inventory)
          names (if seed [seed] (vals crops/seed-of))]
      (when-let [n (->> names (filter #(and (pos? (get carried % 0)) (pos? (get kept % 0))))
                        (sort-by #(- (carried %))) first)]
        {:seed n :held (carried n) :kept (kept n) :reserve-points cost/food-reserve-points}))))

(declare planned sowing)

(def job-sym 'jobs.farm.plant)
(def target [:box :plan :part])
(def defaults {:least 1 :spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(defn count-fail
  "Count a fail of kind k (:fails or :walk-fails) on the cell at pos; the cell is skipped at the max-fails-th."
  [m k pos]
  (crops/count-entry m k pos (max-fails) #(update %1 :skipped (fnil conj []) %2)))

(defn need-of
  "The :need cause for n seeds of a set of item names (non-empty)."
  [seeds n]
  (if (= 1 (count seeds))
    {:reason :need :item (first seeds) :count n}
    {:reason :need :any-of (vec (sort seeds)) :count n}))

(defn seed-need
  "The :need cause for n of the named seed, or of any seed."
  [seed n]
  (if seed
    {:reason :need :item seed :count n}
    {:reason :need :any-of (vec (sort (distinct (vals crops/seed-of)))) :count n}))

(defn sowable-cells
  "The ground cells of cells whose sowing (the cell above) the job may do; one warn when some are refused."
  [c cells]
  (let [ok (set (gate/allowed c :plant.declined "plant" :sow (map #(update % :y inc) cells) {:own-plans-ok? true}))]
    (filterv #(ok (update % :y inc)) cells)))

(defn unsown
  "The bare cells of the field now (a box's, or the plan's planned ones), skipped cells included."
  [c]
  (let [{:keys [box plan]} (:args c)
        p (:primitives c)]
    (cond
      plan (when-let [cells (:cells (planned c))] (vec (fld/planned-bare p cells)))
      box (bare-cells p box []))))

(defn seed-wanted
  "The cause {:reason :need ..} for the seeds a run that has not started lacks (bare cells, no sowable seed, none held
  back by the food reserve), else nil. A plan wants the seeds of its cells."
  [c]
  (when-not (:started (ctx/mem c))
    (let [p (:primitives c)
          inventory (u/inventory p)
          {:keys [seed plan box]} (:args c)]
      (if plan
        (when-let [cells (:cells (planned c))]
          (let [{:keys [ready short]} (sowing c cells)]
            (when (and (empty? ready) (seq short))
              (need-of short (count (fld/planned-bare p cells))))))
        (let [bare (when box (bare-cells p box (:skipped (ctx/mem c))))]
          (when (and (seq bare) (not (pick-seed seed inventory)) (not (held-back seed inventory)))
            (seed-need seed (count bare))))))))

(defn fetch-opts
  "The limits {:how :explore} for fetching seeds, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c job-sym)]
    (when (contains? (:what o) :item) (select-keys o [:how :explore]))))

(defn plan-problem [c]
  (let [{:keys [trouble cells]} (planned c)]
    (if trouble
      nil ; can-run? says it
      (let [{:keys [ready short]} (sowing c cells)]
        (cond
          (seq ready) nil
          (seq short) (when (nil? (fetch-opts c)) (seed-wanted c))
          (seq (fld/planned-bare (:primitives c) cells)) {:reason :refused}
          :else {:reason :nothing-to-do})))))

(defn box-problem [c]
  (let [{:keys [box seed]} (:args c)
        p (:primitives c)
        inventory (u/inventory p)
        bare (bare-cells p box [])
        untilled (untilled-cells p box)]
    (cond
      (empty? bare) (cond-> {:reason :nothing-to-do} (seq untilled) (assoc :untilled (count untilled)))
      (held-back seed inventory) (assoc (held-back seed inventory) :reason :food-reserve)
      (not (pick-seed seed inventory)) (when (nil? (fetch-opts c)) (seed-need seed (count bare)))
      (empty? (sowable-cells c bare)) {:reason :refused})))

(defn problem
  "Why the call cannot run now (before its first round), a cause map, or nil: no zone list read (unless :ignore-zones?),
  a plan that cannot be worked, and a field with no cell to sow or no seed to sow it with. Nil without :box and :plan
  (bad-args says so)."
  [c]
  (let [{:keys [box plan]} (:args c)]
    (when (and (or box plan) (not (:started (ctx/mem c))))
      (or (common/no-zones c :plant.declined "plant")
          (if plan (plan-problem c) (box-problem c))))))

(def bad-args (checks/args-fit "plant needs :box or :plan" #(let [{:keys [box plan]} (:args %)] (or box plan))))

(defn work
  "Ok with something to sow, else the problem's status; a field with no bare cell is a :look until the body has looked
  round, since cells may be unseen."
  [c]
  (if-let [cause (problem c)]
    (checks/cause-status (if (and (= :nothing-to-do (:reason cause)) (not (look/surveyed? c :cell))) :look :later)
                         cause)
    (contract/ok :work "a field to sow")))

(defn can-run? [c] (pf/plan-can-run? (planned c)))

(def can-start? (cond/and bad-args can-run? work))

(declare result-of)

(defn done?
  "Ok once no cell of the field is bare (skipped and refused ones included), no cell needs tilling, a look round has
  been made and the cells sown meet :least; else later with the cells sown and the bare cells left."
  [c]
  (let [{:keys [box plan]} (:args c)
        m (ctx/mem c)
        field (when plan (planned c))
        bare (count (unsown c))
        sown (:planted m 0)
        waiting (cond
                  (:trouble field) :plan-trouble
                  (pos? bare) :planting
                  (and box (seq (untilled-cells (:primitives c) box))) :untilled
                  (and (not (:started m)) (not (look/surveyed? c :cell))) :looking
                  (and (pos? sown) (not (common/enough? c sown sown))) :short)]
    (if waiting
      (contract/later waiting (str "plant: " sown " sown, " bare " bare") :units :cells :done sown :left bare :result (result-of c nil))
      (contract/ok :planted (str "plant done: " sown " sown, no bare cell left") :units :cells :done sown :left 0 :result (result-of c nil)))))

(defn quote-of [c]
  (let [n (count (unsown c))
        seed (pick-seed (:seed (:args c)) (u/inventory (:primitives c)))]
    {:uses (cond-> {:job-time [(* 0.5 n) (* 2 n) (* 10 n)]} (and seed (pos? n)) (assoc :items {seed [0 n n]}))
     :done [0 n n]}))

;; ------------------------------------------------------------------ plan mode

(defn planned
  "Without :plan nil; else {:cells {pos crop}}, or {:trouble text} (warned once per reason) when the plan cannot be
  worked."
  [c]
  (when (:plan (:args c))
    (pf/read-plan c {:cells fld/crop-cells :what "crop cells" :event :plant.declined :label "plant"})))

(defn sowing
  "How the planned bare cells divide: {:ready [{:pos :seed}] (seed carried, not skipped, permitted) :short #{seed}
  (carried none) :refused [{:pos :reason}]}. One plant.declined warn names what refuses the refused cells."
  [c cells]
  (let [plan (:plan (:args c))
        have (set (map key (filter (comp pos? val) (sowable (u/inventory (:primitives c))))))
        skipped (set (:skipped (ctx/mem c)))
        {:keys [short seeded]} (reduce (fn [acc {:keys [pos seed] :as debt}]
                                         (cond
                                           (skipped pos) acc
                                           (not (have seed)) (update acc :short conj seed)
                                           :else (update acc :seeded conj debt)))
                                       {:short #{} :seeded []}
                                       (fld/planned-bare (:primitives c) cells))
        ok (set (gate/allowed c :plant.declined "plant" :sow (map :pos seeded) {:except plan :own-plans-ok? true}))]
    (reduce (fn [acc {:keys [pos] :as debt}]
              (if (ok pos)
                (update acc :ready conj debt)
                (if-let [reason (permit/refusal c plan :sow pos {:own-plans-ok? true})]
                  (update acc :refused conj {:pos pos :reason reason})
                  acc)))
            {:ready [] :short short :refused []}
            seeded)))

(defn ^:async plan-place!
  "Place the seed of debt at its cell after asking the gate once more (the reader sowing uses); a refusal or a failure
  is counted against the cell."
  [c {:keys [pos seed]}]
  (if-not (gate/allowed? c :plant.declined "plant" :sow pos {:except (:plan (:args c)) :own-plans-ok? true})
    (ctx/update-mem! c count-fail :fails pos)
    (let [outcome (await (blocks/place-cell! c pos seed {:ignore-zones? true}))]
      (case outcome
        :continue :continue
        (:placed :already :occupied) (ctx/update-mem! c crops/inc-in :planted)
        (ctx/update-mem! c count-fail :fails pos)))))

(defn result-of
  "The data of the call's ending: what is sown and skipped so far (extra: the plan mode's :refused and :short), and in
  box mode the dirt cells that need tilling."
  [c extra]
  (let [m (ctx/mem c)
        untilled (when (:box (:args c)) (untilled-cells (:primitives c) (:box (:args c))))]
    (cond-> (merge {:planted (:planted m 0) :skipped (vec (:skipped m))} extra)
      (seq untilled) (assoc :untilled untilled))))

(defn conclude!
  "The ending of a run with no cell it can sow. cause: why cells are left bare (:gave-up for skipped ones, else a
  failed condition's cause), nil when none is. Done when every cell is sown, or some are and they meet :least."
  [c cause extra]
  (let [planted (:planted (ctx/mem c) 0)
        left (count (unsown c))
        data (result-of c extra)]
    (cond
      (and (nil? cause) (pos? planted)) (ctx/end c :done nil data)
      (and (pos? planted) (common/enough? c planted (+ planted left))) (ctx/end c :done nil data)
      (nil? cause) (ctx/end c :condition-failed :nothing-to-do data)
      (= :gave-up cause) (ctx/end c :nothing-possible :gave-up data)
      :else (ctx/end c :condition-failed cause data))))

(defn ^:async plan-round [c field]
  (when-not (:started (ctx/mem c))
    (ctx/update-mem! c assoc :started true))
  (let [{:keys [ready short refused]} (update (sowing c (:cells field)) :ready #(fld/above-reserve (u/inventory (:primitives c)) %))]
    (if (empty? ready)
      (conclude! c (cond (seq (:skipped (ctx/mem c))) :gave-up
                         (seq short) (need-of short (count (fld/planned-bare (:primitives c) (:cells field))))
                         (seq (fld/planned-bare (:primitives c) (:cells field))) :refused)
                 {:refused refused :short (vec (sort short))})
      (let [here (u/self-pos c)
            near (filterv #(<= (u/eye-dist here (:pos %)) (reach)) ready)
            target (first (sort-by #(u/dist here (:pos %)) ready))
            walked (when (empty? near) (await (common/walk-to! c (:pos target) 3)))]
        (case walked
          :continue :continue
          :blocked (do (ctx/update-mem! c count-fail :walk-fails (:pos target)) :again)
          (or (loop [todo (if (seq near) near [target])]
                (when (seq todo)
                  (if (= :continue (await (plan-place! c (first todo))))
                    :continue
                    (recur (rest todo)))))
              :again))))))

;; ------------------------------------------------------------------ box mode

(defn ^:async plant-one!
  "Place seed on cell and settle by the status: :no-seed when the seed is gone, else nil."
  [c cell seed]
  (when (seq (sowable-cells c [cell]))
    (let [outcome (await (blocks/place-cell! c (update cell :y inc) seed {:ignore-zones? true}))]
      (case outcome
        :continue :continue
        (:placed :already :occupied) (do (ctx/update-mem! c crops/inc-in :planted) nil)
        (:need :no-item) :no-seed
        (do (ctx/update-mem! c count-fail :fails cell) nil)))))

(defn ^:async plant-all!
  "Plant each cell in turn; :no-seed when the seeds ran out, else nil."
  [c cells seed]
  (loop [todo cells]
    (when (seq todo)
      (or (await (plant-one! c (first todo) seed))
          (recur (rest todo))))))

(defn ^:async box-round [c]
  (when-not (:started (ctx/mem c))
    (ctx/update-mem! c assoc :started true))
  (let [p (:primitives c)
        m (ctx/mem c)
        bare (bare-cells p (:box (:args c)) [])
        cells (sowable-cells c (bare-cells p (:box (:args c)) (:skipped m)))
        inventory (u/inventory p)
        kept (:sowing m)                ; the seed picked first is kept while carried: one run, one crop
        seed (if (and (nil? (:seed (:args c))) kept (pick-seed kept inventory))
               kept
               (pick-seed (:seed (:args c)) inventory))
        no-seed-cause #(if-let [held (held-back (:seed (:args c)) inventory)]
                         (assoc held :reason :food-reserve)
                         (seed-need (:seed (:args c)) (count cells)))]
    (when (and seed (not= seed kept))
      (ctx/update-mem! c assoc :sowing seed))
    (cond
      (empty? cells) (conclude! c (cond (seq (:skipped m)) :gave-up (seq bare) :refused) nil)
      (nil? seed) (conclude! c (no-seed-cause) nil)
      :else
      (let [here (u/self-pos c)
            near (filterv #(<= (u/eye-dist here %) (reach)) cells)
            target (fld/nearest p cells)
            walked (when (empty? near) (await (common/walk-to! c target 3)))]
        (case walked
          :continue :continue
          :blocked (do (ctx/update-mem! c count-fail :walk-fails target) :again)
          (case (await (plant-all! c (vec (take (max 1 (get (sowable inventory) seed 0)) (if (seq near) near [target]))) seed))
            :no-seed (conclude! c (no-seed-cause) nil)
            :continue :continue
            :again))))))

(defn ^:async fetch-seed!
  "Get the seeds a run that has not started lacks: nil when none is wanted, :continue while the fetch waits on the world,
  :again once it got them, else the call's ending (the fetch's kind and cause)."
  [c]
  (when-let [wanted (seed-wanted c)]
    (let [o (fetch-opts c)
          e (await (if (:item wanted)
                     (need/item! c (:item wanted) (:count wanted) o)
                     (need/blocks! c (:any-of wanted) (:count wanted) o)))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else (ctx/end c (:kind e) (:cause e) (result-of c nil))))))

(defn ^:async step [c]
  (let [field (planned c)
        m (ctx/mem c)
        unseen? (and (not (:trouble field)) (not (:started m)) (not (look/surveyed? c :cell))
                     (empty? (unsown c)))
        fetched (when-not unseen? (await (fetch-seed! c)))]
    (cond
      unseen? (await (look/survey! c :cell))
      fetched fetched
      (nil? field) (await (box-round c))
      (:trouble field) :continue
      :else (await (plan-round c field)))))

(defn ^:async round
  "The whole attempt: loop the steps until the field is sown or no seed or cell is left; :continue only while a walk or
  fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
