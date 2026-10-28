(ns jobs.farm.harvest
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.job-common :as common]
            [jobs.farm.field :as fld]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.access :as access]
            [jobs.lib.gate :as gate]
            [jobs.lib.crops :as crops]
            [jobs.lib.farm :refer [work-s]]
            [jobs.lib.look :as look]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.spent :as spent]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.plan-field :as pf]
            [jobs.lib.field-area :as fa]
            [jobs.lib.world :as known]))

(def meta {:tags [:goal]})

(def doc
  "Cut the ripe crops of the :area and replant them, then collect the drops. The area is a box (only crops inside it are cut), a circle {:pos :radius}
  (a sphere) or nil: a circle of radius 12 round the body's position when the job first runs (kept in memory). With {:plan id}
  (optionally :part) the plan's crop cells are the field instead, read again every step: a cell is cut only when ripe and
  holding the crop the plan wants, a planned bare cell is sown unless :replant-bare is false (food seeds only above the food
  reserve), crops outside the plan stand.
  Needs: a zone list (unless :ignore-zones?), seeds carried to replant. Another owner's zone, claim or plan footprint is left
  alone; :ignore-zones? skips the check.
  One call is the whole run, doing the first step that applies until it finishes:
  1. Replant the bare farmland of cells it cut (what it cuts is written to memory with its seed before the dig).
  2. Cut the ripe crops, walking (a go-to child) to the nearest when out of the reach setting; a shut-in body walks out first.
     A body farther than the area's radius (a box's covering sphere) from the centre with nothing cut walks there first. After the give-up setting such crops
     stop it cutting (warn harvest.gave-up); replanting and collecting go on.
  3. Collect the drops (a jobs.items.collect-drops child).
  With no crop seen (or, with :plan, none known ripe while planned crops are out of view) it looks around once first.
  Refused before it starts as failed conditions: {:reason :no-zones}, {:reason :plan-trouble :why} (missing or unreadable plan: a
  wait; no crop cells: impossible), :refused (every ripe crop is refused), :nothing-to-do (no ripe crop, nothing owed).
  Ends with data {:cut :replanted :bare :lost :gave-up :spent :spent-at}: cells not replanted, or no longer a crop, are in :bare
  and :lost. Done when it cut or replanted some and the number meets :least (cells, default 1; :all wants every ripe crop it
  found); no crop seen: condition-failed :no-crop-seen; crops given up on: nothing-possible :gave-up; fewer than :least cut with
  nothing left: nothing-possible :partial; none cut: condition-failed :nothing-to-do; a field the body could not walk to (:max-home-fails walks) with none cut: nothing-possible :unreachable {:center}. done? holds once no ripe crop, owed seed,
  sweep or given-up cell is left (and :least is met): a field with nothing ripe at the start ends :done :already.")

(a/defargs args
  {:area {:doc "the field: a box {:min :max} (corners inclusive, the ground layer is the lowest y and the crops one above it are inside; only crops inside it are cut), a circle {:pos :radius} (a sphere) or {:plan id :part name} (the plan's crop cells); nil: a circle of radius 12 round the body's position when the job first runs" :spec fa/spec :default nil}
   :replant {:doc "replant what was cut" :spec boolean? :default true}
   :replant-bare {:doc "with :plan, also sow the planned cells that stood bare before; false: only the cells this run cut" :spec boolean? :default true}
   :crops {:doc "crop block names to cut; all known crops when nil" :spec (a/coll-of a/name?) :default nil}
   :ignore-zones? :shared})

(def ripe-age crops/ripe-age)

(def seed-of crops/seed-of)

(def collect-items crops/produce-items)

;; ------------------------------------------------------------------ pure helpers

(defn still-ripe
  "The targets p shows ripe now (a ripe reading may be remembered and stale; the cell is in view after the walk)."
  [p targets]
  (filterv #(some->> (u/seen-name p %) (fld/ripe-at? p %)) targets))

(def default-radius 12)

(defn field-sphere
  "[center radius] of the field: the plan's cells' sphere (set by with-field), else the area's; nil for neither."
  [c]
  (or (:sphere c) (fa/sphere (fa/of c))))

(defn center-of
  "The centre of the field, else the remembered one (a bare ctx of a quote test has no job memory), else the
  body's position."
  [c]
  (or (first (field-sphere c)) (when (:view c) (:center (ctx/mem c))) (u/self-pos c)))

(defn radius-of [c] (or (second (field-sphere c)) default-radius))

(a/defargs settings
  {::give-up {:default 4 :doc "Unreachable crops after which cutting stops." :spec (a/int-in 1 nil)}
   ::reach {:default u/eye-reach :doc "Cells whose centre is this close to the eye (dig and place accept 4.5) are worked without walking, in blocks." :spec (a/num-in 0 nil)}
   ::max-place-fails {:default 3 :doc "Failed placements before the job gives up." :spec (a/int-in 0 nil)}
   ::failed-reach {:default 3.0 :doc "Reach for a block a failed attempt left, in blocks." :spec (a/num-in 0 nil)}
   ::max-walk-fails {:default 3 :doc "Failed walks before the job gives up." :spec (a/int-in 0 nil)}
   ::max-home-fails {:default 3 :doc "Failed walks home before the job gives up." :spec (a/int-in 0 nil)}
   ::max-per-round {:default 8 :doc "Most crops one step cuts or plants; the rest wait for the next step." :spec (a/int-in 1 nil)}
   ::walk-range {:default 3 :doc "How near a crop or a cell to sow the body walks when out of reach, in blocks." :spec (a/int-in 0 nil)}
   ::failed-walk-range {:default 2 :doc "The walk range for a cell whose placement was refused, in blocks." :spec (a/int-in 0 nil)}
   ::shut-in-walk-range {:default 1 :doc "How near a crop or the centre a shut-in body walks, in blocks." :spec (a/int-in 0 nil)}
   ::collect-margin {:default 4 :doc "Blocks beyond :radius the drops are collected from." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds a call may take: the digs, the walks and the sweep of the drops." :spec (a/num-in 0 nil)}})
(def fit? (ready/fit-for :work))
(defn give-up [] (settings/get settings ::give-up))
(defn reach [] (settings/get settings ::reach))
(defn max-place-fails [] (settings/get settings ::max-place-fails))

(defn fail-debt
  "Count a refused place on the debt at pos; the cell is bare once it has failed max-place-fails times."
  [m pos]
  (crops/count-debt m :fails pos (max-place-fails) fld/bare-debt))

(defn failed-reach [] (settings/get settings ::failed-reach))

(defn debt-reach
  "The eye-to-centre distance within which debt is placed without walking: a cell that was refused once needs the body closer."
  [debt]
  (if (pos? (:fails debt 0)) (failed-reach) (reach)))

(defn walk-range
  "The range walked to when out of reach, smaller for a debt that was refused."
  [debt]
  (settings/get settings (if (pos? (:fails debt 0)) ::failed-walk-range ::walk-range)))

(defn max-walk-fails [] (settings/get settings ::max-walk-fails))

(defn walk-fail-crop
  "Count a blocked walk to the crop at pos in :walk-fails [{:pos :n}]; the crop is skipped at the max-walk-fails-th."
  [m pos]
  (crops/count-entry m :walk-fails pos (max-walk-fails) fld/skip-crop))

(defn walk-fail-debt
  "Count a blocked walk to the debt at pos; the cell is bare at the max-walk-fails-th."
  [m pos]
  (crops/count-debt m :walk-fails pos (max-walk-fails) fld/bare-debt))

(def inc-in crops/inc-in)

(defn permitted
  "The poss the job may act on with action: zones, claims and the footprints of plans but its own (see jobs.lib.gate).
  A plan this body made never refuses its own work."
  [c action poss]
  (:allowed (gate/look c action poss {:except (fa/plan c) :own-plans-ok? true})))

(defn cut-action
  "The zone action asked for cutting a crop: :harvest in box mode; with :plan :dig, the action the plan jobs (farm.tend)
  ask for a cut."
  [c]
  (if (fa/plan c) :dig :harvest))

(defn ripe-raw
  "The ripe crops to cut before the zone rules: the planned ones with :plan, else those within :radius of the centre."
  [c skipped]
  (if-let [cells (:plan-cells c)]
    (fld/planned-ripe (:primitives c) (:args c) cells skipped)
    (fld/ripe-crops (:primitives c) (assoc (:args c) :radius (radius-of c) :area (let [a (fa/of c)] (when (#{:box :circle} (fa/kind a)) a))) (center-of c) skipped)))

(defn ripe-of
  "The ripe crops to cut: none that zones, claims or other plans' footprints refuse."
  [c skipped]
  (permitted c (cut-action c) (ripe-raw c skipped)))

(defn ripe-warned
  "As ripe-of, with the gate's decline warn when it refuses a ripe crop: for the round, which acts on the rest."
  [c skipped]
  (gate/allowed c :harvest.declined "harvest" (cut-action c) (ripe-raw c skipped) {:except (fa/plan c) :own-plans-ok? true}))

(defn planned
  "Without :plan nil; else {:cells {pos crop}}, or {:trouble text} when the plan
  cannot be worked."
  [c]
  (when (fa/plan c)
    (pf/read-plan c {:cells fld/crop-cells :what "crop cells"})))

(defn with-field
  "c over the field of planned (nil: unchanged): :plan-cells set and :sphere the sphere round them."
  [c field]
  (if-let [cells (:cells field)]
    (let [[center radius] (fld/plan-field cells)]
      (-> c
          (assoc :plan-cells cells)
          (assoc :sphere [center radius])))
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
    (let [have (u/carried-names (:primitives c))
          given-up (set (:bare (ctx/mem c)))]
      (->> (fld/planned-bare (:primitives c) (:plan-cells c))
           (filter #(and (have (:seed %)) (not (given-up (:pos %)))))
           (fld/above-reserve c)
           (permitted-debts c)))))

(defn gave-up? [c]
  (>= (:unreachable (ctx/mem c) 0) (give-up)))

(defn cutting? [c]
  (not (gave-up? c)))

(defn crop-seen?
  "Whether any crop of a wanted name within :radius of the centre has been seen, ripe or not (box mode)."
  [c]
  (let [{:keys [crops]} (:args c)]
    (boolean (seq (crops/seen-crops (:primitives c) (or crops (keys ripe-age)) (+ (radius-of c) (u/dist (u/self-pos c) (center-of c))) 1)))))

(defn blind?
  "No crop seen yet (box mode), or planned crops remembered but not in view and none known ripe, and no look from this
  cell: the round must look around before it can tell."
  [c]
  (and (cutting? c) (not (look/surveyed? c :cell))
       (if-let [cells (:plan-cells c)]
         (and (fld/unwatched? (:primitives c) cells) (empty? (ripe-raw c nil)))
         (not (crop-seen? c)))))

(defn away-start?
  "A body farther than the radius from the field that has cut nothing: it walks there (home!) before it can tell what the field holds, whatever a look round from here showed."
  [c m]
  (and (cutting? c) (zero? (:cut m 0)) (> (u/dist (u/self-pos c) (center-of c)) (radius-of c))))

(defn shut-in-start?
  "A body shut in (a pit) that has neither cut nor left it: it must walk out before it can tell whether there is a crop.
  :in-pit keeps it so while the walk digs the pit open."
  [c m]
  (and (not (:left-pit m)) (zero? (:cut m 0)) (or (:in-pit m) (reach-room/enclosed? (:primitives c) :ground))))

(def target [:area])
(def defaults {:least 1 :spend {:job-time ::job-time-s}})
(def safe? (price/staying :work (ready/when-fed (ready/safe-at-by-night work-place/spot))))

(defn problem
  "Why the call cannot start now, a cause map, or nil: no zone list read (unless :ignore-zones?), a plan that cannot be
  worked, and a field with nothing owed: no debt, no sweep, no ripe crop it may cut (:refused when zones keep it from
  every one) and no need to look around first."
  [c]
  (or (common/no-zones c)
      (let [field (planned c)
            c (with-field c field)
            m (ctx/mem c)]
        (cond
          (:trouble field) nil ; can-run? says it
          (or (:center m)
              (if field (or (seq (sowable c)) (seq (:replant m))) (seq (:replant m)))
              (:collect m)
              (and (cutting? c) (seq (ripe-of c (:skipped m))))
              (blind? c)
              (away-start? c m)
              (shut-in-start? c m)) nil
          (and (cutting? c) (seq (ripe-raw c (:skipped m))))
          (assoc (access/refusal-fields (:refusals (gate/look c (cut-action c) (ripe-raw c (:skipped m)) {:except (fa/plan c) :own-plans-ok? true})))
                 :reason :refused)
          :else {:reason :nothing-to-do}))))

(defn can-run? [c] (pf/plan-can-run? (planned c)))

(def bad-args (checks/args-fit "harvest :area must be a box, circle or plan" #(contains? #{nil :box :circle :plan} (fa/kind (fa/of %)))))

(def can-start? (cond/and bad-args can-run? (checks/refusing :work "something to harvest" problem)))

(declare lost-cells)

(defn done?
  "Ok once nothing ripe is left to cut, no seed is owed, no sweep is owed, no cell was given up on and a look round has
  been made, and the cells worked meet :least; the cells cut this call and the ripe crops left."
  [c]
  (let [field (planned c)
        c (with-field c field)
        m (ctx/mem c)
        ripe (count (ripe-raw c nil))
        cut (:cut m 0)
        worked (max cut (:replanted m 0))
        lost (lost-cells (:primitives c) (:planted m))
        data [:units :cells :done cut :left ripe :result (merge {:cut cut :replanted (:replanted m 0) :bare (vec (distinct (concat (:bare m) (map :pos (:replant m)) lost))) :lost lost :gave-up (gave-up? c)}
                       (spent/summary (spent/log c)))]
        waiting (cond
                  (:trouble field) :plan-trouble
                  (blind? c) :looking
                  (pos? ripe) :ripe
                  (or (seq (:replant m)) (seq (sowable c))) :replanting
                  (:collect m) :collecting
                  (or (seq (:bare m)) (seq lost)) :bare
                  (not (or (:plan-cells c) (crop-seen? c))) :no-crop
                  (checks/child-under-way? c) :wrapping-up
                  (and (pos? worked) (not (common/enough? c worked worked))) :short)]
    (if waiting
      (apply contract/later waiting (str "harvest: cut " cut ", " ripe " ripe left") data)
      (apply contract/ok :harvested (str "harvest done: cut " cut ", no ripe crop left") data))))

(defn quote-of [c]
  (let [c (with-field c (planned c))
        ripe (ripe-raw c nil)
        n (count ripe)]
    {:uses (cond-> {:job-time [(* 0.5 n) (work-s n) (* 10 n)]}
             (and (:replant (:args c)) (pos? n))
             (assoc :items (update-vals (frequencies (keep #(get seed-of (u/seen-name (:primitives c) %)) ripe))
                                        (fn [k] [0 k k]))))
     :done [0 n n]}))

;; ------------------------------------------------------------------ steps

(defn drop-settled!
  "Forget the debts whose cell is no longer bare air, move the farmland-less ones to :bare; the owed debts."
  [c]
  (let [{:keys [owed gone bare]} (fld/sort-debts (:primitives c) (:replant (ctx/mem c)))]
    (when (or (seq gone) (seq bare))
      (ctx/update-mem! c (fn [m] (reduce fld/bare-debt (reduce fld/drop-debt m gone) bare))))
    owed))

(defn ^:async plant-one!
  "Place the seed of debt at its cell and settle the debt by the status."
  [c {:keys [pos seed]}]
  (if-not (seq (permitted c :sow [pos]))
    (ctx/update-mem! c fld/bare-debt pos)
    (let [outcome (await (blocks/place-cell! c pos seed {:ignore-zones? true}))]
      (when (= :placed outcome) (spent/note! c seed pos))
      (case outcome
        :continue :continue
        (:placed :already :occupied) (ctx/update-mem! c (fn [m] (-> (fld/drop-debt m pos)
                                                                    (inc-in :replanted)
                                                                    (update :planted (fn [cells] (vec (distinct (conj (vec cells) pos))))))))
        (ctx/update-mem! c fail-debt pos)))))

(defn max-per-round [] (settings/get settings ::max-per-round))

(defn ^:async replant!
  "Step 1: :again when a seed was planted or a walk made, else nil."
  [c]
  (let [p (:primitives c)
        owed (drop-settled! c)
        have (u/carried-names p)
        plantable (permitted-debts c (filterv #(have (:seed %)) owed))]
    (when (seq plantable)
      (let [here (u/self-pos c)
            near (filterv #(<= (u/eye-dist here (:pos %)) (debt-reach %)) plantable)
            nearest-debt (first (sort-by #(u/dist here (:pos %)) plantable))
            walked (when (empty? near) (await (common/walk-to! c (:pos nearest-debt) (walk-range nearest-debt) {:stay-s (work-s (count plantable))})))
            targets (if (seq near) near [nearest-debt])]
        (case walked
          :continue :continue
          :blocked (do (ctx/update-mem! c walk-fail-debt (:pos (first targets))) :again)
          (loop [todo (take (max-per-round) targets)]
            (if (empty? todo)
              :again
              (if (= :continue (await (plant-one! c (first todo))))
                :continue
                (recur (rest todo))))))))))

(defn ^:async cut-cell!
  "Write the debt of the crop at pos, dig it (a blocks.dig child; the zone verdict is the job's own) and settle by the outcome.
  A cell whose crop is not seen is left (no debt could be written)."
  [c pos]
  (when-let [crop (and (seq (permitted c (cut-action c) [pos])) (u/seen-name (:primitives c) pos))]
    (when-let [seed (and (:replant (:args c)) (get seed-of crop))]
      (ctx/update-mem! c update :replant (fnil conj []) {:pos pos :seed seed :cut true}))
    (let [outcome (await (blocks/dig-cell! c pos {:accept #{:fluid-adjacent :falling-block}
                                                  :ignore-zones? true}))]
      (case outcome
        :continue :continue
        :dug (ctx/update-mem! c inc-in :cut)
        :missing nil
        (ctx/update-mem! c fld/skip-crop pos)))))

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
        _ (when (and (cutting? c) (empty? (ripe-of c (:skipped m))))
            (await (look/survey! c :cell)))
        ripe (when (cutting? c) (ripe-warned c (:skipped m)))]
    (when (seq ripe)
      (let [here (u/self-pos c)
            shut-in? (reach-room/enclosed? p :ground)
            near (when-not shut-in? (filterv #(<= (u/eye-dist here %) (reach)) ripe))
            walked (when (empty? near) (await (common/walk-to! c (first ripe) (if shut-in? (settings/get settings ::shut-in-walk-range) (settings/get settings ::walk-range)) {:stay-s (work-s (count ripe))})))
            targets (if (seq near) near [(first ripe)])
            targets (still-ripe p targets)]
        (when (and (empty? targets) (not (#{:continue :blocked} walked)))
          (ctx/update-mem! c fld/skip-crop (first ripe)))
        (case walked
          :continue :continue
          :blocked (do (ctx/update-mem! c walk-fail-crop (first ripe))
                       (warn-gave-up! c)
                       :again)
          (do (ctx/update-mem! c assoc :collect true)
              (let [r (loop [todo (take (max-per-round) (still-planned c targets))]
                        (if (empty? todo)
                          :again
                          (if (= :continue (await (cut-cell! c (first todo))))
                            :continue
                            (recur (rest todo)))))]
                (warn-gave-up! c)
                r)))))))

(defn max-home-fails [] (settings/get settings ::max-home-fails))

(defn ^:async home!
  "Step 2b: a body farther than :radius from the centre (or shut in, once) with debts or a sweep owed, or with nothing cut yet, walks
  there (range radius/2): :again, or :finish after max-home-fails walks that went nowhere; nil when
  there is nothing to do."
  [c]
  (let [m (ctx/mem c)
        center (center-of c)
        radius (radius-of c)
        shut-in? (shut-in-start? c m)]
    (when (and (or (seq (:replant m)) (:collect m) (zero? (:cut m 0)))
               (or shut-in? (> (u/dist (u/self-pos c) center) radius)))
      (when (and shut-in? (not (:in-pit m))) (ctx/update-mem! c assoc :in-pit true))
      (if (>= (:home-fails m 0) (max-home-fails))
        (do (ctx/update-mem! c assoc :unreachable true) :finish)
        (case (await (common/walk-to! c center (if shut-in? (settings/get settings ::shut-in-walk-range) (quot radius 2)) {:stay-s (work-s (max 1 (count (:replant m))))}))
          :there (do (ctx/update-mem! c assoc :left-pit true) :again)
          :continue :continue
          (do (ctx/update-mem! c inc-in :home-fails) :again))))))

(defn ^:async collect!
  "Step 3: one collect-drops round while a sweep is owed."
  [c]
  (when (:collect (ctx/mem c))
    (let [r (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops
                             {:radius (+ (radius-of c) (settings/get settings ::collect-margin)) :items collect-items})))]
      (when-not (= :continue r) (ctx/update-mem! c dissoc :collect))
      (if (= :continue r) :continue :again))))

(defn lost-cells
  "The planted cells whose block is no longer a crop (nil, an unloaded cell, is not counted)."
  [p planted]
  (filterv #(let [n (u/seen-name p %)] (and n (not (ripe-age n)))) planted))

(defn finish!
  "Step 4: the debts still owed and the planted cells that are no longer crops are bare; warn and end. Done when it cut
  or replanted some and they meet :least of the crops it found, else the cause is why none were."
  [c]
  (let [m (ctx/mem c)
        lost (lost-cells (:primitives c) (:planted m))
        bare (vec (distinct (concat (:bare m) (map :pos (:replant m)) lost)))
        no-crop? (and (not (:plan-cells c)) (zero? (:cut m 0)) (empty? (:replant m)) (not (crop-seen? c)))
        result {:cut (:cut m 0) :replanted (:replanted m 0) :bare bare :lost lost :gave-up (gave-up? c)}
        result (cond-> result (seq (:skipped m)) (assoc :skipped (vec (:skipped m))))
        done-text (str "harvest done: cut " (:cut m 0) ", replanted " (:replanted m 0) ", bare " (count bare))
        worked (max (:cut result) (:replanted result))
        left (count (:skipped m))]
    (cond
      (and (:unreachable m) (zero? worked) (zero? left)) (ctx/end c :nothing-possible :unreachable (assoc result :center (center-of c)))
      no-crop? (ctx/end c :condition-failed :no-crop-seen result)
      (and (pos? worked) (common/enough? c worked (+ worked left))) (ctx/end c :done nil (assoc result :text done-text))
      (pos? left) (ctx/end c :nothing-possible :gave-up result)
      (pos? worked) (ctx/end c :nothing-possible :partial result)
      :else (ctx/end c :condition-failed :nothing-to-do result))))

(defn sync-plan-debts!
  "With :plan and replanting, the debts are the planned cells standing bare (written only when they change)."
  [c]
  (when (and (:plan-cells c) (:replant (:args c)))
    (let [m (ctx/mem c)
          bare (when (:replant-bare (:args c))
                 (fld/above-reserve c (fld/planned-bare (:primitives c) (:plan-cells c))))
          synced (fld/sync-debts m bare)]
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
      :continue
      (await (work (with-field c field))))))

(defn ^:async round
  "The whole attempt: loop the steps (replant, cut, walk home, sweep) until the field is done; :continue only while a
  walk or the sweep waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
