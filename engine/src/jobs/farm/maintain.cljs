(ns jobs.farm.maintain
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.farm.maintain-stock :as stock]
            [jobs.farm.tend :as tend]
            [jobs.farm.tend-box :as box]
            [jobs.farm.tend-decide :as tend-decide]
            [jobs.farm.tend-plan :as tend-plan]
            [jobs.farm.stalks :as stalks]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.farm :as cost-farm]
            [jobs.lib.crops :as crops]
            [jobs.lib.field-area :as fa]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.reserve :as rsv]
            [jobs.lib.storage :as storage]
            [jobs.lib.steps :as steps]
            [jobs.lib.util :as u]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.world :as known]))

(def meta {:tags [:goal]})

(def doc
  "Keep a farm: its field in order, its stock in its place. One run is one pass over four steps in this order, each a
  child job called once (a child that yields is called again on the next call; a step under way is not re-decided):
  - :tidy (jobs.farm.tidy over the plan and its :part): the area is a plan with no :parts and a tree, dirt or other block
    fills some of its crop cells; a body clears its own field.
  - :cut (jobs.blocks.clear-cells): the plan has sugar cane or bamboo cells (a part wanting the stalk) and a stalk stands
    on the base block of one; the stalk block above the base is dug and the rest of the stalk comes down with it, the base
    is never dug.
  - :tend (jobs.farm.tend over the :area, with :till :irrigate :fertilize): harvest, irrigate, till, plant and fertilize as
    its conditions hold. It always runs; a field with nothing to do books it declined.
  - :compost (jobs.farm.compost): a :composter is given and seed above the keep is carried.
  - :deposit (jobs.storage.deposit with :items :farm-goods): a :chest is given and farm goods (seeds, crops, melon,
    pumpkin, hay, cocoa) are carried above the keep. Tools, bread and other non-crop food are never stored.
  The keep, per name, is the largest of: the sowing reserve (twice the number of beds, farmland or untilled ground cells of
  a box, or twice the planned cells of each crop of a plan), a backup stack (64) of each seed type, the units the reserve
  register holds (jobs.lib.reserve: the food reserve, best food first; agent entries) and the :carry entry. Harvested
  crops above it are stored; seed above it is composted.
  Skipped steps are booked {:skipped reason}: :nothing-blocked, :no-stalks, :no-composter, :no-surplus-seed, :no-chest, :nothing-to-store, or
  :declined when the child declined.
  It starts when tend would run or a stock step has something to do. Done once a step ran and tend has nothing
  to do and nothing is left to compost or store; a farm in order before the first pass waits :nothing-to-do. Ends :done with {:steps {step summary}} when a step ran to its end,
  nothing-possible :failed-steps when one stopped early and none ran, else condition-failed :nothing-to-do. Summaries: tidy {:dug}, tend
  {:field :parts}, compost {:fed :bone-meal :reason}, deposit {:gave-up :reason}.")

(a/defargs args
  {:area {:doc "the field, as for jobs.farm.tend: a box {:min :max} with a crop layer, or {:plan id} with :part or :parts; required" :spec fa/spec :default nil}
   :till {:doc "hoe untilled dirt and grass in the ground layer when seed is carried and a hoe is carried or fetchable" :spec boolean? :default true}
   :irrigate {:doc "wet dry farmland with water" :spec boolean? :default true}
   :fertilize {:doc "use bone meal on unripe crops" :spec boolean? :default false}
   :composter {:doc "composter position {:x :y :z} for seed above the keep; nil: do not compost" :spec ::a/pos :default nil}
   :chest {:doc "chest position {:x :y :z} for the farm goods; nil: do not store" :spec ::a/pos :default nil}
   :carry {:doc "{item-name count}: how many of an item compost and deposit leave carried, at least the seed reserve" :spec (a/map-of string? number?) :default {}}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a maintain pass may take: its three steps." :spec (a/num-in 0 nil)}})

(declare quote-of)

(def fit? (ready/trip-fed (ready/fit-for :work) work-place/spot #(quote-of %)))
(def target [:area])
(def ways need/fetch-ways)
(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (price/staying :work (ready/when-fed (ready/safe-at-by-night work-place/spot))))

(def steps [:tidy :cut :tend :compost :deposit])

(def jobs {:tidy 'jobs.farm.tidy :cut 'jobs.blocks.clear-cells :tend 'jobs.farm.tend :compost 'jobs.farm.compost :deposit 'jobs.storage.deposit})

(defn sow-reserve
  "{seed count} the field's sowing keeps carried."
  [c]
  (if (fa/plan c)
    (stock/plan-reserve (or (:crops (tend-plan/planned c)) {}))
    (stock/box-reserve (stock/beds (box/ground-layer (:primitives c) (fa/arg-box c)))
                       (u/inventory (:primitives c)))))

(defn stalk-bases
  "{pos stalk} of the plan's sugar cane and bamboo cells ({} without :plan or when it cannot be read)."
  [c]
  (let [{:keys [plan part]} (:area (:args c))
        answer (some->> plan (known/plan c))]
    (if (and answer (not (:broken answer)))
      (stalks/base-cells answer part)
      {})))

(defn stalk-cuts
  "[[x y z]] of the stalk blocks above the base that the cut step would dig."
  [c]
  (stalks/cut-cells (:primitives c) (stalk-bases c)))

(defn facts
  "{:keep :waste :stored :blocked :cut}: what is kept carried, the seed above it, the farm goods above it and the plan's crop cells holding a block tidy may dig, the stalk blocks to cut."
  [c]
  (let [inventory (u/inventory (:primitives c))
        keep (stock/keeps (sow-reserve c) (rsv/held c :maintain) (:carry (:args c)))]
    {:keep keep
     :waste (stock/surplus inventory keep crops/waste-seeds)
     :stored (stock/surplus inventory keep crops/farm-goods)
     :blocked (if (and (fa/plan c) (empty? (fa/parts c)))
                (count (tend-plan/clearable-cells c (:crops (tend-plan/planned c))))
                0)
     :cut (stalk-cuts c)}))

(defn decide
  "{:skip reason} or {:call {:slot :args}} for a step."
  [step {:keys [area till irrigate fertilize composter chest ignore-zones?]} {:keys [keep waste stored blocked cut]}]
  (case step
    :tidy (if (pos? (or blocked 0))
            {:call {:slot :tidy :args (cond-> {:plan (:plan area)} (:part area) (assoc :part (:part area)) ignore-zones? (assoc :ignore-zones? true))}}
            {:skip :nothing-blocked})
    :cut (if (empty? cut)
           {:skip :no-stalks}
           {:call {:slot :cut :args (cond-> {:area {:cells (vec (take 400 cut))} :only (vec stalks/names) :for-plan (:plan area)}
                                      ignore-zones? (assoc :ignore-zones? true))}})
    :tend {:call {:slot :tend :args (cond-> {:area area :till till :irrigate irrigate :fertilize fertilize}
                                      ignore-zones? (assoc :ignore-zones? true))}}
    :compost (cond
               (nil? composter) {:skip :no-composter}
               (empty? waste) {:skip :no-surplus-seed}
               :else {:call {:slot :compost :args (cond-> {:at composter :items waste :carry keep}
                                                    ignore-zones? (assoc :ignore-zones? true))}})
    :deposit (cond
               (nil? chest) {:skip :no-chest}
               (empty? stored) {:skip :nothing-to-store}
               :else {:call {:slot :deposit :args {:chest chest :items :farm-goods :carry keep}}})))

(defn stock-work?
  "Whether a stock step would call its child over the facts."
  [c]
  (let [f (facts c)]
    (boolean (some #(:call (decide % (:args c) f)) [:compost :deposit]))))

(defn cut-work? [c] (boolean (:call (decide :cut (:args c) (facts c)))))

(defn tend-work? [c] (= :ok (first (tend/problem c))))

(defn tidy-work? [c] (boolean (:call (decide :tidy (:args c) (facts c)))))

(def seed-compost-chance "Chance a composted seed raises the pile a level (minecraft.wiki/w/Composter: seeds 30%)." 0.3)
(def compost-levels "Levels a composter fills before it gives a bone meal (minecraft.wiki/w/Composter: level 7 turns into the full level 8)." 7)
(def bone-meal-stages "Mean growth stages a bone meal gives a crop (minecraft.wiki/w/Bone_Meal: a random 2 to 5)." 3.5)

(defn carried-over
  "[{:name :count}] of the names carried above the keep map."
  [c names keep]
  (let [have (storage/carried-totals (u/inventory (:primitives c)))]
    (filterv #(pos? (:count %)) (map (fn [n] {:name n :count (- (get have n 0) (get keep n 0))}) names))))

(defn field-steps-worth
  "Seconds the field step tend's pass price leaves out is worth: fertilize, the growth seconds bone meal saves on each
  unripe crop at the sown-worth a second of growth earns over the horizon. Irrigate adds nothing: the pass price already
  sows at the wet rate while :irrigate is on, and irrigate only runs then."
  [c f]
  (let [args (tend/decide-args c)
        crop (or (tend/sow-crop c) "wheat")]
    (if (:call (tend-decide/decide :fertilize args f))
      (* (:unripe f) (/ (min bone-meal-stages (get crops/ripe-age crop 7)) (crops/stages-per-s true nil))
         (/ (cost/sown-worth crop (crops/stages-per-s true nil)) (cost-farm/horizon-s)))
      0)))

(defn stock-worth
  "Seconds the stock steps are worth, each only while it would run: the goal-value of the farm goods a deposit stores and
  of the bone meal the waste seed composted makes."
  [c]
  (let [{:keys [keep waste stored] :as f} (facts c)
        meal (/ (transduce (map :count) + 0 (carried-over c waste keep)) (/ compost-levels seed-compost-chance))]
    (+ (if (:call (decide :deposit (:args c) f)) (:value (cost/goal-value (carried-over c stored keep))) 0)
       (if (:call (decide :compost (:args c) f)) (:value (cost/goal-value [{:name "bone_meal" :count meal}])) 0))))

(defn tidy-worth
  "Seconds clearing the blocked crop cells is worth: each cell sown after it (cost/sown-worth of the crop at the dry rate)."
  [c]
  (if (tidy-work? c)
    (* (:blocked (facts c)) (cost/sown-worth (or (tend/sow-crop c) "wheat") (crops/stages-per-s false nil)))
    0))

(defn cut-worth
  "Seconds the stalks the cut step digs are worth: the goal-value of the blocks above the bases."
  [c]
  (let [cuts (:cut (facts c))]
    (if (empty? cuts)
      0
      (:value (cost/goal-value (mapv (fn [[name n]] {:name name :count n})
                                     (frequencies (map #(u/seen-name (:primitives c) (u/cell-pos %)) cuts))))))))

(defn worth-of
  "Seconds a root pass is worth: tend's pass-price :worth, what its fertilize step adds (field-steps-worth),
  the tidy and stock steps' own (tidy-worth, stock-worth) and the cut step's (cut-worth). Never ##Inf: cells never seen are priced by tend's prior. A plan tend cannot work (stalks only) is worth its cut."
  [c]
  (if (= :ok (first (tend/can-run? c)))
    (let [f (tend/check-facts c)]
      (+ (:worth (tend/pass-price c f)) (field-steps-worth c f) (tidy-worth c) (stock-worth c) (cut-worth c)))
    (+ (stock-worth c) (cut-worth c))))

(defn can-start?
  "Tend's start check, or ok while a cut, tidy or stock step has work; a plan of stalks only waits :nothing-to-do while none is tall enough."
  [c]
  (let [t (tend/can-start? c)]
    (cond
      (cut-work? c) (contract/ok :work "stalks to cut")
      (and (= :never (first t)) (seq (stalk-bases c))) (contract/later :nothing-to-do "no stalk is tall enough to cut")
      (#{:ok :never} (first t)) t
      (tidy-work? c) (contract/ok :work "crop cells to clear")
      (stock-work? c) (contract/ok :work "stock to store or compost")
      :else t)))

(defn done?
  "Ok once a step ran and tend has nothing left to do and nothing is left to compost or store; else later with the steps run and left (a farm in order before the first pass stays a failed condition)."
  [c]
  (let [m (ctx/mem c)
        ran (count (remove (some-fn :skipped :stopped) (vals (:report m))))
        left (count (:todo m))]
    (if (or (zero? ran) (cut-work? c) (tend-work? c) (tidy-work? c) (stock-work? c))
      (contract/later :steps (str ran " steps run") :units :steps :done ran :left left)
      (contract/ok :in-order "the farm is in order" :units :steps :done ran :left 0 :result {:steps (:report m)}))))

(defn quote-of [_c]
  {:uses {:job-time [1 60 (settings/get settings ::job-time-s)]}})

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :tidy (select-keys r [:dug])
    :cut (select-keys r [:dug])
    :tend (select-keys r [:field :parts])
    :compost (select-keys r [:fed :bone-meal :reason])
    :deposit {:gave-up (boolean (:gave-up r)) :reason (:reason r)}))

(defn entry-of
  "The report entry of a child's ending e: its summary; a child whose condition failed before its first round is
  {:skipped :declined :reason cause}, any other ending carries :stopped kind."
  [step e]
  (cond
    (= :done (:kind e)) (summary step (:data e))
    (and (#{:condition-failed :impossible} (:kind e)) (empty? (:data e))) {:skipped :declined :reason (contract/cause-key e)}
    :else (assoc (summary step (merge {:reason (contract/cause-key e)} (:data e))) :stopped (:kind e))))

(defn booked
  "Memory after the step's child ended: the step leaves todo, its summary goes in the report."
  [m step entry]
  (-> m (update :todo rest) (dissoc :call-args) (assoc-in [:report step] entry)))

(defn finish!
  "End the pass: done when some step ran to its end, else nothing-possible :failed-steps or condition-failed :nothing-to-do."
  [c report]
  (let [out {:steps report}
        entries (vals report)]
    (cond
      (some (complement (some-fn :skipped :stopped)) entries) (ctx/end c :done nil (assoc out :text (str "farm maintain done: " (count (filter :skipped entries)) " steps skipped")))
      (some :stopped entries) (ctx/end c :nothing-possible :failed-steps out)
      :else (ctx/end c :condition-failed {:reason :nothing-to-do} out))))

(defn ^:async steps-pass!
  "One piece of the pass over the steps: :again after a step ended, :continue while its child waits on the world, or the ending."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {}))
  (let [{:keys [call] :as p} (steps/next-plan decide jobs c facts)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish! c (:report p))
      (let [slot (:slot call)
            _ (ctx/update-mem! c assoc :call-args (:args call))
            e (await (ctx/call c slot (jobs slot) (cond-> (:args call) (= :tend slot) (assoc :ways (into #{:carried} (need/fetch-how c))))))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c booked slot (entry-of slot e)) :again))))))

(defn ^:async round [c]
  (await (pace/steps! c #(steps-pass! c))))
