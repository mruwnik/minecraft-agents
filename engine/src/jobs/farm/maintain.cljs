(ns jobs.farm.maintain
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.farm.maintain-stock :as stock]
            [jobs.farm.tend :as tend]
            [jobs.farm.tend-box :as box]
            [jobs.farm.tend-plan :as tend-plan]
            [jobs.lib.crops :as crops]
            [jobs.lib.field-area :as fa]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.reserve :as rsv]
            [jobs.lib.steps :as steps]
            [jobs.lib.util :as u]
            [jobs.lib.work-place :as work-place]))

(def meta {:tags [:goal]})

(def doc
  "Keep a farm: its field in order, its stock in its place. A body farther than 16 blocks from the field first walks there
  (one jobs.movement.go-to child; one that cannot arrive ends with the go-to's own kind and cause). One run is one pass over four steps in this order, each a
  child job called once (a child that yields is called again on the next call; a step under way is not re-decided):
  - :tidy (jobs.farm.tidy over the plan and its :part): the area is a plan with no :parts and a tree, dirt or other block
    fills some of its crop cells; a body clears its own field.
  - :tend (jobs.farm.tend over the :area, with :till :irrigate :fertilize): harvest, irrigate, till, plant and fertilize as
    its conditions hold. It always runs; a field with nothing to do books it declined.
  - :compost (jobs.farm.compost): a :composter is given and seed above the keep is carried.
  - :deposit (jobs.storage.deposit with :items :farm-goods): a :chest is given and farm goods (seeds, crops, melon,
    pumpkin, hay, cocoa) are carried above the keep. Tools, bread and other non-crop food are never stored.
  The keep, per name, is the largest of: the sowing reserve (twice the number of beds, farmland or untilled ground cells of
  a box, or twice the planned cells of each crop of a plan), a backup stack (64) of each seed type, the units the reserve
  register holds (jobs.lib.reserve: the 3-day food reserve, best food first; agent entries) and the :carry entry. Harvested
  crops above it are stored; seed above it is composted.
  Skipped steps are booked {:skipped reason}: :nothing-blocked, :no-composter, :no-surplus-seed, :no-chest, :nothing-to-store, or
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
  {::min-health {:default 0 :spec (a/num-in 0 20)
                :doc "Health (of 20) below which the job waits to heal; 0 = never (its acts are short and in the body's own field). Poison still holds."}
   ::job-time-s {:default 1800 :doc "Seconds a maintain pass may take: its three steps." :spec (a/num-in 0 nil)}})

(defn fit? [c] (ready/fit-min (settings/get settings ::min-health) c))
(def target [:area])
(def ways need/fetch-ways)
(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (price/staying :work (ready/when-fed (ready/safe-at-by-night work-place/spot))))

(def steps [:tidy :tend :compost :deposit])

(def jobs {:tidy 'jobs.farm.tidy :tend 'jobs.farm.tend :compost 'jobs.farm.compost :deposit 'jobs.storage.deposit})

(defn sow-reserve
  "{seed count} the field's sowing keeps carried."
  [c]
  (if (fa/plan c)
    (stock/plan-reserve (or (:crops (tend-plan/planned c)) {}))
    (stock/box-reserve (stock/beds (box/ground-layer (:primitives c) (fa/arg-box c)))
                       (u/inventory (:primitives c)))))

(defn facts
  "{:keep :waste :stored :blocked}: what is kept carried, the seed above it, the farm goods above it and the plan's crop cells holding a block tidy may dig."
  [c]
  (let [inventory (u/inventory (:primitives c))
        keep (stock/keeps (sow-reserve c) (rsv/held c :maintain) (:carry (:args c)))]
    {:keep keep
     :waste (stock/surplus inventory keep crops/waste-seeds)
     :stored (stock/surplus inventory keep crops/farm-goods)
     :blocked (if (and (fa/plan c) (empty? (fa/parts c)))
                (count (tend-plan/clearable-cells c (:crops (tend-plan/planned c))))
                0)}))

(defn decide
  "{:skip reason} or {:call {:slot :args}} for a step."
  [step {:keys [area till irrigate fertilize composter chest ignore-zones?]} {:keys [keep waste stored blocked]}]
  (case step
    :tidy (if (pos? (or blocked 0))
            {:call {:slot :tidy :args (cond-> {:plan (:plan area)} (:part area) (assoc :part (:part area)) ignore-zones? (assoc :ignore-zones? true))}}
            {:skip :nothing-blocked})
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

(defn tend-work? [c] (= :ok (first (tend/problem c))))

(defn tidy-work? [c] (boolean (:call (decide :tidy (:args c) (facts c)))))

(def near 16)

(defn far-from-field?
  "The field's site when the body is farther than near from it, else nil."
  [c]
  (let [site (tend/site c)]
    (when (and site (> (u/dist (u/self-pos c) site) near)) site)))

(defn can-start?
  "Tend's start check, or ok while a tidy or stock step has work or the field is far (the run walks there, then reads it)."
  [c]
  (let [t (tend/can-start? c)]
    (cond
      (#{:ok :never} (first t)) t
      (and (not= :away (:reason (nth t 2 nil))) (far-from-field? c)) (contract/ok :walk "the field is far: walk there first")
      (tidy-work? c) (contract/ok :work "crop cells to clear")
      (stock-work? c) (contract/ok :work "stock to store or compost")
      :else t)))

(defn done?
  "Ok once a step ran and tend has nothing left to do and nothing is left to compost or store; else later with the steps run and left (a farm in order before the first pass stays a failed condition)."
  [c]
  (let [m (ctx/mem c)
        ran (count (remove (some-fn :skipped :stopped) (vals (:report m))))
        left (count (:todo m))]
    (if (or (zero? ran) (tend-work? c) (tidy-work? c) (stock-work? c))
      (contract/later :steps (str ran " steps run") :units :steps :done ran :left left)
      (contract/ok :in-order "the farm is in order" :units :steps :done ran :left 0 :result {:steps (:report m)}))))

(defn quote-of [_c]
  {:uses {:job-time [1 60 (settings/get settings ::job-time-s)]}})

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :tidy (select-keys r [:dug])
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

(defn ^:async walk-to-field!
  "Walk to the field before the pass, as a player does: :again once there, :continue while the walk waits, or the walk's own ending."
  [c site]
  (let [e (await (ctx/call c :walk 'jobs.movement.go-to {:pos (update site :y inc) :range 8}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :again
      :else (ctx/end c (:kind e) (contract/cause-key e)
                     {:steps {} :text (str "farm maintain: could not walk to the field, go there first: " (contract/cause-key e))}))))

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

(defn ^:async step
  "One piece of the pass: :again after a step ended, :continue while its child waits on the world, or the ending."
  [c]
  (if-let [site (and (not (:todo (ctx/mem c))) (far-from-field? c))]
    (await (walk-to-field! c site))
    (await (steps-pass! c))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))
