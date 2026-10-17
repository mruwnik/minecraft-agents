(ns jobs.apiary.maintain
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.need :as need]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.steps :as steps]
            [jobs.lib.util :as u]))

(def doc
  "Keep one apiary in order. The apiary is the :box ({:from :to}), or :center (the body's position at the first
  run) with :radius.
  One call is one pass over four steps in a fixed order. Each is a child job, called until it ends and never run
  twice in a pass (:continue only while a child waits on the world):
  - :guard (jobs.apiary.guard): a lit fire lacks a carpet or a sink and a carried item does it.
  - :harvest (jobs.apiary.harvest, with :with): a ripe hive is smoked, the tool is carried, and no ripe hive
    stands over a fire that is still unsafe. The harvest child cannot leave one hive out, so one unsafe fire holds
    the whole step back.
  - :breed (jobs.animals.breed, 2 bees): :target is given, the bees in the area (adults and babies) are below it,
    there are 2 adults, it is day, it is not raining and a flower is carried (breed fetches one unless :fetch is off).
  - :deposit (jobs.storage.deposit): a :chest is given and honeycomb or honey bottles above their :keep entry are
    carried. Only produce is deposited; tools, bottles, carpet, campfires and flowers stay carried.
  A step whose conditions do not hold is booked {:skipped reason} (:safe, :no-fire, :no-carpet, :no-campfire,
  :not-ripe, :no-tool, :unsafe-fire, :not-smoked, :open-fire, :no-target, :at-target, :night, :raining,
  :too-few-adults, :no-food, :no-chest, :nothing-to-store). One that declines is booked {:skipped :declined :reason why} (the child's wait)
  and one that throws {:skipped :failed :error text}. The pass goes on either way.
  The job declines (does nothing) unless some step would run, after one look around per run when part of the area was never seen. A started run always
  continues.
  It ends :done with {:target :bees :steps {step summary}} , also when every step was
  skipped. Summaries: guard {:sunk :carpeted :reason :left}, harvest {:harvested :reason :declined}, breed {:fed
  :reason}, deposit {:gave-up :reason}.
  :ignore-zones? is passed to guard and harvest, which check zones and claims (see their docs); with :fetch off a missing item skips the step. :fetch is passed to guard, harvest and breed.")

(a/defargs args
  {:box {:doc "the apiary: {:from pos :to pos}; overrides :center and :radius" :spec (a/map-with {:from a/position? :to a/position?}) :default nil}
   :center {:doc "centre of the apiary; the body's position when the job first runs when nil" :spec ::a/pos :default nil}
   :radius {:doc "hives, fires and bees within this many blocks of the centre count, when :box is nil" :spec (a/num-in 0 nil) :default 12}
   :with {:doc "harvest tool: :shears, :bottle or :either (shears first)" :spec #{:shears :bottle :either} :default :either}
   :target {:doc "bees wanted in the area (babies count); nil: no breeding" :spec (a/int-in 0 nil) :default nil}
   :chest {:doc "chest position {:x :y :z} for the produce; nil: do not store" :spec ::a/pos :default nil}
   :keep {:doc "{item-name count}: how many of honeycomb or honey_bottle deposit leaves carried" :spec (a/map-of a/item? (a/int-in 0 nil)) :default {}}
   :ignore-zones? :shared
   :fetch {:doc "get a carpet, campfire or harvest tool that is not carried (passed to guard and harvest, jobs.lib.need): true, a set of kinds or a map of limits; false skips the step" :spec need/option? :default true}})

(def steps [:guard :harvest :breed :deposit])

(def jobs
  {:guard 'jobs.apiary.guard
   :harvest 'jobs.apiary.harvest
   :breed 'jobs.animals.breed
   :deposit 'jobs.storage.deposit})

(def produce ["honeycomb" "honey_bottle"])

;; ------------------------------------------------------------------ what is read

(defn area [c center]
  (let [{:keys [box radius]} (:args c)]
    {:box box :center center :radius radius}))

(defn reach
  "How far from the body the bee query must look to cover the area."
  [c center]
  (let [{:keys [box radius]} (:args c)
        here (u/self-pos c)]
    (if box
      (apply max (map #(u/dist here %) (apiary/corners box)))
      (+ radius (u/dist here center)))))

(defn bees-in-area [c center]
  (let [a (area c center)]
    (filterv #(apiary/in-area? a (u/pos-of (.-pos %))) (animals/herd (:primitives c) "bee" (reach c center)))))

(defn unsafe-fires
  "The open fires smoking a ripe hive: harvest declines only those (a raised but carpeted fire is worked)."
  [block-at ripe-hives]
  (->> ripe-hives
       (keep #(apiary/smoke-source block-at %))
       (filter #(apiary/open-fire? block-at %))
       distinct
       vec))

(defn facts
  "What the decisions are made from, read live."
  [c]
  (let [p (:primitives c)
        center (apiary/center-of c)
        inventory (u/inventory p)
        self (.self p)
        block-at (apiary/seen-block-at-fn p)
        fires (apiary/survey c center)
        hives (apiary/hives c center)
        seen (apiary/classify c hives)
        bees (bees-in-area c center)]
    {:center center
     :fires fires
     :hives seen
     :unsafe (unsafe-fires block-at (map :pos (filter :ripe hives)))
     :tool (apiary/tool-for (:with (:args c)) inventory)
     :adults (count (remove #(true? (.-baby %)) bees))
     :bees (count bees)
     :day (true? (.-isDay self))
     :raining (true? (.-raining self))
     :flower (animals/food-carried p "bee")
     :stored (into [] (filter (fn [n] (> (reduce + 0 (map :count (filter #(= n (:name %)) inventory)))
                                         (get (:keep (:args c)) n 0)))) produce)
     :reach (reach c center)}))

;; ------------------------------------------------------------------ what is decided

(defn fetching? [{:keys [fetch]}] (boolean (some #(need/allows? fetch %) need/kinds)))

(defn decide-guard
  [args {{:keys [fires todo left]} :fires}]
  (cond
    (seq todo) {:call {:slot :guard :args {}}}
    (and (seq left) (fetching? args)) {:call {:slot :guard :args {}}}
    (seq left) {:skip (first (vals left))}
    (zero? fires) {:skip :no-fire}
    :else {:skip :safe}))

(defn decide-harvest
  [args {{:keys [ripe todo declined]} :hives :keys [tool unsafe]}]
  (cond
    (zero? ripe) {:skip :not-ripe}
    (seq unsafe) {:skip :unsafe-fire}
    (empty? todo) {:skip (or (first (vals declined)) :not-ripe)}
    (and (nil? tool) (not (need/allows? (:fetch args) :tool))) {:skip :no-tool}
    :else {:call {:slot :harvest :args {}}}))

(defn decide-breed
  [{:keys [target] :as args} {:keys [bees adults day raining flower reach]}]
  (cond
    (nil? target) {:skip :no-target}
    (>= bees target) {:skip :at-target}
    (not day) {:skip :night}
    raining {:skip :raining}
    (< adults 2) {:skip :too-few-adults}
    (and (nil? flower) (not (need/allows? (:fetch args) :item))) {:skip :no-food}
    :else {:call {:slot :breed :args {:mob "bee" :count 2 :radius reach}}}))

(defn decide-deposit
  [{:keys [chest keep]} {:keys [stored]}]
  (cond
    (nil? chest) {:skip :no-chest}
    (empty? stored) {:skip :nothing-to-store}
    :else {:call {:slot :deposit :args {:chest chest :items stored :keep keep}}}))

(defn decide
  "{:skip reason} or {:call {:slot :args}} for a step."
  [step args facts]
  (case step
    :guard (decide-guard args facts)
    :harvest (decide-harvest args facts)
    :breed (decide-breed args facts)
    :deposit (decide-deposit args facts)))

(defn child-args
  "The args a child gets: the area for guard and harvest, as decided for the rest."
  [c step center decided]
  (case step
    (:guard :harvest) (merge (area c center) {:fetch (:fetch (:args c))} (when (= :harvest step) {:with (:with (:args c))})
                             (when (:ignore-zones? (:args c)) {:ignore-zones? true}))
    :breed (assoc (:args decided) :fetch (:fetch (:args c)))
    (:args decided)))

(defn would-run?
  "Whether some step would call a child over the facts read now."
  [c]
  (let [f (facts c)]
    (boolean (some #(:call (decide % (:args c) f)) steps))))

(defn can-start?
  "Later :nothing-to-do when nothing would run, with a look first while part of the area was never seen."
  [c]
  (cond
    (would-run? c) (contract/ok :step "a step would run")
    (and (look/unseen? (:primitives c) (apiary/watched-cells c)) (not (look/surveyed? c :near)))
    (contract/look :nothing-to-do "nothing to do at the apiary yet; part of it was never seen")
    :else (contract/later :nothing-to-do "nothing to do at the apiary")))

;; ------------------------------------------------------------------ the run

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :guard (select-keys r [:sunk :carpeted :reason :left])
    :harvest (select-keys r [:harvested :reason :declined])
    :breed {:fed (count (:fed r)) :reason (:reason r)}
    :deposit (select-keys r [:gave-up :reason])))

(defn ran?
  "Whether a step's report entry is one that ran to its end."
  [entry]
  (not (or (:skipped entry) (:stopped entry))))

(defn finish!
  "End the pass: done when some step ran to its end, else nothing-possible :failed-steps (one stopped or threw) or
  condition-failed :nothing-to-do."
  [c report]
  (let [{:keys [target]} (:args c)
        out {:target target :bees (count (bees-in-area c (apiary/center-of c))) :steps report}
        entries (vals report)]
    (cond
      (some ran? entries) (ctx/end c :done nil (assoc out :text (str "maintain done: " (count (filter :skipped entries)) " steps skipped")))
      (some #(or (:stopped %) (= :failed (:skipped %))) entries) (ctx/end c :nothing-possible :failed-steps out)
      :else (ctx/end c :condition-failed {:reason :nothing-to-do} out))))

(defn booked
  "Memory after the step's child ended: the step leaves todo, its summary goes in the report."
  [m step entry]
  (-> m (update :todo rest) (dissoc :call-args) (assoc-in [:report step] entry)))

(defn entry-of
  "The report entry of a child's ending e: its summary; a child whose condition failed before its first round (kind
  condition-failed or impossible, no data) is {:skipped :declined :reason cause}, any other ending carries :stopped kind."
  [step e]
  (cond
    (= :done (:kind e)) (summary step (:data e))
    (and (#{:condition-failed :impossible} (:kind e)) (empty? (:data e))) {:skipped :declined :reason (contract/cause-key e)}
    :else (assoc (summary step (merge {:reason (contract/cause-key e)} (:data e))) :stopped (:kind e))))

(defn ^:async run-child!
  "One round of the child in slot. A child that throws is booked failed instead of failing the pass; a cut or a limit passes through."
  [c step call-args]
  (try
    (await (ctx/call c step (jobs step) call-args))
    (catch :default e
      (when (contract/passes-through? e) (throw e))
      (ctx/update-mem! c booked step {:skipped :failed :error (str e)})
      :failed)))

(defn ^:async step
  "One piece of the pass: :again after a step ended, :continue while its child waits on the world, or the ending."
  [c]
  (let [center (apiary/center-of c)]
    (when-not (:todo (ctx/mem c))
      (ctx/update-mem! c assoc :todo steps :report {} :center center))
    (let [{:keys [call] :as p} (steps/next-plan decide jobs c facts)]
      (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
      (if-not call
        (finish! c (:report p))
        (let [step (:slot call)
              call-args (or (:call-args (ctx/mem c)) (child-args c step center call))
              _ (ctx/update-mem! c assoc :call-args call-args)
              r (await (run-child! c step call-args))]
          (case r
            :continue :continue
            :failed :again
            (do (ctx/update-mem! c booked step (entry-of step r)) :again)))))))

(a/defargs settings
  {::max-steps {:default 400 :doc "Steps of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 1800 :doc "Seconds a maintain pass may take: its four steps." :spec (a/num-in 0 nil)}})
(defn max-steps [] (settings/get settings ::max-steps))

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue)))))

;; ------------------------------------------------------------------ the contract

(def target [:box :center])

(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s}})


(def in-order
  "The skips that mean a step's part of the goal holds."
  {:guard #{:safe :no-fire} :harvest #{:not-ripe} :breed #{:no-target :at-target} :deposit #{:no-chest :nothing-to-store}})

(defn in-order?
  "Whether every step's part of the goal holds over the facts: its decision is a skip that says so."
  [args facts]
  (every? #(contains? (in-order %) (:skip (decide % args facts))) steps))

(defn report-of
  "The report so far with the steps not reached booked as the skip that holds them."
  [c facts]
  (let [report (or (:report (ctx/mem c)) {})]
    (into report (for [st steps :when (not (contains? report st))] [st {:skipped (:skip (decide st (:args c) facts))}]))))

(defn done?
  "Ok when the apiary is in order, wrap-up included (no fire unsafe or lacking a carpet, nothing ripe, bees at :target
  or none wanted, nothing to store); before the start that is done :already. Part of the area never seen is looked at
  first. Else later with the steps run and left."
  [c]
  (let [ran (count (filter ran? (vals (:report (ctx/mem c)))))
        left (count (:todo (ctx/mem c)))
        facts (facts c)]
    (cond
      (not (in-order? (:args c) facts))
      (contract/later :steps (str ran " steps run") :units :steps :done ran :left left)

      (and (look/unseen? (:primitives c) (apiary/watched-cells c)) (not (look/surveyed? c :near)))
      (contract/look :unseen "part of the apiary was never seen")

      :else
      (contract/ok :in-order "the apiary is in order" :units :steps :done ran :left 0
                   :result {:target (:target (:args c)) :bees (:bees facts) :steps (report-of c facts)}))))

(defn quote-of [_c]
  {:uses {:job-time [1 60 (settings/get settings ::job-time-s)]}})
