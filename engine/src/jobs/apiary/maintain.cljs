(ns jobs.apiary.maintain
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.steps :as steps]
            [jobs.lib.util :as u]
            [jobs.apiary.guard :as guard]
            [jobs.apiary.harvest :as harvest]))

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
    there are 2 adults, it is day, it is not raining and a flower is carried.
  - :deposit (jobs.storage.deposit): a :chest is given and honeycomb or honey bottles above their :keep entry are
    carried. Only produce is deposited; tools, bottles, carpet, campfires and flowers stay carried.
  A step whose conditions do not hold is booked {:skipped reason} (:safe, :no-fire, :no-carpet, :no-campfire,
  :not-ripe, :no-tool, :unsafe-fire, :not-smoked, :open-fire, :no-target, :at-target, :night, :raining,
  :too-few-adults, :no-food, :no-chest, :nothing-to-store). One that declines is booked {:skipped :declined}
  and one that throws {:skipped :failed :error text}. The pass goes on either way.
  The job declines (does nothing) unless some step would run, after one look around per run when part of the area was never seen. A started run always
  continues.
  It ends :done with {:target :bees :steps {step summary}} (info maintain.done), also when every step was
  skipped. Summaries: guard {:sunk :carpeted :reason :left}, harvest {:harvested :reason :declined}, breed {:fed
  :reason}, deposit {:gave-up :reason}.
  :ignore-zones? is passed to guard and harvest, which check zones and claims (see their docs).")

(a/defargs args
  {:box {:doc "the apiary: {:from pos :to pos}; overrides :center and :radius" :spec (a/map-with {:from a/position? :to a/position?}) :default nil}
   :center {:doc "centre of the apiary; the body's position when the job first runs when nil" :spec ::a/pos :default nil}
   :radius {:doc "hives, fires and bees within this many blocks of the centre count, when :box is nil" :spec (a/num-in 0 nil) :default 12}
   :with {:doc "harvest tool: :shears, :bottle or :either (shears first)" :spec #{:shears :bottle :either} :default :either}
   :target {:doc "bees wanted in the area (babies count); nil: no breeding" :spec (a/int-in 0 nil) :default nil}
   :chest {:doc "chest position {:x :y :z} for the produce; nil: do not store" :spec ::a/pos :default nil}
   :keep {:doc "{item-name count}: how many of honeycomb or honey_bottle deposit leaves carried" :spec (a/map-of a/item? (a/int-in 0 nil)) :default {}}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to guard and harvest); the rules of the game allow it" :spec boolean? :default false}})

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

(defn corners [{:keys [from to]}]
  (for [x [(:x from) (:x to)] y [(:y from) (:y to)] z [(:z from) (:z to)]]
    {:x x :y y :z z}))

(defn reach
  "How far from the body the bee query must look to cover the area."
  [c center]
  (let [{:keys [box radius]} (:args c)
        here (u/self-pos c)]
    (if box
      (apply max (map #(u/dist here %) (corners box)))
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
        fires (guard/survey c center)
        hives (harvest/hives c center)
        seen (harvest/classify c hives)
        bees (bees-in-area c center)]
    {:center center
     :fires fires
     :hives seen
     :unsafe (unsafe-fires block-at (map :pos (filter :ripe hives)))
     :tool (harvest/tool-for (:with (:args c)) inventory)
     :adults (count (remove #(true? (.-baby %)) bees))
     :bees (count bees)
     :day (true? (.-isDay self))
     :raining (true? (.-raining self))
     :flower (animals/food-carried p "bee")
     :stored (into [] (filter (fn [n] (> (reduce + 0 (map :count (filter #(= n (:name %)) inventory)))
                                         (get (:keep (:args c)) n 0)))) produce)
     :reach (reach c center)}))

;; ------------------------------------------------------------------ what is decided

(defn decide-guard
  [_args {{:keys [fires todo left]} :fires}]
  (cond
    (seq todo) {:call {:slot :guard :args {}}}
    (seq left) {:skip (first (vals left))}
    (zero? fires) {:skip :no-fire}
    :else {:skip :safe}))

(defn decide-harvest
  [_args {{:keys [ripe todo declined]} :hives :keys [tool unsafe]}]
  (cond
    (zero? ripe) {:skip :not-ripe}
    (seq unsafe) {:skip :unsafe-fire}
    (empty? todo) {:skip (or (first (vals declined)) :not-ripe)}
    (nil? tool) {:skip :no-tool}
    :else {:call {:slot :harvest :args {}}}))

(defn decide-breed
  [{:keys [target]} {:keys [bees adults day raining flower reach]}]
  (cond
    (nil? target) {:skip :no-target}
    (>= bees target) {:skip :at-target}
    (not day) {:skip :night}
    raining {:skip :raining}
    (< adults 2) {:skip :too-few-adults}
    (nil? flower) {:skip :no-food}
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
    (:guard :harvest) (merge (area c center) (when (= :harvest step) {:with (:with (:args c))})
                             (when (:ignore-zones? (:args c)) {:ignore-zones? true}))
    (:args decided)))

(defn would-run?
  "Whether some step would call a child over the facts read now."
  [c]
  (let [f (facts c)]
    (boolean (some #(:call (decide % (:args c) f)) steps))))

(defn watched-cells
  "The cells whose never having been seen calls for a look: the corners of the box, or the centre."
  [c]
  (if-let [box (:box (:args c))] (corners box) [(apiary/center-of c)]))

(defn check [c]
  (or (boolean (or (:todo (ctx/mem c)) (would-run? c)))
      (look/wait-unless-surveyed c {:reason :nothing-to-do} (watched-cells c))))

;; ------------------------------------------------------------------ the run

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :guard (select-keys r [:sunk :carpeted :reason :left])
    :harvest (select-keys r [:harvested :reason :declined])
    :breed {:fed (count (:fed r)) :reason (:reason r)}
    :deposit (select-keys r [:gave-up :reason])))

(defn finish!
  [c report]
  (let [{:keys [target]} (:args c)
        out {:target target :bees (count (bees-in-area c (apiary/center-of c))) :steps report}]
    (ctx/emit! c :maintain.done :info (assoc out :text (str "maintain done: " (count (filter :skipped (vals report))) " steps skipped")))
    (ctx/result! c out)
    :done))

(defn booked
  "Memory after the step's child ended with status r: the step leaves todo, its summary goes in the report."
  [m step entry]
  (-> m (update :todo rest) (dissoc :call-args) (assoc-in [:report step] entry)))

(defn ^:async run-child!
  "One round of the child in slot. A child that throws is booked failed instead of failing the pass; a cut passes through."
  [c step call-args]
  (try
    (await (ctx/call-child c step (jobs step) call-args))
    (catch :default e
      (when (= "cut" (.-code e)) (throw e))
      (ctx/update-mem! c booked step {:skipped :failed :error (str e)})
      :failed)))

(defn ^:async pass-step
  "One piece of the pass itself (see step)."
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
            :done (do (ctx/update-mem! c booked step (summary step (ctx/child-result c step))) :again)
            (do (ctx/update-mem! c booked step {:skipped :declined}) :again)))))))

(defn ^:async step
  "One piece of the pass: :again after a step ended, :continue while its child waits on the world, :done. With
  nothing to do in sight before the pass, a look around first (:continue: the check decides again)."
  [c]
  (if (and (not (:todo (ctx/mem c))) (not (look/surveyed? c)) (look/unseen? (:primitives c) (watched-cells c))
           (not (would-run? c)))
    (await (look/survey! c))
    (await (pass-step c))))

(def max-steps "Steps of one call before it gives the round back with :continue." 400)

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))
