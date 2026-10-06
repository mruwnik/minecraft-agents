(ns jobs.animals.tend
  (:require [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.animals.cull :as cull]
            [jobs.combat.hunt :as hunt]))

(def doc
  "Keep one pen of one kind of animal in order. The pen is :box. One run is one pass over five steps in this
  order, each a child job run one round at a time. A step under way is not re-decided.

  - :breed (jobs.animals.breed, 2 animals): adults plus babies are below :target, at least 2 adults, breeding
    food carried.
  - :cull (jobs.animals.cull): adults exceed :keep, the larger of 2 and :target minus the babies.
  - :shear (jobs.animals.shear): sheep, shears carried, an adult not sheared.
  - :collect (jobs.forestry.collect-drops): items lie in the box, only those item names.
  - :deposit (jobs.storage.deposit): a :chest is given and produce of the kind is carried above its :keep
    entry. Produce is the hunt drops table plus eggs for chickens, never tools or breeding food.

  A step whose conditions do not hold is skipped and booked {:skipped reason}: :at-target, :too-few-adults,
  :no-food, :within-target, :not-sheep, :no-shears, :none-shearable, :no-drops, :no-chest, :nothing-to-store,
  or :declined when the child declined.

  The check passes when a box is given and some step would run, and always once started. Otherwise it declines,
  so the job is cheap under repeat.

  Ends :done with info tend.done and {:mob :target :adults :babies (live census in the box) :steps {step
  summary}}. Summaries: breed {:fed n :reason}, cull {:killed :remaining :reason}, shear {:shorn n :reason
  :collected}, collect {:collected}, deposit {:gave-up :reason}.

  Limit: the box is the only pen there is. An animal inside it may be outside the fence. Breed, shear and
  collect-drops look in a radius around the body that covers the box, so animals or items just outside it can
  be fed, sheared or picked up.")

(def args
  {:mob {:doc "mob type name of the animals in the pen" :default "cow"}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive; required (without it the check declines)" :default nil}
   :target {:doc "adult herd size wanted; babies count toward it, they grow" :default 4}
   :chest {:doc "chest position {:x :y :z} for the produce; nil: do not store" :type :pos :default nil}
   :keep {:doc "{item-name count}: how many of a produce item deposit leaves carried" :default {}}})

(def steps [:breed :cull :shear :collect :deposit])

(def jobs
  {:breed 'jobs.animals.breed
   :cull 'jobs.animals.cull
   :shear 'jobs.animals.shear
   :collect 'jobs.forestry.collect-drops
   :deposit 'jobs.storage.deposit})

(def extra-produce
  "What a kind gives besides what jobs.combat.hunt/drops lists."
  {"chicken" ["egg"]})

(defn produce
  "The item names a kind of animal yields."
  [mob]
  (into (vec (get hunt/drops mob)) (get extra-produce mob)))

(defn to-store
  "The carried item names that are produce and exceed their keep."
  [inventory produce reserve]
  (->> (group-by :name inventory)
       (keep (fn [[name stacks]]
               (when (and (some #{name} produce)
                          (> (transduce (map :count) + 0 stacks) (get reserve name 0)))
                 name)))
       vec))

(defn drops-in-box
  "The distinct names of the items lying inside the box."
  [c]
  (->> (look/seen-items (:primitives c) {:radius (cull/reach c) :max 32})
       (filter #(cull/in-bound? c (u/pos-of (.-pos %))))
       (keep #(some-> (.-item %) .-name))
       distinct
       vec))

(defn facts
  "What the decisions are made from, read live."
  [c]
  (let [{:keys [adults babies]} (cull/census c)
        inventory (u/inventory (:primitives c))]
    {:adults (count adults)
     :babies (count babies)
     :drops (drops-in-box c)
     :shears (boolean (some #(= "shears" (:name %)) inventory))
     :shearable (count (remove #(true? (.-sheared %)) adults))
     :stored (to-store inventory (produce (:mob (:args c))) (:keep (:args c)))
     :food (animals/food-carried (:primitives c) (:mob (:args c)))
     :reach (cull/reach c)}))

(defn decide-breed
  [{:keys [mob target]} {:keys [adults babies food reach]}]
  (cond
    (>= (+ adults babies) target) {:skip :at-target}
    (< adults 2) {:skip :too-few-adults}
    (nil? food) {:skip :no-food}
    :else {:call {:slot :breed :job (jobs :breed) :args {:mob mob :count 2 :radius reach}}}))

(defn decide-cull
  [{:keys [mob box target]} {:keys [adults babies]}]
  (let [keep (max 2 (- target babies))]
    (if (<= adults keep)
      {:skip :within-target}
      {:call {:slot :cull :job (jobs :cull) :args {:mob mob :box box :keep keep}}})))

(defn decide-shear
  [{:keys [mob]} {:keys [shears shearable reach]}]
  (cond
    (not= "sheep" mob) {:skip :not-sheep}
    (not shears) {:skip :no-shears}
    (zero? shearable) {:skip :none-shearable}
    :else {:call {:slot :shear :job (jobs :shear) :args {:radius reach}}}))

(defn decide-deposit
  [{:keys [chest keep]} {:keys [stored]}]
  (cond
    (nil? chest) {:skip :no-chest}
    (empty? stored) {:skip :nothing-to-store}
    :else {:call {:slot :deposit :job (jobs :deposit) :args {:chest chest :items stored :keep keep}}}))

(defn decide-collect
  [_args {:keys [drops reach]}]
  (if (empty? drops)
    {:skip :no-drops}
    {:call {:slot :collect :job (jobs :collect) :args {:radius reach :filter drops}}}))

(defn decide
  "{:skip reason} or {:call {:slot :job :args}} for a step."
  [step args facts]
  (case step
    :breed (decide-breed args facts)
    :cull (decide-cull args facts)
    :shear (decide-shear args facts)
    :collect (decide-collect args facts)
    :deposit (decide-deposit args facts)
    {:skip :todo}))

(defn plan
  "Walk todo, skipping the steps that decide skips (booked in report); {:todo :report :call}, :call nil when none is left."
  [todo args facts report]
  (loop [todo todo report report]
    (if (empty? todo)
      {:todo [] :report report :call nil}
      (let [{:keys [skip call]} (decide (first todo) args facts)]
        (if skip
          (recur (rest todo) (assoc report (first todo) {:skipped skip}))
          {:todo (vec todo) :report report :call call})))))

(defn check [c]
  (boolean (and (:box (:args c))
                (or (:todo (ctx/mem c))
                    (let [f (facts c)]
                      (some #(:call (decide % (:args c) f)) steps))))))

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :breed {:fed (count (:fed r)) :reason (:reason r)}
    :cull (select-keys r [:killed :remaining :reason])
    :shear {:shorn (count (:shorn r)) :reason (:reason r) :collected (:collected r)}
    :collect (select-keys r [:collected])
    :deposit (select-keys r [:gave-up :reason])))

(defn finish!
  [c report]
  (let [{:keys [mob target]} (:args c)
        {:keys [adults babies]} (cull/census c)
        out {:mob mob :target target :adults (count adults) :babies (count babies) :steps report}]
    (ctx/emit! c :tend.done :info (assoc out :text (str "tend done: " (count adults) " adults, " (count babies) " babies")))
    (ctx/result! c out)
    :done))

(defn running-call
  "The call of the step a child has already started, from memory, else nil."
  [m]
  (when-let [call-args (:call-args m)]
    (let [step (first (:todo m))]
      {:slot step :job (jobs step) :args call-args})))

(defn next-plan
  "The step to run: the one under way, else the first that decide wants to call."
  [c]
  (let [m (ctx/mem c)]
    (if-let [call (running-call m)]
      {:todo (:todo m) :report (:report m) :call call}
      (plan (:todo m) (:args c) (facts c) (:report m)))))

(defn ^:async round [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {}))
  (let [{:keys [call] :as p} (next-plan c)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish! c (:report p))
      (let [step (:slot call)
            _ (ctx/update-mem! c assoc :call-args (:args call))
            r (await (ctx/call-child c step (:job call) (:args call)))]
        (case r
          :done (ctx/update-mem! c #(-> % (update :todo rest) (dissoc :call-args) (assoc-in [:report step] (summary step (ctx/child-result c step)))))
          :declined (ctx/update-mem! c #(-> % (update :todo rest) (dissoc :call-args) (assoc-in [:report step] {:skipped :declined})))
          nil)
        :continue))))
