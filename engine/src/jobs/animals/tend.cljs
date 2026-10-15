(ns jobs.animals.tend
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.standing :as standing]
            [jobs.lib.started :as started]
            [jobs.lib.steps :as steps]
            [jobs.lib.util :as u]
            [jobs.animals.cull :as cull]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.world :as known]))

(def doc
  "Keep one pen of one kind of animal in order. The pen is :box. One run is one pass over five steps in this
  order, each a child job called once (a child that yields is called again on the next call). A step under way is not re-decided.

  - :breed (jobs.animals.breed, 2 animals): adults plus babies are below :target, at least 2 adults, breeding
    food carried.
  - :cull (jobs.animals.cull): adults exceed :keep, the larger of 2 and :target minus the babies.
  - :shear (jobs.animals.shear): sheep, shears carried, an adult not sheared.
  - :collect (jobs.forestry.collect-drops): items lie in the box, only those item names.
  - :deposit (jobs.storage.deposit): a :chest is given and produce of the kind is carried above its :keep
    entry. Produce is the hunt drops table plus eggs for chickens, never tools or breeding food.

  A body outside the box that lists no animal of the kind in it first walks to the box (go-to, once; the server
  lists animals only near the body) and counts again; the job starts for that look.

  A step whose conditions do not hold is skipped and booked {:skipped reason}: :at-target, :too-few-adults,
  :no-food, :within-target, :not-sheep, :no-shears, :none-shearable, :no-drops, :no-chest, :nothing-to-store,
  or :declined when the child declined.

  It starts when some step would run. Without a :box and a :plan it can never start (impossible :no-box). Otherwise it
  waits, so the job is cheap under repeat: :nothing-to-do with the census {:adults :babies :target} and
  {:skipped {step reason}}.

  Ends :done after the pass, with info tend.done and {:mob :target :adults :babies (live census in the box) :steps {step
  summary}}; a plan part that vanishes after the start ends condition-failed :plan-trouble. Summaries: breed {:fed n :reason}, cull {:killed :remaining :reason}, shear {:shorn n :reason
  :collected}, collect {:collected}, deposit {:gave-up :reason}.

  With :plan and :part the pen is that part of the plan: its :box, and :mob and :target from its :herd
  {:mob :target}; it waits :plan-trouble while the plan, the part or its :herd is missing. A pass that leaves
  a step that would still run rests the pen 10 minutes (jobs.lib.standing): it waits {:reason :nothing-to-do
  :backed-off [pen]}, so the job can stand in the queue under repeat without starving other jobs.

  Limit: the box is the only pen there is. An animal inside it may be outside the fence. Breed, shear and
  collect-drops look in a radius around the body that covers the box, so animals or items just outside it can
  be fed, sheared or picked up.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world; with :part, the pen is that part (its :box, :herd {:mob :target}) and :box, :mob and :target are not used" :spec a/name? :default nil}
   :part {:doc "with :plan, the id of the pen part" :spec a/name? :default nil}
   :mob {:doc "mob type name of the animals in the pen" :spec a/name? :default "cow"}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive; required without :plan" :spec a/box? :default nil}
   :target {:doc "adult herd size wanted; babies count toward it, they grow" :spec (a/int-in 0 nil) :default 4}
   :chest {:doc "chest position {:x :y :z} for the produce; nil: do not store" :spec ::a/pos :default nil}
   :keep {:doc "{item-name count}: how many of a produce item deposit leaves carried" :spec (a/map-of string? number?) :default {}}})

(def target [:plan :part :box])

(a/defargs settings
  {::time-s {:default 600 :doc "Seconds a tend pass may take." :spec (a/num-in 0 nil)}})

(def defaults {:spend {:time ::time-s}})

(def steps [:breed :cull :shear :collect :deposit])

(def jobs
  {:breed 'jobs.animals.breed
   :cull 'jobs.animals.cull
   :shear 'jobs.animals.shear
   :collect 'jobs.forestry.collect-drops
   :deposit 'jobs.storage.deposit})

(def extra-produce
  "What a kind gives besides what jobs.lib.hunting/drops lists."
  {"chicken" ["egg"]})

(defn produce
  "The item names a kind of animal yields."
  [mob]
  (into (vec (get hunting/drops mob)) (get extra-produce mob)))

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

(defn look-due?
  "Whether the body, outside the box, lists no animal of the kind in it and has not walked there to look yet: the
  server lists animals only near the body, and sound gives a direction, not a place."
  [c]
  (let [{:keys [adults babies]} (cull/census c)]
    (boolean (and (not (:looked (ctx/mem c)))
                  (empty? adults) (empty? babies)
                  (not (cull/in-bound? c (u/self-pos c)))))))

(defn look-pos
  "The middle column of the box, as the [x z] go-to takes (the ground of that column)."
  [{:keys [min max]}]
  [(js/Math.floor (/ (+ (:x min) (:x max)) 2)) (js/Math.floor (/ (+ (:z min) (:z max)) 2))])

(defn corner-box
  "A plan part's :box [[x y z] [x y z]] as {:min :max}."
  [[[x1 y1 z1] [x2 y2 z2]]]
  {:min {:x (min x1 x2) :y (min y1 y2) :z (min z1 z2)} :max {:x (max x1 x2) :y (max y1 y2) :z (max z1 z2)}})

(defn pen-of
  "With :plan, {:args {:box :mob :target}} of the pen part, else {:trouble text}; nil without :plan."
  [c]
  (when-let [id (:plan (:args c))]
    (let [{:keys [part]} (:args c)
          answer (known/plan c id)
          found (some #(when (= part (:id %)) %) (:parts (:plan answer)))]
      (cond
        (nil? answer) {:trouble "no such plan"}
        (:broken answer) {:trouble (str "the plan cannot be read: " (:broken answer))}
        (not found) {:trouble (str "no part " (pr-str part))}
        (not (and (:box found) (:herd found))) {:trouble (str "part " part " has no :box and :herd")}
        :else {:args (assoc (:herd found) :box (corner-box (:box found)))}))))

(defn with-pen [c pen] (update c :args merge (:args pen)))

(defn pen-key [c]
  (let [{:keys [plan part box]} (:args c)]
    (if plan (str plan "/" part) (pr-str box))))

(defn pen-fact
  "nil when some step would run (or the body is to look at the box first), else the :nothing-to-do wait with the census."
  [c]
  (let [{:keys [target] :as args} (:args c)]
    (when-not (look-due? c)
      (let [f (facts c)
            decisions (into {} (map (juxt identity #(decide % args f))) steps)]
        (when-not (some :call (vals decisions))
          {:reason :nothing-to-do :adults (:adults f) :babies (:babies f) :target target
           :skipped (into {} (map (fn [[step d]] [step (:skip d)])) decisions)})))))

(defn start-fact
  "Why the pen is not tended now: impossible :no-box without a box or a plan; a wait for :plan-trouble, a resting pen
  or nothing to do."
  [c]
  (let [{:keys [plan box]} (:args c)
        {:keys [trouble] :as pen} (pen-of c)
        c (if pen (with-pen c pen) c)
        resting (get (standing/rested c 'jobs.animals.tend) (pen-key c))]
    (cond
      (not (or plan box)) (contract/impossible :no-box)
      trouble {:reason :plan-trouble :why trouble}
      resting {:reason :nothing-to-do :backed-off [(pen-key c)] :until (:until resting)}
      :else (pen-fact c))))

(def conditions "Starts while some step would run (start-fact)." [[:fn (started/at-start start-fact)]])

(defn done?
  "The steps still to run against those of a pass. Never :done? itself: the run ends with its event and data."
  [c]
  (let [left (count (get (ctx/mem c) :todo steps))]
    {:done? false :units :steps :done (- (count steps) left) :left left}))

(defn quote-of [_c]
  {:uses {:time [0 90 600]}})

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :breed {:fed (count (:fed r)) :reason (:reason r)}
    :cull (select-keys r [:killed :remaining :reason])
    :shear {:shorn (count (:shorn r)) :reason (:reason r) :collected (:collected r)}
    :collect (select-keys r [:collected])
    :deposit (assoc (select-keys r [:reason]) :gave-up (boolean (:gave-up r)))))

(defn finish!
  [c report]
  (let [{:keys [mob target]} (:args c)
        {:keys [adults babies]} (cull/census c)
        f (facts c)
        out {:mob mob :target target :adults (count adults) :babies (count babies) :steps report}]
    (when (some #(:call (decide % (:args c) f)) steps)
      (standing/rest! c 'jobs.animals.tend (pen-key c) :undone))
    (ctx/emit! c :tend.done :info (assoc out :text (str "tend done: " (count adults) " adults, " (count babies) " babies")))
    (ctx/end c :done nil out)))

(defn ^:async step-steps
  "One step: finish, or one call of the next step's child. :again after it ended (a failed child is booked with its kind and cause), :yield while it waits on the world, :done at the end."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {}))
  (let [{:keys [call] :as p} (steps/next-plan decide jobs c facts)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish! c (:report p))
      (let [step (:slot call)
            _ (ctx/update-mem! c assoc :call-args (:args call))
            e (await (ctx/call c step (:job call) (:args call)))
            book! (fn [entry] (ctx/update-mem! c #(-> % (update :todo rest) (dissoc :call-args) (assoc-in [:report step] entry))))]
        (cond
          (= :continue e) :yield
          (= :done (:kind e)) (do (book! (summary step (:data e))) :again)
          :else (do (book! (merge (when (seq (:data e)) (summary step (:data e)))
                                  {:ended (:kind e) :cause (contract/cause-key e)}))
                    :again))))))

(defn ^:async step
  "One step: first a look at the box when the body lists no animal in it, else the next step child (step-steps)."
  [c]
  (if (look-due? c)
    (let [r (await (child/call-walk* c :look {:pos (look-pos (:box (:args c))) :range 3}))]
      (when-not (= :continue r) (ctx/update-mem! c assoc :looked true))
      (if (= :continue r) :yield :again))
    (await (step-steps c))))

(defn ^:async round
  "The whole pass: each step's child once, until the run ends; yields only while a child waits on the world."
  [c0]
  (let [{:keys [trouble] :as pen} (pen-of c0)
        c (if pen (with-pen c0 pen) c0)]
    (if trouble
      (do (ctx/warn-once! c [(:plan (:args c)) trouble] :tend.declined
                          {:plan (:plan (:args c)) :part (:part (:args c)) :reason trouble
                           :text (str "tend declines plan " (:plan (:args c)) " part " (:part (:args c)) ": " trouble)})
          (ctx/end c :condition-failed :plan-trouble {:why trouble}))
      (loop []
        (let [r (await (step c))]
          (case r
            :again (recur)
            :yield :continue
            r))))))
