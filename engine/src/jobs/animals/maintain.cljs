(ns jobs.animals.maintain
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.animals :as animals]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.need :as need]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.standing :as standing]
            [jobs.lib.steps :as steps]
            [jobs.lib.util :as u]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.world :as known]))

(def doc
  "Keep one pen of one kind of animal in order. The pen is :box. One run is one pass over seven steps in this
  order, each a child job called once (a child that yields is called again on the next call). A step under way is not re-decided.

  - :mend (jobs.build.pen over the whole :plan, only with :plan): a fence, gate or wall cell of the plan beside the box
    was seen as something else (blown away), so the plan's fence is built again. Only cells the body has seen count; a gap
    that stays rests the pen.
  - :herd (jobs.animals.herd for 2 adults, or :target if less): fewer adults than that are in the box, so breeding cannot start; herd fetches
    and searches for animals of the kind within ::herd-radius (64) of the body and brings them in.
  - :breed (jobs.animals.breed, 2 animals): adults plus babies are below :target, at least 2 adults, breeding
    food carried (breed fetches it unless the ways are only #{:carried}).
  - :cull (jobs.animals.cull): adults exceed :keep, the larger of 2 and :target minus the babies.
  - :shear (jobs.animals.shear): sheep, shears carried (shear fetches them unless the ways are only #{:carried}), an adult not sheared.
  - :collect (jobs.items.collect-drops): items lie in the box, only those item names.
  - :deposit (jobs.storage.deposit): a :chest is given and produce of the kind is carried above its :carry
    entry. Produce is the hunt drops table plus eggs for chickens, never tools or breeding food.

  A body outside the box that lists fewer animals of the kind in it than the target first walks to the box (go-to, once; the server
  lists animals only near the body) and counts again; the job starts for that look.

  A step whose conditions do not hold is skipped and booked {:skipped reason}: :no-plan, :sound, :at-target, :at-pair,
  :too-few-adults, :no-food and :no-shears (only with ways #{:carried}), :within-target, :not-sheep, :none-shearable, :no-drops, :no-chest, :nothing-to-store,
  or :declined when the child declined.

  Done (:already before the start) when the box needs no look, no step would run and the herd is at :target. It starts when some step would run. Without a :box and a :plan it can never start (impossible :no-box), nor without a :mob and a :plan (:no-mob). Otherwise it
  waits, so the job is cheap under repeat: :nothing-to-do with the census {:adults :babies :target} and
  {:skipped {step reason}}.

  Ends :done after the pass, with {:mob :target :adults :babies (live census in the box) :steps {step
  summary}}. Summaries: mend {:closed? :reason}, herd {:reason :inside :brought}, breed {:fed n :reason}, cull {:killed :remaining :reason}, shear {:shorn n :reason
  :collected}, collect {:collected}, deposit {:gave-up :reason}.

  With :plan and :part the pen is that part of the plan: its :box, and :mob and :target from its :herd
  {:mob :target}; it waits :plan-trouble while the plan is missing or unreadable, and ends impossible :plan-trouble when the part or its :herd is missing.

  Limit: the box is the only pen there is. An animal inside it may be outside the fence. Breed, shear and
  collect-drops look in a radius around the body that covers the box, so animals or items just outside it can
  be fed, sheared or picked up.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world; with :part, the pen is that part (its :box, :herd {:mob :target}) and :box, :mob and :target are not used" :spec a/name? :default nil}
   :part {:doc "with :plan, the id of the pen part" :spec a/name? :default nil}
   :mob {:doc "mob type name of the animals in the pen; required without :plan" :spec a/name? :default nil}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive; required without :plan" :spec pen-read/box-arg :default nil}
   :target {:doc "adult herd size wanted; babies count toward it, they grow" :spec (a/int-in 0 nil) :default 4}
   :chest {:doc "chest position {:x :y :z} for the produce; nil: do not store" :spec ::a/pos :default nil}
   :carry {:doc "{item-name count}: how many of a produce item deposit leaves carried" :spec (a/map-of string? number?) :default {}}})

(def ways need/fetch-ways)

(def target [:plan :part :box])

(a/defargs settings
  {::min-health {:default 0 :spec (a/num-in 0 20) :doc "Health (of 20) below which the job waits to heal; 0 = never (its acts are short and harmless). Poison still holds."}
   ::job-time-s {:default 600 :doc "Seconds a maintain pass may take." :spec (a/num-in 0 nil)}
   ::herd-radius {:default 64 :doc "Blocks from the body within which the herd step fetches or searches for animals of the kind." :spec (a/num-in 0 nil)}})

(defn herd-radius [] (settings/get settings ::herd-radius))

(def defaults {:spend {:job-time ::job-time-s}})

(def steps [:mend :herd :breed :cull :shear :collect :deposit])

(def jobs
  {:mend 'jobs.build.pen
   :herd 'jobs.animals.herd
   :breed 'jobs.animals.breed
   :cull 'jobs.animals.cull
   :shear 'jobs.animals.shear
   :collect 'jobs.items.collect-drops
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
  (->> (look/seen-items (:primitives c) {:radius (hunting/reach c) :max 32})
       (filter #(hunting/in-bound? c (u/pos-of (.-pos %))))
       (keep #(some-> (.-item %) .-name))
       distinct
       vec))

(defn missing-barrier
  "The plan's fence, gate and wall cells beside the box (one column out) that the body has seen as something its plan
  does not want there, as [x y z]; a cell never seen is no gap."
  [c]
  (let [{:keys [plan]} (:args c)
        cells (when plan (:cells (known/plan c plan)))
        {:keys [min max]} (pen-read/arg-box c)
        beside? (fn [[x _ z]] (and (<= (dec (:x min)) x (inc (:x max))) (<= (dec (:z min)) z (inc (:z max)))))
        p (:primitives c)]
    (vec (for [{:keys [pos want]} cells
               :when (and (pen-read/barrier? want) (beside? pos))
               :let [seen (u/sensed p (u/cell-pos pos))]
               :when (and seen (not (true? (.-unknown seen))) (not (some #{(.-name seen)} (pen-read/want-blocks want))))]
           pos))))

(defn facts
  "What the decisions are made from, read live."
  [c]
  (let [{:keys [adults babies]} (hunting/census c)
        inventory (u/inventory (:primitives c))]
    {:gaps (missing-barrier c)
     :adults (count adults)
     :babies (count babies)
     :drops (drops-in-box c)
     :shears (boolean (some #(= "shears" (:name %)) inventory))
     :shearable (count (remove #(true? (.-sheared %)) adults))
     :stored (to-store inventory (produce (:mob (:args c))) (:carry (:args c)))
     :food (animals/food-carried (:primitives c) (:mob (:args c)))
     :reach (hunting/reach c)}))

(defn decide-mend
  [{:keys [plan ways]} {:keys [gaps]}]
  (cond
    (nil? plan) {:skip :no-plan}
    (empty? gaps) {:skip :sound}
    :else {:call {:slot :mend :job (jobs :mend) :args (cond-> {:plan plan} (not (need/fetching? ways)) (assoc :ways #{:carried}))}}))

(defn decide-herd
  [{:keys [mob box ways target]} {:keys [adults]}]
  (if (<= (min 2 target) adults)
    {:skip :at-pair}
    {:call {:slot :herd :job (jobs :herd) :args (cond-> {:mob mob :box box :target (min 2 target) :radius (herd-radius)} (some? ways) (assoc :ways ways))}}))

(defn decide-breed
  [{:keys [mob target ways box]} {:keys [adults babies food reach]}]
  (cond
    (>= (+ adults babies) target) {:skip :at-target}
    (< adults 2) {:skip :too-few-adults}
    (and (nil? food) (not (need/fetching? ways))) {:skip :no-food}
    :else {:call {:slot :breed :job (jobs :breed) :args (cond-> {:mob mob :pairs 1 :radius reach :box box} (some? ways) (assoc :ways ways))}}))

(defn decide-cull
  [{:keys [mob box target]} {:keys [adults babies]}]
  (let [keep (max 2 (- target babies))]
    (if (<= adults keep)
      {:skip :within-target}
      {:call {:slot :cull :job (jobs :cull) :args {:mob mob :box box :keep keep}}})))

(defn decide-shear
  [{:keys [mob ways]} {:keys [shears shearable reach]}]
  (cond
    (not= "sheep" mob) {:skip :not-sheep}
    (and (not shears) (not (need/fetching? ways))) {:skip :no-shears}
    (zero? shearable) {:skip :none-shearable}
    :else {:call {:slot :shear :job (jobs :shear) :args (cond-> {:radius reach} (some? ways) (assoc :ways ways))}}))

(defn decide-deposit
  [{:keys [chest carry]} {:keys [stored]}]
  (cond
    (nil? chest) {:skip :no-chest}
    (empty? stored) {:skip :nothing-to-store}
    :else {:call {:slot :deposit :job (jobs :deposit) :args {:chest chest :items stored :keep carry}}}))

(defn decide-collect
  [_args {:keys [drops reach]}]
  (if (empty? drops)
    {:skip :no-drops}
    {:call {:slot :collect :job (jobs :collect) :args {:radius reach :items drops}}}))

(defn decide
  "{:skip reason} or {:call {:slot :job :args}} for a step."
  [step args facts]
  (case step
    :mend (decide-mend args facts)
    :herd (decide-herd args facts)
    :breed (decide-breed args facts)
    :cull (decide-cull args facts)
    :shear (decide-shear args facts)
    :collect (decide-collect args facts)
    :deposit (decide-deposit args facts)
    {:skip :todo}))

(defn look-due?
  "Whether the body lists fewer animals of the kind in the box than the target and has not looked there yet (walking to
  the box first when outside it; a walled pen shows a few through a gap): the server lists animals only near the body, sound gives a direction, not a place, and animals behind
  the body are not in view."
  [c]
  (let [{:keys [adults babies]} (hunting/census c)]
    (boolean (and (not (:looked (ctx/mem c)))
                  (< (+ (count adults) (count babies)) (:target (:args c)))))))

(defn with-pen [c pen] (update c :args merge (:args pen)))

(defn pen-spot
  "work-place/spot of c with the :box of its plan part (pen-read/pen-of) in place of the plan."
  [c]
  (work-place/spot
   (if-let [box (:box (:args (pen-read/pen-of c)))]
     (update c :args #(-> % (dissoc :plan) (assoc :box box)))
     c)))

(def safe?
  "Not exposed at the middle of the pen: its :box, else the box of its plan part."
  (ready/safe-at-by-night pen-spot))
(defn fit? [c] (ready/fit-min (settings/get settings ::min-health) c))


(defn pen-fact
  "ok when some step would run (or the body is to look at the box first), else the later :nothing-to-do with the census."
  [c]
  (let [{:keys [target] :as args} (need/args-with-ways c)]
    (if (look-due? c)
      (contract/ok :look-due "the box is looked at first")
      (let [f (facts c)
            decisions (into {} (map (juxt identity #(decide % args f))) steps)]
        (if (some :call (vals decisions))
          (contract/ok :step "a step would run")
          (contract/later :nothing-to-do "nothing to do at the pen"
                          :adults (:adults f) :babies (:babies f) :target target
                          :skipped (into {} (map (fn [[step d]] [step (:skip d)])) decisions)))))))

(defn can-run?
  "Later :plan-trouble while the plan is missing or unreadable (it may be saved or fixed later); never when it has no such
  part or the part has no box and herd."
  [c]
  (let [{:keys [trouble later?]} (pen-read/pen-of c)]
    (cond
      (nil? trouble) (contract/ok :pen "the pen can be read")
      later? (contract/later :plan-trouble trouble :why trouble)
      :else (contract/never :plan-trouble trouble :why trouble))))

(defn can-start?
  "Never :no-box without a box or a plan, :no-mob without :mob or a plan; later for :plan-trouble, :away (far from the pen while another job is listed) or nothing to do."
  [c]
  (let [{:keys [plan box mob]} (:args c)
        {:keys [trouble] :as pen} (pen-read/pen-of c)
        c (if pen (with-pen c pen) c)
        {:keys [min max]} (pen-read/arg-box c)
        away (when min (standing/away-cause c {:x (/ (+ (:x min) (:x max)) 2) :y (:y min) :z (/ (+ (:z min) (:z max)) 2)}))]
    (cond
      (not (or plan box)) (contract/never :no-box "maintain needs a :box or a :plan")
      (not (or plan mob)) (contract/never :no-mob "maintain needs a :mob or a :plan")
      trouble (can-run? c)
      away (checks/cause-status :later away)
      :else (pen-fact c))))

(defn report-with-skips
  "The report so far, plus the skip reason of each step not yet reached (a goal that holds mid-pass ends the run before them)."
  [c args f]
  (let [report (:report (ctx/mem c))]
    (into (or report {})
          (keep (fn [s] (when-let [why (and (not (contains? report s)) (:skip (decide s args f)))] [s {:skipped why}])))
          steps)))

(defn done?
  "Ok when the pen holds the goal: the box is not waiting for a look, no step would run and the herd is at :target
  (before the start: already), with the census as data; else later with the steps that would still run."
  [c]
  (let [{:keys [target] :as args} (need/args-with-ways c)
        f (facts c)
        left (count (filter #(:call (decide % args f)) steps))]
    (if (or (look-due? c) (pos? left) (< (+ (:adults f) (:babies f)) target))
      (contract/later :steps (str left " steps left") :units :steps :done (- (count steps) left) :left left)
      (contract/ok :maintained "the pen is as wanted" :units :steps :done (count steps) :left 0
                   :result {:mob (:mob args) :target target :adults (:adults f) :babies (:babies f) :steps (report-with-skips c args f)}))))

(defn quote-of [_c]
  {:uses {:job-time [0 90 600]}})

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :mend (select-keys r [:closed? :reason])
    :herd (select-keys r [:reason :inside :brought])
    :breed {:fed (count (:fed r)) :reason (:reason r)}
    :cull (select-keys r [:killed :remaining :reason])
    :shear {:shorn (count (:shorn r)) :reason (:reason r) :collected (:collected r)}
    :collect (select-keys r [:collected])
    :deposit (assoc (select-keys r [:reason]) :gave-up (boolean (:gave-up r)))))

(defn finish!
  "End the pass: done unless a step ended short of done and none ran to its end, then nothing-possible :failed-steps."
  [c report]
  (let [{:keys [mob target]} (:args c)
        {:keys [adults babies]} (hunting/census c)
        out {:mob mob :target target :adults (count adults) :babies (count babies) :steps report}]
    (if (and (some :ended (vals report)) (not-any? #(not (or (:ended %) (:skipped %))) (vals report)))
      (ctx/end c :nothing-possible :failed-steps out)
      (ctx/end c :done nil (assoc out :text (str "maintain done: " (count adults) " adults, " (count babies) " babies"))))))

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
  (cond
    (not (look-due? c)) (await (step-steps c))
    (hunting/in-bound? c (u/self-pos c)) (do (await (look/survey! c)) :again)
    :else
    (let [r (await (child/call-walk* c :look {:pos (pen-read/look-pos (pen-read/arg-box c)) :range 3}))]
      (when-not (= :continue r) (ctx/update-mem! c assoc :looked true))
      (if (= :continue r) :yield :again))))

(defn ^:async round
  "The whole pass: each step's child once, until the run ends; yields only while a child waits on the world. A plan that goes bad after can-run? is left to the next check (plan-trouble-round-test)."
  [c0]
  (let [{:keys [trouble] :as pen} (pen-read/pen-of c0)
        c (if pen (with-pen c0 pen) c0)]
    (if trouble
      :continue
      (await (pace/steps! c (fn ^:async s [] (let [r (await (step c))] (if (= :yield r) :continue r))))))))
