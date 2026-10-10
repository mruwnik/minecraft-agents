(ns jobs.items.kitchen
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.standing :as standing]
            [jobs.lib.steps :as steps]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(def doc
  "Cook what the body carries, as one standing job (put it under repeat). One run is one pass over three steps, each
  a child job:
  - :smelt (jobs.items.smelt), once per raw food of :foods that is carried, in the order of :foods: all of it at
    :furnace (the nearest seen furnace or smoker when nil; the child fetches fuel). A food is tried once a pass.
  - :bake (jobs.items.bake) when 3 or more wheat are carried above its :keep entry: crafts it at the table near
    :chest and keeps the bread's :keep loaves.
  - :deposit (jobs.storage.deposit) of the cooked foods and bread carried above their :keep entries into :chest.
  A step with nothing to do is skipped and booked {:skipped reason}: :no-food, :no-wheat, :no-chest,
  :nothing-to-store. The check passes when a run is under way or a step would run (raw food carried, wheat for a
  loaf above the keep), so a body with nothing to cook is not held. A pass that leaves such work (no furnace, no
  fuel) rests the kitchen 10 minutes (jobs.lib.standing): the check waits {:reason :nothing-to-do :backed-off
  [\"kitchen\"]}, so the job can stand in the queue under repeat without starving other jobs.
  Ends :done with info kitchen.done and {:steps {step summary}}; smelt summaries are keyed by food.")

(def raw-foods ["beef" "porkchop" "chicken" "mutton" "rabbit" "cod" "salmon"])
(def cooked-foods ["cooked_beef" "cooked_porkchop" "cooked_chicken" "cooked_mutton" "cooked_rabbit" "cooked_cod" "cooked_salmon"])

(a/defargs args
  {:furnace {:doc "furnace or smoker position {:x :y :z}; the nearest one seen when nil" :spec ::a/pos :default nil}
   :chest {:doc "store chest position {:x :y :z}; the known :chest place when nil (none known: wheat is not baked and nothing is stored)" :spec ::a/pos :default nil}
   :foods {:doc "raw foods to smelt, in this order" :spec (a/coll-of a/item?) :default raw-foods}
   :keep {:doc "{item count}: how many of a cooked food, bread or wheat stay carried" :spec (a/map-of string? number?)
          :default (into {"bread" 16} (map (fn [f] [f 8])) cooked-foods)}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(def steps [:smelt :bake :deposit])
(def jobs {:smelt 'jobs.items.smelt :bake 'jobs.items.bake :deposit 'jobs.storage.deposit})
(def job-sym 'jobs.items.kitchen)
(def rest-key "kitchen")

(defn facts
  "What the decisions are made from: {name count} carried, whether a chest is known, the foods tried this pass."
  [c]
  {:carried (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory (:primitives c)))
   :chest (some? (storage/chest-of (ctx/view c) (:args c)))
   :tried (:tried (ctx/mem c))})

(defn to-store
  "The cooked foods and bread carried above their keep."
  [carried keep]
  (vec (filter #(> (get carried % 0) (get keep % 0)) (conj cooked-foods "bread"))))

(defn decide
  [step {:keys [furnace chest foods keep ignore-zones?]} {:keys [carried tried] chest? :chest}]
  (let [zone (when ignore-zones? {:ignore-zones? true})]
    (case step
      :smelt (if-let [food (first (filter #(and (pos? (get carried % 0)) (not (contains? tried %))) foods))]
               {:call {:slot :smelt :job (jobs :smelt) :food food :args (merge zone {:furnace furnace :item food})}}
               {:skip :no-food})
      :bake (cond
              (not chest?) {:skip :no-chest}
              (< (- (get carried "wheat" 0) (get keep "wheat" 0)) 3) {:skip :no-wheat}
              :else {:call {:slot :bake :job (jobs :bake) :args (merge zone {:chest chest :keep (get keep "bread" 16)})}})
      :deposit (let [items (to-store carried keep)]
                 (cond
                   (not chest?) {:skip :no-chest}
                   (empty? items) {:skip :nothing-to-store}
                   :else {:call {:slot :deposit :job (jobs :deposit) :args (merge zone {:chest chest :items items :keep keep})}})))))

(defn decisions [c f] (into {} (map (juxt identity #(decide % (:args c) f))) steps))

(defn would-run? [c f] (boolean (some :call (vals (decisions c f)))))

(defn check
  "A run is under way, or a step would run and the kitchen is not resting."
  [c]
  (let [f (facts c)
        resting (get (standing/rested c job-sym) rest-key)]
    (cond
      (:todo (ctx/mem c)) true
      resting (ctx/wait c {:reason :nothing-to-do :backed-off [rest-key]})
      (would-run? c f) true
      :else (ctx/wait c {:reason :nothing-to-do :skipped (into {} (map (fn [[s d]] [s (:skip d)])) (decisions c f))}))))

(defn summary [step r]
  (case step
    :smelt (select-keys r [:smelted :wanted :reason])
    :bake (select-keys r [:baked :deposited :reason])
    :deposit (select-keys r [:gave-up :reason])))

(defn finish! [c report]
  (when (would-run? c (assoc (facts c) :tried nil))
    (standing/rest! c job-sym rest-key :undone))
  (ctx/emit! c :kitchen.done :info {:steps report :text (str "kitchen done: " (count report) " steps")})
  (ctx/result! c {:steps report})
  :done)

(defn after-child
  "Memory after the child of step ended (outcome :done or :declined): a smelt marks its food tried and stays; the
  other steps move on."
  [m step food outcome r]
  (let [booked (if (= :done outcome) r {:skipped :declined})]
    (-> m
        (dissoc :call-args :food)
        (cond-> (= :smelt step) (-> (update :tried (fnil conj #{}) food) (assoc-in [:report :smelt food] booked))
                (not= :smelt step) (-> (update :todo rest) (assoc-in [:report step] booked))))))

(defn ^:async round
  "One child call after another until the pass is over; :continue only while a child waits on the world."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {} :tried #{}))
  (loop []
    (let [m (ctx/mem c)
          running (when-let [call-args (:call-args m)]
                    {:slot (first (:todo m)) :job (jobs (first (:todo m))) :args call-args :food (:food m)})
          {:keys [todo report call]} (if running
                                       {:todo (:todo m) :report (:report m) :call running}
                                       (steps/plan #(decide %1 %2 %3) (:todo m) (:args c) (facts c) (:report m)))]
      (ctx/update-mem! c assoc :todo todo :report report)
      (if-not call
        (finish! c report)
        (let [step (:slot call)
              _ (ctx/update-mem! c assoc :call-args (:args call) :food (:food call))
              outcome (await (ctx/call-child c (if (= :smelt step) (keyword (str "smelt-" (:food call))) step) (:job call) (:args call)))]
          (if (#{:done :declined} outcome)
            (do (ctx/update-mem! c after-child step (:food call) outcome
                                 (when (= :done outcome) (summary step (ctx/child-result c (if (= :smelt step) (keyword (str "smelt-" (:food call))) step)))))
                (recur))
            :continue))))))
