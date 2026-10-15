(ns jobs.items.kitchen
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.items.bake :as bake]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.standing :as standing]
            [jobs.lib.steps :as steps]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(def doc
  "Cook what the body carries, as one standing job (put it under repeat). One run is one pass over three steps, each
  a child job:
  - :smelt (jobs.items.smelt), once per raw food of :foods that is carried, in the order of :foods: all of it at
    :furnace (the nearest seen furnace or smoker when nil; the child fetches fuel). A food is tried once a pass.
  - :bake (jobs.items.bake) when 3 or more wheat are carried above its :keep entry or the chest holds them (its
    stock booked in :fetch/stock, or unknown): bake takes the chest's wheat, crafts it at the table near :chest and
    keeps the bread's :keep loaves.
  - :deposit (jobs.storage.deposit) of the cooked foods and bread this pass made, above their :keep entries, into
    :chest; food the body brought or withdrew stays carried.
  A step with nothing to do is skipped and booked {:skipped reason}: :no-food, :no-wheat, :no-chest,
  :nothing-to-store. A step whose child stops is booked with its summary and the stop as :reason; the pass goes on.
  Needs (conditions, a listed job waits): a run under way, or a step that would run (raw food carried, wheat for a loaf
  carried above the keep or in the chest), so a body with nothing to cook is not held, and not too far from the chest
  (:away). A pass that leaves such work (no furnace, no fuel) rests the kitchen 10 minutes (jobs.lib.standing; a rest
  after bake found no table ends once a table is seen near the chest): the wait is {:reason :nothing-to-do :backed-off
  [\"kitchen\"]}, so the job can stand in the queue under repeat without starving other jobs.
  Every pass books info kitchen.done, its text naming what was smelted, baked and stored, and data {:steps {step
  summary}}; smelt summaries are keyed by food, the deposit's has :stored {name n}. It ends done
  when something was cooked, baked or stored; otherwise with the ending of the first step that stopped (its kind and
  cause), or nothing-possible {:reason :nothing-to-do} when every step was skipped.
  Quote: a smelt of each raw food carried.")

(def raw-foods ["beef" "porkchop" "chicken" "mutton" "rabbit" "cod" "salmon"])
(def cooked-foods ["cooked_beef" "cooked_porkchop" "cooked_chicken" "cooked_mutton" "cooked_rabbit" "cooked_cod" "cooked_salmon"])

(a/defargs args
  {:furnace {:doc "furnace or smoker position {:x :y :z}; the nearest one seen when nil" :spec ::a/pos :default nil}
   :chest {:doc "store chest position {:x :y :z}; the known :chest place when nil (none known: wheat is not baked and nothing is stored)" :spec ::a/pos :default nil}
   :foods {:doc "raw foods to smelt, in this order" :spec (a/coll-of a/item?) :default raw-foods}
   :keep {:doc "{item count}: how many of a cooked food, bread or wheat stay carried" :spec (a/map-of string? number?)
          :default (into {"bread" 16} (map (fn [f] [f 8])) cooked-foods)}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(a/defargs settings
  {::time-s {:default 1800 :doc "Seconds a kitchen pass may last: the smelts, the bake and the deposit." :spec (a/num-in 0 nil)}
   ::food-s {:default 10 :doc "Seconds a quote counts for each raw food carried: its time in the furnace." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:time ::time-s}})

(defn done? "A standing job with no goal state." [_c] {:done? false})

(def steps [:smelt :bake :deposit])
(def jobs {:smelt 'jobs.items.smelt :bake 'jobs.items.bake :deposit 'jobs.storage.deposit})
(def job-sym 'jobs.items.kitchen)
(def rest-key "kitchen")

(defn made
  "{name n} the pass has cooked or baked so far (and not yet put in the chest), from its booked report."
  [report]
  (let [bake (:bake report)
        loaves (- (:baked bake 0) (:deposited bake 0))]
    (into (if (pos? loaves) {"bread" loaves} {})
          (keep (fn [[food b]] (when (pos? (:smelted b 0)) [(str "cooked_" food) (:smelted b)])))
          (:smelt report))))

(defn facts
  "What the decisions are made from: {name count} carried, {name n} made this pass, whether a chest is known and the
  wheat it is booked to hold (nil when unknown), the foods tried this pass."
  [c]
  (let [chest (storage/chest-of (ctx/view c) (:args c))
        stock (some->> chest storage/cell-of (get (fetch/stock-of (ctx/view c))))]
    {:carried (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory (:primitives c)))
     :made (made (:report (ctx/mem c)))
     :chest (some? chest)
     :chest-wheat (some-> stock (get "wheat" 0))
     :tried (:tried (ctx/mem c))}))

(defn keep-but-made
  "keep raised so that only the made part of a carried food, and only above its keep, may be stored: what the body
  brought itself stays carried."
  [carried made keep]
  (into keep (map (fn [n] [n (max (get keep n 0) (- (get carried n 0) (get made n 0)))])) (conj cooked-foods "bread")))

(defn to-store
  "The cooked foods and bread carried above their keep, of what the pass made."
  [carried made keep]
  (let [keep (keep-but-made carried made keep)]
    (vec (filter #(> (get carried % 0) (get keep % 0)) (conj cooked-foods "bread")))))

(defn decide
  [step {:keys [furnace chest foods keep ignore-zones?]} {:keys [carried made tried chest-wheat] chest? :chest}]
  (let [zone (when ignore-zones? {:ignore-zones? true})]
    (case step
      :smelt (if-let [food (first (filter #(and (pos? (get carried % 0)) (not (contains? tried %))) foods))]
               {:call {:slot :smelt :job (jobs :smelt) :food food :args (merge zone {:furnace furnace :item food})}}
               {:skip :no-food})
      :bake (cond
              (not chest?) {:skip :no-chest}
              (and chest-wheat (< (+ chest-wheat (max 0 (- (get carried "wheat" 0) (get keep "wheat" 0)))) 3)) {:skip :no-wheat}
              :else {:call {:slot :bake :job (jobs :bake) :args (merge zone {:chest chest :keep (get keep "bread" 16)})}})
      :deposit (let [items (to-store carried made keep)]
                 (cond
                   (not chest?) {:skip :no-chest}
                   (empty? items) {:skip (if (some #(pos? (get carried % 0)) (conj cooked-foods "bread")) :kept-carried :nothing-to-store)}
                   :else {:call {:slot :deposit :job (jobs :deposit) :args (merge zone {:chest chest :items items :keep (keep-but-made carried made keep)})}})))))

(defn decisions [c f] (into {} (map (juxt identity #(decide % (:args c) f))) steps))

(defn would-run? [c f] (boolean (some :call (vals (decisions c f)))))

(defn table-seen?
  "A crafting table is seen near the chest, so a rest for a missing one is over."
  [c]
  (let [chest (storage/chest-of (ctx/view c) (:args c))]
    (boolean (and chest (bake/nearest-table (:primitives c) chest 8)))))

(defn resting
  "The cause of a kitchen that rests after a pass that left work undone, or nil."
  [c]
  (let [r (get (standing/rested c job-sym) rest-key)]
    (when-not (and (= :no-table (:why r)) (table-seen? c))
      (when r {:reason :nothing-to-do :backed-off [rest-key]}))))

(defn away
  "The cause of a kitchen far from its chest while another job is listed (jobs.lib.standing/away-wait), or nil."
  [c]
  (let [w (atom nil)]
    (standing/away-wait (assoc c :wait w) (storage/chest-of (ctx/view c) (:args c)))
    @w))

(defn nothing-to-do
  "The cause of a kitchen with no step to run: each step's skip."
  [c]
  (let [f (facts c)]
    (when-not (would-run? c f)
      {:reason :nothing-to-do :skipped (into {} (map (fn [[s d]] [s (:skip d)])) (decisions c f))})))

(defn unless-under-way
  "A condition that holds only for a pass not yet started: a run under way goes on."
  [f]
  (fn [c] (when-not (:todo (ctx/mem c)) (f c))))

(def conditions [[:fn (unless-under-way away)] [:fn (unless-under-way resting)] [:fn (unless-under-way nothing-to-do)]])

(defn quote-of
  "A smelt of each raw food carried; the rest of the pass is a guess."
  [c]
  (let [f (facts c)
        n (reduce + 0 (map #(get (:carried f) % 0) (:foods (:args c))))
        t (* n (settings/get settings ::food-s))]
    {:uses {:time [0 t (max t (settings/get settings ::time-s))]}}))

(defn summary
  "What a step's child ending e books: its data's fields, and the stop's cause as :reason."
  [step e]
  (let [r (cond-> (:data e) (not= :done (:kind e)) (assoc :reason (contract/cause-key e)))]
    (case step
      :smelt (select-keys r [:smelted :wanted :reason])
      :bake (select-keys r [:baked :deposited :reason])
      :deposit (cond-> (select-keys r [:gave-up :reason])
                 (seq (get-in e [:report :uses :items])) (assoc :stored (get-in e [:report :uses :items]))))))

(defn progressed?
  "Whether a pass booked report cooked, baked or stored something: a step that was skipped or stopped is none."
  [report]
  (let [done? (fn [b] (and (map? b) (not (:skipped b)) (not (:reason b))))]
    (boolean (or (some #(pos? (:smelted % 0)) (vals (:smelt report)))
                 (pos? (:baked (:bake report) 0))
                 (pos? (:deposited (:bake report) 0))
                 (done? (:deposit report))))))

(defn done-text
  "\"kitchen done: smelted beef 3; baked 4; stored bread 14\", or \"kitchen done: nothing\"."
  [report]
  (let [parts (concat (keep (fn [[food b]] (when (pos? (:smelted b 0)) (str "smelted " food " " (:smelted b)))) (:smelt report))
                      (when (pos? (:baked (:bake report) 0)) [(str "baked " (:baked (:bake report)))])
                      (when-let [m (:stored (:deposit report))]
                        [(str "stored " (str/join ", " (map (fn [[n k]] (str n " " k)) (sort m))))]))]
    (str "kitchen done: " (if (seq parts) (str/join "; " parts) "nothing"))))

(defn finish!
  "End the pass: done when it cooked, baked or stored something; else the ending of the first step that stopped (its
  kind and cause), or nothing-possible when every step was skipped."
  [c report]
  (let [rested? (would-run? c (assoc (facts c) :tried nil))
        failed (:failed (ctx/mem c))]
    (when rested?
      (standing/rest! c job-sym rest-key (if (= :no-table (get-in report [:bake :reason])) :no-table :undone)))
    (ctx/emit! c :kitchen.done :info {:steps report :text (done-text report)})
    (cond
      (progressed? report) (ctx/end c :done nil {:steps report})
      failed (ctx/end c (:kind failed) (:cause failed) {:steps report})
      :else (ctx/end c :nothing-possible {:reason :nothing-to-do} {:steps report}))))

(defn after-child
  "Memory after the child of step ended with the summary booked: a smelt marks its food tried and stays; the
  other steps move on."
  [m step food booked e]
  (-> m
      (cond-> (and (not= :done (:kind e)) (not (:failed m))) (assoc :failed (select-keys e [:kind :cause])))
      (dissoc :call-args :food)
      (cond-> (= :smelt step) (-> (update :tried (fnil conj #{}) food) (assoc-in [:report :smelt food] booked))
              (not= :smelt step) (-> (update :todo rest) (assoc-in [:report step] booked)))))

(defn ^:async round
  "One child call after another until the pass is over; :continue only while a child waits on the world."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {} :tried #{} :failed nil))
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
              slot (if (= :smelt step) (keyword (str "smelt-" (:food call))) step)
              _ (ctx/update-mem! c assoc :call-args (:args call) :food (:food call))
              e (await (ctx/call c slot (:job call) (:args call)))]
          (if (= :continue e)
            :continue
            (do (ctx/update-mem! c after-child step (:food call) (summary step e) e)
                (recur))))))))
