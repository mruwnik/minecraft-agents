(ns jobs.items.kitchen
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [jobs.lib.checks :as checks]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.stock :as stock]
            [jobs.lib.standing :as standing]
            [jobs.lib.steps :as steps]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(def doc
  "Keep the store chest stocked with ready food, as one standing job (put it under repeat). done? is true when the
  chest's booked stock (jobs.lib.need, from the last deposit, withdraw or look) holds every :stock entry; unknown
  stock is not done. One run is one pass over four steps, each a child job, each using only what the shortfall needs
  (the shortfall of an item is its :stock entry less the chest's stock and what the pass has made of it):
  - :smelt (jobs.items.smelt), once per raw food of :foods carried above its :reserve entry, in the order of :foods:
    at most the shortfall of its cooked food, at :furnace (the nearest seen furnace or smoker when nil; the child
    fetches fuel). A food is tried once a pass.
  - :take (jobs.storage.withdraw): the wheat for the loaves the bread shortfall needs that the carried wheat above
    its :reserve entry does not cover, from the chest (wheat there above what :reserve still holds back).
  - :bake (jobs.items.craft, bread): the loaves of the carried wheat above its :reserve entry, at most the
    shortfall. The table is the craft's: a seen one, else a carried one put down, else one fetched (:fetch false: none).
  - :deposit (jobs.storage.deposit) of the cooked food and bread this pass made, and wheat taken and not used, into
    :chest; food the body brought or withdrew stays carried. Bread is never withdrawn: carrying food is get-food's.
  :reserve is never used, carried or in the chest (wheat for bait and seed: the default keeps a breeding pair's).
  A step with nothing to do is skipped and booked {:skipped reason}: :no-food, :no-wheat, :no-table (none seen or carried, :fetch false), :no-chest, :stocked,
  :nothing-to-store. A step whose child stops is booked with its summary and the stop as :reason; the pass goes on.
  Needs (can-start?, a listed job waits): a step that would run (raw food carried above the
  reserve with a cooked shortfall, wheat for a loaf), so a body with nothing to cook is not held, and not too far from
  the chest (:away). can-run? (between steps): a chest place lost mid-pass waits :no-chest, a chest cell seen to hold
  another block is never :chest-gone. There is no rest timer: a pass that cannot cook ends with its stop and repeat decides.
  Every pass ends with data {:steps {step summary} :text}, the text naming what was smelted, baked and stored; smelt summaries are keyed by food, the deposit's has :stored {name n}. It ends done
  when something was cooked, baked or stored; otherwise with the ending of the first step that stopped (its kind and
  cause), or nothing-possible {:reason :nothing-to-do} when every step was skipped.
  Quote: a smelt of each raw food carried.")

(def raw-foods ["beef" "porkchop" "chicken" "mutton" "rabbit" "cod" "salmon"])
(def cooked-foods ["cooked_beef" "cooked_porkchop" "cooked_chicken" "cooked_mutton" "cooked_rabbit" "cooked_cod" "cooked_salmon"])

(a/defargs args
  {:furnace {:doc "furnace or smoker position {:x :y :z}; the nearest one seen when nil" :spec ::a/pos :default nil}
   :chest {:doc "store chest position {:x :y :z}; the known :chest place when nil (none known: nothing is baked or stored)" :spec ::a/pos :default nil}
   :foods {:doc "raw foods to smelt, in this order" :spec (a/coll-of a/item?) :default raw-foods}
   :stock {:doc "{item count}: what the chest should hold of ready food and bread" :spec (a/map-of string? number?)
           :default (into {"bread" 16} (map (fn [f] [f 8])) cooked-foods)}
   :reserve {:doc "{item count}: never used, carried or in the chest (wheat for bait, seed)" :spec (a/map-of string? number?)
             :default {"wheat" 2}}
   :fetch {:doc "get a crafting table when none is seen or carried (jobs.items.craft's fetch); false skips take and bake then" :spec boolean? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a kitchen pass may last: the smelts, the bake and the deposit." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds a kitchen pass may last from its start, the waits on the furnace's cook and other jobs meanwhile included." :spec (a/num-in 0 nil)}
   ::food-s {:default 10 :doc "Seconds a quote counts for each raw food carried: its time in the furnace." :spec (a/num-in 0 nil)}})

(def target [:stock])

(def safe? (ready/safe-at-by-night #(:furnace (:args %))))
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn reached?
  "Whether the chest's stock {name n} holds every :stock entry (nil: unknown, never reached)."
  [{:keys [stock]} chest-stock]
  (boolean (and chest-stock (every? (fn [[item n]] (>= (get chest-stock item 0) n)) stock))))

(defn chest-stock-of
  "{name n} the chest is booked to hold, or nil when unknown."
  [c]
  (some->> (storage/chest-of (ctx/view c) (:args c)) storage/cell-of (get (stock/stock-of (ctx/view c)))))

(defn done?
  "The chest's booked stock holds every :stock entry and no pass is under way (a pass ends itself once it has stored
  what it made)."
  [c]
  (let [{:keys [stock]} (:args c)
        chest-stock (chest-stock-of c)
        left (count (remove (fn [[item n]] (>= (get chest-stock item 0) n)) stock))
        data [:units :stock-entries :done (- (count stock) left) :left left :result {:steps (or (:report (ctx/mem c)) {})}]]
    (cond
      (not (reached? (:args c) chest-stock)) (apply contract/later :short (str "kitchen: " left " stock entries short") data)
      (:todo (ctx/mem c)) (apply contract/later :pass-under-way "kitchen: stocked, the pass goes on" data)
      :else (apply contract/ok :stocked "kitchen: the chest is stocked" data))))

(def steps [:smelt :take :bake :deposit])
(def jobs {:smelt 'jobs.items.smelt :take 'jobs.storage.withdraw :bake 'jobs.items.craft :deposit 'jobs.storage.deposit})
(def job-sym 'jobs.items.kitchen)

(defn made
  "{name n} the pass has cooked or baked so far (and not yet put in the chest), from its booked report."
  [report]
  (let [loaves (get-in report [:bake :baked] 0)]
    (into (if (pos? loaves) {"bread" loaves} {})
          (keep (fn [[food b]] (when (pos? (:smelted b 0)) [(str "cooked_" food) (:smelted b)])))
          (:smelt report))))

(defn facts
  "What the decisions are made from: {name count} carried, {name n} made this pass, whether a chest is known, the
  stock it is booked to hold (nil when unknown), the wheat carried when the pass began, the foods tried this pass."
  [c]
  {:carried (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory (:primitives c)))
   :made (made (:report (ctx/mem c)))
   :chest (some? (storage/chest-of (ctx/view c) (:args c)))
   :table (boolean (or (pos? (storage/carried (u/inventory (:primitives c)) "crafting_table"))
                       (crafting-table/nearest (:primitives c) 64 nil)))
   :chest-stock (chest-stock-of c)
   :wheat0 (:wheat0 (ctx/mem c))
   :tried (:tried (ctx/mem c))})

(defn shortfall
  "How many of item the chest still lacks, counting what the pass has made."
  [stock item {:keys [chest-stock made]}]
  (max 0 (- (get stock item 0) (get chest-stock item 0) (get made item 0))))

(defn usable
  "{:carried n :chest n-or-nil} of item above the reserve, which holds back what the body carried when the pass began
  (wheat0) first, then the chest's; :chest is nil when the chest's stock is unknown."
  [reserve item {:keys [carried chest-stock wheat0]}]
  (let [have (get carried item 0)
        held (get reserve item 0)
        base (if (= "wheat" item) (or wheat0 have) have)
        held-here (min held base)]
    {:carried (max 0 (- have held-here))
     :chest (when chest-stock (max 0 (- (get chest-stock item 0) (- held held-here))))}))

(defn loaves
  "How many loaves the bread shortfall asks for and the wheat above the reserve allows (an unknown chest is taken to
  have what is asked)."
  [{:keys [stock reserve]} f]
  (let [want (shortfall stock "bread" f)
        {cu :carried chest :chest} (usable reserve "wheat" f)]
    (min want (quot (+ cu (or chest (* 3 want))) 3))))

(defn decide
  [step {:keys [furnace chest foods stock reserve ignore-zones?] :as a} {:keys [carried made tried wheat0] chest? :chest table? :table :as f}]
  (let [zone (when ignore-zones? {:ignore-zones? true})
        reserve (or reserve {})]
    (case step
      :smelt (if-let [[food n] (first (keep (fn [food]
                                              (let [n (min (:carried (usable reserve food f)) (shortfall stock (str "cooked_" food) f))]
                                                (when (and (pos? n) (not (contains? tried food))) [food n])))
                                            foods))]
               {:call {:slot :smelt :job (jobs :smelt) :food food :args (merge zone {:furnace furnace :item food :count n})}}
               {:skip (if (some #(pos? (get carried % 0)) foods) :stocked :no-food)})
      :take (let [want (loaves a f)
                  cu (:carried (usable reserve "wheat" f))
                  need (- (* 3 want) cu)]
              (cond
                (not chest?) {:skip :no-chest}
                (and (not table?) (not (:fetch a true))) {:skip :no-table}
                (not (pos? want)) {:skip :no-wheat}
                (not (pos? need)) {:skip :stocked}
                :else {:call {:slot :take :job (jobs :take)
                              :args (merge zone {:chest chest :items {"wheat" (+ (get carried "wheat" 0) need)}})}}))
      :bake (let [n (min (loaves a f) (quot (:carried (usable reserve "wheat" f)) 3))]
              (cond
                (not chest?) {:skip :no-chest}
                (and (not table?) (not (:fetch a true))) {:skip :no-table}
                (not (pos? n)) {:skip :no-wheat}
                :else {:call {:slot :bake :job (jobs :bake) :args {:item "bread" :count (+ n (get carried "bread" 0)) :fetch (:fetch a true)}}}))
      :deposit (let [items (vec (filter #(pos? (min (get made % 0) (get carried % 0))) (conj cooked-foods "bread")))
                     spare-wheat? (> (get carried "wheat" 0) (or wheat0 (get carried "wheat" 0)))
                     items (cond-> items spare-wheat? (conj "wheat"))
                     keep (cond-> (into {} (map (fn [n] [n (max 0 (- (get carried n 0) (get made n 0)))])) items)
                            spare-wheat? (assoc "wheat" wheat0))]
                 (cond
                   (not chest?) {:skip :no-chest}
                   (empty? items) {:skip :nothing-to-store}
                   :else {:call {:slot :deposit :job (jobs :deposit) :args (merge zone {:chest chest :items items :keep keep})}})))))

(defn decisions [c f] (into {} (map (juxt identity #(decide % (:args c) f))) steps))

(defn would-run? [c f] (boolean (some :call (vals (decisions c f)))))

(defn away
  "The cause of a kitchen far from its chest while another job is listed (jobs.lib.standing/away-cause), or nil."
  [c]
  (standing/away-cause c (storage/chest-of (ctx/view c) (:args c))))

(defn nothing-to-do
  "The cause of a kitchen with no step to run: each step's skip."
  [c]
  (let [f (facts c)]
    (when-not (would-run? c f)
      {:reason :nothing-to-do :skipped (into {} (map (fn [[s d]] [s (:skip d)])) (decisions c f))})))

(defn can-start?
  "Later while the kitchen is far from its chest or has no step to run."
  [c]
  (contract/all [(checks/cause-status :later (away c)) (checks/cause-status :later (nothing-to-do c))]))

(defn chest-usable
  "Between steps: never :chest-gone when the body has seen the chest cell hold another block, later :no-chest when
  no chest is known (a place lost mid-pass), else ok. A chest not seen since is taken as there."
  [c]
  (let [pos (storage/chest-of (ctx/view c) (:args c))
        block (when pos (u/seen-name (:primitives c) pos))]
    (if (places/gone? :chest block)
      (contract/never :chest-gone (str "the chest at " (pr-str pos) " is gone, " block " is there"))
      (checks/chest-known c))))

(def can-run? chest-usable)

(defn quote-of
  "A smelt of each raw food carried; the rest of the pass is a guess."
  [c]
  (let [f (facts c)
        n (reduce + 0 (map #(get (:carried f) % 0) (:foods (:args c))))
        t (* n (settings/get settings ::food-s))]
    {:uses {:job-time [0 t (max t (settings/get settings ::job-time-s))]}}))

(defn summary
  "What a step's child ending e books: its data's fields, and the stop's cause as :reason."
  [step e]
  (let [r (cond-> (:data e) (not= :done (:kind e)) (assoc :reason (contract/cause-key e)))]
    (case step
      :smelt (select-keys r [:smelted :wanted :reason])
      :take (select-keys r [:reason])
      :bake (cond-> (select-keys r [:reason]) (:made r) (assoc :baked (:made r)))
      :deposit (cond-> (select-keys r [:gave-up :reason])
                 (seq (get-in e [:report :uses :items])) (assoc :stored (get-in e [:report :uses :items]))))))

(defn progressed?
  "Whether a pass booked report cooked or baked something, or stored more than leftover wheat."
  [report]
  (boolean (or (some #(pos? (:smelted % 0)) (vals (:smelt report)))
               (pos? (:baked (:bake report) 0))
               (seq (dissoc (:stored (:deposit report)) "wheat")))))

(defn done-text
  "\"kitchen done: smelted beef 3; baked 4; stored bread 14\", or \"kitchen done: nothing\"."
  [report]
  (let [parts (concat (keep (fn [[food b]] (when (pos? (:smelted b 0)) (str "smelted " food " " (:smelted b)))) (:smelt report))
                      (when (pos? (:baked (:bake report) 0)) [(str "baked " (:baked (:bake report)))])
                      (when-let [m (:stored (:deposit report))]
                        [(str "stored " (str/join ", " (map (fn [[n k]] (str n " " k)) (sort m))))]))]
    (str "kitchen done: " (if (seq parts) (str/join "; " parts) "nothing"))))

(defn finish!
  "End the pass: done when it cooked, baked or stored something; else the first step's stopping ending (as :because), or nothing-possible when every step was skipped."
  [c report]
  (let [failed (:failed (ctx/mem c))
        data {:steps report :text (done-text report)}]
    (cond
      (progressed? report) (ctx/end c :done nil data)
      failed (contract/with-because (ctx/end c (:kind failed) (:cause failed) data) failed)
      :else (ctx/end c :nothing-possible {:reason :nothing-to-do} data))))

(defn after-child
  "Memory after the child of step ended with the summary booked: a smelt marks its food tried and stays; the
  other steps move on."
  [m step food booked e]
  (-> m
      (cond-> (and (not= :done (:kind e)) (not (:failed m))) (assoc :failed e))
      (dissoc :call-args :food)
      (cond-> (= :smelt step) (-> (update :tried (fnil conj #{}) food) (assoc-in [:report :smelt food] booked))
              (not= :smelt step) (-> (update :todo rest) (assoc-in [:report step] booked)))))

(defn ^:async step!
  "One child call (:again after it ended), :continue while it waits on the world, or the end of the pass."
  [c]
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
              :again))))))

(defn ^:async round
  "One child call after another until the pass is over; :continue only while a child waits on the world."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {} :tried #{} :failed nil
                     :wheat0 (storage/carried (u/inventory (:primitives c)) "wheat")))
  (await (pace/steps! c #(step! c))))

(def meta {:tags [:food :smelt]})
