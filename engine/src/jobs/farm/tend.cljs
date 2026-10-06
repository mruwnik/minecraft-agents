(ns jobs.farm.tend
  (:require [clojure.string :as str]
            [jobs.farm.permit :as permit]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.farm.fertilize :as fertilize]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.farm.harvest :as harvest]
            [jobs.farm.plant :as plant]
            [jobs.farm.tidy :as tidy]
            [jobs.farm.till :as till]
            [plan.shape :as shape]
            [jobs.lib.world :as known]))

(def doc
  "Keep one field of crops in order. The field is the :box (inclusive). Its lowest layer (y = min) is the
  farmland and the layer above holds the crops, so max y is at least min y + 1.
  One run is one pass over six steps in this order. Each step is a child job that runs when its conditions hold:
  - :harvest (jobs.farm.harvest, replanting): a ripe crop lies in the box.
  - :till (jobs.farm.till): :till is true, a hoe is carried, dirt or grass lies uncovered in the ground layer and
    more seed is carried than there is bare farmland. It tills one cell at a time, nearest first.
  - :plant (jobs.farm.plant): bare farmland lies in the box and a seed is carried.
  - :fertilize (jobs.farm.fertilize): :fertilize is true, bone meal is carried and an unripe crop lies in the box.
  - :compost (jobs.farm.compost): a :composter is given and seed above the keep is carried.
  - :deposit (jobs.storage.deposit): a :chest is given and farm goods are carried above the keep. Farm goods are
    seeds, crops (carrots, potatoes and beetroot too), melon, pumpkin, hay and cocoa. Tools, bread and other
    non-crop food are never stored.
  The keep for compost and deposit, per name, is the largest of: the sowing reserve (twice the number of beds, farmland or
  untilled ground cells, spread over the carried seeds), a backup stack (64) of each seed type, the body's food reserve
  (jobs.lib.cost/food-reserve: 3 days of food, 60 hunger points, best food first; carrots and potatoes count toward it
  first) and the :keep entry. Harvested crops above it are stored; seed above it is composted.
  Skipped steps are booked as {:skipped reason}: :no-ripe, :till-off, :no-hoe, :nothing-to-till, :no-seed,
  :no-bare, :fertilize-off, :no-bone-meal, :none-unripe, :no-composter, :no-surplus-seed, :no-chest,
  :nothing-to-store or :declined (the child declined).
  A step that ran keeps its summary even if it is skipped later in the same run.
  The job declines (does nothing) unless the box has at most 2048 cells and some step would run. A started run
  always continues. A step under way is not re-decided.
  It ends :done with {:steps {step summary} :field {:crops :bare :untilled}} (info farm-tend.done). :field is
  counted live at the end. Summaries: harvest {:cut :replanted :bare :gave-up}, till {:tilled n}, plant {:planted
  :reason :skipped}, fertilize {:used}, compost {:fed :bone-meal :reason}, deposit {:gave-up :reason}.
  Limits: harvest and fertilize work in a sphere around the box centre, so crops just outside the box may be
  touched. A dirt lane inside the box counts as a bed unless :till is false. A ripe crop that cannot be reached
  keeps the check passing on every run (harvest gives up on it and the run still ends).
  :plan (optionally :part) makes the plan's crop cells the field instead of :box. Only crops harvest knows
  (wheat, carrots, potatoes, beetroots) count. The plan's other cells are never touched. The same six steps run
  per cell, with the crop the plan names there:
  - harvest never digs a wrong crop. A ripe crop of another kind in a crop cell stays standing and is listed in
    :field :wrong {:pos :found :want} (warn farm-tend.wrong).
  - till hoes the ground under a crop cell (the cell below, unless the plan names it as something else) while
    a hoe is carried, the cell's seed outnumbers the bare farmland waiting for it and the access rules allow it.
    Farmland the plan wants without a crop over it is not tilled, since it would revert.
  - plant sows every bare crop cell with its own crop's seed. A cell whose seed is not carried stays bare, with one
    farm-tend.short-seed warn per missing seed.
  - fertilize, compost and deposit work as in box mode. The seed reserve is twice the planned cells of each crop.
  Tilling and sowing are checked against zones and the footprints of the other active plans, when a cell is chosen
  and again before the act. Untilled ground the rules refuse is left alone with one farm-tend.refused warn.
  When only refused ground remains, the check stays false: the job waits and the reason is on record.
  The job declines (one farm-tend.declined warn) while the plan is missing, unreadable, has no crop cells, or no
  zone list has been read. A started run declines in its next round if the plan stops being workable.")

(def args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; y min is the farmland layer, max y at least min y + 1; at most 2048 cells; required unless :plan is given (without either the check declines)" :default nil}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field, each cell worked with the crop the plan wants there (then :box is not used)" :default nil}
   :part {:doc "with :plan, only the cells of this part" :default nil}
   :till {:doc "hoe untilled dirt and grass in the ground layer when seed and a hoe are carried" :default true}
   :fertilize {:doc "use bone meal on unripe crops" :default false}
   :composter {:doc "composter position {:x :y :z} for seed above the reserve; nil: do not compost" :type :pos :default nil}
   :chest {:doc "chest position {:x :y :z} for the farm goods; nil: do not store" :type :pos :default nil}
   :keep {:doc "{item-name count}: how many of an item compost and deposit leave carried, at least the seed reserve" :default {}}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to harvest, till, plant, fertilize and compost); the rules of the game allow it" :default false}})

(def steps [:harvest :till :plant :fertilize :compost :deposit])

(def jobs
  {:harvest 'jobs.farm.harvest
   :till 'jobs.farm.till
   :plant 'jobs.farm.plant
   :fertilize 'jobs.farm.fertilize
   :compost 'jobs.farm.compost
   :deposit 'jobs.storage.deposit})

(def max-cells 2048)

(def seed-items ["wheat_seeds" "carrot" "potato" "beetroot_seeds"])

(def waste-seeds ["wheat_seeds" "beetroot_seeds" "melon_seeds" "pumpkin_seeds"])

(def seed-backup "A stack of each seed type stays carried; the rest is composted." 64)

(def farm-goods
  ["wheat_seeds" "beetroot_seeds" "melon_seeds" "pumpkin_seeds" "carrot" "potato" "wheat" "beetroot" "melon_slice"
   "melon" "pumpkin" "poisonous_potato" "hay_block" "cocoa_beans"])

(def crop-names (set (keys harvest/ripe-age)))

;; ------------------------------------------------------------------ pure helpers

(defn box-cells
  "How many cells the box covers."
  [{:keys [min max]}]
  (apply * (map #(inc (- (% max) (% min))) [:x :y :z])))

(defn usable-box?
  "A box with a crop layer above its farmland layer, of at most max-cells cells."
  [box]
  (boolean (and box
                (>= (:y (:max box)) (inc (:y (:min box))))
                (<= (box-cells box) max-cells))))

(defn in-box? [{:keys [min max]} {:keys [x y z]}]
  (and (<= (:x min) x (:x max)) (<= (:y min) y (:y max)) (<= (:z min) z (:z max))))

(defn centre
  "The box centre rounded to cells."
  [{:keys [min max]}]
  (into {} (map (fn [k] [k (js/Math.round (/ (+ (k min) (k max)) 2))])) [:x :y :z]))

(defn radius
  "How far a sphere around mid must reach to cover the box: the farthest corner, rounded up, plus 1."
  [{:keys [min max]} mid]
  (inc (js/Math.ceil (apply js/Math.max (for [x [(:x min) (:x max)] y [(:y min) (:y max)] z [(:z min) (:z max)]]
                                          (u/dist mid {:x x :y y :z z}))))))

(defn carried
  "{name count} of an inventory ([{:name :count}])."
  [inventory]
  (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} inventory))

(defn reserve
  "{seed count} to keep: twice the beds in all, taken one at a time from each carried seed that has more than is reserved already."
  [beds inventory]
  (let [have (carried inventory)]
    (loop [budget (* 2 beds) res {}]
      (let [open (filterv #(> (get have % 0) (get res % 0)) seed-items)]
        (if (or (zero? budget) (empty? open))
          res
          (let [[res budget] (reduce (fn [[r b] item]
                                       (if (zero? b)
                                         (reduced [r b])
                                         [(update r item (fnil inc 0)) (dec b)]))
                                     [res budget]
                                     open)]
            (recur budget res)))))))

(defn surplus
  "The names (in the order of names) of which more is carried than the keep map holds back."
  [inventory keep names]
  (let [have (carried inventory)]
    (filterv #(> (get have % 0) (get keep % 0)) names)))

(defn keeps
  "{name count} kept carried: the sowing reserve, a backup stack of each seed type, the body's 3-day food
  (cost/food-reserve) and the :keep entries, whichever is largest for a name."
  [sow inventory keep]
  (merge-with max sow (zipmap waste-seeds (repeat seed-backup)) (cost/food-reserve inventory) keep))

(defn untilled?
  "A ground cell {:name :above} of dirt or grass with nothing but air or ground cover over it."
  [{:keys [name above]}]
  (boolean (and (till/tillable name)
                (or (till/air above) (till/ground-cover above)))))

(defn beds
  "How many ground cells are farmland or untilled."
  [ground]
  (count (filter #(or (= "farmland" (:name %)) (untilled? %)) ground)))

;; ------------------------------------------------------------------ facts

(defn ground-layer
  "[{:pos :name :above}] for the cells of the lowest layer of the box."
  [p {:keys [min max]}]
  (vec (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))
             :let [pos {:x x :y (:y min) :z z}]]
         {:pos pos :name (u/block-name p pos) :above (u/block-name p (update pos :y inc))})))

(defn unripe-in-box
  "The unripe crop cells of the box, read around its centre."
  [p box mid R]
  (->> (crops/seen-crops p (keys fertilize/ripe-age) (+ R (u/dist (u/pos-of (.-pos (.self p))) mid)) 4096)
       (filter #(some-> (:age %) (< (fertilize/ripe-age (:name %)))))
       (map :pos)
       (filter #(in-box? box %))
       vec))

(defn box-permitted
  "The poss of the box that zones, claims and footprints do not refuse for action, quietly (the child warns when it declines)."
  [c action poss]
  (let [in (access/rules-input c)]
    (filterv #(not (gate/refused? (access/may? in action %))) poss)))

(defn box-facts
  "What the decisions are made from for a box, read live."
  [c]
  (let [p (:primitives c)
        {:keys [box fertilize keep]} (:args c)
        mid (centre box)
        R (radius box mid)
        inventory (u/inventory p)
        me (u/self-pos c)
        tried (:till-tried (ctx/mem c) #{})
        ground (ground-layer p box)
        bare (count (box-permitted c :sow (plant/bare-cells p box [])))
        keep-of (keeps (reserve (beds ground) inventory) inventory keep)]
    {:mid mid
     :radius R
     :ripe (count (box-permitted c :harvest (filter #(in-box? box %) (harvest/ripe-crops p {:radius R :crops nil} mid []))))
     :hoe (some? (till/hoe-of p))
     :untilled (->> ground
                    (filter untilled?)
                    (map :pos)
                    (remove tried)
                    (box-permitted c :dig)
                    (sort-by #(u/dist me %))
                    vec)
     :bare bare
     :seeds (transduce (map #(get (plant/sowable inventory) % 0)) + 0 seed-items)
     :seed (some? (plant/pick-seed nil inventory))
     :meal (boolean (fertilize/has-meal? p))
     :unripe (if fertilize (count (unripe-in-box p box mid R)) 0)
     :keep keep-of
     :waste (surplus inventory keep-of waste-seeds)
     :stored (surplus inventory keep-of farm-goods)}))

(defn box-census
  "The live {:crops :bare :untilled} of the box."
  [c]
  (let [p (:primitives c)
        {:keys [min max] :as box} (:box (:args c))
        above (inc (:y min))]
    {:crops (count (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))
                         :when (crop-names (u/block-name p {:x x :y above :z z}))]
                     1))
     :bare (count (plant/bare-cells p box []))
     :untilled (count (filter untilled? (ground-layer p box)))}))

;; ------------------------------------------------------------------ plan mode

(defn farmland-want?
  "Whether a plan want accepts farmland: the block, a block map of it, or an :any naming it."
  [want]
  (cond
    (string? want) (= "farmland" want)
    (map? want) (= "farmland" (:block want))
    (vector? want) (boolean (some farmland-want? (rest want)))
    :else false))

(defn ground-cells
  "{ground-pos crop}: the plan's farmland under the crop cells crops {pos crop}, the cell below each unless the plan
  (answer) names it with a want that is not farmland."
  [answer crops]
  (let [wants (into {} (map (fn [{:keys [pos want]}] [(harvest/cell-pos pos) want])) (:cells answer))]
    (into {} (keep (fn [[pos crop]]
                     (let [g (update pos :y dec)]
                       (when (or (not (contains? wants g)) (farmland-want? (wants g)))
                         [g crop]))))
          crops)))

(defn planned
  "With :plan, {:answer :crops {pos crop}} when the plan can be worked, else {:trouble text} (warned once per reason);
  nil without :plan."
  [c]
  (when-let [id (:plan (:args c))]
    (let [part (:part (:args c))
          answer (known/plan c id)
          crops (harvest/crop-cells answer part)
          trouble (or (harvest/plan-trouble answer crops)
                      (when (nil? (known/zones c)) "no zone list has been read"))]
      (if-not trouble
        {:answer answer :crops crops}
        (do (ctx/warn-once! c [id trouble] :farm-tend.declined
                            {:plan id :part part :reason trouble
                             :text (str "tend declines plan " id (when part (str " part " part)) ": " trouble)})
            {:trouble trouble})))))

(defn ground-cell
  "{:pos :name :above} of a ground cell."
  [p pos]
  {:pos pos :name (u/block-name p pos) :above (u/block-name p (update pos :y inc))})

(defn unripe-planned
  "The planned cells holding their crop, not yet ripe."
  [p crops]
  (filterv (fn [[pos crop]]
             (let [b (u/block-at p pos)]
               (and b (= crop (.-name b)) (some-> (.-age b) (< (harvest/ripe-age crop))))))
           crops))

(defn wrong-crops
  "[{:pos :found :want}] of the crop cells that hold a crop of another kind."
  [p crops]
  (vec (keep (fn [[{:keys [x y z]} crop]]
               (let [found (u/block-name p {:x x :y y :z z})]
                 (when (and found (tidy/crop-blocks found) (not (contains? (shape/crop-names crop) found)))
                   {:pos [x y z] :found found :want (shape/want-text {:crop crop})})))
             crops)))

(defn seed-reserve
  "{seed count}: twice the planned cells of each crop."
  [crops]
  (into {} (for [[crop n] (frequencies (vals crops))] [(harvest/seed-of crop) (* 2 n)])))

(defn note-refused!
  "One farm-tend.refused warn per job when the rules refuse the hoe on untilled ground cells of the plan for a
  social reason: names the zones, claims and plans and the owners that refuse, so a run with nothing else to do is
  never silent. Returns the refused positions."
  [c plan cells]
  (let [verdicts (into [] (keep (fn [pos] (let [v (permit/permit c plan :dig pos)]
                                            (when (gate/refused? v) [pos v]))))
                       cells)
        vs (map second verdicts)
        fields {:zones (vec (distinct (keep :zone vs))) :claims (vec (distinct (keep :claim vs)))
                :plans (vec (distinct (keep :plan vs))) :owners (vec (distinct (keep :owner vs)))}]
    (when (seq verdicts)
      (ctx/warn-once! c [plan :refused] :farm-tend.refused
                      (assoc fields :plan plan :count (count verdicts)
                             :text (str "tend of " plan " leaves " (count verdicts) " untilled cells: refused by "
                                        (access/refusal-text fields)
                                        (when (seq (:owners fields)) (str " (owner " (str/join ", " (:owners fields)) ")"))))))
    (mapv first verdicts)))

(defn plan-facts
  "What the decisions are made from for a plan, read live."
  [c]
  (let [p (:primitives c)
        {:keys [plan fertilize keep]} (:args c)
        {:keys [answer crops]} (planned c)
        inventory (u/inventory p)
        have (plant/sowable inventory)
        me (u/self-pos c)
        tried (:till-tried (ctx/mem c) #{})
        bare (harvest/planned-bare p crops)
        bare-of (frequencies (map :seed bare))
        sowing (plant/sowing c crops)
        [mid R] (harvest/plan-field crops)
        keep (keeps (seed-reserve crops) inventory keep)
        untilled-cells (->> (ground-cells answer crops)
                            (remove (fn [[pos _]] (tried pos)))
                            (filter (fn [[pos _]] (untilled? (ground-cell p pos)))))
        _ (note-refused! c plan (map key untilled-cells))
        tillable (filter (fn [[pos _]] (permit/ok? c plan :dig pos)) untilled-cells)
        seeded (filter (fn [[_ crop]] (let [seed (harvest/seed-of crop)] (> (get have seed 0) (get bare-of seed 0)))) tillable)]
    {:mid mid
     :radius R
     :ripe (count (harvest/planned-ripe p {} crops []))
     :hoe (some? (till/hoe-of p))
     :untilled (->> seeded (map key) (sort-by #(u/dist me %)) vec)
     :till-short (- (count tillable) (count seeded))
     :bare (- (count bare) (count (:refused sowing)))
     :seed (boolean (seq (:ready sowing)))
     :short (:short sowing)
     :meal (boolean (fertilize/has-meal? p))
     :unripe (if fertilize (count (unripe-planned p crops)) 0)
     :keep keep
     :waste (surplus inventory keep waste-seeds)
     :stored (surplus inventory keep farm-goods)}))

(defn note-short!
  "One farm-tend.short-seed warn per seed the plan wants sown that is not carried (once per job and seed)."
  [c seeds]
  (doseq [seed (sort seeds)]
    (ctx/warn-once! c [(:plan (:args c)) :short seed] :farm-tend.short-seed
                    {:plan (:plan (:args c)) :seed seed
                     :text (str "tend of " (:plan (:args c)) ": no " seed " carried for the bare cells that want it")})))

(defn plan-census
  "The live {:crops :bare :untilled :wrong} of the plan's field, and the seeds short for its bare cells as :short."
  [c]
  (let [p (:primitives c)
        {:keys [answer crops]} (planned c)
        crops (or crops {})
        have (set (map :name (u/inventory p)))]
    {:crops (count (filter (fn [[pos crop]] (contains? (shape/crop-names crop) (u/block-name p pos))) crops))
     :bare (count (harvest/planned-bare p crops))
     :untilled (count (filter #(untilled? (ground-cell p %)) (keys (ground-cells answer crops))))
     :wrong (wrong-crops p crops)
     :short (into #{} (comp (map :seed) (remove have)) (harvest/planned-bare p crops))}))

(defn facts [c] (if (:plan (:args c)) (plan-facts c) (box-facts c)))

(defn census [c] (if (:plan (:args c)) (plan-census c) (box-census c)))

;; ------------------------------------------------------------------ decisions

(defn decide-harvest
  [{:keys [plan part]} {:keys [ripe mid radius]}]
  (if (zero? ripe)
    {:skip :no-ripe}
    {:call {:slot :harvest :job (jobs :harvest)
            :args (if plan
                    {:plan plan :part part :replant true :replant-bare false}
                    {:center mid :radius radius :replant true})}}))

(defn decide-till
  [{:keys [till plan]} {:keys [hoe untilled bare seeds till-short]}]
  (cond
    (not till) {:skip :till-off}
    (not hoe) {:skip :no-hoe}
    (empty? untilled) {:skip (if (pos? (or till-short 0)) :no-seed :nothing-to-till)}
    (and (not plan) (<= seeds bare)) {:skip :no-seed}
    :else {:call {:slot :till :job (jobs :till)
                  :args (cond-> {:from (first untilled) :to (first untilled)} plan (assoc :for-plan plan))}}))

(defn decide-plant
  [{:keys [box plan part]} {:keys [bare seed]}]
  (cond
    (zero? bare) {:skip :no-bare}
    (not seed) {:skip :no-seed}
    :else {:call {:slot :plant :job (jobs :plant) :args (if plan {:plan plan :part part} {:box box})}}))

(defn decide-fertilize
  [{:keys [fertilize]} {:keys [meal unripe mid radius]}]
  (cond
    (not fertilize) {:skip :fertilize-off}
    (not meal) {:skip :no-bone-meal}
    (zero? unripe) {:skip :none-unripe}
    :else {:call {:slot :fertilize :job (jobs :fertilize) :args {:center mid :radius radius}}}))

(defn decide-compost
  [{:keys [composter]} {:keys [waste keep]}]
  (cond
    (nil? composter) {:skip :no-composter}
    (empty? waste) {:skip :no-surplus-seed}
    :else {:call {:slot :compost :job (jobs :compost) :args {:at composter :items waste :keep keep}}}))

(defn decide-deposit
  [{:keys [chest]} {:keys [stored keep]}]
  (cond
    (nil? chest) {:skip :no-chest}
    (empty? stored) {:skip :nothing-to-store}
    :else {:call {:slot :deposit :job (jobs :deposit) :args {:chest chest :items stored :keep keep}}}))

(defn decide
  "{:skip reason} or {:call {:slot :job :args}} for a step."
  [step args facts]
  (case step
    :harvest (decide-harvest args facts)
    :till (decide-till args facts)
    :plant (decide-plant args facts)
    :fertilize (decide-fertilize args facts)
    :compost (decide-compost args facts)
    :deposit (decide-deposit args facts)
    {:skip :todo}))

(defn plan
  "Walk todo, skipping the steps that decide skips (booked in report unless it already holds an entry for the step); {:todo :report :call}, :call nil when none is left."
  [todo args facts report]
  (loop [todo todo report report]
    (if (empty? todo)
      {:todo [] :report report :call nil}
      (let [step (first todo)
            {:keys [skip call]} (decide step args facts)]
        (if skip
          (recur (rest todo) (if (contains? report step) report (assoc report step {:skipped skip})))
          {:todo (vec todo) :report report :call call})))))

(defn would-run?
  "Whether a run is under way, or some step would call a child over the facts read now."
  [c]
  (boolean (or (:todo (ctx/mem c))
               (let [f (facts c)]
                 (when (:plan (:args c)) (note-short! c (:short f)))
                 (some #(:call (decide % (:args c) f)) steps)))))

(defn check [c]
  (boolean
   (if (:plan (:args c))
     (and (not (:trouble (planned c))) (would-run? c))
     (and (usable-box? (:box (:args c))) (would-run? c)))))

;; ------------------------------------------------------------------ rounds

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :harvest (select-keys r [:cut :replanted :bare :gave-up])
    :till {:tilled (:tilled r 0)}
    :plant (select-keys r [:planted :reason :skipped :refused :short])
    :fertilize (select-keys r [:used])
    :compost (select-keys r [:fed :bone-meal :reason])
    :deposit (select-keys r [:gave-up :reason])))

(defn finish!
  [c report]
  (let [{:keys [short] :as seen} (census c)
        field (dissoc seen :short)
        out {:steps report :field field}]
    (when (:plan (:args c))
      (note-short! c short)
      (when (seq (:wrong field))
        (ctx/emit! c :farm-tend.wrong :warn
                   {:plan (:plan (:args c)) :cells (:wrong field)
                    :text (str "tend of " (:plan (:args c)) " left " (count (:wrong field)) " wrong crops standing: "
                               (str/join ", " (map #(str (pr-str (:pos %)) " " (:found %)) (:wrong field))))})))
    (ctx/emit! c :farm-tend.done :info
               (assoc out :text (str "farm tend done: " (:crops field) " crops, " (:bare field) " bare, " (:untilled field) " untilled")))
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

(defn after-till
  "Memory after a till child ended: the cell is tried, its tilled cells counted, the step stays to be decided again."
  [m call-args r-tilled declined?]
  (let [tilled (+ (:tilled m 0) r-tilled)]
    (-> m
        (dissoc :call-args)
        (update :till-tried (fnil conj #{}) (:from call-args))
        (assoc :tilled tilled)
        (update-in [:report :till] #(cond
                                      (pos? tilled) {:tilled tilled}
                                      (and declined? (nil? %)) {:skipped :declined}
                                      :else %)))))

(defn after-child
  "Memory after the child of step ended with :done or :declined."
  [m step call-args outcome r]
  (if (= :till step)
    (after-till m call-args (if (= :done outcome) (:tilled r 0) 0) (= :declined outcome))
    (-> m
        (update :todo rest)
        (dissoc :call-args)
        (assoc-in [:report step] (if (= :done outcome) (summary step r) {:skipped :declined})))))

(defn with-zone-opt-out
  "The args of the child of step, with :ignore-zones? true when the tend job has it (the children that change blocks)."
  [c step child-args]
  (if (and (:ignore-zones? (:args c)) (not= :deposit step))
    (assoc child-args :ignore-zones? true)
    child-args))

(defn ^:async work [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {} :till-tried #{} :tilled 0))
  (let [{:keys [call] :as p} (next-plan c)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish! c (:report p))
      (let [step (:slot call)
            _ (ctx/update-mem! c assoc :call-args (:args call))
            outcome (await (ctx/call-child c step (:job call) (with-zone-opt-out c step (:args call))))]
        (when (#{:done :declined} outcome)
          (ctx/update-mem! c after-child step (:args call) outcome (when (= :done outcome) (ctx/child-result c step))))
        :continue))))

(defn ^:async round [c]
  (if (and (:plan (:args c)) (:trouble (planned c)))
    :declined
    (await (work c))))
