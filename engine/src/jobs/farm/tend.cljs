(ns jobs.farm.tend
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.standing :as standing]
            [jobs.lib.steps :as steps]
            [jobs.lib.util :as u]
            [jobs.farm.fertilize :as fertilize]
            [jobs.lib.crops :as crops]
            [jobs.farm.harvest :as harvest]
            [jobs.farm.plant :as plant]
            [jobs.farm.till :as till]
            [jobs.farm.tend-decide :as decide]
            [jobs.farm.tend-plan :as tend-plan]
            [jobs.farm.tend-stock :as stock]))

(def doc
  "Keep one field of crops in order. The field is the :box (inclusive). Its lowest layer (y = min) is the
  farmland and the layer above holds the crops, so max y is at least min y + 1.
  One run is one pass over six steps in this order. Each step is a child job that runs when its conditions hold:
  - :harvest (jobs.farm.harvest, replanting): a ripe crop lies in the box.
  - :till (jobs.farm.till): :till is true, a hoe is carried, dirt or grass lies uncovered in the ground layer and
    more seed is carried than there is bare farmland. It tills one cell at a time, nearest first.
  - :plant (jobs.farm.plant): bare farmland lies in the box and a seed is carried.
  - :fertilize (jobs.farm.fertilize): :fertilize is true, bone meal (or bones, which the fertilize child crafts) is carried and an unripe crop lies in the box.
  - :compost (jobs.farm.compost): a :composter is given and seed above the keep is carried.
  - :deposit (jobs.storage.deposit): a :chest is given and farm goods are carried above the keep. Farm goods are
    seeds, crops (carrots, potatoes and beetroot too), melon, pumpkin, hay and cocoa. Tools, bread and other
    non-crop food are never stored.
  The keep for compost and deposit, per name, is the largest of: the sowing reserve (twice the number of beds, farmland or
  untilled ground cells, spread over the carried seeds), a backup stack (64) of each seed type, the body's food reserve
  (jobs.lib.cost/food-reserve: 3 days of food, 60 hunger points, best food first; carrots and potatoes count toward it
  first) and the :keep entry. Harvested crops above it are stored; seed above it is composted.
  Skipped steps are booked as {:skipped reason}: :no-ripe, :till-off, :no-hoe, :nothing-to-till, :no-seed,
  :food-reserve (a plan's dry cells wait because the reserve keeps their seed; adds :seed :held :kept :reserve-points; cells
  within 4 blocks of water, level or one up, are tilled anyway), :no-bare, :fertilize-off, :no-bone-meal, :none-unripe, :no-composter, :no-surplus-seed, :no-chest,
  :nothing-to-store or :declined (the child declined).
  A step that ran keeps its summary even if it is skipped later in the same run.
  The job declines (does nothing) unless the box has at most 2048 cells and some step would run (a field partly never
  seen is looked around once first). A started run always continues. A step under way is not re-decided.
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
  :parts (a list of part names) works those parts in one job, a pass each in order, and ends :done with
  {:parts {part {:steps :field}}} (one info farm-tend.done). A part (or the whole field) whose pass leaves a step that would
  still run rests 10 minutes (jobs.lib.standing): its check is false for that long, the wait names it as :backed-off, and the
  other parts go on; so the job can stand in the queue under repeat without starving other jobs.
  The job declines (one farm-tend.declined warn) while the plan is missing, unreadable, has no crop cells, or no
  zone list has been read. A started run declines in its next round if the plan stops being workable.")

(a/defargs args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; y min is the farmland layer, max y at least min y + 1; at most 2048 cells; required unless :plan is given (without either the check declines)" :spec a/box? :default nil}
   :plan {:doc "id of a plan of the body's world whose crop cells are the field, each cell worked with the crop the plan wants there (then :box is not used)" :spec a/name? :default nil}
   :part {:doc "with :plan, only the cells of this part" :spec a/name? :default nil}
   :parts {:doc "with :plan, the parts worked in one job, one pass each in this order (instead of :part)" :spec (a/coll-of a/name?) :default nil}
   :till {:doc "hoe untilled dirt and grass in the ground layer when seed and a hoe are carried" :spec boolean? :default true}
   :fertilize {:doc "use bone meal on unripe crops" :spec boolean? :default false}
   :composter {:doc "composter position {:x :y :z} for seed above the reserve; nil: do not compost" :spec ::a/pos :default nil}
   :chest {:doc "chest position {:x :y :z} for the farm goods; nil: do not store" :spec ::a/pos :default nil}
   :keep {:doc "{item-name count}: how many of an item compost and deposit leave carried, at least the seed reserve" :spec (a/map-of string? number?) :default {}}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to harvest, till, plant, fertilize and compost); the rules of the game allow it" :spec boolean? :default false}})

(a/defargs settings
  {::max-cells {:default 2048 :doc "Cells one call looks at before it gives the round back." :spec (a/int-in 0 nil)}})
(defn max-cells [] (settings/get settings ::max-cells))

(def seed-items ["wheat_seeds" "carrot" "potato" "beetroot_seeds"])

(def crop-names (set (keys crops/ripe-age)))

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
                (<= (box-cells box) (max-cells)))))

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

(defn reserve
  "{seed count} to keep: twice the beds in all, taken one at a time from each carried seed that has more than is reserved already."
  [beds inventory]
  (let [have (stock/carried inventory)]
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

(defn beds
  "How many ground cells are farmland or untilled."
  [ground]
  (count (filter #(or (= "farmland" (:name %)) (stock/untilled? %)) ground)))

;; ------------------------------------------------------------------ facts

(defn ground-layer
  "[{:pos :name :above}] for the cells of the lowest layer of the box."
  [p {:keys [min max]}]
  (vec (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))
             :let [pos {:x x :y (:y min) :z z}]]
         {:pos pos :name (u/seen-name p pos) :above (u/seen-name p (update pos :y inc))})))

(defn unripe-in-box
  "The unripe crop cells of the box, read around its centre."
  [p box mid R]
  (->> (crops/seen-crops p (keys crops/ripe-age) (+ R (u/dist (u/pos-of (.-pos (.self p))) mid)) 4096)
       (filter #(some-> (:age %) (< (crops/ripe-age (:name %)))))
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
        raw-untilled (->> ground (filter stock/untilled?) (map :pos) (remove tried))
        bare (count (box-permitted c :sow (plant/bare-cells p box [])))
        keep-of (stock/keeps (reserve (beds ground) inventory) inventory keep)]
    {:mid mid
     :radius R
     :ripe (count (box-permitted c :harvest (filter #(in-box? box %) (harvest/ripe-crops p {:radius R :crops nil} mid []))))
     :hoe (some? (till/hoe-of p))
     :untilled (->> ground
                    (filter stock/untilled?)
                    (map :pos)
                    (remove tried)
                    (box-permitted c :dig)
                    (sort-by #(u/dist me %))
                    vec)
     :refused (- (count raw-untilled) (count (box-permitted c :dig raw-untilled)))
     :covered (count (filter stock/covered? ground))
     :bare bare
     :seeds (transduce (map #(get (plant/sowable inventory) % 0)) + 0 seed-items)
     :seed (some? (plant/pick-seed nil inventory))
     :meal (boolean (fertilize/has-meal? p))
     :bones (boolean (some #(= "bone" (:name %)) inventory))
     :unripe (if fertilize (count (unripe-in-box p box mid R)) 0)
     :keep keep-of
     :waste (stock/surplus inventory keep-of stock/waste-seeds)
     :stored (stock/surplus inventory keep-of stock/farm-goods)}))

(defn box-census
  "The live {:crops :bare :untilled} of the box."
  [c]
  (let [p (:primitives c)
        {:keys [min max] :as box} (:box (:args c))
        above (inc (:y min))]
    {:crops (count (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))
                         :when (crop-names (u/seen-name p {:x x :y above :z z}))]
                     1))
     :bare (count (plant/bare-cells p box []))
     :untilled (count (filter stock/untilled? (ground-layer p box)))}))

(defn facts [c] (if (:plan (:args c)) (tend-plan/plan-facts c) (box-facts c)))

(defn census [c] (if (:plan (:args c)) (tend-plan/plan-census c) (box-census c)))

(defn would-run?
  "Whether a run is under way, or some step would call a child over the facts f."
  [c f]
  (boolean (or (:todo (ctx/mem c))
               (some #(:call (decide/decide % (:args c) f)) decide/steps))))

(defn field-cells
  "The cells the run decides from: the box's ground and crop layers, or the plan's crop cells and the ground under them."
  [c]
  (if (:plan (:args c))
    (mapcat (fn [pos] [pos (update pos :y dec)]) (keys (:crops (tend-plan/planned c))))
    (let [{:keys [min max]} (:box (:args c))]
      (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max))) y [(:y min) (inc (:y min))]]
        {:x x :y y :z z}))))

(defn check-facts
  "The facts the check decides from: read once per check-cache window, and fresh while a cell was never seen."
  [c]
  (if (look/unseen? (:primitives c) (field-cells c))
    (facts c)
    (check-cache/cached c [:facts (:part (:args c))] #(facts c))))

(defn nothing-to-do-reason
  "The wait reason when no step would run: :covered counts tillable cells with a block over them (the box sits too low),
  :refused untilled cells a zone or claim refuses, :food-reserve the seed the reserve keeps from dry cells."
  [f]
  (cond-> (into {:reason :nothing-to-do} (filter (comp pos? val)) (select-keys f [:covered :refused]))
    (:till-held f) (assoc :food-reserve (:till-held f))))

(defn check-part
  "Passes when a run is under way or a step would call a child. Otherwise it waits: :short-seed names the seeds the
  plan's bare cells want and the body lacks (the round says it with a warn)."
  [c]
  (let [trouble (when (:plan (:args c)) (:trouble (tend-plan/planned c)))]
    (if trouble
      (ctx/wait c {:reason :plan-trouble :why trouble})
      (if (and (not (:plan (:args c))) (not (usable-box? (:box (:args c)))))
        (ctx/wait c {:reason :no-box})
        (let [f (check-facts c)]
          (or (would-run? c f)
              (look/wait-unless-surveyed c (if (seq (:short f)) {:reason :short-seed :seeds (vec (sort (:short f)))} (nothing-to-do-reason f))
                                         (field-cells c))))))))

(def job-sym 'jobs.farm.tend)
(defn key-of [part] (or part "*"))

(defn parts-of
  "The parts a job works: :parts, else the one :part (nil: the whole field)."
  [c]
  (let [{:keys [parts part]} (:args c)]
    (if (seq parts) (vec parts) [part])))

(defn for-part [c part] (assoc-in c [:args :part] part))

(defn check
  "Passes when a run is under way, or when some part not resting passes check-part. Otherwise it waits (the reason of the
  last part checked), naming the resting parts as :backed-off."
  [c]
  (let [m (ctx/mem c)]
    (cond
      (seq (:results m)) true
      (:cur m) (true? (check-part (for-part c (:part (:cur m)))))
      :else
      (let [rested (standing/rested c job-sym)
            live (remove #(contains? rested (key-of %)) (parts-of c))]
        (or (boolean (some #(true? (check-part (for-part c %))) live))
            (do (when (seq rested)
                  (ctx/wait c {:reason :nothing-to-do :backed-off (vec (sort (keys rested)))}))
                false))))))

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

(defn part-report
  "{:steps :field} of the pass just made over the field c works (and the plan's notes about it)."
  [c report]
  (let [{:keys [short] :as seen} (census c)
        field (dissoc seen :short)]
    (when (:plan (:args c))
      (tend-plan/note-short! c short)
      (when (seq (:wrong field))
        (ctx/emit! c :farm-tend.wrong :warn
                   {:plan (:plan (:args c)) :cells (:wrong field)
                    :text (str "tend of " (:plan (:args c)) " left " (count (:wrong field)) " wrong crops standing: "
                               (str/join ", " (map #(str (pr-str (:pos %)) " " (:found %)) (:wrong field))))})))
    {:steps report :field field}))

(defn finish-part!
  "The pass over the field c works is made: book its report, clear the pass, and rest the field when a step would still
  run (the pass could not do it). :again."
  [c report]
  (let [part (:part (:args c))
        out (part-report c report)]
    (when (some #(:call (decide/decide % (:args c) (facts c))) decide/steps)
      (standing/rest! c job-sym (key-of part) :undone))
    (ctx/update-mem! c #(-> % (dissoc :todo :report :call-args :cur :till-tried :tilled)
                            (assoc-in [:results (key-of part)] out)))
    :again))

(defn finish-all!
  "Every part has had its pass: the event and the result, :done."
  [c]
  (let [results (:results (ctx/mem c))
        multi? (seq (:parts (:args c)))
        out (if multi?
              {:parts (into (array-map) (map (fn [part] [part (get results (key-of part))])) (filter #(contains? results (key-of %)) (:parts (:args c))))}
              (val (first results)))
        fields (if multi? (map :field (vals (:parts out))) [(:field out)])
        sum #(reduce + (map (fn [f] (get f % 0)) fields))]
    (ctx/emit! c :farm-tend.done :info
               (assoc out :text (str "farm tend done: " (sum :crops) " crops, " (sum :bare) " bare, " (sum :untilled) " untilled")))
    (ctx/result! c out)
    :done))

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

(defn ^:async work
  "One child call, or the report when no step is left: :again after a child ended, :continue while one waits."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo decide/steps :report {} :till-tried #{} :tilled 0))
  (let [{:keys [call] :as p} (steps/next-plan decide/decide decide/jobs c facts)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish-part! c (:report p))
      (let [step (:slot call)
            _ (ctx/update-mem! c assoc :call-args (:args call))
            outcome (await (ctx/call-child c step (:job call) (with-zone-opt-out c step (:args call))))]
        (when (#{:done :declined} outcome)
          (ctx/update-mem! c after-child step (:args call) outcome (when (= :done outcome) (ctx/child-result c step))))
        (if (= :continue outcome) :continue :again)))))

(defn ^:async step
  "A run over a field partly never seen looks around first and yields (the check decides on what is seen then)."
  [c]
  (cond
    (and (:plan (:args c)) (:trouble (tend-plan/planned c))) :declined
    (and (not (:todo (ctx/mem c))) (not (look/surveyed? c)) (look/unseen? (:primitives c) (field-cells c)))
    (await (look/survey! c))
    :else (await (work c))))

(defn ^:async pass-step
  "One step of the pass over the part in c (a child call or the look round): :again, :continue, :declined."
  [c]
  (await (step c)))

(defn next-part
  "{:part} of the first untried part not resting whose check passes, marking every part looked at as tried; nil when none is left."
  [c]
  (let [rested (standing/rested c job-sym)]
    (loop [parts (remove #(contains? (:tried (ctx/mem c)) (key-of %)) (parts-of c))]
      (when (seq parts)
        (let [part (first parts)]
          (ctx/update-mem! c update :tried (fnil conj #{}) (key-of part))
          (if (and (not (contains? rested (key-of part))) (true? (check-part (for-part c part))))
            {:part part}
            (recur (rest parts))))))))

(defn ^:async run-step
  "One step of the whole run: go on with the part under way, else start the next part, else hand over the result."
  [c]
  (if-let [cur (:cur (ctx/mem c))]
    (await (pass-step (for-part c (:part cur))))
    (if-let [{:keys [part]} (next-part c)]
      (do (ctx/update-mem! c assoc :cur {:part part})
          :again)
      (finish-all! c))))

(defn ^:async round
  "The whole run: one pass per part, each one child call after another over the six steps; :continue only while a
  child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (run-step c))))))
