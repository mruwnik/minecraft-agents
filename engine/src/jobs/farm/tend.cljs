(ns jobs.farm.tend
  (:require [engine.settings :as settings]
            [jobs.lib.need :as need]
            [engine.args :as a]
            [engine.contract :as contract]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.field-area :as fa]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.standing :as standing]
            [jobs.lib.steps :as steps]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]
            [jobs.farm.field :as fld]
            [jobs.lib.farm :as lib-farm]
            [jobs.farm.irrigate :as irrigate]
            [jobs.farm.tend-decide :as decide]
            [jobs.farm.tend-box :refer [box-facts box-census]]
            [jobs.farm.tend-plan :as tend-plan]
            [jobs.lib.plan-field :as pf]))

(def doc
  "Keep one field of crops in order. The field is the :area: a box (inclusive; its lowest layer, y = min, is the
  farmland and the layer above holds the crops, so max y is at least min y + 1) or a plan's crop cells ({:plan id}).
  Seed, produce and compost are not its business (jobs.farm.maintain stores and composts them).
  One run is one pass over five steps in this order. Each step is a child job that runs when its conditions hold:
  - :harvest (jobs.farm.harvest, replanting): a ripe crop lies in the field; only the field's crops are cut.
  - :irrigate (jobs.farm.irrigate over the ground layer, in plan mode over the plan's ground cells, per layer in pieces of at most till's max-cells, one piece per pass, :for-plan): :irrigate is true and a ground cell is seen
    dry (no water seen in reach, the whole reach seen: with :irrigate on, a run looks round and walks within 3 blocks of up to 3 ground cells whose reach is partly unseen first). The child fetches a water bucket when none is carried; a failed
    call is booked as its summary {:poured n} and the run goes on.
  - :till (jobs.farm.till): :till is true, a hoe is carried or the call's :ways allow fetching one (till fetches it), dirt or grass lies uncovered in the ground layer and
    more seed is carried than there is bare farmland. It tills one cell at a time, nearest first.
  - :plant (jobs.farm.plant): bare farmland lies in the field and a seed is carried.
  - :fertilize (jobs.farm.fertilize, over the field's cells): :fertilize is true, bone meal (or bones, which the fertilize child crafts) is carried and an unripe crop lies in the field.
  Skipped steps are booked as {:skipped reason}: :no-ripe, :till-off, :no-hoe, :nothing-to-till, :no-seed,
  :food-reserve (a plan's dry cells wait because the reserve keeps their seed; adds :seed :held :kept :reserve-points; cells
  within 4 blocks of water, level or one up, are tilled anyway), :no-bare, :fertilize-off, :no-bone-meal, :none-unripe or :declined (the child declined).
  A step that ran keeps its summary even if it is skipped later in the same run.
  Start facts, failed conditions: impossible {:reason :no-box} unless the area is a box with a crop layer and at most 2048 cells, or a plan;
  {:reason :away :site :others} while another job that is not standing and not waiting is listed;
  {:reason :nothing-to-do} (with :covered, :refused, :blocked (plan crop cells another block fills), :food-reserve) or {:reason :short-seed :seeds} when no
  step would run, after one look round from here when a cell was never seen; a field cell never seen that no look from here
  shows (look/look-shows?) or one look round did not show is walked to first (go-to, :range 8, once a run) when go-to's quote of the walk
  (walk-pick/quoted-s) costs less than the pass is worth (pass-worth, as the walk survives); else {:reason :not-worth :worth :walk-s}
  while even the least walk there (walk-pick/walk-s) costs more, and a run whose quote costs more ends condition-failed :not-worth. A started run always continues. A step under
  way is not re-decided.
  Ends :done with data {:steps {step summary} :field {:crops :bare :untilled}} (a plan adds :wrong and, when crop cells hold a tree or other block, :blocked [{:pos :found}] with one farm-tend.blocked warn) when some step
  changed something (units steps); nothing-possible :gave-up with the same data when children ran and none did;
  condition-failed :nothing-to-do when no step ran. A child that ends in any failure, a limit of its own too, is booked
  with what it did and the run goes on; tend's own time spend is the only limit it ends on.
  :field is counted live at the end. Summaries: harvest {:cut :replanted :bare :gave-up}, till {:tilled n}, plant {:planted
  :skipped :refused :short}, fertilize {:used}.
  Limits: a dirt lane inside the box counts as a bed unless :till is false. A ripe crop that cannot be reached
  keeps the check passing on every run (harvest gives up on it and the run still ends).
  {:plan id} (optionally :part; with neither :part nor :parts every crop part of the plan is worked) makes the plan's crop cells the field. Only crops harvest knows
  (wheat, carrots, potatoes, beetroots) count. The plan's other cells are never touched. The same steps run
  per cell, with the crop the plan names there:
  - harvest never digs a wrong crop. A ripe crop of another kind in a crop cell stays standing and is listed in
    :field :wrong {:pos :found :want} (warn farm-tend.wrong).
  - till hoes the ground under a crop cell (the cell below, unless the plan names it as something that is not soil (dirt, grass, farmland)) while
    a hoe is carried, the cell's seed outnumbers the bare farmland waiting for it and the access rules allow it.
    Farmland the plan wants without a crop over it is not tilled, since it would revert.
  - plant sows every bare crop cell with its own crop's seed. A cell whose seed is not carried stays bare, with one
    farm-tend.short-seed warn per missing seed.
  Tilling and sowing are checked against zones and the footprints of the other active plans, when a cell is chosen
  and again before the act. Untilled ground the rules refuse is left alone.
  When only refused ground remains, the check stays false: the job waits with :refused n and the :zones, :claims, :plans and :owners that refuse.
  :parts (a list of part names) works those parts in one job, a pass each in order, and ends :done with
  {:parts {part {:steps :field}}}. A part is tried once per run; the caller decides when to run again.
  {:reason :plan-trouble :why text} (one farm-tend.declined warn): a wait while the plan is missing or unreadable or no zone
  list has been read; impossible when it has no crop cells, also in a started run if the plan stops being workable.")

(a/defargs args
  {:area {:doc "the field: a box {:min :max} (corners {:x :y :z} or [x y z], inclusive; y min is the farmland layer, max y at least min y + 1; at most 2048 cells) or {:plan id} whose crop cells are the field, each worked with the crop the plan wants there; :part limits the plan to one part, :parts works a list of parts one pass each; required" :spec fa/spec :default nil}
   :till {:doc "hoe untilled dirt and grass in the ground layer when seed is carried and a hoe is carried or fetchable" :spec boolean? :default true}
   :irrigate {:doc "wet dry farmland with water (jobs.farm.irrigate, a water bucket fetched when none is carried)" :spec boolean? :default true}
   :fertilize {:doc "use bone meal on unripe crops" :spec boolean? :default false}
   :ignore-zones? :shared})

(def target [:area])
(def ways need/fetch-ways)
(def defaults {:least 1 :spend {:job-time ::job-time-s}})
(def safe? (price/staying :work (ready/safe-at-by-night work-place/spot)))

(a/defargs settings
  {::min-health {:default 0 :spec (a/num-in 0 20)
                :doc "Health (of 20) below which the job waits to heal; 0 = never (its acts are short and in the body's own field). Poison still holds."}
   ::job-time-s {:default 1200 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::max-cells {:default 2048 :doc "Most cells a field may cover; a larger box is refused :no-box." :spec (a/int-in 0 nil)}})
(defn fit? [c] (ready/fit-min (settings/get settings ::min-health) c))
(defn max-cells [] (settings/get settings ::max-cells))

(defn usable-box?
  "A box with a crop layer above its farmland layer, of at most max-cells cells."
  [box]
  (boolean (and box
                (>= (:y (:max box)) (inc (:y (:min box))))
                (<= (fa/cell-count box) (max-cells)))))

(defn facts [c] (if (fa/plan c) (tend-plan/plan-facts c) (box-facts c)))

(defn census [c] (if (fa/plan c) (tend-plan/plan-census c) (box-census c)))

(defn decide-args
  "The args the steps decide over: the call's args plus :ways, the fetch ways its children may use (:carried alone: none)."
  [c]
  (assoc (:args c) :ways (into #{:carried} (need/fetch-how c))))

(defn would-run?
  "Whether a run is under way, or some step would call a child over the facts f."
  [c f]
  (boolean (or (:todo (ctx/mem c))
               (some #(:call (decide/decide % (decide-args c) f)) decide/steps))))

(defn field-cells
  "The cells the run decides from: the box's ground and crop layers, or the plan's crop cells and the ground under them."
  [c]
  (if (fa/plan c)
    (mapcat (fn [pos] [pos (update pos :y dec)]) (keys (:crops (tend-plan/planned c))))
    (let [{lo :min hi :max} (fa/arg-box c)]
      (for [x (range (:x lo) (inc (:x hi))) z (range (:z lo) (inc (:z hi))) y [(:y lo) (inc (:y lo))]]
        {:x x :y y :z z}))))

(defn ground-cells
  "The ground cells irrigate works over: the plan's under its crop cells, or the box's lowest layer."
  [c]
  (if (fa/plan c)
    (map #(update % :y dec) (keys (:crops (tend-plan/planned c))))
    (let [{lo :min hi :max} (fa/arg-box c)]
      (for [x (range (:x lo) (inc (:x hi))) z (range (:z lo) (inc (:z hi)))] {:x x :y (:y lo) :z z}))))

(defn water-reach-cells
  "Per y layer of the ground cells, the cells irrigate must see to call a ground cell dry: the layer's bounding square grown by the water reach, at the layer and one above."
  [c]
  (let [r (lib-farm/water-reach)]
    (for [[y ps] (group-by :y (ground-cells c))
          x (range (- (apply min (map :x ps)) r) (+ (apply max (map :x ps)) r 1))
          z (range (- (apply min (map :z ps)) r) (+ (apply max (map :z ps)) r 1))
          dy [0 1]]
      {:x x :y (+ y dy) :z z})))

(defn needs-look?
  "Whether a field cell (or, with :irrigate, a block of the water reach round the ground) was never seen, or (plan) a crop cell is remembered unripe and out of view."
  [c]
  (boolean (or (look/unseen? (:primitives c) (field-cells c))
               (and (:irrigate (:args c)) (look/unseen? (:primitives c) (water-reach-cells c)))
               (and (fa/plan c) (fld/unwatched? (:primitives c) (:crops (tend-plan/planned c)))))))

(defn site
  "A position of the field: the box's centre, else the plan's first crop cell; nil when there is neither."
  [c]
  (if-let [{lo :min hi :max} (fa/arg-box c)]
    (when lo {:x (/ (+ (:x lo) (:x hi)) 2) :y (:y lo) :z (/ (+ (:z lo) (:z hi)) 2)})
    (when (fa/plan c) (first (keys (:crops (tend-plan/planned c)))))))

(defn crop-cells
  "{pos crop} of the field's crop cells: the plan's with the crop it wants, the box's crop layer with nil."
  [c]
  (if (fa/plan c)
    (:crops (tend-plan/planned c))
    (let [{lo :min hi :max} (fa/arg-box c)]
      (into {} (for [x (range (:x lo) (inc (:x hi))) z (range (:z lo) (inc (:z hi)))] [{:x x :y (inc (:y lo)) :z z} nil])))))

(defn cell-worth
  "Seconds the crop cell at pos (crop: the one wanted there, nil for any) is worth to a pass: its ripe crop's cost/crop-worth times the
  chance it is ripe now: seen in view, 1 or 0; remembered out of view, its chance to have ripened since (crops/ripe-chance); never seen,
  1, its last look unboundedly long ago, at the least worth of the crops it may hold (the worse case). Another block is 0."
  [p pos crop]
  (let [b (u/sensed p pos)]
    (if (or (nil? b) (true? (.-unknown b)))
      (apply min (map cost/crop-worth (if crop [crop] (keys crops/ripe-age))))
      (let [name (.-name b)
            age (or (.-age b) 0)]
        (cond
          (not (and (crops/ripe-age name) (or (nil? crop) (= crop name)))) 0
          (false? (.-visible b)) (* (cost/crop-worth name) (crops/ripe-chance name age (/ (or (.-ageMs b) 0) 1000)))
          (>= age (crops/ripe-age name)) (cost/crop-worth name)
          :else 0)))))

(defn pass-worth
  "Seconds a pass over the field c works is worth: the cell-worth of its crop cells, the harvest it expects."
  [c]
  (let [p (:primitives c)]
    (reduce + 0 (map (fn [[pos crop]] (cell-worth p pos crop)) (crop-cells c)))))

(defn unseen-field-cells
  "The field cells never seen."
  [c]
  (let [p (:primitives c)]
    (filter #(true? (some-> (u/sensed p %) .-unknown)) (field-cells c))))

(defn walk-due?
  "The field's site when the run has not walked there, a field cell was never seen and no look from here would show one
  (look/look-shows?), or the body looked round near here already: the run walks there first."
  [c]
  (let [site (site c)
        p (:primitives c)
        unseen (when (and site (not (:site-walked (ctx/mem c)))) (seq (unseen-field-cells c)))]
    (when (and unseen (or (look/surveyed? c :near) (not-any? #(look/look-shows? p %) unseen)))
      site)))

(def site-range "Blocks from the site within which the walk to an unseen field ends." 8)

(defn walk-args [c site] {:pos (update site :y inc) :range site-range})

(defn not-worth
  "{:reason :not-worth :worth :walk-s} when even the least walk to the unseen site (walk-pick/walk-s) costs more than the pass is worth, else nil."
  [c site]
  (let [worth (pass-worth c)
        walk (pick/walk-s (u/self-pos c) (walk-args c site))]
    (when (> walk worth)
      {:reason :not-worth :worth worth :walk-s walk})))

(defn check-facts
  "The facts the check decides from: read once per check-cache window, and fresh while a cell was never seen."
  [c]
  (if (needs-look? c)
    (facts c)
    (check-cache/cached c [:facts (fa/part c)] #(facts c))))

(defn blocked-text
  "Why crop cells stay unsown: n of them hold the blocks found (tidy digs none of those), ground of them at soil level."
  [n found ground]
  (str n " crop cells hold " (str/join ", " found) " that tidy does not dig"
       (when (pos? ground) (str "; " ground " are at ground level: the plan's crop layer must be one above the soil"))))

(defn nothing-to-do-reason
  "The wait reason when no step would run: :covered counts tillable cells with a block over them (the box sits too low),
  :refused untilled cells a zone or claim refuses, :food-reserve the seed the reserve keeps from dry cells."
  [f]
  (cond-> (into (into {:reason :nothing-to-do} (filter (comp pos? val)) (select-keys f [:covered :refused :blocked :ground-level])) (:refusers f))
    (:till-held f) (assoc :food-reserve (:till-held f))
    (pos? (:blocked f 0)) (assoc :text (blocked-text (:blocked f) (:blocked-found f) (:ground-level f 0)))))

(defn part-status
  "Why no step of the part in c would run, a :later status, or nil when a run is under way or a step would call a child.
  :short-seed names the seeds the plan's bare cells want and the body lacks. A :look while a cell was never seen and
  the body has not looked round near here."
  [c]
  (let [trouble (when (fa/plan c) (:trouble (tend-plan/planned c)))]
    (if trouble
      (checks/cause-status :later {:reason :plan-trouble :why trouble})
      (let [f (check-facts c)]
        (when-not (would-run? c f)
          (if-let [site (walk-due? c)]
            (some->> (not-worth c site) (checks/cause-status :later))
            (checks/cause-status (if (and (needs-look? c) (not (look/surveyed? c :near))) :look :later)
                                 (if (seq (:short f)) {:reason :short-seed :seeds (vec (sort (:short f)))} (nothing-to-do-reason f)))))))))

(def job-sym 'jobs.farm.tend)
(defn key-of [part] (or part "*"))

(defn parts-of
  "The parts a job works: :parts, else the one :part (nil: the whole field)."
  [c]
  (if (seq (fa/parts c)) (vec (fa/parts c)) [(fa/part c)]))

(defn for-part [c part] (fa/with-part c part))

(defn usable-args
  "Never :no-box without a usable box (or plan)."
  [c]
  (let [box (fa/arg-box c)]
    (if (or (fa/plan c) (and (= :box (fa/kind (fa/of c))) (usable-box? box)))
      (contract/ok :args "a box or plan given")
      (contract/never :no-box "no usable box or plan area"))))

(defn problem
  "Why no run can start (a started run decides through its steps), a status: away while another
  runnable job is listed, else the status of the last part (the first to ask for a look), ok when some part has work."
  [c]
  (let [away (standing/away-cause c (site c))]
    (if away
      (checks/cause-status :later away)
      (let [statuses (map #(part-status (for-part c %)) (parts-of c))]
        (cond
          (some nil? statuses) (contract/ok :work "a part has work")
          :else (or (first (filter #(= :look (first %)) statuses)) (last statuses)))))))

(defn can-run? [c] (pf/plan-can-run? (when (fa/plan c) (tend-plan/planned c))))

(defn can-start?
  "usable-args, then can-run?, then problem (it reads the box)."
  [c]
  (let [args (usable-args c)]
    (if (= :ok (first args))
      (let [run (can-run? c)] (if (= :ok (first run)) (problem c) run))
      args)))

(defn worked?
  "Whether the summary of a step says it changed something."
  [step s]
  (boolean
   (case step
     :harvest (pos? (+ (:cut s 0) (:replanted s 0)))
     :irrigate (pos? (:poured s 0))
     :till (pos? (:tilled s 0))
     :plant (pos? (:planted s 0))
     :fertilize (pos? (:used s 0)))))

(defn step-summaries
  "[[step summary]] of the pass under way and the finished parts."
  [m]
  (for [r (cons (:report m) (map :steps (vals (:results m)))) entry r] entry))

(declare out-of)

(defn in-order?
  "Whether the part in c needs no step and nothing else: part-status says only :nothing-to-do (no short seed, refused or
  covered cells, no look owed)."
  [c]
  (let [[kind _ data] (part-status c)]
    (and (= :later kind) (= :nothing-to-do (:reason data))
         (not-any? data [:covered :refused :food-reserve]))))

(defn done?
  "Ok once a pass changed something and every part is in order (no step would run); else later with the steps that
  changed something and the steps not yet decided. A field in order before the first pass stays a failed condition
  (nothing-to-do names why: zones, no seed)."
  [c]
  (let [m (ctx/mem c)
        worked (count (filter #(apply worked? %) (step-summaries m)))
        data [:units :steps :done worked :left (count (:todo m)) :result (or (out-of c) {})]]
    (if (and (pos? worked)
             (every? #(in-order? (for-part c %)) (parts-of c)))
      (apply contract/ok :in-order "farm tend: the field is in order" data)
      (apply contract/later :tending "farm tend: the field is being tended" data))))

(defn quote-of
  "A guess: a second and a half per cell of the field."
  [c]
  {:uses {:job-time [0 (* 1.5 (count (field-cells c))) ##Inf]}})

;; ------------------------------------------------------------------ rounds

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :harvest (select-keys r [:cut :replanted :bare :gave-up])
    :irrigate {:poured (count (:poured r))}
    :till {:tilled (:tilled r 0)}
    :plant (select-keys r [:planted :skipped :refused :short])
    :fertilize (select-keys r [:used])))

(defn part-report
  "{:steps :field} of the pass just made over the field c works (and the plan's notes about it)."
  [c report]
  (let [{:keys [short] :as seen} (census c)
        field (dissoc seen :short)]
    (when (fa/plan c)
      (tend-plan/note-short! c short)
      (when (seq (:blocked field))
        (ctx/warn-once! c [(fa/plan c) :blocked] :farm-tend.blocked
                        {:plan (fa/plan c) :cells (:blocked field)
                         :text (str "tend of " (fa/plan c) ": " (count (:blocked field)) " crop cells are filled by other blocks ("
                                    (str/join ", " (distinct (map :found (:blocked field)))) "); farm.tidy clears what it may dig")}))
      (when-let [ground (seq (tend-plan/ground-level-cells (:blocked field)))]
        (ctx/warn-once! c [(fa/plan c) :ground-level] :farm-tend.plan-error
                        {:plan (fa/plan c) :cells (vec ground)
                         :text (str "plan error in " (fa/plan c) ": " (count ground) " crop cells are at ground level (they hold "
                                    (str/join ", " (distinct (map :found ground))) "), so nothing can be sown there; the crop layer must be one above the soil: "
                                    (str/join " " (map (comp pr-str :pos) (take 6 ground))))}))
      (when (seq (:wrong field))
        (ctx/emit! c :farm-tend.wrong :warn
                   {:plan (fa/plan c) :cells (:wrong field)
                    :text (str "tend of " (fa/plan c) " left " (count (:wrong field)) " wrong crops standing: "
                               (str/join ", " (map #(str (pr-str (:pos %)) " " (:found %)) (:wrong field))))})))
    {:steps report :field field}))

(defn finish-part!
  "The pass over the field c works is made: book its report and clear the pass. :again."
  [c report]
  (let [part (fa/part c)
        out (part-report c report)]
    (ctx/update-mem! c #(-> % (dissoc :todo :report :call-args :cur :till-tried :tilled :irrigate-tried :irrigated :reach-tried :site-walked)
                            (assoc-in [:results (key-of part)] out)))
    :again))

(defn out-of
  "The ending's data from the parts' results so far: the one part's, or {:parts {part result}}."
  [c]
  (let [results (:results (ctx/mem c))]
    (if (seq (fa/parts c))
      {:parts (into (array-map) (map (fn [part] [part (get results (key-of part))])) (filter #(contains? results (key-of %)) (fa/parts c)))}
      (some-> results first val))))

(defn finish-all!
  "Every part has had its pass: the ending."
  [c]
  (let [m (ctx/mem c)
        results (:results m)
        multi? (seq (fa/parts c))
        out (out-of c)
        summaries (step-summaries m)
        fields (if multi? (map :field (vals (:parts out))) [(:field out)])
        sum #(reduce + (map (fn [f] (get f % 0)) fields))
        done (cond-> out (seq results) (assoc :text (str "farm tend done: " (sum :crops) " crops, " (sum :bare) " bare, " (sum :untilled) " untilled")))]
    (cond
      (some #(apply worked? %) summaries) (ctx/end c :done nil done)
      (some (fn [[_ s]] (not (:skipped s))) summaries) (ctx/end c :nothing-possible :gave-up out)
      :else (let [blocked (mapcat :blocked fields)]
              (ctx/end c :condition-failed
                       (cond-> {:reason :nothing-to-do}
                         (seq blocked) (assoc :blocked (count blocked)
                                              :text (blocked-text (count blocked) (sort (distinct (map :found blocked)))
                                                                  (count (tend-plan/ground-level-cells blocked)))))
                       out)))))

(defn after-till
  "Memory after a till child ended: the cell is tried, its tilled cells counted, the step stays to be decided again."
  [m call-args r-tilled declined?]
  (let [tilled (+ (:tilled m 0) r-tilled)]
    (-> m
        (dissoc :call-args)
        (update :till-tried (fnil conj #{}) (:min (:area call-args)))
        (assoc :tilled tilled)
        (update-in [:report :till] #(cond
                                      (pos? tilled) {:tilled tilled}
                                      (and declined? (nil? %)) {:skipped :declined}
                                      :else %)))))

(defn after-irrigate
  "Memory after a plan irrigate child ended: the piece is tried, its pours counted, the step stays to be decided again
  for the next piece."
  [m call-args r-poured declined?]
  (let [poured (+ (:irrigated m 0) r-poured)]
    (-> m
        (dissoc :call-args)
        (update :irrigate-tried (fnil conj #{}) (:min (:area call-args)))
        (assoc :irrigated poured)
        (update-in [:report :irrigate] #(cond
                                          (pos? poured) {:poured poured}
                                          (and declined? (nil? %)) {:skipped :declined}
                                          :else %)))))

(defn refused?
  "Whether child ending e is a contract child's refusal before it started (condition-failed, no data): it counts as declined."
  [e]
  (and (= :condition-failed (:kind e)) (empty? (:data e))))

(defn after-child
  "Memory after the child of step ended with e: its summary is booked whatever its ending, and the run goes on."
  [m step call-args e]
  (let [declined? (or (refused? e) (= :impossible (:kind e)))
        r (:data e)]
    (cond
      (= :till step)
      (after-till m call-args (if declined? 0 (:tilled r 0)) declined?)
      (and (= :irrigate step) (:for-plan call-args))
      (after-irrigate m call-args (if declined? 0 (count (:poured r))) declined?)
      :else
      (-> m
          (update :todo rest)
          (dissoc :call-args)
          (assoc-in [:report step] (if declined? {:skipped :declined} (summary step r)))))))

(defn with-zone-opt-out
  "The args of a child, with :ignore-zones? true when the tend job has it."
  [c child-args]
  (if (:ignore-zones? (:args c))
    (assoc child-args :ignore-zones? true)
    child-args))

(defn ^:async work
  "One child call, or the report when no step is left: :again after a child ended, :continue while one waits."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo decide/steps :report {} :till-tried #{} :tilled 0 :irrigate-tried #{} :irrigated 0))
  (let [{:keys [call] :as p} (steps/next-plan decide/decide decide/jobs (assoc c :args (decide-args c)) facts)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish-part! c (:report p))
      (let [step (:slot call)
            _ (ctx/update-mem! c assoc :call-args (:args call))
            e (await (ctx/call c step (:job call) (with-zone-opt-out c (:args call))))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c after-child step (:args call) e)
              :again))))))

(def max-reach-walks 3)

(defn unseen-reach-target
  "The nearest ground cell not yet walked to whose water reach is partly unseen after the body looked round, when irrigate is on."
  [c]
  (let [tried (:reach-tried (ctx/mem c) #{})]
    (when (and (:irrigate (:args c)) (< (count tried) max-reach-walks))
      (->> (ground-cells c)
           (remove tried)
           (remove #(irrigate/reach-seen? (:primitives c) %))
           (sort-by #(u/dist (u/self-pos c) %))
           first))))

(defn ^:async look-from-unseen-reach!
  "Walk (a go-to child) within 3 blocks of the ground cell whose reach is unseen and look round there, as a player steps closer to see a far row; each cell once."
  [c]
  (let [cell (unseen-reach-target c)
        e (await (ctx/call c :walk 'jobs.movement.go-to {:pos (update cell :y inc) :range 3}))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c update :reach-tried (fnil conj #{}) cell)
          (await (look/look-around! c (look/search-effort)))))))

(defn ^:async walk-to-site!
  "Walk to the unseen field before the pass, as a player does, when go-to's quote of the walk costs less than the pass is
  worth as the walk survives (walk-pick/quoted-s, ev-worth; a quote that cannot price it leaves the walk to say): :again
  once there, :continue while the quote or the walk waits, the walk's own ending, or condition-failed :not-worth."
  [c site]
  (let [args (walk-args c site)
        worth (pass-worth c)
        q (await (ctx/quote c 'jobs.movement.go-to args))
        price (pick/quoted-s c q)]
    (cond
      (:estimate q) :continue
      (and (js/isFinite price) (> price (pick/ev-worth worth q)))
      (ctx/end c :condition-failed :not-worth
               {:steps {} :worth worth :walk-s price
                :text (str "farm tend: the walk to the unseen field costs " (js/Math.round price) " s, more than the pass is worth (" (js/Math.round worth) " s)")})
      :else
      (let [e (await (ctx/call c :walk 'jobs.movement.go-to (assoc args :worth worth)))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (ctx/update-mem! c assoc :site-walked true) :again)
          :else (ctx/end c (:kind e) (contract/cause-key e)
                         {:steps {} :text (str "farm tend: could not walk to the field, go there first: " (contract/cause-key e))}))))))

(defn ^:async step
  "A run over a field partly never seen walks to it when no look from here shows it, looks around and yields (the check decides on what is seen then)."
  [c]
  (cond
    (and (fa/plan c) (:trouble (tend-plan/planned c)))
    :continue
    (and (not (:todo (ctx/mem c))) (walk-due? c))
    (await (walk-to-site! c (walk-due? c)))
    (and (not (:todo (ctx/mem c))) (not (look/surveyed? c)) (needs-look? c))
    (await (look/survey! c :run (look/search-effort)))
    (and (not (:todo (ctx/mem c))) (some? (unseen-reach-target c)))
    (await (look-from-unseen-reach! c))
    :else (await (work c))))

(defn next-part
  "{:part} of the first untried part whose check passes, marking every part looked at as tried; nil when none is left."
  [c]
  (loop [parts (remove #(contains? (:tried (ctx/mem c)) (key-of %)) (parts-of c))]
    (when (seq parts)
      (let [part (first parts)]
        (ctx/update-mem! c update :tried (fnil conj #{}) (key-of part))
        (if (let [s (part-status (for-part c part))] (or (nil? s) (= :look (first s))))
          {:part part}
          (recur (rest parts)))))))

(defn ^:async run-step
  "One step of the whole run: go on with the part under way, else start the next part, else hand over the result."
  [c]
  (if-let [cur (:cur (ctx/mem c))]
    (await (step (for-part c (:part cur))))
    (if-let [{:keys [part]} (next-part c)]
      (do (ctx/update-mem! c assoc :cur {:part part})
          :again)
      (finish-all! c))))

(defn ^:async round
  "The whole run: one pass per part, each one child call after another over the seven steps; :continue only while a
  child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (run-step c))))))
