(ns jobs.farm.tend
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [jobs.farm.fertilize :as fertilize]
            [jobs.farm.harvest :as harvest]
            [jobs.farm.plant :as plant]
            [jobs.farm.till :as till]))

(def doc
  "Keep one field of crops in order. The field is the :box (inclusive; its lowest
  layer y = min is the farmland, the layer above it holds the crops, so max y is
  at least min y + 1); one run is one convergent pass over six steps in a fixed
  order, each a child job run one round at a time: :harvest (jobs.farm.harvest,
  replanting) when a ripe crop lies in the box; :till (jobs.farm.till, one ground
  cell per call, the nearest to the body first) when :till is true, a hoe is
  carried, dirt or grass lies uncovered in the ground layer and more seed is
  carried than there is bare farmland (bare farmland reverts to dirt, so a bed is
  only made when it can be sown), re-decided after each cell until it skips;
  :plant (jobs.farm.plant) when bare farmland lies in the box and a seed is
  carried; :fertilize (jobs.farm.fertilize) when :fertilize is true, bone meal is
  carried and an unripe crop lies in the box; :compost (jobs.farm.compost) when a
  :composter is given and seed above the reserve is carried; :deposit
  (jobs.storage.deposit) when a :chest is given and farm goods (seeds, crops,
  melon, pumpkin, hay, cocoa; never tools, food or anything else) above the keep
  are carried. The seed reserve is enough to sow the field twice over: twice the
  beds (farmland or untilled cells of the ground layer), spread over the carried
  seeds in turn; the keep of compost and deposit is that reserve or the :keep
  entry, whichever is larger. A step whose conditions do not hold is skipped and
  booked {:skipped reason} (:no-ripe, :till-off, :no-hoe, :nothing-to-till,
  :no-seed, :no-bare, :fertilize-off, :no-bone-meal, :none-unripe,
  :no-composter, :no-surplus-seed, :no-chest, :nothing-to-store, or :declined
  when the child declined); a till that tilled keeps its {:tilled n} when it
  later skips. The check passes when a box of at most 2048 cells is given and
  some step would run, and always once started (a cut job resumes); otherwise
  the job declines and does nothing, so it is cheap under repeat. A step under
  way is not re-decided, it runs until its child is done. Hands over {:steps
  {step summary} :field {:crops :bare :untilled} (live census at the end)} (info
  farm-tend.done) and ends :done, also when every step was skipped after the
  first. Summaries: harvest {:cut :replanted :bare :gave-up}, till {:tilled n},
  plant {:planted :reason :skipped}, fertilize {:used}, compost {:fed
  :bone-meal :reason}, deposit {:gave-up :reason}. Known limits: harvest and
  fertilize work in a sphere around the box centre, so crops just outside the box
  may be cut or fertilized; a dirt lane inside the box is a bed unless :till is
  false; a ripe crop that cannot be reached makes the check pass on every run
  (harvest gives up on it, the run still ends).")

(def args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; y min is the farmland layer, max y at least min y + 1; at most 2048 cells; required (without it the check declines)" :default nil}
   :till {:doc "hoe untilled dirt and grass in the ground layer when seed and a hoe are carried" :default true}
   :fertilize {:doc "use bone meal on unripe crops" :default false}
   :composter {:doc "composter position {:x :y :z} for seed above the reserve; nil: do not compost" :default nil}
   :chest {:doc "chest position {:x :y :z} for the farm goods; nil: do not store" :default nil}
   :keep {:doc "{item-name count}: how many of an item compost and deposit leave carried, at least the seed reserve" :default {}}})

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
  (->> (array-seq (.blocks p #js {:radius (+ R (u/dist (u/pos-of (.-pos (.self p))) mid))
                                  :names (clj->js (vec (keys fertilize/ripe-age))) :max 256}))
       (filter fertilize/unripe?)
       (map #(u/pos-of (.-pos %)))
       (filter #(in-box? box %))
       vec))

(defn facts
  "What the decisions are made from, read live."
  [c]
  (let [p (:primitives c)
        {:keys [box fertilize keep]} (:args c)
        mid (centre box)
        R (radius box mid)
        inventory (u/inventory p)
        me (u/self-pos c)
        tried (:till-tried (ctx/mem c) #{})
        ground (ground-layer p box)
        bare (count (plant/bare-cells p box []))
        res (reserve (beds ground) inventory)]
    {:mid mid
     :radius R
     :ripe (count (filter #(in-box? box %) (harvest/ripe-crops p {:radius R :crops nil} mid [])))
     :hoe (some? (till/hoe-of p))
     :untilled (->> ground
                    (filter untilled?)
                    (map :pos)
                    (remove tried)
                    (sort-by #(u/dist me %))
                    vec)
     :bare bare
     :seeds (transduce (map #(get (carried inventory) % 0)) + 0 seed-items)
     :seed (some? (plant/pick-seed nil inventory))
     :meal (boolean (fertilize/has-meal? p))
     :unripe (if fertilize (count (unripe-in-box p box mid R)) 0)
     :keep (merge-with max res keep)
     :waste (surplus inventory (merge-with max res keep) waste-seeds)
     :stored (surplus inventory (merge-with max res keep) farm-goods)}))

(defn census
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

;; ------------------------------------------------------------------ decisions

(defn decide-harvest
  [_args {:keys [ripe mid radius]}]
  (if (zero? ripe)
    {:skip :no-ripe}
    {:call {:slot :harvest :job (jobs :harvest) :args {:center mid :radius radius :replant true}}}))

(defn decide-till
  [{:keys [till]} {:keys [hoe untilled bare seeds]}]
  (cond
    (not till) {:skip :till-off}
    (not hoe) {:skip :no-hoe}
    (empty? untilled) {:skip :nothing-to-till}
    (<= seeds bare) {:skip :no-seed}
    :else {:call {:slot :till :job (jobs :till) :args {:from (first untilled) :to (first untilled)}}}))

(defn decide-plant
  [{:keys [box]} {:keys [bare seed]}]
  (cond
    (zero? bare) {:skip :no-bare}
    (not seed) {:skip :no-seed}
    :else {:call {:slot :plant :job (jobs :plant) :args {:box box}}}))

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

(defn check [c]
  (let [{:keys [box]} (:args c)]
    (boolean (and (usable-box? box)
                  (or (:todo (ctx/mem c))
                      (let [f (facts c)]
                        (some #(:call (decide % (:args c) f)) steps)))))))

;; ------------------------------------------------------------------ rounds

(defn summary
  "What the report keeps of a child's result."
  [step r]
  (case step
    :harvest (select-keys r [:cut :replanted :bare :gave-up])
    :till {:tilled (:tilled r 0)}
    :plant (select-keys r [:planted :reason :skipped])
    :fertilize (select-keys r [:used])
    :compost (select-keys r [:fed :bone-meal :reason])
    :deposit (select-keys r [:gave-up :reason])))

(defn finish!
  [c report]
  (let [field (census c)
        out {:steps report :field field}]
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

(defn ^:async round [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo steps :report {} :till-tried #{} :tilled 0))
  (let [{:keys [call] :as p} (next-plan c)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish! c (:report p))
      (let [step (:slot call)
            _ (ctx/update-mem! c assoc :call-args (:args call))
            outcome (await (ctx/call-child c step (:job call) (:args call)))]
        (when (#{:done :declined} outcome)
          (ctx/update-mem! c after-child step (:args call) outcome (when (= :done outcome) (ctx/child-result c step))))
        :continue))))
