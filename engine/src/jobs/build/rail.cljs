(ns jobs.build.rail
  "The head-first builder of jobs.build.rail-line, a job of its own."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.pace :as pace]
            [jobs.lib.world :as known]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.near :as near]
            [jobs.build.from-plan :as build]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.build.text :as text]
            [plan.rail :as rail]))

(def doc
  "Build the rail cells of a plan (:plan, optionally only its :part) head-first. A rail takes its shape from the rails
  beside it when placed, so a line with corners and slopes is built in line order, standing on the line.
    1. Work is ordered station by station along the chain (plan.rail/line), from the end nearer the body at the
       start. At each station: the cells below the rail (bed, redstone block), then the rail, then what stands beside
       it (torch, lever, with its own bed).
    2. Each round places cells in that order while they are within reach. When the next is not, it walks to the rail
       behind it, else to a cell beside it as jobs.build.from-plan does.
    3. A rail has settled once every neighbour it has in the chain is a rail in the world. A settled rail of the wrong
       shape is dug and placed again (:fix times), then given up as :shape.
  Placing, access rules, refusals, give-ups and the result are jobs.build.from-plan's functions over this job's
  memory. Every dig and place asks jobs.lib.access.rules right before it acts.
  Needs: a readable plan and the items for the cells to place carried (the caller fetches them). Ends done with the
  data {:placed :missing :short :given-up :wrong :refused} once nothing more can be done; a plan that is missing or
  unreadable is a wait (later :plan-trouble), a readable one with no cells never (as jobs.build.from-plan); a dig child's failure ends it with that child's kind and cause.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :reach {:doc "as jobs.build.from-plan" :spec (a/num-in 0 nil) :default u/eye-reach}
   :give-up {:doc "as jobs.build.from-plan" :spec (a/int-in 1 nil) :default 3}
   :accept {:doc "as jobs.build.from-plan" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :fix {:doc "times a rail whose settled shape is wrong is dug and placed again, then given up as :shape (0 or false: given up at once)" :spec (a/or-of (a/int-in 0 nil) false?) :default 1}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one build may take." :spec (a/int-in 1 nil)}})

(def target [:plan :part])

(def can-run? build/plan-problem)

(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(defn quote-of [_c]
  {:uses {:job-time [30 300 (settings/get settings ::job-time-s)]}})

(defn done?
  [c]
  (if (:built (ctx/mem c))
    (contract/ok :built "rail: the build is finished")
    (contract/later :building "rail: cells still to build")))


;; ------------------------------------------------------------------ order

(defn column-index
  "{[x z] chain index} of a chain of positions."
  [ps]
  (into {} (map-indexed (fn [i [x _ z]] [[x z] i])) ps))

(defn station
  "The chain index of the rail cell at or nearest in plan view to pos."
  [ps by-column [x _ z]]
  (or (by-column [x z])
      (some->> [[1 0] [-1 0] [0 1] [0 -1]]
               (keep (fn [[dx dz]] (by-column [(+ x dx) (+ z dz)])))
               seq
               (apply min))
      (apply min-key (fn [i] (let [[px _ pz] (ps i)] (+ (abs (- x px)) (abs (- z pz))))) (range (count ps)))))

(defn work-order
  "The cells in the order they are built: station by station from the end `from` (:first or :last), lower cells first,
  at the same height the rail before what stands beside it."
  [ps from cells]
  (let [by-column (column-index ps)
        n (count ps)]
    (sort-by (fn [{:keys [pos] :as cell}]
               (let [i (station ps by-column pos)]
                 [(if (= :first from) i (- n i)) (second pos) (if (rail/rail-cell? cell) 0 1)]))
             cells)))

(defn nearer-end
  "The end of the chain ps the body is nearer to: :first or :last."
  [c ps]
  (let [body (u/self-pos c)
        d (fn [[x y z]] (u/dist body {:x x :y y :z z}))]
    (if (<= (d (first ps)) (d (peek ps))) :first :last)))

;; ------------------------------------------------------------------ shapes

(defn wrong-shapes
  "The positions of settled rails (every chain neighbour is a rail in the world) whose shape differs from the chain's."
  [c ps]
  (let [at (partial plan/world-block (:primitives c))
        rail? #(rail/rail-name? (:name (at %)))]
    (vec (for [i (range (count ps))
               :let [pos (ps i)
                     block (at pos)
                     joins (keep #(get ps %) [(dec i) (inc i)])]
               :when (and (rail? pos) (every? rail? joins) (rail/state block :shape)
                          (not= (rail/shape-joining pos joins) (rail/state block :shape)))]
           pos))))

(defn fix-budget
  "How many times a wrong rail may be dug and placed again: the :fix argument (a number, true for 1, else 0)."
  [c]
  (let [f (:fix (:args c))]
    (cond (number? f) f f 1 :else 0)))

(defn idle?
  "Whether the builder has nothing to do: nothing placeable with what is carried, no unseen cell with its item
  carried, no settled wrong shape."
  [c cells]
  (let [carried (storage/carried-totals (u/inventory (:primitives c)))
        ps (mapv :pos (rail/line cells))]
    (and (empty? (plan/buildable cells carried {}))
         (not-any? #(pos? (get carried (:item %) 0)) (plan/unseen cells {}))
         (empty? (wrong-shapes c ps)))))

;; ------------------------------------------------------------------ steps

(defn stand-behind
  "The nearest of the 3 rails behind cell (on the side the build comes from) that is placed and not a bad stand, or nil."
  [c ps from cell]
  (let [by-column (column-index ps)
        i (station ps by-column (:pos cell))
        step (if (= :first from) -1 1)
        bad (set (:bad-stands (ctx/mem c)))]
    (->> [1 2 3]
         (keep #(get ps (+ i (* step %))))
         (remove #(or (bad %) (= % (:pos cell))))
         (filter #(rail/rail-name? (:name (plan/world-block (:primitives c) %))))
         first)))

(defn beside-stand
  "The stand beside pos nearest the body, on the ground there (plan/ground-stands), not planned or bad; nil if none."
  [c body pos planned bad]
  (->> (plan/stand-cells pos (js/Math.floor (:y body)) nil planned)
       (plan/ground-stands (:primitives c))
       (remove bad)
       (sort-by #(u/dist body (zipmap [:x :y :z] %)))
       first))

(defn ^:async walk-to!
  "Walk to a stand for cell: the rail behind it, else a cell beside it. Counts a failure when the stand cannot be
  walked to, or on arrival the cell is still out of reach or unseen (:unloaded). :continue while the walk waits, else
  :again."
  [c ps from cells cell]
  (let [body (u/self-pos c)
        planned (set (map :pos cells))
        bad (set (:bad-stands (ctx/mem c)))
        beside (beside-stand c body (:pos cell) planned bad)
        stand (or (stand-behind c ps from cell) beside)
        give-up (:give-up (:args c))]
    (if-not stand
      (do (ctx/update-mem! c build/count-fail (:pos cell) :unreachable give-up)
          :again)
      (let [w (await (near/go-near! c (zipmap [:x :y :z] stand) 0 {:zone-tolls true :ways #{:walk :clear-seal}}))]
        (when (= :blocked w)
          (ctx/update-mem! c #(-> (build/count-fail % (:pos cell) :unreachable give-up)
                                  (update :bad-stands (fnil conj []) stand))))
        (when (and (= :there w) (nil? (:found cell)))
          (ctx/update-mem! c build/count-fail (:pos cell) :unloaded give-up))
        (when (and (= :there w) (:found cell) (empty? (build/in-reach c [cell])))
          (ctx/update-mem! c build/count-fail (:pos cell) :unreachable give-up))
        (if (= :partial w) :continue :again)))))

(defn ^:async place-run!
  "Place the cells in order while each is within reach. Returns {:placed n} and, when one was out of reach, :next cell."
  [c ordered]
  (loop [left ordered n 0]
    (if-let [cell (first left)]
      (if (seq (build/in-reach c [cell]))
        (do (await (build/place-one! c cell))
            (recur (rest left) (inc n)))
        {:placed n :next cell})
      {:placed n})))

(defn ^:async dig-away!
  "Dig the rail at pos through the jobs.blocks.dig child (hazards, tidy record, tool, drops) if the rules agree, and
  count one fix when it is dug. Water beside is taken as :accept says, lava never. A dig that ended done without
  digging, on a hazard or unreachable counts a :shape failure (the next round re-reads the cell); any other ending (no tool, a limit) is the job's.
  Resolves to :continue while the child waits or is cut off (the build parks), else :again."
  [c pos]
  (let [v (rules/may-dig? (assoc (plan/rules-input c) :cell pos))
        [x y z] pos
        lava? (some #{:lava-adjacent} (plan/hazards (:block-at (plan/rules-input c)) pos))
        args (merge (select-keys (:args c) [:ignore-zones?])
                    {:accept (if lava? [] (filter #{:fluid-adjacent} (:accept (:args c))))}
                    {:pos {:x x :y y :z z} :need-drop false :collect true :on-fluid :fail :for-plan (:plan (:args c))})]
    (if-not (:ok v)
      (do (ctx/update-mem! c plan/refuse pos (select-keys v [:reason :zone :plan :claim]))
          :again)
      (let [r (await (ctx/call c :dig 'jobs.blocks.dig args))]
        (cond
          (= :continue r) :continue
          (= :cut-off (:kind r)) :continue
          (and (= :done (:kind r)) (:dug (:data r))) (do (ctx/update-mem! c update-in [:fixes pos] (fnil inc 0)) :again)
          (or (= :done (:kind r)) (#{:hazard :fluid-adjacent :cannot :unreachable} (contract/cause-key r)))
          (do (ctx/update-mem! c build/count-fail pos :shape (:give-up (:args c))) :again)
          :else (ctx/end c (:kind r) (:cause r)))))))

(defn ^:async fix-step!
  "Deal with the settled wrong rail at pos: give up as :shape once the fixes are spent, else dig it (walking into
  reach first). It is placed again in its turn."
  [c ps from cells pos]
  (let [cell (first (filter #(= pos (:pos %)) cells))]
    (cond
      (>= (get-in (ctx/mem c) [:fixes pos] 0) (fix-budget c)) (do (ctx/update-mem! c assoc-in [:given-up pos] :shape)
                                                                   :again)
      (empty? (build/in-reach c [cell])) (await (walk-to! c ps from cells cell))
      :else (await (dig-away! c pos)))))

(defn finish!
  "End the build with its result as the ending's data."
  [c cells]
  (let [result (text/summary c cells (ctx/mem c) true)]
    (text/announce! c result)
    (ctx/update-mem! c assoc :built result)
    (ctx/end c :done nil result)))

(defn ^:async step!
  "One round of the builder over the plan's judged cells, in this order: fix a settled wrong rail, place work in reach,
  walk on, go toward unseen cells, else finish. :again, or :continue while a walk or the dig child waits."
  [c cells]
  (let [p (:primitives c)
        ps (mapv :pos (rail/line cells))
        _ (build/settle-placing! c cells)
        _ (when-not (:from (ctx/mem c)) (ctx/update-mem! c assoc :from (nearer-end c ps)))
        from (:from (ctx/mem c))
        closed (merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        bad (remove #(contains? closed %) (wrong-shapes c ps))
        buildable (plan/buildable cells (storage/carried-totals (u/inventory p)) closed)
        todo (plan/placeable c (plan/permitted c buildable))
        underfoot (filter #((plan/body-cells (u/self-pos c)) (:pos %)) buildable)
        unseen (plan/unseen cells closed)]
    (cond
      (seq bad) (await (fix-step! c ps from cells (first bad)))
      (seq todo) (let [{:keys [placed next]} (await (place-run! c (work-order ps from todo)))]
                   (if (and (zero? placed) next) (await (walk-to! c ps from cells next)) :again))
      (seq underfoot) (await (walk-to! c ps from cells (first (work-order ps from underfoot))))
      (seq unseen) (await (walk-to! c ps from cells (first (work-order ps from unseen))))
      :else (finish! c cells))))

(defn cells-of
  "The judged cells of the plan in the args, nil when it cannot be read."
  [c]
  (let [{:keys [plan part]} (:args c)
        p (:primitives c)
        answer (known/plan c plan)]
    (when (and answer (not (:broken answer)))
      (plan/judged p answer part (set (keys (storage/carried-totals (u/inventory p)))) nil))))

(defn ^:async round
  "Build in steps (pace/steps!); :continue while a walk or the dig child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s []
                          (if-let [cells (seq (cells-of c))]
                            (await (step! c (vec cells)))
                            :continue)))))
