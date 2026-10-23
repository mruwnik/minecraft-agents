(ns jobs.survival.restore-broken
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]))

(def doc
  "Put back what a job broke in another's zone, claim or plan, and the holes a go-to escalation dug there (the :tidy
  entries jobs.lib.tidy and jobs.movement.go-to write; the body's own diggings elsewhere stay). A dug block is placed
  again from an item of the same name carried, or for an escalation hole what the block drops (cobblestone for stone). An escalation hole waits while
  the body is shut in (:shut-in), as it is the body's way on. A placed block is dug again. Both go through jobs.blocks.place / jobs.blocks.dig children (zones ignored on purpose, nothing fetched, no sight needed: the cell is one the body itself dug), so a cell with nothing solid beside it cannot be placed. It digs nothing else.
  Works only when the body is safe and fit (the standard readiness checks, asked before every round); a danger mid-run
  is the hostile reflex's: it cuts the run, and no cell is skipped for it.
  Touches only a cell whose block is still what the job left (a changed cell is somebody's, left alone),
  with at most 3 tries per cell.
  One run over every entry, paced between steps, a cell put back forgotten at once; a step takes the first cell
  (while the pass's search allowance lasts) whose put-back cuts the body's way to no other it reaches now (a cell with no block beside it to place against comes last); a walk is one go-to call and
  a failed try is repeated until the cell gave up. A cell not restored for now (item not carried) keeps its
  entry. The others are forgotten.
  Done at once with no :tidy entry pending. Ends :done {:restored n :cells [cells put back] :not-restored [{:cell :was :why}]}
  (the lists only when not empty) when every cell was settled, else (also when max-passes ran out with cells
  unvisited) nothing-possible :not-restored with the same data and :failed n. A cut run is :continue and goes on from the entries
  left. Spends :time
  (default ::job-time-s).
  Memory: reads :tidy. Writes :tidy-reported, the cells still waiting, which the :tidy-pending trigger reads
  to warn once per set of cells.")

(declare skip!)

(a/defargs settings
  {::step-radius {:default 4 :doc "Blocks around the body in which a standing cell is looked for." :spec (a/int-in 0 nil)}
   ::max-others-checked {:default 12 :doc "Other broken cells checked per pass for a standing cell." :spec (a/int-in 0 nil)}
   ::max-search-cells {:default 6000 :doc "Cells the walk searches of one pass decision expand at most; a search that would pass it answers unknown." :spec (a/int-in 0 nil)}
   ::max-step-cells-checked {:default 3 :doc "Nearest standable cells checked for a walk when stepping clear of a cell." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 300 :doc "Seconds a run may take before it ends." :spec (a/num-in 0 nil)}
   ::s-per-cell {:default 6 :doc "Seconds a quote counts for one cell: the walk and the put-back." :spec (a/num-in 0 nil)}
   ::max-passes {:default 100 :doc "Steps in one run before it stops." :spec (a/int-in 0 nil)}})
(defn step-radius [] (settings/get settings ::step-radius))

(defn halo-clear?
  "Whether a body standing at the middle of feet cell [x y z] (its 0.6 wide hitbox can reach into the neighbouring
  columns) is clear of every cell in reserved at feet and head height."
  [reserved [x y z]]
  (not-any? reserved (for [dx [-1 0 1] dz [-1 0 1] dy [0 1]] [(+ x dx) (+ y dy) (+ z dz)])))

(defn max-step-cells-checked [] (settings/get settings ::max-step-cells-checked))

(defn clear-cell
  "The nearest standable cell within step-radius of the body where its hitbox is clear of the cell of entry e and of
  every entry of waiting (still to be restored), if the body can walk to it (only the max-step-cells-checked nearest
  are searched, over one block lookup), or nil."
  [c {:keys [cell]} waiting]
  (let [p (:primitives c)
        kind-at (reach/seen-lookup p)
        here (u/self-pos c)
        floor (fn [k] (js/Math.floor (k here)))
        [x y z] [(floor :x) (floor :y) (floor :z)]
        reserved (into #{cell} (map :cell) waiting)
        centre (fn [pos] (-> pos (update :x + 0.5) (update :z + 0.5)))
        cands (for [dx (range (- (step-radius)) (inc (step-radius)))
                    dz (range (- (step-radius)) (inc (step-radius)))
                    dy (range -1 3)
                    :let [pos {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                    :when (halo-clear? reserved [(:x pos) (:y pos) (:z pos)])
                    :when (reach/standable-cell? p pos kind-at)]
                pos)]
    (->> cands
         (sort-by #(u/dist here (centre %)))
         (take (max-step-cells-checked))
         (some #(when (reach/way? kind-at (reach/cell-of here) (reach/cell-of %)) %)))))

(defn max-others-checked [] (settings/get settings ::max-others-checked))
(defn max-search-cells [] (settings/get settings ::max-search-cells))

(defn probe
  "What the checks of one pass decision share, so its work does not grow with the entries waiting or the terrain: one
  block lookup, the body's enclosure (flooded at most once), whether the body walks to each other cell now (searched
  at most once), each cell's seal answer, and max-search-cells cells for all its walk searches."
  [c]
  (let [p (:primitives c)]
    {:kind-at (reach/seen-lookup p)
     ;; raw on purpose: enclosure confirms go-to's own shut-in verdict, and a dark pocket's walls are unseen
     :enclosed (delay (reach-room/enclosed? p :raw))
     :reaches (volatile! {})
     :seals (volatile! {})
     :cells-left (volatile! (max-search-cells))}))

(defn search
  "reach/search over the probe's allowance: :budget (unknown) without searching once fewer than a full search's cells
  are left, each cell expanded counted against it. step: reach/forward or reach/backward."
  [{:keys [cells-left]} step kind-at from to]
  (if (< @cells-left reach/node-budget)
    :budget
    (reach/search (fn [x y z] (vswap! cells-left dec) (step kind-at x y z)) from to)))

(defn search-left?
  "Whether the probe's allowance still covers a full search."
  [{:keys [cells-left]}]
  (<= reach/node-budget @cells-left))

(defn way?
  "reach/way? over the probe's searches: a forward search, settled by a backward one when it runs out of budget;
  unknown counts as a way."
  [pr kind-at from to]
  (case (search pr reach/forward kind-at from to)
    :found true
    :closed false
    (not= :closed (search pr reach/backward kind-at to from))))

(defn reaches-now?
  "Whether the body's walk search finds cell now within its budget (an unknown answer is no: such a cell is too far
  to judge a seal by)."
  [c {:keys [kind-at reaches] :as pr} cell]
  (let [hit (get @reaches cell)]
    (if (some? hit)
      hit
      (let [v (= :found (search pr reach/forward kind-at (reach/cell-of (u/self-pos c)) cell))]
        (vswap! reaches assoc cell v)
        v))))

(defn seals-others?
  "Whether putting back the dug block of e would shut the body off from another entry of waiting it can still reach
  now (the body must be able to walk next to a cell to place a block in it). pr: the pass's probe."
  [c {:keys [cell action] :as e} waiting pr]
  (when (= :dig action)
    (let [hit (get @(:seals pr) cell)]
      (if (some? hit)
        hit
        (let [p (:primitives c)
              kind-at (:kind-at pr)
              [cx cy cz] cell
              filled (fn [x y z] (if (and (== x cx) (== y cy) (== z cz)) :solid (kind-at x y z)))
              here (reach/cell-of (u/self-pos c))
              others (->> waiting
                          (remove #(= cell (:cell %)))
                          (filter #(and (= :dig (:action %)) (nil? (tidy/why-not p % (:enclosed pr)))))
                          (take (max-others-checked)))
              v (boolean (some #(and (reaches-now? c pr (:cell %))
                                     (not (way? pr filled here (:cell %)))) others))]
          (vswap! (:seals pr) assoc cell v)
          v)))))

(defn seals-body?
  "Whether putting back the dug block of e would shut the body itself in: it has room to walk now and would be left
  fewer than shut-room-cells with the cell solid (its doorway, its stair; not one cell off a room-sized floor).
  pr: the pass's probe."
  [c {:keys [cell action]} pr]
  (and (= :dig action)
       (let [p (:primitives c)]
         (and (not @(:enclosed pr))
              (reach-room/closes-in? p :fill cell)))))

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)


(defn done?
  "No :tidy entry waits; the cells put back so far against the entries waiting."
  [c]
  (let [left (count (tidy/entries c))]
    (checks/goal (zero? left) [:restored "every broken cell is back"] [:owed (str left " cells to put back")]
                 {:units :cells :done (count (:restored (ctx/mem c))) :cells (vec (:restored (ctx/mem c))) :left left})))

(defn quote-of [c]
  (let [t (* (settings/get settings ::s-per-cell) (count (tidy/entries c)))]
    {:uses {:job-time [0 t (max t (settings/get settings ::job-time-s))]}}))

(defn why-not
  "Why entry cannot be restored now (:changed :not-carried :shut-in :occupied), or nil. pr: the pass's probe."
  [c e pr]
  (tidy/why-not (:primitives c) e (:enclosed pr)))

(defn ^:async walk-to!
  "Walk to within reach of cell: true when it arrived, :continue when the walk yielded, false when it gave up."
  [c cell]
  (case (await (near/go-near! c (zipmap [:x :y :z] cell) (tidy/reach-blocks) {:ways #{:walk :clear-seal} :one-way :open}))
    :there true
    :partial :continue
    false))

(defn place-item
  "The carried item that puts back the dug cell: tidy/place-item of its entry (an escalation hole takes what the
  block drops), else was."
  [c cell was]
  (or (some->> (tidy/entries c) (filter #(= cell (:cell %))) first (tidy/place-item (:primitives c))) was))

(defn ^:async put-back!
  "Place the recorded block again (a jobs.blocks.place child), or dig the placed one (jobs.blocks.dig): both act in
  the cell of another's zone on purpose. True when it worked, :continue while the child waits or stopped for a
  wait reason (a hostile near, low health): that is the world's, not this cell's failure."
  [c {:keys [cell action was]}]
  (let [pos (zipmap [:x :y :z] cell)
        r (await (if (= :dig action)
                   (child/run!* c :put 'jobs.blocks.place {:pos pos :item (place-item c cell was) :ignore-zones? true :ways #{:carried} :clear false :see false})
                   (child/run!* c :put 'jobs.blocks.dig {:pos pos :ignore-zones? true :ways #{:carried} :collect false :need-drop false})))]
    (cond
      (and (= :continue r) (some-> (:child-wait c) deref)) :wait
      (= :continue r) :continue
      :else (= :done (:kind r)))))

(defn ^:async restore-one!
  "One try at putting entry e back: a walk that yields (:continue) is no try (the next pass walks again); else the
  try is counted before the put-back (a cut run still used one), the cell is forgotten as soon as it is put back.
  A cell that failed is tried again on the next pass until it gave up."
  [c {:keys [cell tries] :as e}]
  (let [walked (await (walk-to! c cell))]
    (if (= :continue walked)
      :continue
      (do (tidy/count-try! c cell)
          (let [put (when walked (await (put-back! c e)))]
            (cond
              (= :continue put) :continue
              (= :wait put) (do (tidy/refund-try! c cell) :continue)
              put (do (tidy/forget-cell! c cell)
                      (ctx/update-mem! c update :restored (fnil conj []) cell)
                      (ctx/update-mem! c update :seen (fnil conj #{}) cell))
              (>= (inc tries) (tidy/max-tries)) (skip! c e :gave-up)))))))

(defn report!
  "Forget the cells that are settled (changed, given up), end: :done, or stopped :not-restored while a cell was not put
  back (its entry kept unless it changed or was given up). The data names the :cells restored and the :not-restored ones."
  [c]
  (let [{:keys [restored failed seen]} (ctx/mem c)
        left (remove #(contains? (or seen #{}) (:cell %)) (tidy/entries c))
        unvisited (count left)
        not-restored (into (vec failed) (map (fn [{:keys [cell was]}] {:cell cell :was was :why :unvisited})) left)
        data (cond-> {:restored (count restored)}
               (seq restored) (assoc :cells (vec restored))
               (seq not-restored) (assoc :not-restored not-restored))]
    (doseq [{:keys [cell why]} failed :when (#{:changed :gave-up} why)] (tidy/forget-cell! c cell))
    (ctx/remember! c :tidy-reported {:cells (mapv :cell (tidy/entries c))} tidy/reported-policy)
    (if (or (pos? unvisited) (seq (remove #(= :changed (:why %)) failed)))
      (let [n (+ unvisited (count (remove #(= :changed (:why %)) failed)))]
        (ctx/end c :nothing-possible :not-restored (assoc data :failed n
                                                          :text (str n " broken blocks not restored, " (count restored) " restored"))))
      (ctx/end c :done nil data))))

(defn skip! [c {:keys [cell was]} why]
  (ctx/update-mem! c update :failed (fnil conj []) {:cell cell :was was :why why})
  (ctx/update-mem! c update :seen (fnil conj #{}) cell))

(defn ^:async step-clear!
  "The body's hitbox overlaps the cell of e: one go-to to a clear standable cell, or, with none or after a walk
  that did not clear it, skip e as :occupied (its entry stays for when the body is out). The cell is clear of every
  entry still waiting."
  [c e waiting]
  (let [{:keys [x y z]} (u/self-pos c)
        _ (await (dig-look/look-unknown! c (step-off/floors {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)} 2)))
        target (when-not (:stepped (ctx/mem c)) (clear-cell c e waiting))]
    (if-not target
      (skip! c e :occupied)
      (do (await (child/call-walk* c :go {:pos target :range 0 :ways #{:walk :clear-seal}}))
          (ctx/update-mem! c assoc :stepped true)))))

(defn farthest-first
  "waiting by distance from the body, the farthest first: a dead end seals nothing, and a sealing candidate costs a
  search of the whole room, which the pass's allowance cannot pay for many times."
  [c waiting]
  (let [[x y z] (reach/cell-of (u/self-pos c))]
    (sort-by (fn [{[cx cy cz] :cell}] (- (+ (* (- cx x) (- cx x)) (* (- cy y) (- cy y)) (* (- cz z) (- cz z))))) waiting)))

(defn supported-first
  "waiting with the cells that have a block beside them to place against first (a cell hanging in the air waits for the
  neighbour put back first), the order within each kept."
  [c waiting]
  (let [p (:primitives c)
        hangs? (fn [{[x y z] :cell action :action}] (and (= :dig action) (not (b/support? p {:x x :y y :z z}))))]
    (concat (remove hangs? waiting) (filter hangs? waiting))))

(defn max-passes [] (settings/get settings ::max-passes))

(defn decide
  "What one pass does with the waiting entries (synchronous, its searches and floods bounded whatever the number
  waiting): [:done], [:skip e why], [:step-clear e] or [:restore e]. An occupied cell comes first, then the first of
  the first, farthest from the body first, whose put-back seals no other (checked while the pass's search allowance
  lasts), else the first in order."
  [c waiting]
  (let [pr (probe c)
        e (or (first (filter #(= :occupied (why-not c % pr)) waiting))
              (first (remove #(seals-others? c % waiting pr) (take-while (fn [_] (search-left? pr)) (supported-first c (farthest-first c waiting)))))
              (first (supported-first c waiting)))
        why (when e (why-not c e pr))]
    (cond
      (nil? e) [:done]
      (= :occupied why) [:step-clear e]
      why [:skip e why]
      (tidy/unreachable? (:primitives c) e) [:skip e :unreachable]
      (>= (:tries e) (tidy/max-tries)) [:skip e :gave-up]
      (or (seals-body? c e pr) (seals-others? c e waiting pr)) [:skip e :seals]
      :else [:restore e])))

(defn ^:async pass!
  "One step over the next waiting entry: restore it, step clear of it, or skip it with a reason. Resolves :again,
  :continue while a walk or put-back child yields, or :done when none is waiting."
  [c]
  (let [seen (:seen (ctx/mem c) #{})
        waiting (vec (remove #(contains? seen (:cell %)) (tidy/entries c)))
        [verdict e why] (decide c waiting)]
    (case verdict
      :done :done
      :skip (do (if (and (= :changed why) (= (:was e) (tidy/block-now (:primitives c) (:cell e))))
                  (do (tidy/forget-cell! c (:cell e))
                      (ctx/update-mem! c update :restored (fnil conj []) (:cell e))
                      (ctx/update-mem! c update :seen (fnil conj #{}) (:cell e)))
                  (skip! c e why))
                :again)
      :step-clear (do (await (step-clear! c e waiting)) :again)
      :restore (if (= :continue (await (restore-one! c e))) :continue :again))))

(defn ^:async round [c]
  (let [i (volatile! 0)
        r (await (pace/steps! c #(if (< (max-passes) (vswap! i inc)) :done (pass! c))))]
    (if (= :continue r) :continue (report! c))))
