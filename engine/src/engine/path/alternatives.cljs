(ns engine.path.alternatives
  "Up to k clearly different paths for one query, best first, each with its true cost and what sets it apart.

   Different: a path is returned only when, against the paths returned before it, its set of kinds (ladder, water, door;
   none of them is on foot) is not one of theirs, or fewer than 60% of its cells lie within 2 blocks of their cells.
   (A distance is the largest of |dx|, |dy| and |dz|. Within 2, not 1: the penalised search below steers just past 1 block,
   so over open ground it finds the same walk shifted 2 blocks over, which is not another way.)

   How: the first path is `plan` itself. Candidates for the next come from the same search run again with options.avoid:
   once per kind seen in a returned path with that kind refused (and once with all of the first path's kinds refused), and
   once with every cell within 1 block of a returned path costing (1 + factor) times its own cost, factor 1, then 4, then
   16 while the result is still too close and still pays a penalty away from its ends. These searches are weighted (WEIGHT)
   and capped (NODES-TIMES the first search's expansions, at least NODES-MIN). Each round returns the cheapest candidate
   that passes; none passing ends the list, so one path is a normal answer. Costs are the true cost of the walk (the
   penalty orders the search only), and every step is an edge of the normal search (avoiding only adds cost or refuses)."
  (:require [clojure.string :as str]
            [engine.path.planner-tuned :as planner]))

(def NEAR-SHARE "a path with at least this share of its cells within 2 blocks of earlier ones is not a separate way" 0.6)
(def FACTORS "the penalty factors tried in turn for a cell within 1 block of an earlier path" [1 4 16])
(def WEIGHT "the heuristic weight of a search for an alternative: its order is penalised anyway, so it need not be optimal" 1.5)
(def NODES-TIMES "a search for an alternative may expand this many times the nodes the first search did..." 10)
(def NODES-MIN "...and at least this many (under options.maxNodes): the penalty loosens the heuristic, so they expand more" 2000)

;; the kinds of a path: [kind, bit in options.avoid.kinds, words for the summary]
(def KINDS [[:ladder planner/AVOID-CLIMB "by ladder"]
            [:water planner/AVOID-WATER "through water"]
            [:door planner/AVOID-OPEN "through a door"]])

(def ^:private MOVE-CLIMB-UP 7)
(def ^:private MOVE-OPEN 10)
(def ^:private MOVE-SWIM 11)

(defn- step-kinds [^js step]
  (let [move (.-move step)]
    (cond-> #{}
      (<= MOVE-CLIMB-UP move MOVE-OPEN) (conj :ladder)
      (or (>= move MOVE-SWIM) (true? (.-swim step))) (conj :water)
      (some? (.-opens step)) (conj :door))))

(defn path-kinds [^js path]
  (reduce into #{} (map step-kinds (.-steps path))))

(defn- kinds-words [kinds]
  (if (empty? kinds)
    "on foot"
    (str/join " and " (keep (fn [[kind _ words]] (when (kinds kind) words)) KINDS))))

(defn- kinds-bits [kinds]
  (reduce + 0 (keep (fn [[kind bit _]] (when (kinds kind) bit)) KINDS)))

(defn- cells [^js path]
  (mapv (fn [^js s] [(.-x s) (.-y s) (.-z s)]) (.-steps path)))

;; the keys of every cell within r blocks of a cell of `cs`
(defn- around [cs r]
  (let [offsets (range (- r) (inc r))]
    (persistent!
     (reduce (fn [acc [x y z]]
               (reduce conj! acc (for [dx offsets dy offsets dz offsets] (planner/cell-key (+ x dx) (+ y dy) (+ z dz)))))
             (transient #{})
             cs))))

(defn- total [^js path risk-weight]
  (let [^js cost (.-cost path)]
    (+ (.-seconds cost) (* risk-weight (.-risk cost)))))

(defn- entry [^js path risk-weight]
  {:path path :cells (cells path) :kinds (path-kinds path) :total (total path risk-weight)})

;; what the returned paths so far rule out: their kind sets; the cells within 1 block of them (penalised) and 2 (counted)
(defn- taken [chosen]
  (let [cs (mapcat :cells chosen)]
    {:kind-sets (set (map :kinds chosen)) :near (around cs 1) :within-2 (around cs 2)}))

(defn- near-share [{:keys [cells]} near]
  (/ (count (filter #(near (apply planner/cell-key %)) cells)) (count cells)))

(defn different?
  "the sentence of the ns docstring"
  [candidate {:keys [kind-sets within-2]}]
  (or (not (kind-sets (:kinds candidate)))
      (< (near-share candidate within-2) NEAR-SHARE)))

(def ^:private SIDES [[0 -1 "north"] [0 1 "south"] [1 0 "east"] [-1 0 "west"]])

;; where a separate way runs: the side of the earlier paths its own cells (those over 2 blocks from them) lie on, by centre
(defn- side [{:keys [cells]} chosen within-2]
  (let [mean (fn [cs] (let [n (count cs)] [(/ (reduce + (map first cs)) n) (/ (reduce + (map #(nth % 2) cs)) n)]))
        own (filter #(not (within-2 (apply planner/cell-key %))) cells)
        [ox oz] (mean own)
        [bx bz] (mean (mapcat :cells chosen))
        [dx dz] [(- ox bx) (- oz bz)]]
    (last (apply max-key (fn [[sx sz _]] (+ (* sx dx) (* sz dz))) SIDES))))

(defn- differs [candidate chosen {:keys [kind-sets within-2]}]
  (let [first-kinds (:kinds (first chosen))]
    (if-not (kind-sets (:kinds candidate))
      (str (kinds-words (:kinds candidate)) " instead of " (kinds-words first-kinds))
      (str (kinds-words (:kinds candidate)) ", a separate way to the " (side candidate chosen within-2) " ("
           (js/Math.round (* 100 (- 1 (near-share candidate within-2)))) "% of its cells over 2 blocks from earlier paths)"))))

(defn- found-path [^js result]
  (when (= "found" (.-status result)) (.-path result)))

;; a JS Set of the keys, as options.avoid.cells takes them
(defn- key-set [ks]
  (let [out (js/Set.)]
    (doseq [k ks] (.add out k))
    out))

(defn- searcher [snapshot query ^js options ^js first-result]
  (let [max-nodes (min (or (.-maxNodes options) 200000) (max NODES-MIN (* NODES-TIMES (.-expanded first-result))))]
    (fn [avoid] (planner/plan snapshot query (js/Object.assign #js {} options #js {:avoid avoid :maxNodes max-nodes :weight WEIGHT})))))

;; does the path cross a penalised cell more than 2 blocks from its start and its end (cells every path passes near)?
(defn- crosses-penalty? [{:keys [cells]} near]
  (let [[sx sy sz] (first cells)
        [ex ey ez] (peek cells)
        far (fn [[x y z] [ax ay az]] (> (max (abs (- x ax)) (abs (- y ay)) (abs (- z az))) 2))]
    (boolean (some #(and (far % [sx sy sz]) (far % [ex ey ez]) (near (apply planner/cell-key %))) cells))))

;; the candidate of a penalised search, trying the factors in turn while the result fails the test and still pays a penalty
;; away from the ends (one that pays none is found again by any larger factor): [entry-or-nil searches]
(defn- penalised [search rules risk-weight]
  (let [cells (key-set (:near rules))]
    (loop [[factor & more] FACTORS n 1]
      (let [path (found-path (search #js {:kinds 0 :cells cells :factor factor}))
            candidate (when path (entry path risk-weight))]
        (cond
          (nil? candidate) [nil n]
          (different? candidate rules) [candidate n]
          (and (seq more) (crosses-penalty? candidate (:near rules))) (recur more (inc n))
          :else [nil n])))))

(defn- answer [^js first-result paths searches t0]
  #js {:status (.-status first-result) :reason (.-reason first-result) :paths (into-array paths) :searches searches
       :ms (- (js/performance.now) t0)})

(defn- shown-path [^js path total differs-text]
  #js {:steps (.-steps path) :cost (.-cost path) :summary (.-summary path) :total total :differs differs-text})

(defn- shown [{:keys [path total]} differs-text] (shown-path path total differs-text))

(defn plan-alternatives
  "{status, reason, paths, searches, ms}: up to k (default 3) paths for one query, best first, as the ns docstring says.
   paths[i]: {steps, cost, summary, total (seconds + riskWeight * risk), differs}. The first path is what `plan` returns;
   status and reason are its. A query without a found path gives that one path (partial) or none."
  ([snapshot query options] (plan-alternatives snapshot query options 3))
  ([snapshot query ^js options k]
   (let [t0 (js/performance.now)
         ^js first-result (planner/plan snapshot query options)
         path (found-path first-result)
         risk-weight (if (some? (.-riskWeight options)) (.-riskWeight options) 2)]
     (if (or (nil? path) (<= k 1))
       (answer first-result (if-let [^js p (.-path first-result)] [(shown-path p (total p risk-weight) "best")] []) 1 t0)
       (let [search (searcher snapshot query options first-result)
             first-kinds (path-kinds path)
             forbid-results (atom {})
             forbid (fn [bits]
                      (when-not (contains? @forbid-results bits)
                        (swap! forbid-results assoc bits (some-> (found-path (search #js {:kinds bits :cells (js/Set.) :factor 0}))
                                                                 (entry risk-weight))))
                      (get @forbid-results bits))]
         (loop [chosen [(entry path risk-weight)]
                shown-paths [(shown (entry path risk-weight) "best")]
                searches 1]
           (if (>= (count chosen) k)
             (answer first-result shown-paths searches t0)
             (let [rules (taken chosen)
                   masks (distinct (concat (when (> (count first-kinds) 1) [(kinds-bits first-kinds)])
                                           (for [kinds (map :kinds chosen) kind kinds] (kinds-bits #{kind}))))
                   before (count @forbid-results)
                   by-kind (into [] (keep forbid) masks)
                   [by-penalty n] (penalised search rules risk-weight)
                   searches (+ searches n (- (count @forbid-results) before))
                   passing (filter #(different? % rules) (cond-> by-kind by-penalty (conj by-penalty)))]
               (if (empty? passing)
                 (answer first-result shown-paths searches t0)
                 (let [best (apply min-key :total passing)]
                   (recur (conj chosen best)
                          (conj shown-paths (shown best (differs best chosen rules)))
                          searches)))))))))))
