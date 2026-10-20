(ns jobs.lib.build.plan
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.build.kept :as kept]
            [jobs.lib.placement :as placement]
            [jobs.lib.reach :as lib-reach]
            [plan.rail :as rail]
            [plan.shape :as shape]
            [jobs.lib.build.wood :as wood]
            [jobs.lib.trees :as trees]
            [jobs.lib.need :as need]
            [plan.surface :as surface]
            [jobs.lib.world :as known]))

(a/defargs settings
  "Tuning numbers of the stand choice and of retrying walks."
  {::stand-level {:default 3 :doc "A body within this many blocks of a cell's height also looks for stands at its own level (after the cell's)." :spec (a/int-in 0 nil)}
   ::retry-distance {:default 8 :doc "A cell given up for a failed walk is tried again once the body is this far from where it gave up, in blocks." :spec (a/num-in 0 nil)}
   ::max-retries {:default 2 :doc "Times one cell given up for a failed walk is opened again." :spec (a/int-in 0 nil)}
   ::reach {:default u/eye-reach :doc "Cells whose centre is this close to the eye are placed without walking, in blocks." :spec (a/num-in 0 nil)}})

(defn stand-level [] (settings/get settings ::stand-level))
(defn retry-distance [] (settings/get settings ::retry-distance))
(defn reach [] (settings/get settings ::reach))
(defn max-retries [] (settings/get settings ::max-retries))

;; ------------------------------------------------------------------ pure helpers

(defn item-for
  "The item to place for want: the block it names (a wall torch with a torch), for an :any the first choice carried
  (else the first); nil for :clear, crops and trees. carried: a set of names, or {name n} where n above 0 counts."
  [want carried]
  (cond
    (string? want) (placement/item-of want)
    (vector? want) (let [names (map #(placement/item-of (if (string? %) % (:block %))) (rest want))]
                     (or (first (filter #(if (map? carried) (pos? (get carried % 0)) (contains? carried %)) names)) (first names)))
    (and (map? want) (:block want)) (placement/item-of (:block want))))

(defn block-want
  "The block want that item places: for an :any the choice placed with it."
  [want item]
  (if (vector? want)
    (or (first (filter #(= item (placement/item-of (if (string? %) % (:block %)))) (rest want))) item)
    want))

(def facing-step {"north" [0 -1] "south" [0 1] "east" [1 0] "west" [-1 0]})

(defn facing-of
  "The horizontal :facing a want asks for, as a string, or nil."
  [want]
  (let [f (when (map? want) (:facing want))]
    (when (and f (facing-step (name f))) (name f))))

(defn facing-ok?
  "Whether a body at body (feet position) looks toward cell [x y z] mostly in the direction facing (nil: any)."
  [facing [x _ z] body]
  (if-not facing
    true
    (let [[sx sz] (facing-step facing)
          dx (- (+ x 0.5) (:x body))
          dz (- (+ z 0.5) (:z body))
          along (+ (* sx dx) (* sz dz))
          across (js/Math.abs (+ (* sz dx) (* sx dz)))]
      (and (pos? along) (>= along across)))))

(defn body-cells [{:keys [x y z]}]
  (let [fx (js/Math.floor x) fy (js/Math.floor y) fz (js/Math.floor z)]
    #{[fx fy fz] [fx (inc fy) fz]}))

(defn stand-cells
  "Cells one to three blocks beside cell (or diagonally off it) at height y, where a body stands to place it: for a
  facing, only straight out on the side it faces away from; never a planned cell."
  [[x _ z] y facing planned]
  (let [straight (if facing [(mapv - (facing-step facing))] [[1 0] [-1 0] [0 1] [0 -1]])
        diagonal (if facing [] [[1 1] [1 -1] [-1 1] [-1 -1]])]
    (->> (concat (for [d [2 1 3] [sx sz] straight] [(+ x (* d sx)) y (+ z (* d sz))])
                 (for [d [1 2] [sx sz] diagonal] [(+ x (* d sx)) y (+ z (* d sz))]))
         (remove #(or (planned %) (planned (update % 1 inc)))))))

(defn stand-levels
  "The feet heights to look for stands at, in order: the cell's (a body deep underground aims for the site, not for its
  own depth), then the body's own when within stand-level of it (a pit cell is placed from the rim)."
  [body-y cell-y]
  (if (<= (js/Math.abs (- body-y cell-y)) (stand-level)) (distinct [cell-y body-y]) [cell-y]))

(defn grounded
  "The cands put on the ground (see ground-stands); empty when none can be stood on."
  [p cands]
  (let [fit (fn [[x y z]]
              ;; a low block (bed, slab) under or in a stand cell makes it no stand: the body stands half a block high, inside the roof cell
              (some (fn [dy] (when (and (lib-reach/standable-cell? p {:x x :y (+ y dy) :z z})
                                        (not-any? #(u/low-floor? (u/seen-block p {:x x :y (+ y dy %) :z z})) [-1 0]))
                               [x (+ y dy) z]))
                    [0 1 -1 -2]))]
    (keep fit cands)))

(defn ground-stands
  "The stand cells of cands ([x y z], from stand-cells) put on the ground: each takes the nearest feet height (y, then one
  up, one down, two down) where a body can stand (jobs.lib.reach/standable-cell?); one with none is dropped. When none
  of them can (unloaded land) the candidates stay as they are."
  [p cands]
  (let [found (grounded p cands)]
    (if (seq found) found cands)))

(defn on-plan
  "The stand cells one above cell-pos's height standing on a plan cell already placed, as a player builds a floor
  from its edge inwards: a candidate that falls into a plan cell still empty is no stand."
  [p cell-pos facing planned bad]
  (->> (remove bad (stand-cells cell-pos (inc (second cell-pos)) facing planned))
       (grounded p)
       (remove #(or (planned %) (planned (update % 1 inc))))))

(defn grounded-stands
  "The stand cells to place or dig cell-pos from that can be stood on, at the first of stand-levels that has any, else
  on plan cells already placed; nil when none (a cell above the ground's reach, or unloaded land)."
  [p cell-pos body-y facing planned bad]
  (let [at (fn [y] (remove bad (stand-cells cell-pos y facing planned)))]
    (or (some #(seq (remove bad (grounded p (at %)))) (stand-levels body-y (second cell-pos)))
        (seq (on-plan p cell-pos facing planned bad)))))

(defn stands-for
  "The stand cells to place or dig cell-pos from: those standable (grounded-stands), else the cell-height candidates as
  they are (unloaded land); never a planned cell or one in bad."
  [p cell-pos body-y facing planned bad]
  (or (grounded-stands p cell-pos body-y facing planned bad)
      (remove bad (ground-stands p (remove bad (stand-cells cell-pos (second cell-pos) facing planned))))))

(defn shortage
  "{item n} of the missing cells' items beyond what is carried (carried {item n})."
  [cells carried]
  (into (sorted-map)
        (keep (fn [[item need]] (let [n (- need (get carried item 0))] (when (pos? n) [item n]))))
        (frequencies (keep :item cells))))

(defn shortage-text [short]
  (str/join ", " (map (fn [[item n]] (str item " " n)) short)))

(defn left-text
  "What a build left, in words: refused and given-up cells, the material short."
  [{:keys [refused given-up short]}]
  (str/join "; " (concat (when (seq refused)
                           [(str "refused " (str/join ", " (map #(str (pr-str (:pos %)) " " (name (:reason %))) refused)))])
                         (when (seq given-up)
                           [(str "gave up " (str/join ", " (map (fn [[pos why]] (str (pr-str pos) " " (name why))) given-up)))])
                         (when (seq short)
                           [(str "short of " (shortage-text short))]))))

(def tall-plants #{"tall_grass" "large_fern" "sunflower" "lilac" "rose_bush" "peony" "tall_seagrass" "pitcher_plant"})

(defn companion?
  "Whether want is the other half of a two-block item: a door's or tall plant's upper half, a bed's head. The lower or
  foot cell places both; the companion is never a target of its own."
  [want]
  (let [b (when (map? want) (:block want))
        is (fn [k v] (= v (some-> (get want k) name)))]
    (boolean (and b (or (and (is :half "upper") (or (str/ends-with? b "_door") (tall-plants b)))
                        (and (is :part "head") (str/ends-with? b "_bed")))))))

(defn plan-trouble
  "Why a plan answer with these cells cannot be built, or nil."
  [answer cells]
  (cond
    (nil? answer) "no such plan"
    (:broken answer) (str "the plan cannot be read: " (:broken answer))
    (and (empty? cells) (seq (:errors answer))) (str "the plan cannot be built: " (str/join "; " (map #(if (map? %) (:error %) %) (:errors answer))))
    (empty? cells) "no cells to build"))

;; ------------------------------------------------------------------ access

(defn hazard-reason
  "The reason a hazard of placing is accepted by: lava beside is :lava-adjacent, kept apart from water."
  [{:keys [reason fluid]}]
  (if (and (= :fluid-adjacent reason) (= "lava" fluid)) :lava-adjacent reason))

(defn hazards
  "The fluid hazards of placing at cell: one per fluid neighbour, as the rules name them for a dig."
  [block-at cell]
  (mapv (fn [[n at]] (hazard-reason {:reason :fluid-adjacent :fluid n :at at}))
        (rules/fluid-neighbours block-at cell)))

(defn decide
  "What to do with the cell at pos under rules input in (access/rules-input) and the hazards accepted: :place, :skip
  (not loaded, own body or not replaceable: the place primitive and the plan judge deal with it) or
  [:refuse {:reason :zone|:claim|:footprint|:hazard ...}]."
  [in accept pos]
  (let [v (rules/may-place? (assoc in :cell pos))
        bad (remove (set accept) (hazards (:block-at in) pos))]
    (cond
      (#{:zone :claim :footprint} (:reason v)) [:refuse (select-keys v [:reason :zone :plan :claim])]
      (not (:ok v)) :skip
      (seq bad) [:refuse {:reason :hazard :hazards (vec (distinct bad))}]
      :else :place)))

(defn rules-input [c] (access/rules-input c {:except (:plan (:args c))}))

(defn count-fail
  "m with one more failure on pos; given up as reason at the give-up-th."
  [m pos reason give-up]
  (let [n (inc (get-in m [:fails pos] 0))]
    (if (>= n give-up)
      (-> m (update :fails dissoc pos) (assoc-in [:given-up pos] reason))
      (assoc-in m [:fails pos] n))))

(defn refuse
  "m with pos refused for good, as {:reason ...why}."
  [m pos why]
  (-> m (update :fails dissoc pos) (assoc-in [:refused pos] why)))

(defn permitted
  "The cells of todo the access rules allow now; a refused one is booked on the way."
  [c todo]
  (let [in (rules-input c)
        accept (:accept (:args c))
        decided (map (juxt identity #(decide in accept (:pos %))) todo)]
    (doseq [[cell d] decided :when (vector? d)]
      (ctx/update-mem! c refuse (:pos cell) (second d)))
    (into [] (keep (fn [[cell d]] (when (= :place d) cell))) decided)))

;; ------------------------------------------------------------------ reading the world

(defn world-block
  "The block at [x y z] in plan.shape's shape: nil when unloaded or never seen, else {:name n} with :state when it has properties."
  [p pos]
  (when-let [b (u/seen-block p (zipmap [:x :y :z] pos))]
    (cond-> {:name (.-name b)}
      (.-properties b) (assoc :state (js->clj (.-properties b) :keywordize-keys true)))))

(defn allot
  "cells with their :item. carried {name n}: an :any want takes the first choice still left after the cells before it
  (each cell to place takes one), so a choice that runs out passes to the next, and the first choice when none is
  left; carried as a set of names: the first choice carried."
  [carried cells]
  (if-not (map? carried)
    (mapv #(assoc % :item (item-for (:want %) carried)) cells)
    (:cells
     (reduce (fn [{:keys [left] :as acc} cell]
               (let [item (item-for (:want cell) left)
                     take? (and item (#{:missing :wrong} (:answer cell)))]
                 (-> acc
                     (update :cells conj (assoc cell :item item))
                     (cond-> take? (update-in [:left item] #(max 0 (dec (or % 0))))))))
             {:cells [] :left carried}
             cells))))

(defn top-fn
  "(fn [x z y]) the y of the seen ground top of column x z around y, nil when unknown."
  [c]
  (surface/top-fn (partial u/seen-name (:primitives c))))

(defn surfaced
  "answer with its cells put on the seen ground when the plan asks for it (:surface true)."
  [c answer]
  (if-not (and answer (:surface (:plan answer)))
    answer
    (update answer :cells surface/follow (top-fn c))))

(defn judged
  "The plan's cells (of part) judged against the world, each with the :item to place it with. A generic wood want
  (jobs.lib.build.wood) is wanted in species sp first, any other counts where it stands."
  [p answer part carried sp]
  (->> (:cells answer)
       (filter #(or (nil? part) (= part (:part %))))
       (mapv #(cond-> % sp (update :want wood/resolve-want sp)))
       (#(shape/plan-minus-world % (partial world-block p)))
       (allot carried)))

(defn wood-choice
  "The wood species of the plan's generic wants: nil when it has none, else the booked one (step books it), else a new
  choice (carried most, else the nearest log seen, else oak)."
  [c answer carried-map]
  (when (wood/generic-wants? (map :want (:cells answer)))
    (or (:wood (ctx/mem c))
        (wood/choose carried-map (some-> (trees/scan (:primitives c) trees/default-radius trees/log-name? 1) first :name)))))

(def opening-pattern #"(_door|_fence_gate|_trapdoor)$")

(defn loosened
  "Cells (as judged) with a replaceable block read as the placement sees it: a wrong cell holding one is :missing
  (placed into directly), a :clear cell holding one is :match unless it is a fluid (a clear cell is meant dry)."
  [cells]
  (mapv (fn [{:keys [answer found] :as cell}]
          (cond
            (not (rules/replaceable found)) cell
            (= :wrong answer) (assoc cell :answer :missing)
            (and (= :extra answer) (not (rules/fluids found))) (assoc cell :answer :match)
            :else cell))
        cells))

(defn prepared
  "Cells (as judged) ready for the rounds: loosened; a companion half is marked :companion; a wrong block of another kind that the body carries the item for,
  that is no container, door or gate, no rail of the wanted kind (its shape settles as its neighbours land), not already misplaced by this job, is marked :dig? (dug, then placed). So is a block that is
  no replaceable one in a :clear cell (:extra), whatever is carried."
  [cells carried misplaced]
  (mapv (fn [{:keys [answer found want item pos] :as cell}]
          (let [wrong? (= :wrong answer)
                extra? (= :extra answer)
                settling? (and (rail/rail-name? found) (= found (shape/want-block want)))
                dug? #(and (not (contains? misplaced pos))
                           (not settling?)
                           (not (kept/kept? [] found))
                           (not (re-find opening-pattern found)))
                digs? (and (or extra? (and wrong? item (contains? carried item))) (dug?))
                short? (and wrong? item (not (contains? carried item)) (dug?))]
            (cond-> cell
              short? (assoc :short? true)
              (companion? want) (assoc :companion true)
              digs? (assoc :dig? true))))
        (loosened cells)))

(defn sturdy-ground?
  "Whether the plan of the call says (:sturdy-ground true) that a sturdy block on the ground of its rail line is no wrong block."
  [c]
  (boolean (:sturdy-ground (:plan (known/plan c (:plan (:args c)))))))

(defn kept-ground
  "The predicate of cells that stay unlisted as wrong: with the plan's :sturdy-ground, ground of a rail line holding a sturdy block."
  [c cells sturdy-ground]
  (let [ground (when sturdy-ground (rail/ground cells))]
    (fn [{:keys [pos]}] (and (contains? ground pos) (rail/sturdy? (world-block (:primitives c) pos))))))

(def no-zones "no zone list has been read")

(defn planned
  "{:cells judged} for the plan in the args, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        answer (surfaced c (known/plan c plan))
        usable? (and answer (not (:broken answer)))
        carried-map (when usable? (storage/carried-totals (u/inventory (:primitives c))))
        wood (when usable? (wood-choice c answer carried-map))
        cells (when usable?
                (let [carried (set (keys carried-map))]
                  (let [cells (prepared (judged (:primitives c) answer part carried-map wood) carried (:misplaced (ctx/mem c) {}))
                        kept? (kept-ground c cells (sturdy-ground? c))]
                    (mapv #(if (and (:short? %) (kept? %)) (dissoc % :short?) %) cells))))
        trouble (or (plan-trouble answer cells)
                    (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) no-zones))]
    (if-not trouble
      {:cells cells :wood wood}
      (do (ctx/warn-once! c [plan trouble] :build.declined
                          {:plan plan :part part :reason trouble
                           :text (str "build declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble :later? (and (not= no-zones trouble) (known/unreadable? answer))}))))

(def neighbour-steps [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(defn supported?
  "Whether a block stands beside, above or below pos that a click can be made on."
  [block-at pos]
  (boolean (some #(placement/clickable? (block-at (mapv + pos %))) neighbour-steps)))

(defn how
  "How the cell is placed from where the body stands (jobs.lib.placement/click). A plain place of a cell with nothing
  to click beside it is refused :no-support (it waits for a neighbour; the primitive would answer no-support)."
  [c {:keys [pos want item]}]
  (let [block-at (partial world-block (:primitives c))
        h (placement/click (block-want want item) pos (placement/eye (u/self-pos c)) block-at)]
    (if (or (:click h) (:refused h) (supported? block-at pos))
      h
      (assoc h :refused :no-support))))

(defn unseen-support
  "The neighbours of pos (below first) the body has never seen: a cell refused :no-support may only lack a look at its floor."
  [c pos]
  (filterv #(nil? (u/seen-block (:primitives c) (zipmap [:x :y :z] %))) (map #(mapv + pos %) neighbour-steps)))

(defn placeable
  "The cells of todo a click places now, each with its :click (nil: placed plainly); the others are booked under
  :unplaceable with the reason, which only says why at the end."
  [c todo]
  (let [decided (map (juxt identity #(how c %)) todo)]
    (ctx/update-mem! c assoc :unplaceable (into {} (keep (fn [[cell d]] (when (:refused d) [(:pos cell) (:refused d)]))) decided))
    (into [] (keep (fn [[cell d]] (when-not (:refused d) (assoc cell :click (:click d))))) decided)))

(defn misplaced
  "The text of what was placed (the want's state keys only) when block does not hold want, else nil. A rail's shape
  and power settle as its neighbours land, so the place result is judged by the rail's name alone."
  [want item block]
  (let [wanted (block-want want item)
        want (if (rail/rail-name? (shape/want-block wanted)) (shape/want-block wanted) want)]
    (when (and block (= :wrong (shape/judge want block)))
      (let [ks (when (map? wanted) (keys (dissoc wanted :block)))]
        (shape/want-text (into {:block (:name block)} (select-keys (:state block) ks)))))))

(defn missing
  "The cells to place: empty (or holding a replaceable block), with an item; never a companion half."
  [cells]
  (filterv #(and (= :missing (:answer %)) (:item %) (not (:companion %))) cells))

(defn digging
  "The cells holding a wrong block to dig before they are placed."
  [cells]
  (filterv :dig? cells))

(defn unseen
  "The cells with an item, or to clear, that nobody can see now (unloaded), not given up."
  [cells given-up]
  (filterv #(and (nil? (:found %)) (or (:item %) (= :clear (:want %))) (not (:companion %)) (not (contains? given-up (:pos %)))) cells))

(defn short-wrong
  "The cells holding a wrong block that cannot be dug yet: the body carries none of the item."
  [cells]
  (filterv :short? cells))

(defn owed
  "The cells still to build: missing, wrong to dig, wrong and short of the item, or unseen."
  [cells]
  (-> (missing cells) (into (digging cells)) (into (short-wrong cells)) (into (unseen cells {}))))

(defn buildable
  "The missing cells this body can work on: item carried, not given up, the cell below not itself owed."
  [cells carried given-up]
  (let [owed (set (map :pos (into (missing cells) (digging cells))))]
    (filterv #(and (pos? (get carried (:item %) 0))
                   (not (contains? given-up (:pos %)))
                   (not (owed (update (:pos %) 1 dec))))
             (missing cells))))

;; ------------------------------------------------------------------ check

(def fetch-job 'jobs.build.from-plan)

(defn fetch-key
  "What a failed fetch is booked under: the item, or [:tool block]."
  [w]
  (if (= :no-tool (:reason w)) [:tool (:block w)] (:item w)))

(defn due
  "w (a :need or :no-tool wait) when the :fetch arg allows its kind and no fetch for it failed in this call, else nil."
  [c w]
  (let [o (need/opts c fetch-job)]
    (when (and w o (contains? (:what o) (if (= :no-tool (:reason w)) :tool :item))
               (not (contains? (:fetch-failed (ctx/mem c)) (fetch-key w))))
      w)))

(defn tool-of
  "The :no-tool wait for the first open dig whose block cannot be harvested with what is carried, else nil."
  [c cells]
  (let [mem (ctx/mem c)
        closed (merge (:given-up mem) (:refused mem))]
    (some #(when (and (not (contains? closed (:pos %)))
                      (tools/needs-tool-to-clear? (:primitives c) (:found %)))
             {:reason :no-tool :block (:found %)})
          (digging cells))))

(defn need-of
  "The first :need wait (items not carried enough) or :no-tool wait (a pickaxe block to dig without a tool) that a
  fetch is due for, else nil."
  [c cells]
  (let [mem (ctx/mem c)
        closed (merge (:given-up mem) (:refused mem))
        open (remove #(contains? closed (:pos %)) (into (missing cells) (short-wrong cells)))
        waits (map (fn [[item n]] {:reason :need :item item :count n}) (shortage open (storage/carried-totals (u/inventory (:primitives c)))))
        tool (tool-of c cells)]
    (some #(due c %) (cond-> (vec waits) tool (conj tool)))))

(defn placeable-now?
  "Whether a cell can be placed from what is carried: the fetch for the rest waits until these are in."
  [c]
  (let [mem (ctx/mem c)]
    (boolean (seq (buildable (:cells (planned c)) (storage/carried-totals (u/inventory (:primitives c))) (merge (:given-up mem) (:refused mem)))))))

(defn problem
  "The fetch due for the plan as it stands now, nil when none or the plan is in trouble. While a cell can be placed from
  what is carried, only a tool is fetched: the items for the rest wait until those are in."
  [c]
  (let [{:keys [cells trouble]} (planned c)]
    (when-not trouble (if (placeable-now? c) (due c (tool-of c cells)) (need-of c cells)))))
