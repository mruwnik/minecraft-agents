(ns jobs.build.from-plan
  (:require [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.build.clear-box :as clear-box]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.placement :as placement]
            [jobs.lib.reach :as reach]
            [plan.rail :as rail]
            [plan.shape :as shape]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.world :as known]))

(def doc
  "Build what a plan wants (:plan, optionally only its :part) from what the body carries.
  A planned cell that stands empty gets its block placed. So does a cell holding a replaceable block (leaf_litter,
  fern, vine, water ...), which a place takes over.
  A cell holding any other wrong block (dirt, leaves, stone ...) is dug first and then placed, when its item is
  carried and the rules allow the dig. The best carried tool is used, or the hand for blocks that need none. A
  pickaxe block without a pickaxe is fetched a tool for, else given up as :no-tool. A block that could not be dug, or may not be, is listed
  under :wrong. So is a block that came out of a place in the wrong state (:placed true); it is never dug or placed
  again.
  A door's upper half, a bed's head and a tall plant's upper half are never targets: the lower or foot cell places
  the whole item. A :clear cell holding a block that is not replaceable and no container is dug (nothing carried
  needed). Crops and trees are not this job's.
  One call works until the plan is built or nothing more applies (:continue only while a walk or fetch waits). Every
  step re-reads the plan and the world, then takes the first step that applies:
  0. A place that would shut the body in (it has a way out and would have none) is held back; the body first walks
     out of the plan's footprint (go-to child), then places from outside. Held back :give-up times, a cell is given
     up as :unreachable.
  1. Place every buildable cell within :reach of the eye, lowest first. A cell is buildable when its block is
     carried, the cell below it is not itself still owed, and it is not the body's own feet or head cell.
     jobs.lib.placement picks the click (neighbour, face, cursor, look, sneak) that gives the wanted state. A
     :facing want that is placed plainly waits until the body looks the right way, from the far side.
  2. Dig the wrong blocks within reach.
  3. Walk to a stand cell one to three blocks beside the nearest buildable (or diggable) cell, on the ground there
     (the body's feet height, or one up or down on a slope).
  4. Walk toward the nearest unloaded cell. Unseen cells are never taken as built.
  5. Finish.
  A cell whose state no neighbour gives, or with no block to click beside it yet, waits for one. If still missing at
  the end it is given up with jobs.lib.placement's reason (:no-support, :no-room, :opened, :double-slab). A cell whose place is refused, or whose stand cell
  cannot be reached, :give-up times is given up (:refused, :occupied, :no-item, :unreachable or :unloaded).
  Zones: every place is checked against zones and the footprints of the other active plans, when the cell is
  chosen and again before the place. A cell in a zone that does not allow :place, or in another plan's footprint,
  is refused for good (not counted as given up). It is listed in :refused [{:pos :reason :zone|:claim|:footprint|:hazard}], with one
  build.refused warn per reason.
  Water beside a cell is no obstacle by default. :accept names the fluid hazards taken (:fluid-adjacent,
  :lava-adjacent). A cell with an untaken one is refused as :hazard.
  :sturdy-ground true accepts a sturdy block on the ground of a rail line (plan.rail/ground) where the plan wants
  fill; it is not listed :wrong.
  Result: {:placed n :missing [[x y z] ...] :short {item n} :given-up {[x y z] reason} :wrong [{:pos :found
  :want}] :refused [...]}. A build that leaves cells missing ends :stopped (:reason :incomplete, :text naming the
  missing count and what it left); only a complete one has no :status. For a block placed in the wrong state, :found names the state it came out in.
  Events: build.done (info), build.short, build.gave-up, build.refused and build.wrong (warns). Event texts name
  the count and the first few cells. The whole list is in :cells of the event and in the result.
  The job declines (one build.declined warn naming the plan and the reason):
  - while the plan is missing, unreadable or has no cells to build (in :part).
  - while no zone list has been read.
  - before the job has begun, while cells are missing but none of their blocks is carried and none can be fetched.
  Material: while cells wait for an item not carried (and not given up or refused), each round first fetches it (:fetch,
  jobs.lib.fetch, a jobs.items.obtain child for the missing count); a pickaxe block to dig without a pickaxe fetches the tool the same way
  (jobs.items.get-tool). With :fetch false, or once a fetch failed, the build goes on with what is carried and ends
  :short as before, a pickaxe block given up :no-tool.")

(def args
  {:plan {:doc "id of a plan of the body's world" :default nil}
   :part {:doc "only the cells of this part" :default nil}
   :reach {:doc "cells whose centre is this close to the eye are placed without walking, in blocks" :default u/eye-reach}
   :give-up {:doc "refused places or failed walks after which a cell is given up" :default 3}
   :accept {:doc "fluid hazards of a cell taken: :fluid-adjacent (water beside; placing beside or into water seals and bridges), :lava-adjacent (lava beside; not taken by default: the body stands beside the cell)" :default [:fluid-adjacent]}
   :fetch {:doc "get the blocks and tools the plan lacks (jobs.lib.fetch): true, a set of kinds or a map of limits; false builds with what is carried" :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :sturdy-ground {:doc "a sturdy block on the ground of a rail line (plan.rail/ground) is no wrong block, whatever fill the plan wants there" :default false}})


;; ------------------------------------------------------------------ pure helpers

(defn item-for
  "The item to place for want: the block it names (a wall torch with a torch), for an :any the first choice carried
  (else the first); nil for :clear, crops and trees."
  [want carried]
  (cond
    (string? want) (placement/item-of want)
    (vector? want) (let [names (map #(placement/item-of (if (string? %) % (:block %))) (rest want))]
                     (or (first (filter carried names)) (first names)))
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

(defn ground-stands
  "The stand cells of cands ([x y z], from stand-cells) put on the ground: each takes the nearest feet height (y, then one
  up, one down, two down) where a body can stand (jobs.lib.reach/standable-cell?); one with none is dropped. When none
  of them can (unloaded land) the candidates stay as they are."
  [p cands]
  (let [fit (fn [[x y z]]
              (some (fn [dy] (when (reach/standable-cell? p {:x x :y (+ y dy) :z z}) [x (+ y dy) z])) [0 1 -1 -2]))
        found (keep fit cands)]
    (if (seq found) found cands)))

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
  "The block at [x y z] in plan.shape's shape: nil when unloaded, else {:name n} with :state when it has properties."
  [p pos]
  (when-let [b (u/block-at p (zipmap [:x :y :z] pos))]
    (cond-> {:name (.-name b)}
      (.-properties b) (assoc :state (js->clj (.-properties b) :keywordize-keys true)))))

(defn carried-counts [p]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory p)))

(defn judged
  "The plan's cells (of part) judged against the world, each with the :item to place it with."
  [p answer part carried]
  (->> (:cells answer)
       (filter #(or (nil? part) (= part (:part %))))
       (#(shape/plan-minus-world % (partial world-block p)))
       (mapv #(assoc % :item (item-for (:want %) carried)))))

(defn prepared
  "Cells (as judged) ready for the rounds: a wrong cell holding a replaceable block is :missing (placed into
  directly); a companion half is marked :companion; a wrong block of another kind that the body carries the item for,
  that is no container, not already misplaced by this job, is marked :dig? (dug, then placed). So is a block that is
  no replaceable one in a :clear cell (:extra), whatever is carried."
  [cells carried misplaced]
  (mapv (fn [{:keys [answer found want item pos] :as cell}]
          (let [wrong? (= :wrong answer)
                loose? (and wrong? (rules/replaceable found))
                extra? (and (= :extra answer) (not (rules/replaceable found)))
                digs? (and (or extra? (and wrong? (not loose?) item (contains? carried item)))
                           (not (contains? misplaced pos))
                           (not (clear-box/kept? [] found)))]
            (cond-> cell
              (companion? want) (assoc :companion true)
              loose? (assoc :answer :missing)
              digs? (assoc :dig? true))))
        cells))

(defn planned
  "{:cells judged} for the plan in the args, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        answer (known/plan c plan)
        cells (when (and answer (not (:broken answer)))
                (let [carried (set (keys (carried-counts (:primitives c))))]
                  (prepared (judged (:primitives c) answer part carried) carried (:misplaced (ctx/mem c) {}))))
        trouble (or (plan-trouble answer cells)
                    (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) "no zone list has been read"))]
    (if-not trouble
      {:cells cells}
      (do (ctx/warn-once! c [plan trouble] :build.declined
                          {:plan plan :part part :reason trouble
                           :text (str "build declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

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

(defn placeable
  "The cells of todo a click places now, each with its :click (nil: placed plainly); the others are booked under
  :unplaceable with the reason, which only says why at the end."
  [c todo]
  (let [decided (map (juxt identity #(how c %)) todo)]
    (ctx/update-mem! c assoc :unplaceable (into {} (keep (fn [[cell d]] (when (:refused d) [(:pos cell) (:refused d)]))) decided))
    (into [] (keep (fn [[cell d]] (when-not (:refused d) (assoc cell :click (:click d))))) decided)))

(defn placed-block
  "The block a place result reports, in plan.shape's shape, or nil."
  [r]
  (when-let [b (.-placed r)]
    {:name (.-name b) :state (js->clj (.-properties b) :keywordize-keys true)}))

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
  "The cells with an item that nobody can see now (unloaded), not given up."
  [cells given-up]
  (filterv #(and (nil? (:found %)) (:item %) (not (:companion %)) (not (contains? given-up (:pos %)))) cells))

(defn owed
  "The cells still to build: missing, wrong to dig, or unseen."
  [cells]
  (-> (missing cells) (into (digging cells)) (into (unseen cells {}))))

(defn buildable
  "The missing cells this body can work on: item carried, not given up, the cell below not itself owed."
  [cells carried given-up]
  (let [owed (set (map :pos (into (missing cells) (digging cells))))]
    (filterv #(and (pos? (get carried (:item %) 0))
                   (not (contains? given-up (:pos %)))
                   (not (owed (update (:pos %) 1 dec))))
             (missing cells))))

;; ------------------------------------------------------------------ check

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
  "The wait of the open cells: the :need for the first item of the missing ones not carried enough, else the
  :no-tool for a pickaxe block to dig without a tool, else nil."
  [c cells]
  (let [mem (ctx/mem c)
        closed (merge (:given-up mem) (:refused mem))
        open (remove #(contains? closed (:pos %)) (missing cells))
        [item n] (first (shortage open (carried-counts (:primitives c))))]
    (if item
      {:reason :need :item item :count n}
      (tool-of c cells))))

(defn problem
  "need-of for the plan as it stands now, nil when the plan is in trouble."
  [c]
  (let [{:keys [cells trouble]} (planned c)]
    (when-not trouble (need-of c cells))))

(defn check [c]
  (let [{:keys [cells trouble]} (planned c)
        p (:primitives c)]
    (cond
      trouble (ctx/wait c {:reason :plan-trouble :why trouble})
      (or (:begun (ctx/mem c))
          (empty? (owed cells))
          (seq (buildable cells (carried-counts p) {}))
          (seq (digging cells))
          (some #(pos? (get (carried-counts p) (:item %) 0)) (unseen cells {}))
          (when-let [w (need-of c cells)] (some? (fetch/due c 'jobs.build.from-plan w)))) true
      :else
      (do (ctx/warn-once! c [(:plan (:args c)) :no-items] :build.declined
                          (let [reason (str "nothing carried to build with: "
                                            (shortage-text (shortage (owed cells) (carried-counts p))))]
                            {:plan (:plan (:args c)) :part (:part (:args c)) :reason reason
                             :text (str "build declines plan " (:plan (:args c)) ": " reason)}))
          (ctx/wait c {:reason :no-items})))))

;; ------------------------------------------------------------------ steps

(defn count-fail
  "m with one more failure on pos; given up as reason at the give-up-th."
  [m pos reason give-up]
  (let [n (inc (get-in m [:fails pos] 0))]
    (if (>= n give-up)
      (-> m (update :fails dissoc pos) (assoc-in [:given-up pos] reason))
      (assoc-in m [:fails pos] n))))

(defn ^:async place-one!
  "Place the cell's item after asking the access rules and jobs.lib.placement once more; a refusal is booked, nothing
  is placed."
  [c {:keys [pos item want] :as cell}]
  (let [d (decide (rules-input c) (:accept (:args c)) pos)
        h (how c cell)]
    (cond
      (vector? d) (ctx/update-mem! c refuse pos (second d))
      (not= :place d) nil
      (:refused h) (ctx/update-mem! c assoc-in [:unplaceable pos] (:refused h))
      :else
      (let [_ (ctx/update-mem! c assoc :placing pos)
            r (await (ctx/act c :place (clj->js (cond-> {:pos (zipmap [:x :y :z] pos) :item item}
                                                  (:click h) (assoc :click (placement/js-click (:click h)))))))
            wrong (misplaced want item (placed-block r))
            give-up (:give-up (:args c))]
        (case (.-status r)
          "placed" (ctx/update-mem! c #(cond-> (update (dissoc % :placing) :placed (fnil inc 0))
                                         wrong (assoc-in [:misplaced pos] wrong)))
          ("occupied" "no-item") (ctx/update-mem! c #(count-fail (dissoc % :placing) pos (keyword (.-status r)) give-up))
          (ctx/update-mem! c #(count-fail (dissoc % :placing) pos :refused give-up)))))))

(defn settle-placing!
  "A place that a cut left unbooked (:placing, set before the act): the world is the answer, so a cell the plan now
  judges :match was placed and counts. Another body placing the same block there in the gap would count too."
  [c cells]
  (when-let [pos (:placing (ctx/mem c))]
    (let [placed? (boolean (some #(and (= pos (:pos %)) (= :match (:answer %))) cells))]
      (ctx/update-mem! c #(cond-> (dissoc % :placing) placed? (update :placed (fnil inc 0)))))))

(defn pos-map [pos] (zipmap [:x :y :z] pos))

(defn ^:async dig-one!
  "Dig the wrong block of the cell (a blocks.dig child) after asking the access rules once more; a refusal is
  booked, a failed dig counts a failure, a dug cell counts a dig (given up as :refilled when it keeps coming back)."
  [c {:keys [pos]}]
  (let [v (access/may-dig? (rules-input c) (pos-map pos))
        accept (set (:accept (:args c)))
        judged (access/judge v accept)]
    (case judged
      :ok (let [outcome (await (blocks/dig-cell! c (pos-map pos)
                                                 {:accept #{:fluid-adjacent :falling-block :under-feet}
                                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                                  :for-plan (:plan (:args c))}))]
            (case outcome
              :continue :continue
              :dug (ctx/update-mem! c #(let [m (update-in % [:digs pos] (fnil inc 0))]
                                         (if (>= (get-in m [:digs pos]) (:give-up (:args c)))
                                           (assoc-in m [:given-up pos] :refilled)
                                           m)))
              :missing (ctx/update-mem! c count-fail pos :missing (:give-up (:args c)))
              (ctx/update-mem! c count-fail pos :refused (:give-up (:args c)))))
      :refused (ctx/update-mem! c refuse pos (select-keys v [:reason :zone :plan :claim]))
      :hazard (ctx/update-mem! c refuse pos {:reason :hazard
                                             :hazards (vec (distinct (remove accept (map :reason (:hazards v)))))})
      nil)))

(defn diggable
  "The cells of digs not closed (given up or refused); a block no carried tool harvests is given up here as :no-tool."
  [c digs closed]
  (let [open (remove #(contains? closed (:pos %)) digs)
        {no-tool true ok false} (group-by #(tools/needs-tool-to-clear? (:primitives c) (:found %)) open)]
    (when (seq no-tool)
      (ctx/update-mem! c update :given-up merge (into {} (map (fn [cell] [(:pos cell) :no-tool])) no-tool)))
    (vec ok)))

(defn in-reach
  "The buildable cells the body can place from where it stands, lowest first, then nearest."
  [c todo]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> todo
         (filter #(and (<= (u/eye-dist body (:pos %)) (:reach (:args c)))
                       (not (mine (:pos %)))
                       (or (:click %) (facing-ok? (facing-of (:want %)) (:pos %) body))))
         (sort-by (juxt #(get (:pos %) 1) #(u/eye-dist body (:pos %)))))))

(defn in-dig-reach
  "The cells to dig the body can reach from where it stands, lowest first, then nearest."
  [c cells]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> cells
         (filter #(and (<= (u/eye-dist body (:pos %)) (:reach (:args c))) (not (mine (:pos %)))))
         (sort-by (juxt #(get (:pos %) 1) #(u/eye-dist body (:pos %)))))))

;; ------------------------------------------------------------------ keeping a way out

(defn seals?
  "Whether placing at cell would shut the body in: it has a way out now (jobs.lib.reach/enclosed?) and would have none
  with the cell solid. Only a cell at the body's own height band (feet to a block above the head) can; the flood is
  bounded (reach/room-cells)."
  [c cell]
  (let [p (:primitives c)
        by (js/Math.floor (:y (u/self-pos c)))
        cy (second cell)]
    (boolean (and (<= by cy (+ by 2))
                  (not (reach/enclosed? p))
                  (reach/enclosed? p #{(vec cell)})))))

(defn exit-point
  "A cell two blocks outside the box of the planned cells, on the side nearest the body, at its feet height."
  [cells body]
  (let [xs (map #(nth (:pos %) 0) cells)
        zs (map #(nth (:pos %) 2) cells)
        y (js/Math.floor (:y body))
        [x0 x1 z0 z1] [(apply min xs) (apply max xs) (apply min zs) (apply max zs)]
        bx (js/Math.floor (:x body))
        bz (js/Math.floor (:z body))
        clamp (fn [v lo hi] (max lo (min hi v)))]
    (->> [[(- x0 2) y (clamp bz z0 z1)] [(+ x1 2) y (clamp bz z0 z1)]
          [(clamp bx x0 x1) y (- z0 2)] [(clamp bx x0 x1) y (+ z1 2)]]
         (sort-by (fn [[x _ z]] (+ (js/Math.abs (- x bx)) (js/Math.abs (- z bz)))))
         first)))

(defn ^:async leave!
  "One go-to call (child :leave) out of the plan's footprint to the :leave cell; dropped once the walk ends (arrived
  or not: the next step judges where the body stands). :continue while the walk waits, else :again."
  [c]
  (let [[x y z] (:leave (ctx/mem c))
        r (await (ctx/call-child c :leave 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 1 :escalate false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (when-not (= :continue r)
      (ctx/update-mem! c dissoc :leave))
    (if (= :continue r) :continue :again)))

(defn ^:async walk-to!
  "Walk to a stand cell beside cell (:continue while the walk waits, else :again); a cell that cannot be walked to, or is still out of reach (or unseen, as
  :unloaded) on arrival, counts a failure."
  [c cells cell]
  (let [body (u/self-pos c)
        planned (set (map :pos cells))
        bad (set (:bad-stands (ctx/mem c)))
        facing (when-not (or (:click cell) (:dig? cell)) (facing-of (:want cell)))
        stands (remove bad (ground-stands (:primitives c) (stand-cells (:pos cell) (js/Math.floor (:y body)) facing planned)))
        stand (first (sort-by #(u/dist body (zipmap [:x :y :z] %)) stands))
        give-up (:give-up (:args c))]
    (if-not stand
      (do (ctx/update-mem! c count-fail (:pos cell) :unreachable give-up)
          :again)
      (let [w (await (near/go-near! c (zipmap [:x :y :z] stand) 0 {:zone-tolls true :escalate false}))]
        (when (= :blocked w)
          (ctx/update-mem! c #(-> (count-fail % (:pos cell) :unreachable give-up)
                                  (update :bad-stands (fnil conj []) stand))))
        (when (and (= :there w) (nil? (:found cell)))
          (ctx/update-mem! c count-fail (:pos cell) :unloaded give-up))
        (when (and (= :there w) (:found cell) (empty? (if (:dig? cell) (in-dig-reach c [cell]) (in-reach c [cell]))))
          (ctx/update-mem! c count-fail (:pos cell) :unreachable give-up))
        (if (= :partial w) :continue :again)))))

(defn kept-ground
  "The predicate of cells that stay unlisted as wrong: with sturdy-ground, ground of a rail line holding a sturdy block."
  [c cells sturdy-ground]
  (let [ground (when sturdy-ground (rail/ground cells))]
    (fn [{:keys [pos]}] (and (contains? ground pos) (rail/sturdy? (world-block (:primitives c) pos))))))

(defn summary
  "What a build of cells left, from the job memory m: {:placed :missing :short :given-up :wrong :refused}."
  [c cells m sturdy-ground]
  (let [kept? (kept-ground c cells sturdy-ground)
        left (owed cells)
        given-up (merge (select-keys (:unplaceable m) (map :pos left)) (:given-up m {}))
        wrong (mapv (fn [{:keys [pos found want]}]
                      (if-let [placed (get-in m [:misplaced pos])]
                        {:pos pos :found placed :want (shape/want-text want) :placed true}
                        {:pos pos :found found :want (shape/want-text want)}))
                    (filter #(and (#{:wrong :extra} (:answer %)) (not (kept? %))) cells))
        refused (->> (:refused m) (sort-by key) (mapv (fn [[pos why]] (assoc why :pos pos))))]
    {:placed (:placed m 0) :missing (mapv :pos left) :short (shortage left (carried-counts (:primitives c)))
     :given-up given-up :wrong wrong :refused refused}))

(def listed 8)

(defn cells-text
  "The count of items and the first few, as text for an event; the whole list goes in the event's data. Never clipped
  with an ellipsis: it says how many more there are."
  [items show]
  (let [shown (take listed items)
        more (- (count items) (count shown))]
    (str (str/join ", " (map show shown)) (when (pos? more) (str ", and " more " more (all in :cells)")))))

(defn announce!
  "Emit the build events of a summary: build.short, build.gave-up, build.refused and build.wrong warns, build.done."
  [c {:keys [short given-up refused wrong missing] :as result}]
  (let [plan (:plan (:args c))
        left missing]
    (when (seq short)
      (ctx/emit! c :build.short :warn {:plan plan :short short
                                       :text (str "build of " plan " is short of " (shortage-text short))}))
    (when (seq given-up)
      (ctx/emit! c :build.gave-up :warn {:plan plan :cells given-up
                                         :text (str "build of " plan " gave up " (count given-up) " cells: "
                                                    (cells-text (sort-by key given-up) (fn [[pos why]] (str (pr-str pos) " " (name why)))))}))
    (doseq [[reason group] (group-by :reason refused)]
      (ctx/emit! c :build.refused :warn {:plan plan :reason reason :cells group
                                         :text (str "build of " plan " refused " (count group) " cells (" (name reason) "): "
                                                    (cells-text group #(str (pr-str (:pos %)) " " (or (:zone %) (:plan %) (str/join "," (map name (:hazards %)))))))}))
    (when (seq wrong)
      (ctx/emit! c :build.wrong :warn {:plan plan :cells wrong
                                       :text (str "build of " plan " left " (count wrong) " wrong blocks: "
                                                  (cells-text wrong #(str (pr-str (:pos %)) " " (:found %))))}))
    (ctx/emit! c :build.done :info {:plan plan :placed (:placed result) :missing (count left)
                                    :text (str "build of " plan " done: placed " (:placed result) ", still missing " (count left))})))

(defn stopped-text
  "Why a build that left cells ended stopped: the plan, how many were placed and are missing, and what it left."
  [plan {:keys [placed missing] :as result}]
  (let [left (left-text result)]
    (str "build of " plan " stopped: placed " placed ", still missing " (count missing) (when (seq left) (str " (" left ")")))))

(defn finish! [c cells]
  (let [result (summary c cells (ctx/mem c) (:sturdy-ground (:args c)))]
    (announce! c result)
    (ctx/result! c (if (seq (:missing result))
                     (assoc result :status :stopped :reason :incomplete :text (stopped-text (:plan (:args c)) result))
                     result))
    :done))

(defn ^:async build-step!
  "One step of the build (:again, or :continue while a walk waits): place or dig what is in reach, else walk, else finish."
  [c cells]
  (let [closed #(merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        todo (placeable c (permitted c (buildable cells (carried-counts (:primitives c)) (closed))))
        digs (diggable c (digging cells) (closed))
        given-up (closed)
        near (in-reach c todo)
        dig-near (in-dig-reach c digs)
        nearest #(first (sort-by (fn [cell] (u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos cell)))) %))]
    (cond
      (:leave (ctx/mem c)) (await (leave! c))
      (seq near) (do (await (watch/watch! c {}))
                     (loop [left near]
                       (when (seq left)
                         (if (seals? c (:pos (first left)))
                           (ctx/update-mem! c #(-> (count-fail % (:pos (first left)) :unreachable (:give-up (:args c)))
                                                   (assoc :leave (exit-point cells (u/self-pos c)))))
                           (do (await (place-one! c (first left)))
                               (recur (rest left))))))
                     :again)
      (seq dig-near) (loop [left dig-near]
                       (if (seq left)
                         (if (= :continue (await (dig-one! c (first left))))
                           :continue
                           (recur (rest left)))
                         :again))
      (seq todo) (await (walk-to! c cells (nearest todo)))
      (seq digs) (await (walk-to! c cells (nearest digs)))
      (seq (unseen cells given-up)) (await (walk-to! c cells (nearest (unseen cells given-up))))
      :else (finish! c cells))))

(defn ^:async step [c]
  (let [{:keys [trouble]} (planned c)]
    (if trouble
      :declined
      (do (when-not (:begun (ctx/mem c)) (ctx/update-mem! c assoc :begun true))
          (or (await (fetch/fetch! c 'jobs.build.from-plan problem))
              (let [cells (:cells (planned c))]
                (settle-placing! c cells)
                (await (build-step! c cells))))))))

(defn ^:async round
  "The whole attempt: loop the steps until the plan is built or nothing more applies (finish); :continue only while a
  walk or a fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
