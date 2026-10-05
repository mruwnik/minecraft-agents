(ns jobs.build.from-plan
  (:require [clojure.string :as str]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.tidy :as tidy]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [engine.jobs.watch :as watch]
            [jobs.build.clear-box :as clear-box]
            [engine.path.near :as near]
            [engine.placement :as placement]
            [plan.rail :as rail]
            [plan.shape :as shape]))

(def doc
  "Build what a plan of the body's world wants (:plan, optionally only its :part) from what the body carries: every
  planned cell that stands empty (plan.shape judges it :missing) gets its block placed, and so does one that holds a
  replaceable block (leaf_litter, fern, vine, water ...; engine.access.rules/replaceable), which a place takes over.
  A cell holding any other wrong block (dirt, leaves, stone ...) is dug first and then placed, when its item is
  carried and the rules allow the dig (own or unzoned cells, another plan's footprint and foreign zones refuse it as
  for a place; with :ignore-zones? the cell is dug and the trespass noted as tidy does) with the best carried tool,
  by hand for what needs none (everything but pickaxe blocks, leaves excepted: a pickaxe block without a pickaxe is given up
  as :no-tool); a block it could not dig, or that is not allowed, is listed under :wrong, and one that came out of a
  place in the wrong state is listed and never dug or placed again. A door's upper half and a bed's head (and a tall
  plant's upper half) are never targets: the lower or foot cell places the whole item, and a wrong block in the other
  half is dug first. :clear cells, crops and trees are not this job's. Each round re-reads the plan and the world and
  takes the first step that applies: (0) dig the wrong blocks in reach (see above), after the places; (1) place every buildable cell within :reach of the eye, lowest first (a cell
  is buildable when its block is carried, the cell below it is not itself still owed, it is not the body's own
  feet or head cell, and engine.placement finds a click that gives its state: the neighbour, face, cursor, look and
  sneak go with the place, from wherever the body stands; a :facing want that engine.placement places plainly is
  placed only while the body looks the way it should face, standing on the far side); (2) else walk to a stand
  cell two blocks beside the nearest buildable cell (for such a plain :facing want, on the side it faces away from); (3) else walk toward the nearest cell nobody can see (unloaded), which is never taken as built; (4) else
  finish. A cell whose state no neighbour gives now waits; still missing at the end it is given up with engine.placement's
  reason (:no-support, :no-room, :opened, :double-slab). A placed block whose reported state is not the want is listed under :wrong
  with :placed true and the state it came out in; it is never dug or placed again. A cell whose place is refused (anything but placed, occupied or
  no-item) or whose stand cell cannot be walked to :give-up times is given up. It finishes with a result {:placed n
  :missing [[x y z] ...] :short {item n} :given-up {[x y z] :refused|:unreachable|:unloaded|reason} :wrong [{:pos :found :want}]
  :refused [...]}
  and the events build.done (info), build.short (warn: the items still lacking), build.gave-up (warn) and
  build.wrong (warn). The event texts name the count and the first few cells; the whole list is in :cells of the event and in the result. The check declines, with one build.declined warn naming the plan and the reason, while the
  plan is missing, unreadable or has no cells to build (in :part), and, before the job has begun,
  while cells are missing but none of their blocks is carried, and while no zone list has been read. Every place goes
  through engine.access.rules/may-place? with the zones and the footprints of the OTHER active plans, when the cell is
  chosen and again right before the place; a cell in a zone that does not allow :place, or in another active plan's
  footprint, is refused for good (not retried, not counted as given up) and listed in the result's :refused
  [{:pos :reason :zone|:plan}] with one build.refused warn per reason. Water beside a cell is no obstacle by default;
  :accept names the fluid hazards taken (:fluid-adjacent water beside, :lava-adjacent lava beside), a cell with an
  untaken one is refused as :hazard (with :hazards). With :sturdy-ground a sturdy block on the ground of a rail
  line (plan.rail/ground: under every rail, the buffers and the torches) is right where the plan wants fill, and is
  not listed :wrong. The stand cell is taken at the body's feet height:
  it assumes flat ground.")

(def args
  {:plan {:doc "id of a plan of the body's world" :default nil}
   :part {:doc "only the cells of this part" :default nil}
   :reach {:doc "cells whose centre is this close to the eye are placed without walking, in blocks" :default 4.2}
   :give-up {:doc "refused places or failed walks after which a cell is given up" :default 3}
   :accept {:doc "fluid hazards of a cell taken: :fluid-adjacent (water beside; placing beside or into water seals and bridges), :lava-adjacent (lava beside; not taken by default: the body stands beside the cell)" :default [:fluid-adjacent]}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :sturdy-ground {:doc "a sturdy block on the ground of a rail line (plan.rail/ground) is no wrong block, whatever fill the plan wants there" :default false}})

(def eye-height 1.62)

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

(defn eye [body] {:x (:x body) :y (+ (:y body) eye-height) :z (:z body)})

(defn eye-dist [body [x y z]]
  (u/dist {:x (:x body) :y (+ (:y body) eye-height) :z (:z body)} {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}))

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

(defn shortage
  "{item n} of the missing cells' items beyond what is carried (carried {item n})."
  [cells carried]
  (into (sorted-map)
        (keep (fn [[item need]] (let [n (- need (get carried item 0))] (when (pos? n) [item n]))))
        (frequencies (keep :item cells))))

(defn shortage-text [short]
  (str/join ", " (map (fn [[item n]] (str item " " n)) short)))

(def tall-plants #{"tall_grass" "large_fern" "sunflower" "lilac" "rose_bush" "peony" "tall_seagrass" "pitcher_plant"})

(defn companion?
  "Whether want is the other half of a two-block item: a door's or tall plant's upper half, a bed's head. The lower or
  foot cell places both; the companion is never a target of its own."
  [want]
  (let [b (when (map? want) (:block want))
        is (fn [k v] (= v (some-> (get want k) name)))]
    (boolean (and b (or (and (is :half "upper") (or (str/ends-with? b "_door") (tall-plants b)))
                        (and (is :part "head") (str/ends-with? b "_bed")))))))

(def soft-by-hand #"_leaves$|_wool$|_carpet$|glass")

(defn by-hand?
  "Whether the block is dug without a tool that suits it (everything but pickaxe blocks, and soft ones)."
  [block-name]
  (or (not= "pickaxe" (tools/tool-kind block-name)) (some? (re-find soft-by-hand block-name))))

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
  (when-let [b (.blockAt p (clj->js (zipmap [:x :y :z] pos)))]
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
  that is no container, not already misplaced by this job, is marked :dig? (dug, then placed)."
  [cells carried misplaced]
  (mapv (fn [{:keys [answer found want item pos] :as cell}]
          (let [wrong? (= :wrong answer)
                loose? (and wrong? (rules/replaceable found))
                digs? (and wrong? (not loose?) item (contains? carried item) (not (contains? misplaced pos))
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
        answer (ctx/plan c plan)
        cells (when (and answer (not (:broken answer)))
                (let [carried (set (keys (carried-counts (:primitives c))))]
                  (prepared (judged (:primitives c) answer part carried) carried (:misplaced (ctx/mem c) {}))))
        trouble (or (plan-trouble answer cells)
                    (when (and (nil? (ctx/zones c)) (not (:ignore-zones? (:args c)))) "no zone list has been read"))]
    (if-not trouble
      {:cells cells}
      (do (ctx/warn-once! c [plan trouble] :build.declined
                          {:plan plan :part part :reason trouble
                           :text (str "build declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

(defn how
  "How the cell is placed from where the body stands (engine.placement/click)."
  [c {:keys [pos want item]}]
  (placement/click (block-want want item) pos (eye (u/self-pos c)) (partial world-block (:primitives c))))

(defn placeable
  "The cells of todo a click places now, each with its :click (nil: placed plainly); the others are booked under
  :unplaceable with the reason, which only says why at the end."
  [c todo]
  (let [decided (map (juxt identity #(how c %)) todo)]
    (ctx/update-mem! c assoc :unplaceable (into {} (keep (fn [[cell d]] (when (:refused d) [(:pos cell) (:refused d)]))) decided))
    (into [] (keep (fn [[cell d]] (when-not (:refused d) (assoc cell :click (:click d))))) decided)))

(defn js-click [{:keys [against cursor] :as click}]
  (merge (select-keys click [:yaw :pitch :sneak])
         {:against (zipmap [:x :y :z] against) :cursor (zipmap [:x :y :z] cursor)}))

(defn placed-block
  "The block a place result reports, in plan.shape's shape, or nil."
  [r]
  (when-let [b (.-placed r)]
    {:name (.-name b) :state (js->clj (.-properties b) :keywordize-keys true)}))

(defn misplaced
  "The text of what was placed (the want's state keys only) when block does not hold want, else nil."
  [want item block]
  (when (and block (= :wrong (shape/judge want block)))
    (let [wanted (block-want want item)
          ks (when (map? wanted) (keys (dissoc wanted :block)))]
      (shape/want-text (into {:block (:name block)} (select-keys (:state block) ks))))))

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

(defn check [c]
  (let [{:keys [cells trouble]} (planned c)
        p (:primitives c)]
    (boolean
     (and (not trouble)
          (or (:begun (ctx/mem c))
              (empty? (owed cells))
              (seq (buildable cells (carried-counts p) {}))
              (seq (digging cells))
              (some #(pos? (get (carried-counts p) (:item %) 0)) (unseen cells {}))
              (do (ctx/warn-once! c [(:plan (:args c)) :no-items] :build.declined
                                  (let [reason (str "nothing carried to build with: "
                                                    (shortage-text (shortage (owed cells) (carried-counts p))))]
                                    {:plan (:plan (:args c)) :part (:part (:args c)) :reason reason
                                     :text (str "build declines plan " (:plan (:args c)) ": " reason)}))
                  false))))))

;; ------------------------------------------------------------------ steps

(defn count-fail
  "m with one more failure on pos; given up as reason at the give-up-th."
  [m pos reason give-up]
  (let [n (inc (get-in m [:fails pos] 0))]
    (if (>= n give-up)
      (-> m (update :fails dissoc pos) (assoc-in [:given-up pos] reason))
      (assoc-in m [:fails pos] n))))

(defn ^:async place-one!
  "Place the cell's item after asking the access rules and engine.placement once more; a refusal is booked, nothing
  is placed."
  [c {:keys [pos item want] :as cell}]
  (let [d (decide (rules-input c) (:accept (:args c)) pos)
        h (how c cell)]
    (cond
      (vector? d) (ctx/update-mem! c refuse pos (second d))
      (not= :place d) nil
      (:refused h) (ctx/update-mem! c assoc-in [:unplaceable pos] (:refused h))
      :else
      (let [r (await (ctx/act c :place (clj->js (cond-> {:pos (zipmap [:x :y :z] pos) :item item}
                                                  (:click h) (assoc :click (js-click (:click h)))))))
            wrong (misplaced want item (placed-block r))]
        (case (.-status r)
          "placed" (ctx/update-mem! c #(cond-> (update % :placed (fnil inc 0))
                                         wrong (assoc-in [:misplaced pos] wrong)))
          ("occupied" "no-item") nil
          (ctx/update-mem! c count-fail pos :refused (:give-up (:args c))))))))

(defn pos-map [pos] (zipmap [:x :y :z] pos))

(defn ^:async dig-one!
  "Dig the wrong block of the cell with the best carried tool after asking the access rules once more; a refusal is
  booked, a failed dig counts a failure, a dug cell counts a dig (given up as :refilled when it keeps coming back)."
  [c {:keys [pos]}]
  (let [p (:primitives c)
        v (access/may-dig? (rules-input c) (pos-map pos))
        accept (set (:accept (:args c)))
        judged (access/judge v accept)
        n (u/block-name p (pos-map pos))]
    (case judged
      :ok (let [tool (tools/best-tool (map :name (u/inventory p)) n)]
            (when (and tool (not= tool (.-held (.self p))))
              (await (ctx/act c :equip #js {:item tool :dest "hand"})))
            (let [status (.-status (await (tidy/dig! c (pos-map pos))))]
              (case status
                "dug" (ctx/update-mem! c #(let [m (update-in % [:digs pos] (fnil inc 0))]
                                            (if (>= (get-in m [:digs pos]) (:give-up (:args c)))
                                              (assoc-in m [:given-up pos] :refilled)
                                              m)))
                "missing" nil
                (ctx/update-mem! c count-fail pos :refused (:give-up (:args c))))))
      :refused (ctx/update-mem! c refuse pos (select-keys v [:reason :zone :plan :claim]))
      :hazard (ctx/update-mem! c refuse pos {:reason :hazard
                                             :hazards (vec (distinct (remove accept (map :reason (:hazards v)))))})
      nil)))

(defn diggable
  "The cells of digs not closed (given up or refused); a pickaxe block without a pickaxe is given up here as :no-tool."
  [c digs closed]
  (let [names (map :name (u/inventory (:primitives c)))
        open (remove #(contains? closed (:pos %)) digs)
        {no-tool true ok false} (group-by #(boolean (and (not (by-hand? (:found %)))
                                                         (nil? (tools/best-tool names (:found %)))))
                                          open)]
    (when (seq no-tool)
      (ctx/update-mem! c update :given-up merge (into {} (map (fn [cell] [(:pos cell) :no-tool])) no-tool)))
    (vec ok)))

(defn in-reach
  "The buildable cells the body can place from where it stands, lowest first, then nearest."
  [c todo]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> todo
         (filter #(and (<= (eye-dist body (:pos %)) (:reach (:args c)))
                       (not (mine (:pos %)))
                       (or (:click %) (facing-ok? (facing-of (:want %)) (:pos %) body))))
         (sort-by (juxt #(get (:pos %) 1) #(eye-dist body (:pos %)))))))

(defn in-dig-reach
  "The cells to dig the body can reach from where it stands, lowest first, then nearest."
  [c cells]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> cells
         (filter #(and (<= (eye-dist body (:pos %)) (:reach (:args c))) (not (mine (:pos %)))))
         (sort-by (juxt #(get (:pos %) 1) #(eye-dist body (:pos %)))))))

(defn ^:async walk-to!
  "Walk to a stand cell beside cell; a cell that cannot be walked to, or is still out of reach (or unseen, as
  :unloaded) on arrival, counts a failure."
  [c cells cell]
  (let [body (u/self-pos c)
        planned (set (map :pos cells))
        bad (set (:bad-stands (ctx/mem c)))
        facing (when-not (or (:click cell) (:dig? cell)) (facing-of (:want cell)))
        stands (remove bad (stand-cells (:pos cell) (js/Math.floor (:y body)) facing planned))
        stand (first (sort-by #(u/dist body (zipmap [:x :y :z] %)) stands))
        give-up (:give-up (:args c))]
    (if-not stand
      (ctx/update-mem! c count-fail (:pos cell) :unreachable give-up)
      (let [w (await (near/walk-near! c (zipmap [:x :y :z] stand) 0))]
        (when (= :blocked w)
          (ctx/update-mem! c #(-> (count-fail % (:pos cell) :unreachable give-up)
                                  (update :bad-stands (fnil conj []) stand))))
        (when (and (= :there w) (nil? (:found cell)))
          (ctx/update-mem! c count-fail (:pos cell) :unloaded give-up))
        (when (and (= :there w) (:found cell) (empty? (if (:dig? cell) (in-dig-reach c [cell]) (in-reach c [cell]))))
          (ctx/update-mem! c count-fail (:pos cell) :unreachable give-up))))
    :continue))

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

(defn finish! [c cells]
  (let [result (summary c cells (ctx/mem c) (:sturdy-ground (:args c)))]
    (announce! c result)
    (ctx/result! c result)
    :done))

(defn ^:async round [c]
  (let [{:keys [cells trouble]} (planned c)]
    (if trouble
      :declined
      (do (when-not (:begun (ctx/mem c)) (ctx/update-mem! c assoc :begun true))
          (let [closed #(merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
                todo (placeable c (permitted c (buildable cells (carried-counts (:primitives c)) (closed))))
                digs (diggable c (digging cells) (closed))
                given-up (closed)
                near (in-reach c todo)
                dig-near (in-dig-reach c digs)
                nearest #(first (sort-by (fn [cell] (u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos cell)))) %))]
            (cond
              (seq near) (do (await (watch/watch! c {}))
                             (loop [left near]
                               (when (seq left)
                                 (await (place-one! c (first left)))
                                 (recur (rest left))))
                             :continue)
              (seq dig-near) (do (loop [left dig-near]
                                   (when (seq left)
                                     (await (dig-one! c (first left)))
                                     (recur (rest left))))
                                 :continue)
              (seq todo) (await (walk-to! c cells (nearest todo)))
              (seq digs) (await (walk-to! c cells (nearest digs)))
              (seq (unseen cells given-up)) (await (walk-to! c cells (nearest (unseen cells given-up))))
              :else (finish! c cells)))))))
