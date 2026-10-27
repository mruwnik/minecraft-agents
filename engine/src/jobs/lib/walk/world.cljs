(ns jobs.lib.walk.world
  "The walk driver's view of the world: the primitives' pathWorld and the decorations the walks plan over (walls, dangers, dark,
  avoided and tolled cells), the executor policy of the body and the cell it stands in."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.path.executor :as executor]
            [engine.path.blocks :as blocks]
            [engine.path.planner.base :as base]
            [engine.path.planner-tuned :as planner]
            [engine.path.space :as space]
            [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.look :as look]
            [jobs.lib.placement :as placement]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.walk.edges :as edges]
            [jobs.lib.walk.rock :as rock]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.way-on :as way-on]
            [engine.perception.store :as store]
            [jobs.lib.memory-age :as memory-age]))

(a/defargs settings
  {::default-food {:default 20 :doc "The food a walk counts on when it has neither a :food arg nor a body food: fed (nothing caps the damage)." :spec (a/int-in 0 nil)}
   ::min-health {:default false :doc "The health every walk of this body keeps (a floor of 1 to 20), false: the code's default floor. A job's own :min-health wins." :spec (a/or-of false? (a/int-in 1 20))}
   ::max-damage {:default false :doc "The most hp every walk of this body may spend (0 or more), false: no cap but the budget. A job's own :max-damage wins." :spec (a/or-of false? (a/num-in 0 nil))}
   ::dangers {:default true :doc "Whether walks plan to keep away from known dangers; false plans straight past them (a walk up to the hostile being fought)." :spec boolean?}
   ::zone-tolls {:default true :doc "Whether walks also toll the cells of other bodies' zones near each walk (jobs.lib.toll-cells/zone-walk-tolls), so a way round a neighbour's field is taken; none under the call's :ignore-zones?." :spec boolean?}
   ::dark {:default true :doc "Whether walks cost a dark cell (seen dark, or unseen and guessed dark) twice a lit one; false plans dark cells like lit ones." :spec boolean?}
   ::y-margin {:default false :doc "Blocks a walk's search may reach above the higher and below the lower of body and goal (a route dipping deeper, such as through deep caves, is not planned); false: 96, and a buried body's dive floor applies." :spec (a/or-of false? (a/int-in 0 nil))}
   ::rock-beyond {:default 3 :doc "Under ground (no sky light at the feet; head not under water), the blocks beyond every seen open cell, the feet and the goal past which a walk over what the body has seen (go-to :unknown) reads a cell it never saw as rock, not air (jobs.lib.walk.rock); false: every unseen cell reads air." :spec (a/or-of false? (a/int-in 1 15))}
   ::unknown-max-nodes {:default 12000 :doc "The most nodes a walk's search over what the body has seen only (go-to :unknown) holds before it ends :budget: a goal no seen route reaches floods unseen cells otherwise to the planner's limit (200000)." :spec (a/int-in 1000 nil)}
   ::dive-pause-s {:default 120 :doc "Seconds after a breathe reflex fired in which walks near the spot (::dive-pause-near) plan no dive (head under water) except to air: the walk that was cut would plan the same dive again." :spec (a/num-in 0 nil)}
   ::dive {:default true :doc "Whether walks may plan a dive (head under water); false plans none except to air, as after a breathe reflex (a walk to a block to dig has no business under a lake)." :spec boolean?}
   ::dive-pause-near {:default 30 :doc "Blocks from where a breathe reflex fired within which walks plan no dive for ::dive-pause-s." :spec (a/num-in 0 nil)}
   ::gait {:default :auto :doc "How go-to walks when its :gait is nil: :auto, :walk or :sneak." :spec cost/gaits}})

(defn unknown-spec
  "go-to's :unknown as the mask spec of one go-to call (the planner's options.unknown less what path-world adds): the prices
  (jobs.lib.cost/walk-unknown-prices) and one missed Set, shared by every pathWorld made with this spec (the floors the
  walk looked for and could not see), and one blocked Set of cells it looked at and could not see (they read as stone).
  With ctx c, deathHp: the hp a death of its body here weighs (jobs.lib.cost/lethal-hp of a certain one)."
  ([]
   (let [{:keys [peek-s aim-s see-lit-depth light-depth]} (cost/walk-unknown-prices)]
     #js {:peekS peek-s :aimS aim-s :seeLitDepth see-lit-depth :lightDepth light-depth :missed (js/Set.)
          :noLight (js/Set.) :blocked (js/Set.)}))
  ([c]
   (let [p (:primitives c)]
     (doto (unknown-spec)
       (unchecked-set "deathHp" (cost/lethal-hp (cost/death-body p (ctx/view c)) 1 (u/self-pos c)))))))

(def torch-base*
  (delay (let [^js t (blocks/default-state-table)
               a (js/Uint8Array. (.-length (.-top t)))]
           (.forEach (.-nameIds t) (fn [^js r name]
                                     (when (placement/torch-base? {:name name})
                                       (doseq [id (range (aget r 0) (inc (aget r 1)))] (aset a id 1)))))
           a)))

(defn torch-base
  "The planner's options.unknown.torchBase: 1 by state id for a block a floor torch stands on (jobs.lib.placement/torch-base?)."
  []
  @torch-base*)

(defn with-light
  "The mask spec unknown (unknown-spec) with torches on: the planner's lightCost (seconds, cost/light-cost or the caller's),
  torchBase and lightOk (fn [x y z] true where a torch may go); its missed and noLight Sets stay shared."
  [^js unknown light-cost light-ok]
  (js/Object.assign #js {} unknown #js {:lightCost light-cost :torchBase (torch-base) :lightOk light-ok}))

(defn memory-sections
  "(fn [cx sy cz]) the ids of the body's memory of that section (sy the world's section y; ids + 1, 0 never seen; however
  old: the planner prices an aging cell by options.unknown.changed), or nil; nil without a perception. The section's own
  array: copy it to keep it."
  [p]
  (when-let [per (aget p "perception")]
    (let [^js st (:st per)
          ^js sections (store/store-of st (.-dim st))]
      (fn [cx sy cz]
        (some-> ^js (.get sections (store/section-key cx sy cz)) .-ids)))))

(defn- memory-epoch-fn
  "(fn []) memory-age/epoch of the body's memory, which moves whenever a cell is seen or changed or a remembered cell's
  change chance moves a level (a masked view kept for a while re-reads memory only then)."
  [p]
  (when-let [per (aget p "perception")]
    (let [^js st (:st per)] (fn [] (memory-age/epoch st)))))

(defonce ^:private counted (js/WeakMap.))

(defn way-scan
  "way-on/scan from the body's feet ({:k :n :ways}: the seen stands nearest the body of primitives p, how many lead out, its
  first unseen ways), nil without a perception. Kept while the body stands in one cell and no block it remembers changed
  or moved a change-chance level (memory-age/epoch): every costed world of a flight step asks it."
  [p]
  (when-let [known (memory-sections p)]
    (let [^js pos (.-pos (.self p))
          ^js st (:st (aget p "perception"))
          start (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])
          k [(.-dim st) (memory-age/epoch st) start (settings/get way-on/settings ::way-on/sample)]
          hit (.get counted st)]
      (if (= k (:k hit))
        (:v hit)
        (let [v (way-on/scan known start)] (.set counted st {:k k :v v}) v)))))

(defn way-counts
  "{:k :n} of way-scan, {:k 0 :n 0} without a perception."
  [p]
  (if-let [v (way-scan p)] (select-keys v [:k :n]) {:k 0 :n 0}))

(declare kit-of)

(defn unseen-share
  "Seconds a second of the body's walk over cells it has not seen adds here (cost/unseen-share of way-counts and its
  kit): the planner's options.unknown.cell."
  [p]
  (cost/unseen-share (way-counts p) (kit-of {:primitives p})))

(defn- see-depth-now
  "The deepest drop the body sees the landing floor of in the dark from a peek, with what it holds now."
  [p]
  (wsight/see-depth (or (look/near-of p) 0)))

(def ^:private unloaded-id 0xFFFF)

(defn rock-beyond "The setting ::rock-beyond, nil when off." [] (or (settings/get settings ::rock-beyond) nil))

(defonce ^:private shares (js/WeakMap.))

(defn solid-share
  "The chance that a block sight meets round the feet is solid: base-rates/solid-share at the feet's height of the blocks
  the body remembers, aged ones too, in the feet's section and the 26 round it (the window the prior is fitted to). Sight
  meets every open block along a ray and the solid one that ends it, as the prior counts open blocks and the walls beside
  them. Nil without a perception (no memory, no guessed rock). Kept while the body stands in one section and no block it
  remembers changed or aged."
  [p]
  (when-let [known (memory-sections p)]
    (let [{:keys [x y z]} (sh-sense/feet p)
          ^js st (:st (aget p "perception"))
          [cx sy cz :as at] (mapv #(bit-shift-right % 4) [x y z])
          k [(.-dim st) (memory-age/epoch st) at]
          hit (.get shares st)]
      (if (= k (:k hit))
        (:v hit)
        (let [^js open (rock/open-ids)
              [solid seen] (reduce (fn [[solid seen :as acc] ^js ids]
                                     (if-not ids
                                       acc
                                       (loop [i 0 solid solid seen seen]
                                         (if (== i 4096)
                                           [solid seen]
                                           (let [v (aget ids i)]
                                             (cond (zero? v) (recur (inc i) solid seen)
                                                   (== 1 (aget open (dec v))) (recur (inc i) solid (inc seen))
                                                   :else (recur (inc i) (inc solid) (inc seen))))))))
                                   [0 0]
                                   (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]] (known (+ cx dx) (+ sy dy) (+ cz dz))))
              v (base-rates/solid-share y solid (- seen solid))]
          (.set shares st {:k k :v v})
          v)))))

(defn rock-likely?
  "Whether a cell k + 1 blocks past a seen open one, the nearest the rock guess reads as stone, is more likely solid than
  open when each block sight meets there is solid at chance share (solid-share): k + 1 open blocks in a row are under even."
  [share k]
  (< (js/Math.pow (- 1 share) (inc k)) 0.5))

(defn rock-guess?
  "Whether a walk over mask spec unknown (nil: none) reads far unseen cells as rock (path-world): ::rock-beyond is on, a
  cell past it is more likely solid than open (rock-likely? of solid-share), the body's head is out of water, and the mask
  is neither :edge nor without-rock. Light counts only through what sight made out (a dark cell beyond perception's
  :near is not met): it says how dangerous a place is, not what an unseen cell holds."
  [p ^js unknown]
  (boolean (and unknown (not (.-edge unknown)) (not (.-noRock unknown)) (not (breath/head-under? p))
                (when-let [k (rock-beyond)] (some-> (solid-share p) (rock-likely? k))))))

(defn- rock-seeds
  "The cells path-world's rock guess spares for a walk over mask spec unknown: the feet and the spec's seeds (with-seeds)."
  [p ^js unknown]
  (let [{:keys [x y z]} (sh-sense/feet p)]
    (cond-> (into [[x y z]] (.-own unknown)) (.-goal unknown) (conj (.-goal unknown)))))

(defn unseen-kind
  "How a walk over mask spec unknown (nil: over the world as it is) reads a cell the body has not seen, as a
  jobs.lib.reach/world-kinds unseen policy: :raw with no spec; else :solid where its plans read stone (the rock guess, a
  cell looked at and not seen), else :ground. A flood asking whether that walk can get out reads this."
  [p ^js unknown]
  (if-not unknown
    :raw
    (let [rock (when-let [known (and (rock-guess? p unknown) (memory-sections p))]
                 (rock/rock-at {:k (rock-beyond) :known (memory-sections p) :seeds (rock-seeds p unknown)}))
          ^js blocked (.-blocked unknown)
          blocked (when (and blocked (pos? (.-size blocked))) blocked)]
      (if (or rock blocked)
        (fn [x y z] (if (or (and blocked (.has blocked (str x "," y "," z))) (and rock (rock x y z))) :solid :ground))
        :ground))))

(defn without-rock
  "The mask spec unknown with no rock guess: a walk whose search prices unseen cells itself (jobs.lib.flee-search)."
  [^js unknown]
  (doto unknown (unchecked-set "noRock" true)))

(defn with-seeds
  "The mask spec unknown with the cells its rock guess spares (path-world): goal ([x y z], or nil) and own (feet cells the
  body dug its ways through itself); the spec is per call, the seeds per round."
  [^js unknown goal own]
  (doto unknown (unchecked-set "goal" goal) (unchecked-set "own" own)))

(defn path-world
  "The primitives' pathWorld sensing (a snapshot over the world) with the planner's block table and free-space module, nil when
  they have none or cannot sense now. With unknown (unknown-spec), over a body with a perception: cells the body never saw
  or felt read void_air (its known cells as remembered, however old), and the snapshot carries the planner's options.unknown:
  the spec with the mask's id, changed (memory-age/changed-fn: the chance a remembered cell changed), cell (unseen-share), seeDepth (from what the body holds now) and seeLit (look/see-lit-fn). Where
  the rock guess holds (rock-guess?), an unseen cell past ::rock-beyond of every seen open cell, the
  feet and the spec's seeds (with-seeds) reads stone (jobs.lib.walk.rock), or with the spec's rockEdge the loaded edge (a
  frontier, the snapshot's edge); the mask's rockBeyond says so (0: none)."
  ([p] (path-world p nil))
  ([p unknown]
   (when (fn? (.-pathWorld p))
     (let [table (blocks/default-state-table)
           known (when unknown (memory-sections p))
           under? (when known (breath/head-under? p))
           stone (first (blocks/state-ids table "stone"))
           k (when (and known (rock-guess? p unknown)) (rock-beyond))
           mask (when known
                  (js/Object.assign #js {} unknown
                                    #js {:id (if (.-edge unknown) unloaded-id (first (blocks/state-ids table (if under? "seagrass" "void_air")))) :blockedId stone :known known :memoryEpoch (memory-epoch-fn p)
                                         :changed (memory-age/changed-fn (aget p "perception"))
                                         :cell (unseen-share p) :seeDepth (see-depth-now p) :seeLit (look/see-lit-fn p) :rockBeyond (or k 0)}))]
       (when-let [pw (if mask (.pathWorld p mask) (.pathWorld p))]
         (let [pw (js/Object.assign #js {:table table :space space/space} pw)]
           (when k
             (set! (.-snapshot pw) (rock/with-rock (.-snapshot pw) {:mask (.-id mask) :stone (if (.-rockEdge unknown) unloaded-id stone) :edge (true? (.-rockEdge unknown))
                                                                    :k k :known (memory-sections p) :seeds (rock-seeds p unknown)})))
           pw))))))

(defn unknown-of
  "The mask's options (the planner's options.unknown) pw's snapshot carries, nil when it is not masked."
  [pw]
  (some-> pw .-snapshot .-unknown))

(defn unknown-key
  "What a kept search's key holds of pw's mask: nil unmasked, else the floors missed so far, the see depth, the takeoffs
  where a torch failed, the torch price and the rock guess (and whether it reads as the loaded edge)."
  [pw]
  (when-let [^js u (unknown-of pw)]
    [(.-size ^js (.-missed u)) (.-size ^js (.-blocked u)) (.-seeDepth u) (some-> ^js (.-noLight u) .-size) (.-lightCost u) (.-rockBeyond u) (true? (.-rockEdge u))]))

(defn wall-id
  "A state id of a full block with no collision tricks (stone, the first state that is one), of a state table."
  [table]
  (let [top (.-top table) kind (.-kind table) special (.-special table) openable (.-openable table) hazard (.-hazard table)]
    (first (filter (fn [id] (and (== 16 (aget top id)) (== 1 (aget kind id)) (zero? (aget special id))
                                 (zero? (aget openable id)) (zero? (aget hazard id))))
                   (range 1 (.-length top))))))

(defn wall-cells
  "The cells [x y z] of walls ({:x :y :z} maps), each with the cell over it when the block there now stands taller than
  a block (a fence gate, 1.5): the body can no more jump onto it than through it."
  [snapshot table walls]
  (let [top (.-top table)]
    (into #{} (mapcat (fn [{:keys [x y z]}]
                        (if (> (aget top (.stateAt snapshot x y z)) 16) [[x y z] [x (inc y) z]] [[x y z]])))
          walls)))

(defn with-walls
  "pw (the primitives' pathWorld) with the cells walls ({:x :y :z} maps) read as stone: a cell the walk found it cannot
  pass (wall-cells: the cell over a block taller than a block too)."
  [pw walls]
  (if (empty? walls)
    pw
    (let [snapshot (.-snapshot pw)
          id (wall-id (.-table pw))
          wall? (wall-cells snapshot (.-table pw) walls)
          walled (js/Object.create snapshot)]
      (set! (.-stateAt walled) (fn [x y z] (if (contains? wall? [x y z]) id (.stateAt snapshot x y z))))
      #js {:snapshot walled :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :avoid (.-avoid pw) :dark (.-dark pw) :tolls (.-tolls pw)})))

(defn with-dangers
  "pw with dangers (the planner's options.dangers, a JS array; nil for none) for plan-options to pass on."
  [pw dangers]
  (if (nil? dangers)
    pw
    #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers dangers :avoid (.-avoid pw) :dark (.-dark pw) :tolls (.-tolls pw)}))

(defn with-dark
  "pw whose plans cost dark cells more (the planner's options.dark): dark is look/dark-fn's {:at ..} (nil: pw). A dark
  cell costs cost/dark-factor of kit (kit-of; nil: the floor) times its own seconds more. With a kit, a cell water
  (look/water-fn's class, nil: none) gives costs its class's cost/water-factors too, by day as well: a drowned keeps to the
  water, and a dark one pays the land mobs' dark-factor on top (classes 4 and 5: dark 2 and 3); a phantom floor beside
  seen water its cost/unseen-water-factors (classes from look/lag-class): wet and phantom, those two of kit."
  [pw dark water kit wet phantom]
  (if (nil? dark)
    pw
    (let [at (:at dark)]
      #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :avoid (.-avoid pw) :tolls (.-tolls pw)
           :dark (if (and kit water)
                   (let [factor (cost/dark-factor kit)]
                     #js {:at (fn [x y z] (let [w (water x y z)]
                                            (cond (>= w look/lag-class) w
                                                  (pos? w) (if (== 1 (at x y z)) (+ w 2) w)
                                                  :else (at x y z))))
                          :factor factor :water (clj->js (concat wet (map #(+ factor %) wet) phantom))})
                   #js {:at at :factor (if kit (cost/dark-factor kit) (cost/dark-factor))})})))

(defn kit-of
  "{:weapon :equipment :health :absorption :food} of the body: its best default weapon (jobs.lib.combat), the armour worn
  and what the answers' prices read of it."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    {:weapon (combat/best-weapon p combat/default-weapons)
     :equipment (cost/equipment-of (.-equipment self))
     :health (.-health self) :absorption (.-absorption self) :food (.-food self)}))

(declare with-tolls with-edges water-read-fn phantom-read-fn)

(defn costed-world
  "pw (the primitives' pathWorld) costed the way go-to plans: with the mobs the body knows may hurt it, each over its
  field (dangers?, jobs.lib.threats threat-tolls; planner-dangers keeps plans off sensed mobs' cells and deaths by terrain) and with dark cells (dark?, look/dark-fn; over a
  masked pw an unseen cell is priced by the mask, not as dark) and with the steps the walker failed at (jobs.lib.walk.edges).
  Every search that ranks routes by cost plans over this.
  keep? (a fn of a known threat): only the threats it holds of are tolled (a flight races its chasers instead)."
  [c pw {:keys [dangers? dark? keep?]}]
  (cond-> pw
    dangers? (-> (with-dangers (threats/planner-dangers c))
                 (with-tolls (if keep? (threats/tolls-of c (filter keep? (threats/known-threats c))) (threats/threat-tolls c))))
    dark? (as-> pw' (let [kit (kit-of c)
                          wet (cost/water-factors kit)
                          phantom (cost/unseen-water-factors kit wet)]
                      (with-dark pw' (look/dark-fn (:primitives c) {:unseen-dark? (nil? (unknown-of pw))})
                                 (look/water-fn (:primitives c) {:wet? (water-read-fn pw) :phantom? (phantom-read-fn pw) :lags (count phantom)}) kit wet phantom)))
    true (with-edges (edges/tolls (:primitives c)))))

(def avoid-factor
  "How many times a cell's own cost an avoided cell costs more (the planner's options.avoid.factor): a detour is taken
  over a stretch of several cells, the only way over the cell is still taken."
  50)

(defn with-avoid
  "pw whose plans cost cells ([x y z] each) avoid-factor times more to enter (the planner's options.avoid; none: pw)."
  [pw cells]
  (if (empty? cells)
    pw
    (let [sorted (vec (sort cells))]
      #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :dark (.-dark pw) :tolls (.-tolls pw)
           :avoid #js {:kinds 0 :factor avoid-factor :list sorted
                       :cells (js/Set. (clj->js (mapv (fn [[x y z]] (planner/cell-key x y z)) sorted)))}})))

(defn tolls-problem
  "Why tolls is not a usable go-to :tolls (nil or a sequence of {:x :y :z :factor}, x y z finite, factor not negative and
  not NaN: ##Inf is a cell no plan enters), else nil."
  [tolls]
  (let [finite? #(and (number? %) (js/isFinite %))
        ok? #(and (map? %) (every? (comp finite? %) [:x :y :z]) (number? (:factor %)) (>= (:factor %) 0))]
    (cond
      (nil? tolls) nil
      (not (sequential? tolls)) (str "tolls is a list of {:x :y :z :factor}; got " (pr-str tolls))
      :else (when-let [[bad] (seq (remove ok? tolls))]
              (str "a toll is {:x :y :z :factor}: x y z finite numbers, factor 0 or more (##Inf: never entered); got " (pr-str bad))))))

(defonce toll-merges (js/WeakMap.))

(defn merged-tolls
  "base and tolls ([{:x :y :z :factor}], base nil for none) as one list, a cell tolled twice once at the sum, in cell-key
  order, with the planner's Map of cell-key to factor: #js {:list :cells}."
  [base tolls]
  (let [rows (js/Map.)]
    (doseq [{:keys [x y z factor]} (concat base tolls)]
      (let [k (planner/cell-key x y z)
            ^js r (.get rows k)]
        (if r (aset r 3 (+ (aget r 3) factor)) (.set rows k #js [x y z factor]))))
    (let [ks (.sort (js/Array.from (.keys rows)) (fn [a b] (- a b)))
          cells (js/Map.)]
      (doseq [k ks] (.set cells k (aget (.get rows k) 3)))
      #js {:list (mapv (fn [k] (let [r (.get rows k)] {:x (aget r 0) :y (aget r 1) :z (aget r 2) :factor (aget r 3)})) ks)
           :cells cells})))

(defn with-tolls
  "pw whose plans cost the cells of tolls ([{:x :y :z :factor}], jobs.lib.cost farm-tolls and zone-tolls, jobs.lib.threats threat-tolls) factor times their
  own seconds more, on top of pw's own tolls (the planner's options.tolls; a cell tolled twice costs both; none: pw).
  The same tolls on the same base are merged once (a cached threat tolls vector is one object)."
  [pw tolls]
  (if (empty? tolls)
    pw
    (let [base (some-> (.-tolls pw) .-list)
          kept (.get toll-merges tolls)
          merged (if (and kept (identical? base (:base kept)))
                   (:v kept)
                   (let [v (merged-tolls base tolls)] (.set toll-merges tolls {:base base :v v}) v))]
      #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :dark (.-dark pw) :avoid (.-avoid pw)
           :tolls (if-let [^js t (.-tolls pw)]
                    #js {:list (.-list merged) :cells (.-cells merged) :edges (.-edges t) :edgeRows (.-edgeRows t)}
                    merged)})))

(defn with-edges
  "pw whose plans price the steps the walker failed at (jobs.lib.walk.edges/tolls, [edges rows]; nil: pw), on top of pw's
  tolls."
  [pw [edges rows]]
  (if (nil? edges)
    pw
    (let [^js t (.-tolls pw)]
      #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :dark (.-dark pw) :avoid (.-avoid pw)
           :tolls #js {:list (some-> t .-list) :cells (some-> t .-cells) :edges edges :edgeRows rows}})))

(defn tolls-key
  "What a kept search's key holds of pw's tolls (with-tolls, with-edges): the cells with their factors and the edges with
  their seconds, nil for none."
  [pw]
  (when-let [^js t (.-tolls pw)]
    (if-let [rows (.-edgeRows t)] [(.-list t) rows] (.-list t))))

(defn avoid-key
  "What a kept search's key holds of pw's avoided cells (with-avoid): the cells, nil for none."
  [pw]
  (some-> (.-avoid pw) .-list))

(defn danger-key
  "What a kept search's key holds of pw's dangers: the mob, place (rounded to 4 blocks) and rate (to 0.1 hp/s) of each (a
  mob that moved on, died or came along, or a weapon picked up or health lost, is a new search)."
  [pw]
  (some->> (.-dangers pw) array-seq
           (mapv (fn [^js d] [(.-mob d) (js/Math.round (/ (.-x d) 4)) (js/Math.round (/ (.-y d) 4)) (js/Math.round (/ (.-z d) 4))
                            (/ (js/Math.round (* 10 (.-rate d))) 10)]))))

(defn solid-fn
  "solid? for executor/with-free-sides over a pathWorld."
  [pw]
  (let [snapshot (.-snapshot pw) tops (.-top (.-table pw))]
    (fn [x y z] (pos? (aget tops (.stateAt snapshot x y z))))))

(def bounce-blocks
  "The blocks a fall bounces off (prismarine-physics bounces only slime)."
  ["slime_block"])

(defn bounce-fn
  "bounce? for executor/with-bounces over a pathWorld: the block at x y z is a bouncing one."
  [pw]
  (let [snapshot (.-snapshot pw)
        ids (set (mapcat #(blocks/state-ids (.-table pw) %) bounce-blocks))]
    (fn [x y z] (contains? ids (.stateAt snapshot x y z)))))

(defn wall-fn
  "wall? for executor/with-bounces over a pathWorld: the block at x y z has a whole collision top."
  [pw]
  (let [snapshot (.-snapshot pw) tops (.-top (.-table pw))]
    (fn [x y z] (>= (aget tops (.stateAt snapshot x y z)) base/WHOLE))))

(defn water-read-fn
  "Whether the planner reads x y z of a pathWorld as water (breath/swimmable): the mask's seagrass for an unseen cell under
  water, else the block."
  [pw]
  (let [snapshot (.-snapshot pw)
        ids (set (mapcat #(blocks/state-ids (.-table pw) %) breath/swimmable))]
    (fn [x y z] (contains? ids (.stateAt snapshot x y z)))))

(defn phantom-read-fn
  "Whether the planner reads x y z of a pathWorld as a cell the body has not seen (the mask's id; nil unmasked)."
  [pw]
  (when-let [^js u (unknown-of pw)]
    (let [snapshot (.-snapshot pw) id (.-id u)]
      (fn [x y z] (== id (.stateAt snapshot x y z))))))

(defn default-food [] (settings/get settings ::default-food))

(defn food-of
  "The food level (0-20) the walk counts on: the :food of the job's args (a number), else default-food. go-to fills it with the body's own food (jobs.movement.go-to/step!); the walk does not read the body."
  [c]
  (let [food (:food (:args c))]
    (if (number? food) food (default-food))))

(defn self-body
  "The body of primitives p as {:health :absorption :on-fire :effects} for jobs.lib.cost/survivable-budget."
  [p]
  (let [self (.self p)]
    {:health (.-health self) :absorption (.-absorption self) :on-fire (.-onFire self)
     :effects (map #(.-name %) (array-seq (.-effects self)))}))

(defn damage-body
  "The body's {:health :absorption :food :on-fire :effects} for jobs.lib.cost/damage-budget; the food is the job's (food-of)."
  [c]
  (assoc (self-body (:primitives c)) :food (food-of c)))

(defn dangers? "Whether walks keep away from known dangers (the setting ::dangers)." [] (settings/get settings ::dangers))
(defn zone-tolls? "Whether walks toll other bodies' zones (the setting ::zone-tolls)." [] (settings/get settings ::zone-tolls))
(defn unknown-max-nodes "The setting ::unknown-max-nodes." [] (settings/get settings ::unknown-max-nodes))
(defn dark? "Whether walks cost dark cells (the setting ::dark)." [] (settings/get settings ::dark))
(defn y-margin "The setting ::y-margin, nil when none." [] (or (settings/get settings ::y-margin) nil))

(defn walk-settings
  "The {:min-health :max-damage} the walk keeps: the body's settings (those not false), each overridden by the job's arg (go-to's) when given."
  [c]
  (into (into {} (remove (comp false? val))
              {:min-health (settings/get settings ::min-health) :max-damage (settings/get settings ::max-damage)})
        (remove (comp nil? val)) (select-keys (:args c) [:min-health :max-damage])))

(defn gait
  "The gait the walk keeps: go-to's :gait arg over the body's :gait setting."
  [c]
  (or (:gait (:args c)) (settings/get settings ::gait)))

(defn damage-budget
  "The hp the body may spend walking now (jobs.lib.cost/damage-budget): its health, food and effects, and the walk-settings
  :min-health and :max-damage (the job's args over the body's settings); with the ctx's :over-budget (go-to chose to go over it) the survivable-budget."
  [c]
  ((if (:over-budget c) cost/survivable-budget cost/damage-budget) (damage-body c) (walk-settings c)))

(defn known-fn
  "(fn [x y z]) true when the body of primitives p has seen or felt the block there (now or remembered, not unknown: see
  u/sensed); nil without a perception."
  [p]
  (when (some? (.-sensedAt p))
    (let [memo (js/Map.)]
      (fn [x y z]
        (let [k (str x "," y "," z)]
          (if (.has memo k)
            (.get memo k)
            (let [cell {:x x :y y :z z}
                  ^js b (u/sensed p cell)
                  known (boolean (and b (not (true? (.-unknown b)))))]
              (.set memo k known)
              known)))))))

(defn- drowning
  "{:health :drown-prices}: the hp (health and absorption) drowning to which is lethal, and the price of each hp of it."
  [^js self hp-s]
  (let [pool (+ (.-health self) (or (.-absorption self) 0))]
    (when (and (number? pool) (js/isFinite pool))
      {:health pool :drown-prices (cost/drown-prices pool hp-s)})))

(defn dive-paused?
  "Whether a breathe reflex fired within ::dive-pause-near blocks of the body in the last ::dive-pause-s seconds (the :breathe entries); breathe's own walks set the pause to 0."
  [c]
  (when (:view c)
   (let [pos (.-pos (.self (:primitives c)))
        now (ctx/now c)
        near (settings/get settings ::dive-pause-near)
        within (* 1000 (settings/get settings ::dive-pause-s))]
    (boolean (some (fn [{:keys [t data]}]
                     (let [q (:pos data)]
                       (and (< (- now t) within)
                            (<= (js/Math.hypot (- (.-x pos) (:x q)) (- (.-y pos) (:y q)) (- (.-z pos) (:z q))) near))))
                   (ctx/entries c :breathe))))))

(defn body-policy
  "executor/policy for the body: the gait (:walk, :sneak) never sprints; with the walk's food (food-of) 6 or less the client does not sprint, so :sprint is false (a corner jump past
  a high block is then refused). :damage-budget (hp, damage-budget) and :damage-weight (seconds an hp costs at its health)
  price the damage of a walk, :danger-cap the total hp a second its known dangers cost (jobs.lib.cost/danger-cap, more when the job's :danger-max-rate is) (the job's :hp-seconds arg: the seconds an hp costs at full health, default jobs.lib.cost/hp-seconds);
  :max-drop and :fall-factor follow its fall enchantments and the longest drop it survives (the survivable-budget under the job's :max-damage,
  whatever the budget: a drop the budget refuses is a refusal the planner reports as :damageRefused, jobs.lib.cost/fall-profile).
  :air-drain and :air-grace follow the helmet (jobs.lib.cost/air-profile), :swim-h Depth Strider on the boots (swim-profile); with the head under water, or the breath not yet full, :air-used is the air
  its oxygen says it has used, and under water there is no turtle grace (it may be spent on this dive), only Water Breathing's seconds left; :health is the hp drowning to which
  is lethal, :drown-prices the price of each hp of drowning (jobs.lib.cost/drown-prices), :air-seen (known-fn) which air counts."
  [c]
  (let [self (.self (:primitives c))
        food (food-of c)
        under? (breath/head-under? (:primitives c))]
    (cond-> (merge executor/policy
                   (cost/fall-profile {:damage-budget (cost/survivable-budget (damage-body c) (select-keys (walk-settings c) [:max-damage]))
                                       :equipment (cost/equipment-of (.-equipment self))})
                   (cost/air-profile (.-equipment self) (array-seq (.-effects self)))
                   (cost/swim-profile (.-equipment self))
                   {:damage-budget (damage-budget c)
                    :damage-weight (cost/hp-price (.-health self))
                    :danger-cap (max cost/danger-cap (:max-rate (cost/danger-opts)))}
                   (when-some [landing (cost/walk-landing)] {:landing landing})
                   {:drop-cost (cost/walk-drop-cost)}
                   (when-some [costs (cost/walk-costs)] {:costs costs})
                   (when-some [m (y-margin)] {:y-margin m})
                   (when-some [f (:dive-floor c)] {:y-below (max 0 (- (js/Math.floor (.-y (.-pos self))) f))})
                   (when-some [d (:descent-s c)] {:costs (assoc (cost/walk-costs) :descent d)}))
      (or under? (and (.-inWater self) (number? (.-oxygen self)) (< (.-oxygen self) 20))) (-> (cond-> under? (-> (dissoc :air-grace) (merge (cost/air-profile nil (array-seq (.-effects self)))))) (assoc :air-used (cost/air-used (.-oxygen self)))
                                               (merge (drowning self (cost/walk-hp-seconds)))
                                               (merge (some->> (known-fn (:primitives c)) (hash-map :air-seen))))
      (or (dive-paused? c) (false? (settings/get settings ::dive))) (assoc :air-limit 0)
      (and (number? food) (<= food 6)) (assoc :sprint false)
      (= :walk (gait c)) (assoc :sprint false :gait :walk)
      (= :sneak (gait c)) (assoc :sprint false :gait :sneak))))

(defn body-cell
  "The cell the body stands in, as a step's {:x :y :z}."
  [c]
  (let [pos (.-pos (.self (:primitives c)))]
    {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}))

(defn stand-goal
  "Goal feet cell pos {:x :y :z} as the planner stands at it: the block under it when that block is a short floor (a top
  under a whole block from its bottom: farmland, a path, a bottom slab), for the planner stands in such a block's own
  cell; else pos. pw: a pathWorld (path-world), nil leaves pos."
  [^js pw {:keys [x y z] :as pos}]
  (let [^js t (some-> pw .-table)
        top (when t (fn [y] (aget (.-top t) (.stateAt (.-snapshot pw) x y z))))
        under (when t (top (dec y)))]
    (if (and under (< 0 under base/WHOLE) (zero? (aget (.-base t) (.stateAt (.-snapshot pw) x (dec y) z))) (zero? (top y)))
      (update pos :y dec)
      pos)))

(defn body-name [c](.-username (.self (:primitives c))))

(def landing-memory-ms "How long after the body saw a block it still counts as seen for a landing: a look over an edge outlasts the head turning back." 30000)

(defn landing-answer
  "Whether the body of primitives p counts the block at x y z as seen for a landing (landing-seen); wall? as there."
  [p x y z wall?]
  (let [^js b (u/sensed p {:x x :y y :z z})]
    (boolean (and b (or (true? (.-visible b)) (true? (.-felt b))
                        (and (not (true? (.-unknown b))) (or wall? (<= (or (.-ageMs b) js/Infinity) landing-memory-ms))))))))

(defn landing-seen
  "The planner's options.landingSeen for the body of primitives p: (fn [x y z wall?]) true when the body sees or feels the
  block, or saw it landing-memory-ms ago or less, so a bounce is priced only on a pad it has looked at (an older memory
  or an unknown block is not); nil without a perception. As a wall of the pad (wall?) a block remembered at any age
  counts: the walls of a 1x1 shaft face away from the edge a body drops from. A block never seen or an unloaded one
  never counts. Each answer is kept in memo (a js/Map of [x y z wall?] to bool; default a private one) for landing-reads-hold?."
  ([p] (landing-seen p (js/Map.)))
  ([p ^js memo]
   (when (some? (.-sensedAt p))
     (fn [x y z wall?]
       (let [k #js [x y z (boolean wall?)]
             ks (.join k ",")]
         (if (.has memo ks)
           (.-seen (.get memo ks))
           (let [seen (landing-answer p x y z wall?)]
             (.set memo ks #js {:cell k :seen seen})
             seen)))))))

(defn landing-reads-hold?
  "Whether every answer kept in memo (landing-seen's) is the answer now: a price planned over them is still the price."
  [p ^js memo]
  (every? (fn [^js e] (let [[x y z wall?] (.-cell e)] (= (.-seen e) (landing-answer p x y z wall?)))) (es6-iterator-seq (.values memo))))
