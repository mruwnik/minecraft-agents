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
            [jobs.lib.look :as look]
            [jobs.lib.placement :as placement]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.walk.rock :as rock]
            [jobs.lib.walk.sight :as wsight]
            [engine.perception.store :as store]))

(a/defargs settings
  {::default-food {:default 20 :doc "The food a walk counts on when it has neither a :food arg nor a body food: fed (nothing caps the damage)." :spec (a/int-in 0 nil)}
   ::min-health {:default false :doc "The health every walk of this body keeps (a floor of 1 to 20), false: the code's default floor. A job's own :min-health wins." :spec (a/or-of false? (a/int-in 1 20))}
   ::max-damage {:default false :doc "The most hp every walk of this body may spend (0 or more), false: no cap but the budget. A job's own :max-damage wins." :spec (a/or-of false? (a/num-in 0 nil))}
   ::dangers {:default true :doc "Whether walks plan to keep away from known dangers; false plans straight past them (a walk up to the hostile being fought)." :spec boolean?}
   ::zone-tolls {:default true :doc "Whether walks also toll the cells of other bodies' zones near each walk (jobs.lib.toll-cells/zone-walk-tolls), so a way round a neighbour's field is taken; none under the call's :ignore-zones?." :spec boolean?}
   ::dark {:default true :doc "Whether walks cost a dark cell (seen dark, or unseen at night) twice a lit one; false plans dark cells like lit ones." :spec boolean?}
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
  walk looked for and could not see), and one blocked Set of cells it looked at and could not see (they read as stone)."
  []
  (let [{:keys [cell floor peek-s aim-s see-lit-depth light-depth]} (cost/walk-unknown-prices)]
    #js {:cell cell :floor floor :peekS peek-s :aimS aim-s :seeLitDepth see-lit-depth :lightDepth light-depth :missed (js/Set.)
         :noLight (js/Set.) :blocked (js/Set.)}))

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

(defn- memory-sections
  "(fn [cx sy cz]) the ids of the body's memory of that section (sy the world's section y; ids + 1, 0 never seen), or nil;
  nil without a perception."
  [p]
  (when-let [per (aget p "perception")]
    (let [^js st (:st per)
          ^js sections (store/store-of st (.-dim st))]
      (fn [cx sy cz] (some-> ^js (.get sections (store/section-key cx sy cz)) .-ids)))))

(defn- see-depth-now
  "The deepest drop the body sees the landing floor of in the dark from a peek, with what it holds now."
  [p]
  (wsight/see-depth (or (look/near-of p) 0)))

(def ^:private unloaded-id 0xFFFF)

(defn rock-beyond "The setting ::rock-beyond, nil when off." [] (or (settings/get settings ::rock-beyond) nil))

(defn rock-guess?
  "Whether a walk over mask spec unknown (nil: none) reads far unseen cells as rock (path-world): ::rock-beyond is on, the
  body is under ground (sh-sense/buried?) with its head out of water, and the mask is not :edge."
  [p ^js unknown]
  (boolean (and unknown (not (.-edge unknown)) (rock-beyond) (not (breath/head-under? p)) (sh-sense/buried? p))))

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
                 (rock/rock-at {:k (rock-beyond) :known known :seeds (rock-seeds p unknown)}))
          ^js blocked (.-blocked unknown)
          blocked (when (and blocked (pos? (.-size blocked))) blocked)]
      (if (or rock blocked)
        (fn [x y z] (if (or (and blocked (.has blocked (str x "," y "," z))) (and rock (rock x y z))) :solid :ground))
        :ground))))

(defn with-seeds
  "The mask spec unknown with the cells its rock guess spares (path-world): goal ([x y z], or nil) and own (feet cells the
  body dug its ways through itself); the spec is per call, the seeds per round."
  [^js unknown goal own]
  (doto unknown (unchecked-set "goal" goal) (unchecked-set "own" own)))

(defn path-world
  "The primitives' pathWorld sensing (a snapshot over the world) with the planner's block table and free-space module, nil when
  they have none or cannot sense now. With unknown (unknown-spec), over a body with a perception: cells the body never saw
  or felt read void_air (its known cells read as they are now), and the snapshot carries the planner's options.unknown:
  the spec with the mask's id, seeDepth (from what the body holds now) and seeLit (look/see-lit-fn). Under ground
  (sh-sense/buried?, head not under water, not :edge), an unseen cell past ::rock-beyond of every seen open cell, the
  feet and the spec's seeds (with-seeds) reads stone (jobs.lib.walk.rock); the mask's rockBeyond says so (0: none)."
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
                                    #js {:id (if (.-edge unknown) unloaded-id (first (blocks/state-ids table (if under? "seagrass" "void_air")))) :blockedId stone :known known
                                         :seeDepth (see-depth-now p) :seeLit (look/see-lit-fn p) :rockBeyond (or k 0)}))]
       (when-let [pw (if mask (.pathWorld p mask) (.pathWorld p))]
         (let [pw (js/Object.assign #js {:table table :space space/space} pw)]
           (when k
             (set! (.-snapshot pw) (rock/with-rock (.-snapshot pw) {:mask (.-id mask) :stone stone :k k :known known :seeds (rock-seeds p unknown)})))
           pw))))))

(defn unknown-of
  "The mask's options (the planner's options.unknown) pw's snapshot carries, nil when it is not masked."
  [pw]
  (some-> pw .-snapshot .-unknown))

(defn unknown-key
  "What a kept search's key holds of pw's mask: nil unmasked, else the floors missed so far, the see depth, the takeoffs
  where a torch failed, the torch price and the rock guess."
  [pw]
  (when-let [^js u (unknown-of pw)]
    [(.-size ^js (.-missed u)) (.-size ^js (.-blocked u)) (.-seeDepth u) (some-> ^js (.-noLight u) .-size) (.-lightCost u) (.-rockBeyond u)]))

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
  cell costs cost/dark-factor of kit ({:weapon :equipment}; nil: the floor) times its own seconds more."
  [pw dark kit]
  (if (nil? dark)
    pw
    #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :avoid (.-avoid pw) :tolls (.-tolls pw)
         :dark #js {:at (:at dark) :factor (if kit (cost/dark-factor kit) (cost/dark-factor))}}))

(defn kit-of
  "{:weapon :equipment} of the body: its best default weapon (jobs.lib.combat) and the armour worn."
  [c]
  (let [p (:primitives c)]
    {:weapon (combat/best-weapon p combat/default-weapons)
     :equipment (cost/equipment-of (.-equipment (.self p)))}))

(declare with-tolls)

(defn costed-world
  "pw (the primitives' pathWorld) costed the way go-to plans: with the mobs the body knows may hurt it, each over its
  field (dangers?, jobs.lib.threats threat-tolls; planner-dangers keeps plans off sensed mobs' cells and deaths by terrain) and with dark cells (dark?, look/dark-fn; over a
  masked pw an unseen cell is priced by the mask, not as dark). Every search that ranks routes by cost plans over this.
  keep? (a fn of a known threat): only the threats it holds of are tolled (a flight races its chasers instead)."
  [c pw {:keys [dangers? dark? keep?]}]
  (cond-> pw
    dangers? (-> (with-dangers (threats/planner-dangers c))
                 (with-tolls (if keep? (threats/tolls-of c (filter keep? (threats/known-threats c))) (threats/threat-tolls c))))
    dark? (with-dark (look/dark-fn (:primitives c) {:unseen-dark? (nil? (unknown-of pw))}) (kit-of c))))

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
           :tolls merged})))

(defn tolls-key
  "What a kept search's key holds of pw's tolls (with-tolls): the cells with their factors, nil for none."
  [pw]
  (some-> (.-tolls pw) .-list))

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

(defn default-food [] (settings/get settings ::default-food))

(defn food-of
  "The food level (0-20) the walk counts on: the :food of the job's args (a number), else default-food. go-to fills it with the body's own food (jobs.movement.go-to/step!); the walk does not read the body."
  [c]
  (let [food (:food (:args c))]
    (if (number? food) food (default-food))))

(defn damage-body
  "The body's {:health :absorption :food :on-fire :effects} for jobs.lib.cost/damage-budget; the food is the job's (food-of)."
  [c]
  (let [self (.self (:primitives c))]
    {:health (.-health self) :absorption (.-absorption self) :food (food-of c) :on-fire (.-onFire self)
     :effects (map #(.-name %) (array-seq (.-effects self)))}))

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
  "(fn [x y z]) true when the body of primitives p has seen or felt the block there (now or remembered, not unknown, or
  water and air breath/remembered-water-name still counts); nil without a perception."
  [p]
  (when (some? (.-sensedAt p))
    (let [memo (js/Map.)]
      (fn [x y z]
        (let [k (str x "," y "," z)]
          (if (.has memo k)
            (.get memo k)
            (let [cell {:x x :y y :z z}
                  ^js b (u/sensed p cell)
                  known (boolean (and b (or (not (true? (.-unknown b))) (breath/remembered-water-name p cell))))]
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

(defn landing-seen
  "The planner's options.landingSeen for the body of primitives p: (fn [x y z wall?]) true when the body sees or feels the
  block, or saw it landing-memory-ms ago or less, so a bounce is priced only on a pad it has looked at (an older memory
  or an unknown block is not); nil without a perception. As a wall of the pad (wall?) a block remembered at any age
  counts: the walls of a 1x1 shaft face away from the edge a body drops from. A block never seen or an unloaded one
  never counts."
  [p]
  (when (some? (.-sensedAt p))
    (let [memo (js/Map.)]
      (fn [x y z wall?]
        (let [k (str x "," y "," z "," (boolean wall?))]
          (if (.has memo k)
            (.get memo k)
            (let [^js b (u/sensed p {:x x :y y :z z})
                  seen (boolean (and b (or (true? (.-visible b)) (true? (.-felt b))
                                   (and (not (true? (.-unknown b))) (or wall? (<= (or (.-ageMs b) js/Infinity) landing-memory-ms))))))]
              (.set memo k seen)
              seen)))))))
