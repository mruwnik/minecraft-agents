(ns jobs.lib.walk.world
  "The walk driver's view of the world: the primitives' pathWorld and the decorations the walks plan over (walls, dangers, dark,
  avoided and tolled cells), the executor policy of the body and the cell it stands in."
  (:require [engine.settings :as settings]
            [engine.path.executor :as executor]
            [engine.path.blocks :as blocks]
            [engine.path.planner-tuned :as planner]
            [engine.path.space :as space]
            [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]))

(def settings
  {::default-food {:default 20 :doc "The food a walk counts on when it has neither a :food arg nor a body food: fed (nothing caps the damage)." :type :int :min 0}})

(defn path-world
  "The primitives' pathWorld sensing (a snapshot over the world) with the planner's block table and free-space module, nil when
  they have none or cannot sense now."
  [p]
  (when (fn? (.-pathWorld p))
    (when-let [pw (.pathWorld p)]
      (js/Object.assign #js {:table (blocks/default-state-table) :space space/space} pw))))

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
  cell costs cost/dark-factor times its own seconds more."
  [pw dark]
  (if (nil? dark)
    pw
    #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :avoid (.-avoid pw) :tolls (.-tolls pw)
         :dark #js {:at (:at dark) :factor (cost/dark-factor)}}))

(defn costed-world
  "pw (the primitives' pathWorld) costed the way go-to plans: with the dangers the body knows of now (dangers?, jobs.lib.threats)
  and with dark cells (dark?, look/dark-fn). Every search that ranks routes by cost plans over this."
  [c pw {:keys [dangers? dark?]}]
  (cond-> pw
    dangers? (with-dangers (threats/planner-dangers c))
    dark? (with-dark (look/dark-fn (:primitives c)))))

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
  "Why tolls is not a usable go-to :tolls (nil or a sequence of {:x :y :z :factor}, finite numbers, factor not negative), else nil."
  [tolls]
  (let [finite? #(and (number? %) (js/isFinite %))
        ok? #(and (map? %) (every? (comp finite? %) [:x :y :z :factor]) (>= (:factor %) 0))]
    (cond
      (nil? tolls) nil
      (not (sequential? tolls)) (str "tolls is a list of {:x :y :z :factor}; got " (pr-str tolls))
      :else (when-let [[bad] (seq (remove ok? tolls))]
              (str "a toll is {:x :y :z :factor} of finite numbers, factor 0 or more; got " (pr-str bad))))))

(defn with-tolls
  "pw whose plans cost the cells of tolls ([{:x :y :z :factor}], jobs.lib.cost farm-tolls and zone-tolls) factor times their
  own seconds more (the planner's options.tolls; none: pw)."
  [pw tolls]
  (if (empty? tolls)
    pw
    #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw) :dark (.-dark pw) :avoid (.-avoid pw)
         :tolls #js {:list (vec (sort-by (juxt :x :y :z) tolls))
                     :cells (js/Map. (clj->js (mapv (fn [{:keys [x y z factor]}] [(planner/cell-key x y z) factor]) tolls)))}}))

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

(defn default-food [] (settings/get settings ::default-food))

(defn food-of
  "The food level (0-20) the walk counts on: the job's :food arg (a number), else default-food. go-to fills the arg with the body's own food (jobs.movement.go-to/step!); the walk does not read the body."
  [c]
  (let [food (:food (:args c))]
    (if (number? food) food (default-food))))

(defn damage-body
  "The body's {:health :absorption :food :on-fire :effects} for jobs.lib.cost/damage-budget; the food is the job's (food-of)."
  [c]
  (let [self (.self (:primitives c))]
    {:health (.-health self) :absorption (.-absorption self) :food (food-of c) :on-fire (.-onFire self)
     :effects (map #(.-name %) (array-seq (.-effects self)))}))

(def setting-checks
  "The body setting's keys and what a valid value is."
  {:min-health #(and (number? %) (<= 1 % 20))
   :max-damage #(and (number? %) (>= % 0))
   :gait #(contains? cost/gaits %)})

(defn body-setting
  "The body's memory :walk-settings entry's {:min-health :max-damage} (written with jobs.memory.remember, :ttl-s :forever,
  :cap 1), a value outside the go-to arg's range left out; the second item names the keys left out."
  [c]
  (let [data (if (:view c) (select-keys (:data (ctx/latest c :walk-settings)) (keys setting-checks)) {})
        bad (into [] (comp (remove (fn [[k v]] ((setting-checks k) v))) (map key)) data)]
    [(apply dissoc data bad) bad]))

(defn walk-settings
  "The {:min-health :max-damage} the walk keeps: the body's setting (body-setting), each overridden by the job's arg (go-to's) when given."
  [c]
  (into (first (body-setting c)) (remove (comp nil? val)) (select-keys (:args c) [:min-health :max-damage])))

(defn gait
  "The gait the walk keeps: go-to's :gait arg over the body's :gait setting (body-setting), else :auto."
  [c]
  (or (:gait (:args c)) (:gait (first (body-setting c))) :auto))

(defn warn-bad-settings!
  "An :info walk-settings.bad event naming the keys of the body's setting that are ignored; go-to calls it once per attempt."
  [c]
  (let [bad (second (body-setting c))]
    (when (seq bad)
      (ctx/emit! c :walk-settings.bad :info
                 {:keys bad :text (str "the body's :walk-settings " (pr-str bad) " is out of range (:min-health 1-20, :max-damage >= 0, :gait :auto|:walk|:sneak); ignored")}))))

(defn damage-budget
  "The hp the body may spend walking now (jobs.lib.cost/damage-budget): its health, food and effects, and the walk-settings
  :min-health and :max-damage (the job's args over the body's setting); with the ctx's :over-budget (go-to chose to go over it) the survivable-budget."
  [c]
  ((if (:over-budget c) cost/survivable-budget cost/damage-budget) (damage-body c) (walk-settings c)))

(defn body-policy
  "executor/policy for the body: the gait (:walk, :sneak) never sprints; with the walk's food (food-of) 6 or less the client does not sprint, so :sprint is false (a corner jump past
  a high block is then refused). :damage-budget (hp, damage-budget) and :damage-weight (seconds an hp costs at its health)
  price the damage of a walk, :danger-cap the total hp a second its known dangers cost (jobs.lib.cost/danger-cap, more when the job's :danger-max-rate is) (the job's :hp-seconds arg: the seconds an hp costs at full health, default jobs.lib.cost/hp-seconds);
  :max-drop and :fall-factor follow its fall enchantments and the longest drop it survives (the survivable-budget under the job's :max-damage,
  whatever the budget: a drop the budget refuses is a refusal the planner reports as :damageRefused, jobs.lib.cost/fall-profile).
  :air-drain and :air-grace follow the helmet (jobs.lib.cost/air-profile); with the head under water :air-used is the air
  its oxygen says it has used, and there is no grace (it may be spent on this dive)."
  [c]
  (let [self (.self (:primitives c))
        food (food-of c)]
    (cond-> (merge executor/policy
                   (cost/fall-profile {:damage-budget (cost/survivable-budget (damage-body c) (select-keys (walk-settings c) [:max-damage]))
                                       :equipment (cost/equipment-of (.-equipment self))})
                   (cost/air-profile (.-equipment self))
                   {:damage-budget (damage-budget c)
                    :damage-weight (* (or (:hp-seconds (:args c)) cost/hp-seconds) (cost/health-scale (.-health self)))
                    :danger-cap (max cost/danger-cap (or (:danger-max-rate (:args c)) 0))}
                   (when-some [landing (:landing (:args c))] {:landing landing}))
      (breath/head-under? (:primitives c)) (-> (dissoc :air-grace) (assoc :air-used (cost/air-used (.-oxygen self))))
      (and (number? food) (<= food 6)) (assoc :sprint false)
      (= :walk (gait c)) (assoc :sprint false :gait :walk)
      (= :sneak (gait c)) (assoc :sprint false :gait :sneak))))

(defn body-cell
  "The cell the body stands in, as a step's {:x :y :z}."
  [c]
  (let [pos (.-pos (.self (:primitives c)))]
    {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}))

(defn body-name [c] (.-username (.self (:primitives c))))

(defn landing-seen
  "The planner's options.landingSeen for the body of primitives p: (fn [x y z]) true when the body sees or feels the block
  there now, so a bounce is priced only on a pad in view (a remembered or unknown block is not); nil without a perception."
  [p]
  (when (some? (.-sensedAt p))
    (let [memo (js/Map.)]
      (fn [x y z]
        (let [k (str x "," y "," z)]
          (if (.has memo k)
            (.get memo k)
            (let [^js b (u/sensed p {:x x :y y :z z})
                  seen (boolean (and b (or (true? (.-visible b)) (true? (.-felt b)))))]
              (.set memo k seen)
              seen)))))))
