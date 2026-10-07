(ns jobs.lib.walk.world
  "The walk driver's view of the world: the primitives' pathWorld and the decorations the walks plan over (walls, dangers, dark,
  avoided and tolled cells), the executor policy of the body and the cell it stands in."
  (:require [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.threats :as threats]))

(defn path-world
  "The primitives' pathWorld sensing, nil when they have none or cannot sense now."
  [p]
  (when (fn? (.-pathWorld p))
    (.pathWorld p)))

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
         :dark #js {:at (:at dark) :factor cost/dark-factor}}))

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

(defn body-policy
  "executor/policy for the body: with food 6 or less the client does not sprint, so :sprint is false (a corner jump past
  a high block is then refused). :max-drop and :fall-factor follow its fall enchantments and health (jobs.lib.cost/fall-profile)."
  [c]
  (let [self (.self (:primitives c))
        food (.-food self)]
    (cond-> (merge executor/policy (cost/fall-profile {:health (.-health self) :equipment (cost/equipment-of (.-equipment self))}))
      (and (number? food) (<= food 6)) (assoc :sprint false))))

(defn body-cell
  "The cell the body stands in, as a step's {:x :y :z}."
  [c]
  (let [pos (.-pos (.self (:primitives c)))]
    {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}))

(defn body-name [c] (.-username (.self (:primitives c))))
