(ns engine.path.fixture
  "Tiny hand-built worlds for planner tests: a few named blocks in an otherwise empty (air) column set. Ported from
  engine/js/path/fixture.mjs. The snapshot (path/snapshot.mjs), the block table (path/blocks.mjs) and block state ids
  (prismarine-block) stay interop: they are the JS the planner reads."
  (:require ["module" :refer [createRequire]]))

;; not engine.test-util: that requires engine.fake, which requires engine.fake.steer, which requires this
(def require-here (createRequire (str (js/process.cwd) "/")))

(def MC-VERSION "26.1")
(def registry ((require-here "prismarine-registry") MC-VERSION))
(def Block ((require-here "prismarine-block") registry))
(def snapshot-mod (require-here "./js/path/snapshot.mjs"))
(def blocks-mod (require-here "./js/path/blocks.mjs"))
(def UNLOADED (.-UNLOADED ^js snapshot-mod))

(defn block-by-name [name]
  (or (aget (.-blocksByName ^js registry) name)
      (throw (js/Error. (str "unknown block " name)))))

(defn default-state [name] (.-defaultState ^js (block-by-name name)))

(defn state-id
  "The state id of block `name` with props (a cljs map); unspecified properties keep the block's default state values
  (Block.fromProperties would zero them)."
  ([name] (state-id name {}))
  ([name props]
   (let [defaults (.getProperties ^js (.fromStateId ^js Block (default-state name) 0))]
     (.-stateId ^js (.fromProperties ^js Block name (js/Object.assign #js {} defaults (clj->js props)) 0)))))

(defn block-at-id
  "{:name :props} of a state id; props is a cljs map keyword -> the registry's value (strings, booleans)."
  [id]
  (let [block (.fromStateId ^js Block id 0)]
    {:name (.-name block)
     :props (js->clj (.getProperties block) :keywordize-keys true)}))

(defn block-at [snapshot x y z] (block-at-id (.stateAt ^js snapshot x y z)))

(defn fill-box! [snapshot [x0 y0 z0 x1 y1 z1 name props]]
  (let [id (state-id name props)]
    (doseq [y (range y0 (inc y1)) z (range z0 (inc z1)) x (range x0 (inc x1))]
      (.setState ^js snapshot x y z id))))

(defn box-columns [[x0 _ z0 x1 _ z1]]
  (for [cx (range (bit-shift-right x0 4) (inc (bit-shift-right x1 4)))
        cz (range (bit-shift-right z0 4) (inc (bit-shift-right z1 4)))]
    [cx cz]))

(defn touched-columns [blocks fills]
  (set (concat (map (fn [[x _ z]] [(bit-shift-right x 4) (bit-shift-right z 4)]) blocks)
               (mapcat box-columns fills))))

;; ---- connections: the server joins fences, walls and panes to their neighbours when they are placed; a bare fill would
;; leave posts with gaps a body slips through, so the fixture joins them too ----

(defn family-of-name
  "nil for blocks that do not connect; :nether-fence does not join wooden fences."
  [name]
  (cond
    (= name "nether_brick_fence") :nether-fence
    (re-find #"(^|_)fence$" name) :fence
    (re-find #"_wall$" name) :wall
    (re-find #"_pane$|(^|_)bars$" name) :pane
    (re-find #"_fence_gate$" name) :gate))

(def SIDES [["east" 1 0] ["west" -1 0] ["south" 0 1] ["north" 0 -1]])

(def family-table
  "state id -> family, for the ids of the connecting blocks only."
  (delay (into {}
               (mapcat (fn [^js b]
                         (when-let [family (family-of-name (.-name b))]
                           (map vector (range (.-minStateId b) (inc (.-maxStateId b))) (repeat family)))))
               (array-seq (.-blocksArray ^js registry)))))

(defn full-cube? [^js table id]
  (and (>= (aget (.-top table) id) 16) (zero? (aget (.-base table) id)) (zero? (aget (.-partial table) id))))

(defn joins?
  "does a post of `family` join the block `id` on its side (dx, dz)? Its own family, a gate across the line, a full block."
  [family id dx dz table]
  (when-not (= id UNLOADED)
    (let [other (get @family-table id)]
      (cond
        (= other family) true
        (= other :gate) (and (contains? #{:fence :wall} family)
                             (= (not (zero? dx)) (contains? #{"north" "south"} (:facing (:props (block-at-id id))))))
        (some? other) false
        :else (full-cube? table id)))))

(defn connect! [snapshot [x0 y0 z0 x1 y1 z1]]
  (let [table (.defaultStateTable ^js blocks-mod)]
    (doseq [y (range (dec y0) (+ y1 2)) z (range (dec z0) (+ z1 2)) x (range (dec x0) (+ x1 2))]
      (let [id (.stateAt ^js snapshot x y z)
            family (when-not (= id UNLOADED) (get @family-table id))]
        (when (and family (not= family :gate))
          (let [{:keys [name props]} (block-at-id id)
                linked (mapv (fn [[_ dx dz]] (boolean (joins? family (.stateAt ^js snapshot (+ x dx) y (+ z dz)) dx dz table))) SIDES)
                [e w s n] linked
                sides (if (= family :wall)
                        (assoc (into {} (map (fn [[side] l] [(keyword side) (if l "low" "none")]) SIDES linked))
                               :up (not (or (and n s (not e) (not w)) (and e w (not n) (not s)))))
                        (into {} (map (fn [[side] l] [(keyword side) l]) SIDES linked)))]
            (.setState ^js snapshot x y z (state-id name (merge props sides)))))))))

(defn fixture-snapshot
  "A planner snapshot of {:fill [[x0 y0 z0 x1 y1 z1 name props] ...] :blocks [[x y z name props] ...]}: fills first, then
  blocks; columns the entries touch are air, the rest unloaded; fences, walls and panes are joined to their neighbours."
  [{:keys [blocks fill]}]
  (let [snapshot (.createSnapshot ^js snapshot-mod #js {})
        section-count (bit-shift-right (.-height ^js snapshot) 4)]
    (doseq [[cx cz] (touched-columns blocks fill) sy (range section-count)]
      (.setSection ^js snapshot cx sy cz (js/Uint16Array. 4096)))
    (run! #(fill-box! snapshot %) fill)
    (doseq [[x y z name props] blocks] (.setState ^js snapshot x y z (state-id name props)))
    (let [placed (concat (map (fn [box] [box (nth box 6)]) fill)
                         (map (fn [[x y z name :as b]] [[x y z x y z] name]) blocks))]
      (when (some (fn [[_ name]] (family-of-name name)) placed)
        (run! (fn [[box]] (connect! snapshot box)) placed)))
    snapshot))

(defn layers
  "ascii layers (bottom first) of z rows of x chars -> blocks; '.' is air, ' ' is skipped; the legend maps a char to a
  name or [name props]."
  [rows legend {:keys [x y z]}]
  (vec (for [[dy layer] (map-indexed vector rows)
             [dz row] (map-indexed vector layer)
             [dx ch] (map-indexed vector (seq row))
             :when (not= ch " ")
             :let [[name props] (if (= ch ".") ["air"] (let [e (get legend ch)] (if (vector? e) e [e])))
                   at [(+ x dx) (+ y dy) (+ z dz) name]]]
         (if props (conj at props) at))))
