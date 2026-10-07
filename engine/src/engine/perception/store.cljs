(ns engine.perception.store
  "Block memory: per dimension and 16^3 section, the state ids and seen times of the cells the body has seen, capped by
  forgetting the least recently seen section; and reading it back (seen-block, seen-blocks).
  A cell holding a mutable state (fluid, door, gate, trapdoor, fire: `mutable-id?`) also keeps its seen time to the ms,
  in the section's :fine map (not saved to file)."
  (:require [engine.game :as game]))

(def section-bytes 12288) ; ids (8192) and per-cell seen times (4096)
(def minute-ms 60000)
(def max-seen-radius 64)

(defn section-key [cx sy cz]
  (+ (* (+ (* (+ cx 2097152) 4194304) (+ cz 2097152)) 256) (+ sy 128)))

(defn cell-index [x y z]
  (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15)))

(defn store-of
  "The section map of a dimension, made when first asked for."
  [^js st dim]
  (or (.get (.-stores st) dim)
      (let [m (js/Map.)] (.set (.-stores st) dim m) m)))

(defn evict-oldest! [^js st]
  (let [oldest (reduce (fn [best ^js m]
                         (let [e (.next (.entries m))]
                           (if (or (.-done e) (and best (<= (.-seen ^js (aget (:entry best) 1)) (.-seen ^js (aget (.-value e) 1)))))
                             best
                             {:map m :entry (.-value e)})))
                       nil (es6-iterator-seq (.values (.-stores st))))]
    (when oldest
      (.delete ^js (:map oldest) (aget (:entry oldest) 0))
      (set! (.-count st) (dec (.-count st)))
      (set! (.-lastKey st) -1))))

(defn section-for!
  "The section holding key in store, made (forgetting the least recently seen one past the cap) and moved to the
  newest end once per stamp."
  [^js st ^js store key cx sy cz now]
  (let [found (.get store key)
        ^js sec (or found
                    (do (while (>= (.-count st) (.-cap st)) (evict-oldest! st))
                        (set! (.-count st) (inc (.-count st)))
                        #js {:ids (js/Uint16Array. 4096) :times (js/Uint8Array. 4096) :base now :seen now :stamp -1 :cx cx :sy sy :cz cz}))]
    (when (not= (.-stamp sec) (.-stamp st))
      (.delete store key)
      (.set store key sec)
      (set! (.-seen sec) now)
      (set! (.-stamp sec) (.-stamp st)))
    (set! (.-lastKey st) key)
    (set! (.-lastSec st) sec)
    sec))

(defn minute-of!
  "The cell time (minutes after the section's base) for a write at now; the base moves on when it would pass 255."
  [^js sec now]
  (let [m (js/Math.max 0 (js/Math.floor (/ (- now (.-base sec)) minute-ms)))]
    (if (<= m 255)
      m
      (let [shift (- m 255) ^js times (.-times sec)]
        (dotimes [i 4096] (aset times i (js/Math.max 0 (- (aget times i) shift))))
        (set! (.-base sec) (+ (.-base sec) (* shift minute-ms)))
        255))))

(defn cell-seen
  "When the cell was last seen, in ms: to the ms for a mutable state, else to the minute (never later than the truth)."
  [^js sec i]
  (or (some-> ^js (.-fine sec) (.get i))
      (+ (.-base sec) (* minute-ms (aget (.-times sec) i)))))

(def mutable-names #{"water" "lava" "bubble_column" "fire" "soul_fire"})

(defn mutable-name?
  "Fluids, doors, fence gates, trapdoors and fire: cells that change out of sight, so an old memory of them is unsafe."
  [name]
  (or (contains? mutable-names name)
      (.endsWith name "_door") (.endsWith name "_fence_gate") (.endsWith name "_trapdoor")))

(defn mutable-id?
  "Whether state id holds a mutable-name? block. False while the sight table (and so the state count) is not known."
  [^js st id]
  (let [^js kinds (or (.-kinds st)
                      (when-let [^js sight (.-sight st)] (set! (.-kinds st) (js/Int8Array. (.-length sight)))))]
    (and (some? kinds) (< id (.-length kinds))
         (let [k (aget kinds id)]
           (if (zero? k)
             (let [yes (mutable-name? (.-name ^js ((.-infoOf st) id)))]
               (aset kinds id (if yes 1 -1))
               yes)
             (== k 1))))))

(def fluid-names #{"water" "lava" "bubble_column"})

(def neighbour-offsets [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn record! [^js st store x y z id now]
  (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
        key (section-key cx sy cz)
        ^js sec (if (== key (.-lastKey st)) (.-lastSec st) (section-for! st store key cx sy cz now))]
    (let [i (cell-index x y z)]
      (aset (.-ids sec) i (inc id))
      (aset (.-times sec) i (minute-of! sec now))
      (if (mutable-id? st id)
        (.set (or (.-fine sec) (set! (.-fine sec) (js/Map.))) i now)
        (when-let [^js fine (.-fine sec)] (.delete fine i))))))

(defn new-stamp!
  "A new write round: sections written from now on are moved to the newest end again."
  [^js st]
  (set! (.-stamp st) (inc (.-stamp st)))
  (set! (.-lastKey st) -1))

;; ---- reading memory

(defn remembered
  "[state id, seen time in ms] of the remembered cell (x y z) in the current dimension, or nil when never seen."
  [^js st x y z]
  (when-let [^js sec (.get (store-of st (.-dim st))
                           (section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)))]
    (let [v (aget (.-ids sec) (cell-index x y z))]
      (when (pos? v) [(dec v) (cell-seen sec (cell-index x y z))]))))

(defn properties-of [^js info]
  (let [props (js->clj (.-properties info) :keywordize-keys true)]
    (when (seq props) props)))

(defn seen-block
  "{:name :pos :properties :age-ms :state-id} as last seen, or {:unknown true :pos} for a cell the body never saw."
  [{:keys [raw opts st]} [x y z :as pos]]
  (if-let [[id seen-at] (remembered st x y z)]
    (let [^js info (.stateInfo ^js raw id)]
      (cond-> {:name (.-name info) :pos pos :age-ms (- ((:now opts)) seen-at) :state-id id}
        (properties-of info) (assoc :properties (properties-of info))))
    {:unknown true :pos pos}))

(defn seen-blocks
  "Remembered blocks within radius (at most max-seen-radius) of the body's feet, nearest first:
  [{:name :pos :distance :age-ms}]. names (a coll) or match (name -> truthy) filters; max caps the list."
  [{:keys [raw opts st]} {:keys [radius names match max] :or {radius 16 max 64}}]
  (let [^js st st ^js eye (.eye ^js raw)]
    (if-not eye
      []
      (let [radius (min radius max-seen-radius)
            fx (.-x eye) fy (- (.-y eye) game/eye-height) fz (.-z eye)
            wanted? (cond names (set names) match match :else (constantly true))
            by-id (js/Map.)
            ok? (fn [id] (if (.has by-id id)
                           (.get by-id id)
                           (let [name (.-name ^js (.stateInfo ^js raw id))
                                 hit (when (wanted? name) name)]
                             (.set by-id id hit)
                             hit)))
            now ((:now opts))
            found (array)]
        (.forEach (store-of st (.-dim st))
                  (fn [^js sec]
                    (let [bx (* 16 (.-cx sec)) by (* 16 (.-sy sec)) bz (* 16 (.-cz sec))
                          ^js ids (.-ids sec)]
                      (when (<= (js/Math.hypot (- (+ bx 8) fx) (- (+ by 8) fy) (- (+ bz 8) fz)) (+ radius 14))
                        (dotimes [i 4096]
                          (let [v (aget ids i)]
                            (when (pos? v)
                              (when-let [name (ok? (dec v))]
                                (let [x (+ bx (bit-and i 15)) y (+ by (bit-shift-right i 8)) z (+ bz (bit-and (bit-shift-right i 4) 15))
                                      d (js/Math.hypot (- (+ x 0.5) fx) (- (+ y 0.5) fy) (- (+ z 0.5) fz))]
                                  (when (<= d radius)
                                    (.push found #js {:name name :pos [x y z] :distance d :ageMs (- now (cell-seen sec i))})))))))))))
        (.sort found (fn [^js a ^js b] (- (.-distance a) (.-distance b))))
        (into [] (comp (take max) (map (fn [^js r] {:name (.-name r) :pos (.-pos r) :distance (.-distance r) :age-ms (.-ageMs r)}))) found)))))

(defn beside-fluid?
  "Whether the remembered cell (x y z) with state id holds a non-solid block next to a remembered fluid: fluids flow, so
  its memory goes stale like a fluid's."
  [^js st x y z id]
  (and (= "empty" (.-boundingBox ^js ((.-infoOf st) id)))
       (some (fn [[dx dy dz]]
               (when-let [[nid] (remembered st (+ x dx) (+ y dy) (+ z dz))]
                 (contains? fluid-names (.-name ^js ((.-infoOf st) nid)))))
             neighbour-offsets)))
