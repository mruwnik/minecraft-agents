(ns jobs.lib.memory-age
  "The chance a remembered cell is still as the body saw it: the one rule every reader of memory uses (perception keeps
  the raw memory with its age; this prices it).

  A cell of kind k seen t s ago is unchanged with chance (1 + t/b)^-a: the Gamma(a, b) prior on its change rate (a
  changes seen over b s of watching), whose long tail keeps a wide prior honest. Kinds: fire; door (doors, fence gates,
  trapdoors); fluid (water, lava, a bubble column, and an empty cell beside a remembered fluid, which a flow fills).
  Every other cell stays as seen. The priors are the settings below (fluid: measured, tools/event-counts.mjs resight).
  level: q, the chance it changed, in doublings from q-min up to 1/2, then in halvings of 1 - q; level-chance is the q
  a reader prices a level at, so a price holds while the level does. q-min: below it no price moves by a second (the
  dearest worse case, a death, costs about 1000 s).
  likelier-changed?: q >= 1/2, how a reader that takes one state reads it (jobs.lib.util/sensed: unknown).
  epoch: a key that moves when memory is written or any remembered cell's level changes (its due time passes, or an
  aged cell is seen again). changed-fn: the planner's options.unknown.changed."
  (:require [engine.args :as a]
            [engine.perception.rays :as rays]
            [engine.perception.store :as store]
            [engine.settings :as settings]
            [jobs.lib.cost.survival :as survival]))

(a/defargs settings
  {::fire-changes {:default 2 :spec a/pos-num? :doc "Prior changes of a remembered fire (vanilla: one off a non-flammable floor dies in ~20-60 s)."}
   ::fire-seconds {:default 60 :spec a/pos-num? :doc "Seconds of watching those fire changes were seen over."}
   ::door-changes {:default 0.5 :spec a/pos-num? :doc "Prior changes of a remembered door, fence gate or trapdoor (villagers toggle them, the wild never)."}
   ::door-seconds {:default 600 :spec a/pos-num? :doc "Seconds of watching those door changes were seen over."}
   ::fluid-changes {:default 1.47 :spec a/pos-num? :doc "Changes of a remembered fluid, or an empty cell beside one (a flow after a neighbour changed, ice, sponges, buckets): the resight posterior, 20 of 1449491 cells re-recorded changed, 1733 sections each one unit, Jeffreys prior."}
   ::fluid-seconds {:default 470686494 :spec a/pos-num? :doc "Seconds of watching those fluid changes were seen over (the resight posterior's b)."}})

(def ^:const NONE 0)
(def ^:const FLUID 1)
(def ^:const DOOR 2)
(def ^:const FIRE 3)
(def ^:private EMPTY 8)

(def q-min (js/Math.pow 2 -10))
(def ^:const TOP 20)
(def ^:const LIKELIER 10)
(def ^:private PRIOR (* 4 (inc TOP)))

(defn name-kind
  "The kind of a block name: FLUID, DOOR, FIRE or NONE."
  [name]
  (cond (contains? #{"water" "lava" "bubble_column"} name) FLUID
        (contains? #{"fire" "soul_fire"} name) FIRE
        (or (.endsWith name "_door") (.endsWith name "_fence_gate") (.endsWith name "_trapdoor")) DOOR
        :else NONE))

(defn- prior [kind]
  (case kind
    1 [(settings/get settings ::fluid-changes) (settings/get settings ::fluid-seconds)]
    2 [(settings/get settings ::door-changes) (settings/get settings ::door-seconds)]
    3 [(settings/get settings ::fire-changes) (settings/get settings ::fire-seconds)]))

(def ^:const HORIZON-MS
  "The longest a memory ages: a cell seen longer ago prices as one seen this long ago. Past it a level's due age is
  beyond what a double tells apart."
  (* 10 365 24 3600 1000))

(defn- chance
  "q of a cell seen age-ms ago (at most HORIZON-MS) under a prior of a changes over b-ms."
  [a b-ms age-ms]
  (- 1 (survival/gamma-survival a b-ms (js/Math.min HORIZON-MS (js/Math.max 0 age-ms)))))

(defn changed-chance
  "q: the chance a remembered cell of kind seen age-ms ago has changed since."
  [kind age-ms]
  (if (== kind NONE)
    0
    (let [[a b] (prior kind)]
      (chance a (* 1000 b) age-ms))))

(defn level
  "The level of change chance q: 0 under q-min, 1-9 its doublings up to 1/2, 10-TOP the halvings of 1 - q past it."
  [q]
  (cond (< q q-min) 0
        (< q 0.5) (js/Math.min 9 (inc (js/Math.floor (js/Math.log2 (/ q q-min)))))
        :else (js/Math.min TOP (+ LIKELIER (js/Math.floor (js/Math.log2 (/ 0.5 (js/Math.max (- 1 q) 1e-12))))))))

(defn level-chance
  "The change chance a level is priced at: its lowest."
  [l]
  (cond (zero? l) 0
        (< l LIKELIER) (js/Math.pow 2 (- l 11))
        :else (- 1 (js/Math.pow 2 (- (dec LIKELIER) l)))))

(defn top-level
  "The highest level a cell of kind reaches: its level at HORIZON-MS."
  [kind]
  (level (changed-chance kind HORIZON-MS)))

(defn due-age-ms
  "The age at which a cell of kind at level l reaches the next level (the formula's inverse), Infinity at its top-level."
  [kind l]
  (if (>= l (top-level kind))
    js/Infinity
    (let [[a b] (prior kind)
          t (js/Math.ceil (* 1000 b (- (js/Math.pow (- 1 (level-chance (inc l))) (/ -1 a)) 1)))]
      ;; rounding may leave the exact age a hair short of the next level, and then the due time would never pass
      (if (> (level (changed-chance kind t)) l) t (inc t)))))

;; ---- kinds of remembered cells

(defonce ^:private codes (js/WeakMap.))

(defn- codes-of [^js st] (or (.get codes st) (let [a (js/Int8Array. 65536)] (.set codes st a) a)))

(defn- code
  "kind of state id, plus EMPTY when it has no collision box; kept in a (codes-of st: plus 1, 0 not yet)."
  [^js a ^js st id]
  (let [c (aget a id)]
    (if (pos? c)
      (dec c)
      (let [^js info ((.-infoOf st) id)
            c (bit-or (name-kind (.-name info)) (if (= "empty" (.-boundingBox info)) EMPTY 0))]
        (aset a id (inc c))
        c))))

(defn- id-code [^js st id] (code (codes-of st) st id))

(defn- fluid-v? [^js st v] (and (pos? v) (== FLUID (bit-and 7 (id-code st (dec v))))))

(defn- beside-fluid?
  "Whether a remembered cell (x y z), index i of section sec, has a remembered fluid beside it."
  [^js st ^js sec i x y z]
  (let [^js ids (.-ids sec)
        lx (bit-and x 15) ly (bit-and y 15) lz (bit-and z 15)]
    (or (fluid-v? st (if (< lx 15) (aget ids (inc i)) (inc (store/remembered-id st (inc x) y z))))
        (fluid-v? st (if (pos? lx) (aget ids (dec i)) (inc (store/remembered-id st (dec x) y z))))
        (fluid-v? st (if (< lz 15) (aget ids (+ i 16)) (inc (store/remembered-id st x y (inc z)))))
        (fluid-v? st (if (pos? lz) (aget ids (- i 16)) (inc (store/remembered-id st x y (dec z)))))
        (fluid-v? st (if (< ly 15) (aget ids (+ i 256)) (inc (store/remembered-id st x (inc y) z))))
        (fluid-v? st (if (pos? ly) (aget ids (- i 256)) (inc (store/remembered-id st x (dec y) z)))))))

(defn- cell-kind [^js st ^js sec i x y z]
  (let [v (aget (.-ids sec) i)]
    (if (zero? v)
      NONE
      (let [c (id-code st (dec v)) k (bit-and c 7)]
        (cond (pos? k) k
              (and (pos? (bit-and c EMPTY)) (beside-fluid? st sec i x y z)) FLUID
              :else NONE)))))

(defonce ^:private fluid-secs (js/WeakMap.))

(defn- has-fluid?
  "Whether section sec holds a remembered fluid; kept per write of it."
  [^js st ^js sec]
  (let [hit (.get fluid-secs sec)
        w (or (.-wrote sec) 0)]
    (if (and hit (== w (aget hit 0)))
      (aget hit 1)
      (let [^js ids (.-ids sec)
            ^js a (codes-of st)
            v (loop [i 0] (cond (== i 4096) false
                                (let [v (aget ids i)] (and (pos? v) (== FLUID (bit-and 7 (code a st (dec v)))))) true
                                :else (recur (inc i))))]
        (.set fluid-secs sec #js [w v])
        v))))

(defn remembered-kind
  "The kind of the remembered cell (x y z) of the current dimension, NONE when never seen."
  [^js st x y z]
  (if-let [^js sec (store/section-at st x y z)]
    (cell-kind st sec (store/cell-index x y z) x y z)
    NONE))

(defn half-age-ms
  "The age at which a cell of kind is likelier changed than not."
  [kind]
  (let [[a b] (prior kind)] (* 1000 b (- (js/Math.pow 2 (/ 1 a)) 1))))

(defonce ^:private name-kinds (js/Map.))

(defn- cached-name-kind [name]
  (let [k (.get name-kinds name)]
    (if (some? k) k (let [k (name-kind name)] (.set name-kinds name k) k))))

(defn likelier-changed?
  "Whether the cell (x y z) the body remembers (not sees or feels now), name, seen age-ms ago, has likelier changed than
  not: how a reader that takes one state reads it. An empty cell asks about its fluid neighbours."
  [^js st name age-ms x y z]
  (let [k (cached-name-kind name)]
    (if (pos? k)
      (>= age-ms (half-age-ms k))
      (let [id (store/remembered-id st x y z)]
        (and (>= id 0) (pos? (bit-and EMPTY (id-code st id)))
             (>= age-ms (half-age-ms FLUID)) (== FLUID (remembered-kind st x y z)))))))

(defonce ^:private read-memo (js/WeakMap.))

(defn reads-changed?
  "likelier-changed? of a sensed answer b (perception's JS one) of a body with perception per: false unless b is
  remembered. Kept per answer object (perception shares one per cell and tick)."
  [per ^js b]
  (boolean
   (and per b (false? (.-visible b))
        (let [hit (.get read-memo b)]
          (if (some? hit)
            hit
            (let [^js pos (.-pos b)
                  v (likelier-changed? (:st per) (.-name b) (.-ageMs b) (.-x pos) (.-y pos) (.-z pos))]
              (.set read-memo b v)
              v))))))

;; ---- levels of the whole memory, and the key over them

(defonce ^:private entries (js/WeakMap.))
(defonce ^:private states (js/WeakMap.))

(def ^:private faces
  "[dx dy dz step-in-index]: the 6 sides of a section, and how far the index moves one cell that way."
  [[1 0 0 1] [-1 0 0 -1] [0 1 0 256] [0 -1 0 -256] [0 0 1 16] [0 0 -1 -16]])

(defn- on-face?
  "Whether index i lies on the side (dx dy dz) of its section."
  [i dx dy dz]
  (cond (pos? dx) (== 15 (bit-and i 15)) (neg? dx) (zero? (bit-and i 15))
        (pos? dy) (== 15 (bit-shift-right i 8)) (neg? dy) (zero? (bit-shift-right i 8))
        (pos? dz) (== 15 (bit-and (bit-shift-right i 4) 15)) :else (zero? (bit-and (bit-shift-right i 4) 15))))

(def ^:private face-cells
  "The 256 indices on each side of faces, in its order."
  (mapv (fn [[dx dy dz]] (js/Int32Array.from (filter #(on-face? % dx dy dz) (range 4096)))) faces))

(defn- build-entry
  "The aging cells of section sec (index, kind) with no levels yet: its fluids, doors and fires, and each empty cell
  beside a fluid (of its own, or across a side from a section that holds one)."
  [^js st ^js sec]
  (let [^js store (store/store-of st (.-dim st))
        ^js ids (.-ids sec)
        ^js a (codes-of st)
        mark (js/Uint8Array. 4096)
        fluids #js []
        empty? (fn [j] (let [v (aget ids j)] (and (pos? v) (zero? (aget mark j)) (pos? (bit-and EMPTY (code a st (dec v)))))))]
    (dotimes [i 4096]
      (let [v (aget ids i)]
        (when (pos? v)
          (let [k (bit-and 7 (code a st (dec v)))]
            (when (pos? k)
              (aset mark i k)
              (when (== k FLUID) (.push fluids i)))))))
    (doseq [i fluids [dx dy dz step] faces]
      (when-not (on-face? i dx dy dz)
        (let [j (+ i step)] (when (empty? j) (aset mark j FLUID)))))
    (doseq [[[dx dy dz step] ^js on] (map vector faces face-cells)]
      (when-let [^js n (.get store (store/section-key (+ (.-cx sec) dx) (+ (.-sy sec) dy) (+ (.-cz sec) dz)))]
        (when (has-fluid? st n)
          (let [^js nids (.-ids n)
                across (- step (* 16 step))]
            (dotimes [f 256]
              (let [i (aget on f)]
                (when (and (empty? i) (fluid-v? st (aget nids (+ i across))))
                  (aset mark i FLUID))))))))
    (let [cells #js [] kinds #js []]
      (dotimes [i 4096]
        (when (pos? (aget mark i)) (.push cells i) (.push kinds (aget mark i))))
      (let [e #js {:sec sec :cells (js/Int32Array.from cells) :kinds (js/Uint8Array.from kinds) :levels (js/Uint8Array. (.-length cells))
                   :stamp nil :due js/Infinity :aged 0 :version 0 :flips 0 :q nil :qVersion -1 :wet (pos? (.-length fluids))}]
        (.set entries sec e)
        e))))

(defn- relevel!
  "Sets entry e's levels to the ages at now; true when one changed (its version moves then, and its flips when a cell
  crossed LIKELIER)."
  [^js e now ^js dues]
  (let [^js sec (.-sec e) ^js cells (.-cells e) ^js kinds (.-kinds e) ^js levels (.-levels e)]
    (loop [j 0 changed false flipped false due js/Infinity aged 0]
      (if (== j (.-length cells))
        (do (set! (.-stamp e) (.-stamp sec))
            (when flipped (set! (.-flips e) (inc (.-flips e))))
            (set! (.-due e) due)
            (set! (.-aged e) aged)
            (when changed (set! (.-version e) (inc (.-version e))))
            changed)
        (let [k (aget kinds j)
              seen (store/cell-seen sec (aget cells j))
              age (- now seen)
              l (if (< age (aget dues (* k (inc TOP))))
                  0
                  (level (chance (aget dues (+ PRIOR (* 2 k))) (aget dues (+ PRIOR (* 2 k) 1)) age)))
              old (aget levels j)]
          (aset levels j l)
          (recur (inc j) (or changed (not (== l old))) (or flipped (not (identical? (>= l LIKELIER) (>= old LIKELIER)))) (js/Math.min due (+ seen (aget dues (+ (* k (inc TOP)) l)))) (if (pos? l) (inc aged) aged)))))))

(defn- dues-table
  "due-age-ms by kind and level, flat; from PRIOR each kind's prior changes and ms."
  []
  (let [t (js/Float64Array. (+ PRIOR 8))]
    (doseq [k [FLUID DOOR FIRE]]
      (doseq [l (range (inc TOP))] (aset t (+ (* k (inc TOP)) l) (due-age-ms k l)))
      (let [[a b] (prior k)] (aset t (+ PRIOR (* 2 k)) a) (aset t (+ PRIOR (* 2 k) 1) (* 1000 b))))
    t))

(defn- state-of [^js st]
  (or (.get states st)
      (let [s #js {:writes -1 :stamp -1 :dim nil :due 0 :count 0 :aged 0 :key nil :list #js []}]
        (.set states st s)
        s)))

(defn- written-since
  "The sections of store written after writes (st's count), and those beside one that holds or held a fluid."
  [^js st ^js store writes]
  (let [out (js/Set.)]
    (.forEach store (fn [^js sec]
                      (when (> (or (.-wrote sec) 0) writes)
                        (.add out sec)
                        ;; the empty cells across its sides are beside a fluid of its, or were
                        (when (or (has-fluid? st sec) (some-> ^js (.get entries sec) .-wet))
                          (doseq [[dx dy dz] faces]
                            (when-let [n (.get store (store/section-key (+ (.-cx sec) dx) (+ (.-sy sec) dy) (+ (.-cz sec) dz)))]
                              (.add out n)))))))
    out))

(defn- refresh!
  [^js st ^js s now]
  (let [dim (.-dim st)
        ^js store (store/store-of st dim)
        dues (dues-table)
        rewritten? (or (not (== (.-writes s) (.-writes st))) (not (identical? (.-dim s) dim)))
        ^js fresh (if rewritten? (written-since st store (if (identical? (.-dim s) dim) (.-writes s) -1)) (js/Set.))
        moved (volatile! false)]
    (when rewritten?
      (let [list #js []]
        (.forEach store (fn [^js sec]
                          (let [^js old (.get entries sec)
                                ^js e (if (or (nil? old) (.has fresh sec)) (build-entry st sec) old)]
                            (when (and old (not (identical? e old)))
                              (set! (.-version e) (inc (.-version old)))
                              (set! (.-flips e) (inc (.-flips old))))
                            (when (pos? (.-length (.-cells e)))
                              (when-not (identical? e old) (relevel! e now dues))
                              (.push list e)))))
        (set! (.-list s) list)))
    (doseq [^js e (.-list s)]
      (when (or (not (identical? (.-stamp e) (.-stamp ^js (.-sec e)))) (>= now (.-due e)))
        (when (relevel! e now dues) (vreset! moved true))))
    (set! (.-due s) (reduce (fn [d ^js e] (js/Math.min d (.-due e))) js/Infinity (.-list s)))
    (set! (.-aged s) (reduce (fn [n ^js e] (+ n (.-aged e))) 0 (.-list s)))
    (when @moved (set! (.-count s) (inc (.-count s))))
    (set! (.-writes s) (.-writes st))
    (set! (.-stamp s) (.-stamp st))
    (set! (.-dim s) dim)
    (when (or (nil? (.-key s)) @moved (not= (.-writes st) (first (.-key s))))
      (set! (.-key s) [(.-writes st) (.-count s)]))))

(defn epoch
  "A key (compare with =) of the body's memory st that moves when a cell is written, or any remembered cell's level
  changes: what a price read from memory rests on. Holds while nothing does. nil for a perception with no memory (light
  data only)."
  [^js st]
  (when-let [now-fn (.-nowFn st)]
    (let [^js s (state-of st)
          now (now-fn)]
      (when-not (and (== (.-writes s) (.-writes st)) (== (.-stamp s) (.-stamp st)) (identical? (.-dim s) (.-dim st)) (< now (.-due s)))
        (refresh! st s now))
      (.-key s))))

(defn- q-array
  "Entry e's level-chance by cell index (Float32Array 4096), kept per version."
  [^js e]
  (if (== (.-qVersion e) (.-version e))
    (.-q e)
    (let [q (or (.-q e) (js/Float32Array. 4096))
          ^js cells (.-cells e) ^js levels (.-levels e)]
      (.fill q 0)
      (dotimes [j (.-length cells)] (aset q (aget cells j) (level-chance (aget levels j))))
      (set! (.-q e) q)
      (set! (.-qVersion e) (.-version e))
      q)))

(defn- sees-as-remembered?
  "Whether the body sees the cell (x y z) now (rays/visible-now?) and it holds what memory has for it: memory is current
  there, whatever its seen time."
  [{:keys [raw st] :as per} x y z]
  (when-let [^js eye (.eye ^js raw)]
    (let [id (.stateAt ^js raw x y z)]
      (and (>= id 0) (== id (store/remembered-id st x y z)) (boolean (rays/visible-now? per st eye x y z id))))))

(defn changed-fn
  "The planner's options.unknown.changed for perception per: (fn [x y z]) the level-chance the remembered cell changed, 0
  for one that does not age, was never seen, or that the body sees now as remembered (sees-as-remembered?, asked
  once a cell per fn); nil while no cell is past level 0."
  [{:keys [st] :as per}]
  (epoch st)
  (when (pos? (.-aged ^js (state-of st)))
    (let [^js store (store/store-of st (.-dim st))
          last-key (volatile! nil) last-q (volatile! nil)
          ^js in-view (js/Map.)]
      (fn [x y z]
        (let [k (store/section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4))
              ^js q (if (== k @last-key)
                      @last-q
                      (let [^js sec (.get store k)
                            ^js e (when sec (.get entries sec))
                            q (when (and e (pos? (.-aged e))) (q-array e))]
                        (vreset! last-key k)
                        (vreset! last-q q)
                        q))
              i (when q (store/cell-index x y z))
              c (if q (aget q i) 0)]
          (if (zero? c)
            0
            (let [^js seen (or (.get in-view k) (let [a (js/Int8Array. 4096)] (.set in-view k a) a))]
              (when (zero? (aget seen i)) (aset seen i (if (sees-as-remembered? per x y z) 1 2)))
              (if (== 1 (aget seen i)) 0 c))))))))

(defn section-flips
  "A number that moves whenever a cell of section cx sy cz of st's current dimension becomes likelier changed than not
  or stops being so (what a reader that takes one state reads); nil when it holds no aging cell."
  [^js st cx sy cz]
  (epoch st)
  (when-let [^js sec (.get (store/store-of st (.-dim st)) (store/section-key cx sy cz))]
    (when-let [^js e (.get entries sec)]
      (when (pos? (.-length (.-cells e))) (.-flips e)))))
