(ns jobs.lib.pen
  "The pen check: from a standing cell or a box, flood-fill what a cow can walk
  and say whether it stays inside. Pure over a block-at function that takes
  {:x :y :z} and returns a block (#js {:name :properties}) or nil (unloaded).

  The model, after vanilla's ground pathing as far as it is known:
  - every block has a collision column [lo hi] inside its cell (fence, wall and
    closed gate 1.5, slab 0.5 or 1, carpet 1/16, most others 1); an open gate or
    door, a plant, air and water have none; lava, fire and the like are
    hazards an animal never enters; a block this file does not know counts as
    a full block;
  - an animal stands on a surface, the top of a column with 1.4 free above it;
  - from a surface it steps to the neighbouring column's highest surface at
    most 1 above (so a block, a slab or a carpet is climbed, a fence top is
    not, from level ground) and falls to the first surface below, at most 3
    down, never through one; 4 down is not walked;
  - a diagonal step needs both orthogonal neighbours steppable.

  Leaks: with a :box, any step out of it. Without one, the fill runs into open
  ground until :max-cells and the way out is read off the walk to its far end
  (an open gate, a gap or a ridge climbed over). A leaky pen is refilled with
  those cells walled off, so :inside and :gates are the pen's own."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::max-drop {:default 3.0 :doc "Blocks a penned animal may drop when it walks." :spec (a/num-in 0 nil)}
   ::body-height {:default 1.4 :doc "Height of an animal's box a pen needs clear." :spec (a/num-in 0 nil)}
   ::gate-scan {:default 600 :doc "The most cells searched for gates when the flood was cut short." :spec (a/int-in 1 nil)}
   ::max-seals {:default 4 :doc "Rounds of leaks shut when planning a pen." :spec (a/int-in 0 nil)}
   ::max-listed {:default 12 :doc "Leaks listed in a pen report." :spec (a/int-in 1 nil)}})

(def step-up 1.0)
(defn max-drop [] (settings/get settings ::max-drop))
(defn body-height [] (settings/get settings ::body-height))
(def eps 1e-6)
(def default-max-cells 2000)
(defn gate-scan [] (settings/get settings ::gate-scan))
(defn max-seals [] (settings/get settings ::max-seals))

;; ------------------------------------------------------------------ what a block is to an animal

(def passes
  "Names an animal walks through: no collision."
  #{"air" "cave_air" "void_air" "water" "bubble_column" "short_grass" "tall_grass" "fern" "large_fern" "dead_bush"
    "seagrass" "tall_seagrass" "kelp" "kelp_plant" "vine" "glow_lichen" "snow" "cobweb" "light" "structure_void"
    "lily_pad" "sugar_cane" "redstone_wire" "rail" "powered_rail" "detector_rail" "activator_rail" "lever" "tripwire"
    "tripwire_hook" "string" "ladder" "torch" "wall_torch" "redstone_torch" "redstone_wall_torch"
    "dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip"
    "oxeye_daisy" "cornflower" "lily_of_the_valley" "torchflower" "sunflower" "lilac" "rose_bush" "peony" "pitcher_plant"
    "wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "nether_wart" "hanging_roots" "bamboo_sapling"})

(def pass-patterns
  [#"_sapling$" #"_button$" #"_pressure_plate$" #"_sign$" #"_banner$" #"_torch$" #"^attached_.*_stem$" #"_propagule$"])

(def hazards
  "Blocks animals refuse to enter (vanilla gives them a negative path cost)."
  #{"lava" "fire" "soul_fire" "cactus" "sweet_berry_bush" "magma_block" "wither_rose" "powder_snow"})

(def heights
  "Top of the collision box for blocks lower than a full block."
  {"farmland" 0.9375 "dirt_path" 0.9375 "soul_sand" 0.875 "mud" 0.875 "chest" 0.875 "trapped_chest" 0.875
   "ender_chest" 0.875 "enchanting_table" 0.75 "stonecutter" 0.5625 "cake" 0.5 "flower_pot" 0.375
   "daylight_sensor" 0.375})

(defn prop [b k] (some-> b .-properties (aget k)))

(defn on? [b k] (let [v (prop b k)] (or (true? v) (= "true" v))))

(defn ends? [n suffix] (.endsWith n suffix))

(defn passes? [n]
  (or (contains? passes n) (some #(re-find % n) pass-patterns)))

(defn gate? [b] (some-> b .-name (ends? "_fence_gate")))

(defn collision
  "{:lo :hi} (heights inside the cell, plus :hazard?) of block b, or nil when an animal passes through."
  [b]
  (let [n (.-name b)]
    (cond
      (contains? hazards n) {:lo 0 :hi 1 :hazard? true}
      (passes? n) nil
      (ends? n "_carpet") {:lo 0 :hi 0.0625}
      (ends? n "_fence_gate") (when-not (on? b "open") {:lo 0 :hi 1.5})
      (re-find #"_(fence|wall)$" n) {:lo 0 :hi 1.5}
      (ends? n "_slab") (case (prop b "type") "bottom" {:lo 0 :hi 0.5} "top" {:lo 0.5 :hi 1} {:lo 0 :hi 1})
      (ends? n "_trapdoor") (when-not (on? b "open") (if (= "top" (prop b "half")) {:lo 0.8125 :hi 1} {:lo 0 :hi 0.1875}))
      (ends? n "_door") (when-not (on? b "open") {:lo 0 :hi 1})
      (ends? n "_bed") {:lo 0 :hi 0.5625}
      (contains? heights n) {:lo 0 :hi (heights n)}
      :else {:lo 0 :hi 1})))

;; ------------------------------------------------------------------ the world, read once per cell

(def absent ::absent)

(defn make-world
  "Memoised reader over block-at: {:block f :cell f :unloaded volatile-of-set}; both take x y z. The cells in
  sealed ([x y z]) read as a wall two high."
  [block-at sealed]
  (let [blocks (volatile! {})
        cells (volatile! {})
        unloaded (volatile! #{})
        block (fn [x y z]
                (let [k [x y z]
                      known (get @blocks k absent)]
                  (if-not (identical? known absent)
                    known
                    (let [b (block-at {:x x :y y :z z})]
                      (vswap! blocks assoc k b)
                      (when (nil? b) (vswap! unloaded conj k))
                      b))))
        read (fn [x y z]
               (if-let [b (block x y z)]
                 (collision b)
                 {:lo 0 :hi 1 :unloaded? true}))]
    {:block block
     :unloaded unloaded
     :max-drop (max-drop)
     :body-height (body-height)
     :cell (fn [x y z]
             (let [k [x y z]
                   known (get @cells k absent)]
               (cond
                 (contains? sealed k) {:lo 0 :hi 2}
                 (not (identical? known absent)) known
                 :else (let [c (read x y z)] (vswap! cells assoc k c) c))))}))

(defn floor-cell [n] (js/Math.floor (+ n eps)))

(defn overlaps? [{:keys [lo hi]} y bottom top]
  (and (< (+ y lo) (- top eps)) (> (+ y hi) (+ bottom eps))))

(defn clear?
  "True when nothing in column x z collides with the open height interval (bottom, top)."
  [{:keys [cell]} x z bottom top]
  (every? (fn [y] (let [c (cell x y z)] (or (nil? c) (not (overlaps? c y bottom top)))))
          (range (js/Math.floor bottom) (inc (js/Math.floor (- top eps))))))

(defn standable?
  [{:keys [body-height] :as world} x z c y]
  (and c
       (not (:hazard? c))
       (not (:unloaded? c))
       (clear? world x z (+ y (:hi c)) (+ y (:hi c) body-height))))

(defn reach
  "Where an animal standing at height s steps into column x z: {:top :perch?}, or nil."
  [{:keys [cell max-drop body-height] :as world} x z s]
  (let [highest (js/Math.floor (+ s step-up eps))
        lowest (js/Math.floor (- s max-drop 1 eps))]
    (loop [y highest]
      (when (>= y lowest)
        (let [c (cell x y z)
              t (when c (+ y (:hi c)))]
          (if-not (and c (<= t (+ s step-up eps)))
            (recur (dec y))
            (when (and (not (:hazard? c))
                       (not (:unloaded? c))
                       (>= t (- s max-drop eps))
                       (clear? world x z t (+ (max s t) body-height)))
              {:top t :perch? (> (:hi c) 1)})))))))

(def orthogonal [[1 0] [-1 0] [0 1] [0 -1]])
(def diagonal [[1 1] [1 -1] [-1 1] [-1 -1]])

(defn moves
  "[[dx dz] {:top :perch?}] for every step from column x z at height s; a diagonal needs both orthogonals."
  [world x z s]
  (let [at (fn [[dx dz]] (reach world (+ x dx) (+ z dz) s))
        straight (into {} (keep (fn [d] (when-let [r (at d)] [d r]))) orthogonal)
        corners (for [[dx dz] diagonal
                      :when (and (straight [dx 0]) (straight [0 dz]))
                      :let [r (at [dx dz])]
                      :when r]
                  [[dx dz] r])]
    (concat straight corners)))

;; ------------------------------------------------------------------ where the fill starts

(defn surfaces-in
  "{:key :s :perch?} for each surface in column x z whose feet cell is within [lo-y hi-y], highest first."
  [{:keys [cell] :as world} x z lo-y hi-y]
  (for [y (range (inc hi-y) (- lo-y 2) -1)
        :let [c (cell x y z)]
        :when (and c (standable? world x z c y))
        :let [t (+ y (:hi c))
              feet (floor-cell t)]
        :when (<= lo-y feet hi-y)]
    {:key [x feet z] :s t :perch? (> (:hi c) 1)}))

(defn seeds-at [world {:keys [x y z]}]
  (take 1 (surfaces-in world x z y y)))

(defn seeds-in-box [world {:keys [min max]}]
  (for [x (range (:x min) (inc (:x max)))
        z (range (:z min) (inc (:z max)))
        s (surfaces-in world x z (:y min) (:y max))
        :when (not (:perch? s))]
    s))

(defn outside? [{:keys [min max]} x z]
  (not (and (<= (:x min) x (:x max)) (<= (:z min) z (:z max)))))

;; ------------------------------------------------------------------ the fill

(defn fill
  "Breadth first from seeds ({:key :s}) until the queue is empty or max-cells are seen. With a box, a step
  into a column outside it is recorded as an exit and not taken. {:seen {key {:s :from}} :order [key]
  :exits [{:from key :to key}] :truncated? bool :last key}."
  [world seeds box max-cells]
  (loop [queue (into cljs.core/PersistentQueue.EMPTY (map :key) seeds)
         seen (into {} (map (fn [{:keys [key s]}] [key {:s s :from nil}])) seeds)
         order (mapv :key seeds)
         exits []]
    (cond
      (empty? queue) {:seen seen :order order :exits exits :truncated? false}
      (>= (count seen) max-cells) {:seen seen :order order :exits exits :truncated? true :last (peek order)}
      :else
      (let [[x _ z :as k] (peek queue)
            s (get-in seen [k :s])
            steps (moves world x z s)
            [seen' order' exits' queue']
            (reduce (fn [[seen order exits queue] [[dx dz] {:keys [top]}]]
                      (let [nx (+ x dx) nz (+ z dz)
                            nk [nx (floor-cell top) nz]]
                        (cond
                          (and box (outside? box nx nz)) [seen order (conj exits {:from k :to nk :top top}) queue]
                          (contains? seen nk) [seen order exits queue]
                          :else [(assoc seen nk {:s top :from k}) (conj order nk) exits (conj queue nk)])))
                    [seen order exits (pop queue)]
                    steps)]
        (recur queue' seen' order' exits')))))

(defn path-to
  "The cells from a seed to key, each {:key :s}."
  [seen key]
  (loop [k key acc ()]
    (if-not k
      (vec acc)
      (recur (get-in seen [k :from]) (conj acc {:key k :s (get-in seen [k :s])})))))

;; ------------------------------------------------------------------ where it leaks

(defn barrier-at?
  "True when cell x y z holds something that blocks an animal standing there (more than half a block high)."
  [{:keys [cell]} x y z]
  (let [c (cell x y z)]
    (boolean (and c (> (:hi c) 0.5)))))

(defn barrier-both-sides?
  "True when a barrier stands within d cells on both opposite sides of x y z, along one axis."
  [world [x y z] d]
  (let [side (fn [dx dz] (some #(barrier-at? world (+ x (* dx %)) y (+ z (* dz %))) (range 1 (inc d))))]
    (boolean (or (and (side 1 0) (side -1 0))
                 (and (side 0 1) (side 0 -1))))))

(defn open-gate-at? [{:keys [block]} [x y z]]
  (let [b (block x y z)]
    (boolean (and (gate? b) (on? b "open")))))

(defn ridge?
  "True when the path climbs onto cell i and steps down off it again: a wall or fence top crossed."
  [path i]
  (let [s (fn [j] (:s (nth path j)))]
    (and (< 0 i (dec (count path)))
         (> (s i) (+ (s (dec i)) 0.25))
         (> (s i) (+ (s (inc i)) 0.25)))))

(defn evidence
  "The first cell of path that shows how the animal got out, {:index :why}, or nil: an open gate, then a gap
  with a barrier either side at 1 cell, then a ridge climbed over, then a gap at 2 cells."
  [world path]
  (let [first-where (fn [pred] (first (filter pred (range (count path)))))
        key-at (fn [i] (:key (nth path i)))]
    (or (some->> (first-where #(open-gate-at? world (key-at %))) (hash-map :why :open-gate :index))
        (some->> (first-where #(barrier-both-sides? world (key-at %) 1)) (hash-map :why :gap :index))
        (some->> (first-where #(ridge? path %)) (hash-map :why :climb :index))
        (some->> (first-where #(barrier-both-sides? world (key-at %) 2)) (hash-map :why :gap :index)))))

(defn leak-at
  "The leak {:pos :why} on path, a path that ends outside. With box? the crossing itself is the answer when
  nothing shows better; without it, nil when nothing shows."
  [world path box?]
  (let [tail (if box? (vec (take-last 3 path)) path)
        found (evidence world tail)
        end (last path)
        pos (fn [[x y z]] {:x x :y y :z z})]
    (cond
      found {:pos (pos (:key (nth tail (:index found)))) :why (:why found)}
      (not box?) nil
      (apply not= (map :s tail)) {:pos (pos (:key end)) :why :climb}
      :else {:pos (pos (:key end)) :why :open})))

;; ------------------------------------------------------------------ gates

(defn gates-near
  "Every fence gate in or beside the cells, {:pos :open?}, in position order."
  [{:keys [block]} cells]
  (->> cells
       (mapcat (fn [[x y z]] (for [[dx dz] (cons [0 0] orthogonal)] [(+ x dx) y (+ z dz)])))
       distinct
       (keep (fn [[x y z :as k]]
               (let [b (block x y z)]
                 (when (gate? b) {:pos {:x x :y y :z z} :open? (on? b "open")}))))
       (sort-by (juxt (comp :x :pos) (comp :y :pos) (comp :z :pos)))
       vec))

;; ------------------------------------------------------------------ the answer

(defn answer [closed? reason inside leaks gates]
  {:closed? closed? :reason reason :inside inside :leaks leaks :gates gates})

(defn unloaded-leaks [{:keys [unloaded]}]
  (->> @unloaded sort (take 5) (mapv (fn [[x y z]] {:pos {:x x :y y :z z} :why :unloaded}))))

(defn dedupe-leaks [leaks]
  (vec (first (reduce (fn [[out seen] l]
                        (if (contains? seen (:pos l)) [out seen] [(conj out l) (conj seen (:pos l))]))
                      [[] #{}] leaks))))

(defn analyse
  "One fill: {:world :fill :leaks} with the leaks found (the unloaded cells too); nil when there is no start."
  [{:keys [block-at at box max-cells sealed]}]
  (let [world (make-world block-at sealed)
        seeds (cond
                at (seeds-at world at)
                box (seeds-in-box world box))]
    (when (seq seeds)
      (let [{:keys [seen exits truncated?] end :last :as fill} (fill world seeds box max-cells)
            found (cond
                    (seq exits) (keep #(leak-at world (conj (path-to seen (:from %)) {:key (:to %) :s (:top %)}) true) exits)
                    (and truncated? (not box)) (keep identity [(leak-at world (path-to seen end) false)])
                    :else [])]
        {:world world
         :fill fill
         :leaks (dedupe-leaks (concat found (unloaded-leaks world)))}))))

(defn pen-without-leaks
  "The cells the pen would enclose with its leaks shut, or nil when shutting up to max-seals rounds of them does
  not close it. Each pass walls off the leaks the last fill found; a climbed wall is no cell to shut."
  [opts first-leaks]
  (loop [sealed #{} leaks first-leaks n 0]
    (let [real (remove #(= :unloaded (:why %)) leaks)]
      (when (and (seq real) (not-any? #(= :climb (:why %)) real) (< n (max-seals)))
        (let [sealed (into sealed (map (fn [{{:keys [x y z]} :pos}] [x y z])) real)
              {:keys [fill leaks]} (analyse (assoc opts :sealed sealed))]
          (if (:truncated? fill)
            (recur sealed leaks (inc n))
            fill))))))

(defn check
  "The pen check.
  opts: :block-at (required), :at {:x :y :z} (feet cell to start from) and/or :box {:min :max} (every surface in it
  starts the fill and a step out of it is a leak), :max-cells (default 2000).
  Returns {:closed? :reason :inside :leaks :gates}.
  :reason is nil when closed, else :leak, :unbounded (more than :max-cells reached and no way out pinned down; give
  a :box for exact crossings), :unloaded (a needed cell was not loaded) or :no-start.
  :inside is a set of [x y z] feet cells. For a leaky pen without a box it is what the pen encloses with its leaks
  shut, or empty when that does not close it.
  :leaks is [{:pos {:x :y :z} :why}], :why one of :open-gate :gap :climb :open :unloaded."
  [{:keys [max-cells] :or {max-cells default-max-cells} :as opts}]
  (let [opts (assoc opts :max-cells max-cells :sealed #{})
        {:keys [world fill leaks]} (analyse opts)]
    (if-not world
      (answer false :no-start #{} [] [])
      (let [{:keys [order truncated?]} fill
            real? (some #(not= :unloaded (:why %)) leaks)
            reason (cond real? :leak
                         truncated? :unbounded
                         (seq leaks) :unloaded)
            closed? (nil? reason)
            shut (when (and (not closed?) (not (:box opts)) truncated?) (pen-without-leaks opts leaks))
            cells (cond
                    (or closed? (:box opts) (not truncated?)) order
                    shut (:order shut)
                    :else [])]
        (answer closed? reason (set cells) leaks
                (gates-near world (if (and truncated? (not shut)) (take (gate-scan) order) cells)))))))

(defn in-pen?
  "True when position pos ({:x :y :z}, floats) is on a feet cell of the pen's inside."
  [{:keys [inside]} {:keys [x y z]}]
  (contains? inside [(js/Math.floor x) (js/Math.floor (+ y 0.01)) (js/Math.floor z)]))

(defn max-listed [] (settings/get settings ::max-listed))

(defn summary
  "The answer as the event and result carry it: the cells counted, the leaks capped."
  [{:keys [closed? reason inside leaks gates]}]
  (cond-> {:closed? closed? :reason reason :cells (count inside) :leaks (vec (take (max-listed) leaks)) :gates gates}
    (> (count leaks) (max-listed)) (assoc :leaks-total (count leaks))))
