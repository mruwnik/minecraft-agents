(ns engine.jobs.forestry
  "Helpers the forestry jobs (jobs.forestry.*) share: finding trees, the
  replant debts in body memory, saplings. Not a job namespace."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def default-radius 16)
(def logs-per-round 2)
(def leaf-reach 3.5)
(def max-partials
  "Consecutive partial walks toward one tree before it counts as unreachable."
  3)

(defn log-name? [n] (str/ends-with? n "_log"))
(defn leaves-name? [n] (str/ends-with? n "_leaves"))
(defn species-of [log-name] (str/replace log-name #"_log$" ""))

(defn scan
  "Blocks from the JS side as cljs maps, nearest first."
  [p radius match]
  (mapv (fn [b] {:name (.-name b) :pos (u/pos-of (.-pos b))})
        (array-seq (.blocks p #js {:radius radius :max 256 :match match}))))

(defn scan-logs [p radius species]
  (scan p radius (if species #(= % (str species "_log")) log-name?)))

(defn find-trees
  "The log columns whose top log has leaves close by, nearest first (lazily), each as
  {:column {:x :z} :base pos :species name}. logs and leaves are scan results; columns in excluded (a set of [x z])
  are skipped."
  [logs leaves excluded]
  (let [logs (remove (fn [{:keys [pos]}] (excluded [(:x pos) (:z pos)])) logs)
        columns (group-by (fn [{:keys [pos]}] [(:x pos) (:z pos)]) logs)
        tree? (fn [col]
                (let [top (apply max-key #(get-in % [:pos :y]) col)]
                  (some #(<= (u/dist (:pos top) (:pos %)) leaf-reach) leaves)))]
    (keep (fn [[x z :as k]]
            (let [col (get columns k)]
              (when (tree? col)
                (let [base (apply min-key #(get-in % [:pos :y]) col)]
                  {:column {:x x :z z}
                   :base (:pos base)
                   :species (species-of (:name base))}))))
          (distinct (map (fn [{:keys [pos]}] [(:x pos) (:z pos)]) logs)))))

(defn find-tree
  "The nearest tree of find-trees, or nil."
  [logs leaves excluded]
  (first (find-trees logs leaves excluded)))

(defn trees-near
  "The trees of species (any when nil) within radius, nearest first (find-trees)."
  ([p radius species] (trees-near p radius species #{}))
  ([p radius species excluded]
   (find-trees (scan-logs p radius species) (scan p (+ radius 4) leaves-name?) excluded)))

(defn tree-near
  ([p radius species] (tree-near p radius species #{}))
  ([p radius species excluded]
   (first (trees-near p radius species excluded))))

(def max-column
  "How far up a column logs-at looks, in blocks."
  40)

(defn logs-at
  "The logs of species in the column over pos, lowest first, read cell by cell up from pos. Skips the air where the
  lowest logs were dug and ends at the first other block above the first log."
  [p {:keys [x y z]} species]
  (let [log (str species "_log")]
    (->> (range max-column)
         (map (fn [dy] (let [pos {:x x :y (+ y dy) :z z}] {:name (u/block-name p pos) :pos pos})))
         (drop-while #(not= log (:name %)))
         (take-while #(= log (:name %)))
         vec)))

(defn tree-at
  "The tree whose base log is at pos, as find-tree gives it, or nil when pos holds no log. Leaves are not asked for."
  [p pos]
  (let [n (u/block-name p pos)]
    (when (some-> n log-name?)
      {:column {:x (:x pos) :z (:z pos)} :base pos :species (species-of n)})))

(def eye-height 1.62)
(def dig-reach
  "Eye-to-centre distance within which a block is dug without walking (the primitive accepts 4.5)."
  4.2)

(defn eye-dist
  "Distance from the eye of a body at feet position here to the centre of cell."
  [here cell]
  (u/dist {:x (:x here) :y (+ (:y here) eye-height) :z (:z here)}
          {:x (+ (:x cell) 0.5) :y (+ (:y cell) 0.5) :z (+ (:z cell) 0.5)}))

(defn unreachable-set [memory]
  (set (map vec (:unreachable memory))))

(def replant-kind
  "Body memory kind of replant debts: {:pos base :species name} per felled
  tree, oldest first, until a sapling is planted there."
  :forestry/replant)

(def replant-policy {:cap 50 :ttl :forever})

(defn debts [c]
  (mapv :data (ctx/entries c replant-kind)))

;; ------------------------------------------------------------ planting

(defn pick-debt [debts species]
  (->> debts
       (filter #(or (nil? species) (= species (:species %))))
       first))

(defn target-of
  "The planting plan {:pos :species :debt} from args and the replant debts, or nil."
  [debts args]
  (if-let [at (:at args)]
    {:pos at :species (:species args)}
    (when-let [d (pick-debt debts (:species args))]
      {:pos (:pos d) :species (or (:species args) (:species d)) :debt d})))

(defn sapling-for
  "The name of a sapling carried, of species when given."
  [items species]
  (let [want (when species (str species "_sapling"))]
    (->> items
         (map :name)
         (filter #(if want (= want %) (str/ends-with? % "_sapling")))
         first)))

(defn drop-filter [species]
  (when species [(str species "_log") (str species "_sapling") "stick" "apple"]))
