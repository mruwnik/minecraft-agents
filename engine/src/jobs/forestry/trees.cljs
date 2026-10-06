(ns jobs.forestry.trees
  "Helpers the forestry jobs (jobs.forestry.*) share: finding trees, the
  replant debts in body memory, saplings. Not a job namespace."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

(def default-radius 16)
(def leaf-reach 3.5)
(def max-partials
  "Consecutive partial walks toward one tree before it counts as unreachable."
  3)

(def log-names
  "The overworld log blocks (the stem woods are not logs)."
  (mapv #(str % "_log") ["oak" "spruce" "birch" "jungle" "acacia" "dark_oak" "mangrove" "cherry"]))

(defn log-name? [n] (boolean (some-> n (str/ends-with? "_log"))))
(defn leaves-name? [n] (boolean (some-> n (str/ends-with? "_leaves"))))
(defn species-of [log-name] (str/replace log-name #"_log$" ""))

(def max-logs "The most logs one scan reads." 1024)
(def max-leaves
  "The most leaves one scan reads: their own cap, so a dense forest's far trees keep theirs."
  4096)

(defn scan
  "The blocks the body has seen that match, nearest first, as {:name :pos}, at most max."
  [p radius match max]
  (mapv #(select-keys % [:name :pos]) (look/seen-blocks p {:radius radius :max max :match match :live? true})))

(defn scan-logs [p radius species]
  (scan p radius (if species #(= % (str species "_log")) log-name?) max-logs))

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

(defn trees-near
  "The trees of species (any when nil) within radius, nearest first (find-trees)."
  ([p radius species] (trees-near p radius species #{}))
  ([p radius species excluded]
   (find-trees (scan-logs p radius species) (scan p (+ radius 4) leaves-name? max-leaves) excluded)))

(defn tree-near
  ([p radius species] (tree-near p radius species #{}))
  ([p radius species excluded]
   (first (trees-near p radius species excluded))))

(def max-column
  "How far up a column logs-at looks, in blocks."
  40)

(defn seen-name
  "The name of the block at pos as the body last saw it, nil for a cell it has not seen."
  [p pos]
  (:name (look/seen-block p pos)))

(defn logs-at
  "The logs of species in the column over pos, lowest first, read cell by cell up from pos (as seen). Skips the air where the
  lowest logs were dug and ends at the first other block above the first log."
  [p {:keys [x y z]} species]
  (let [log (str species "_log")]
    (->> (range max-column)
         (map (fn [dy] (let [pos {:x x :y (+ y dy) :z z}] {:name (seen-name p pos) :pos pos})))
         (drop-while #(not= log (:name %)))
         (take-while #(= log (:name %)))
         vec)))

(defn tree-at
  "The tree whose base log is at pos, as find-trees gives it, or nil when pos holds no seen log. Leaves are not asked for."
  [p pos]
  (let [n (seen-name p pos)]
    (when (some-> n log-name?)
      {:column {:x (:x pos) :z (:z pos)} :base pos :species (species-of n)})))

(def dig-reach
  "Eye-to-centre distance within which a block is dug without walking (the primitive accepts 4.5)."
  4.2)

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

(defn near-debt?
  "True when the debt's cell is within radius blocks (flat distance) of near; always when near is nil."
  [near radius d]
  (or (nil? near)
      (<= (js/Math.hypot (- (:x near) (:x (:pos d))) (- (:z near) (:z (:pos d)))) radius)))

(defn pick-debt
  "The oldest debt of species (any when nil), within radius of near when given."
  ([debts species] (pick-debt debts species nil nil))
  ([debts species near radius]
   (->> debts
        (filter #(or (nil? species) (= species (:species %))))
        (filter #(near-debt? near radius %))
        first)))

(defn target-of
  "The planting plan {:pos :species :debt} from args and the replant debts, or nil."
  [debts args]
  (if-let [at (:at args)]
    {:pos at :species (:species args)}
    (when-let [d (pick-debt debts (:species args) (:near args) (:within args))]
      {:pos (:pos d) :species (or (:species args) (:species d)) :debt d})))

(defn sapling-of [species]
  (case species
    "mangrove" "mangrove_propagule"
    ("crimson" "warped") (str species "_fungus")
    (str species "_sapling")))

(defn sapling-for
  "The name of a sapling carried, of species when given."
  [items species]
  (let [want (when species (sapling-of species))]
    (->> items
         (map :name)
         (filter #(if want (= want %) (or (str/ends-with? % "_sapling") (= "mangrove_propagule" %))))
         first)))

(defn drop-filter [species]
  (when species [(str species "_log") (sapling-of species) "stick" "apple"]))
