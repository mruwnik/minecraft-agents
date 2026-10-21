(ns jobs.lib.trees
  "Helpers the forestry jobs (jobs.forestry.*) share: finding trees, the
  replant debts in body memory, saplings. Not a job namespace."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(a/defargs settings
  {::leaf-reach {:default 3.5 :doc "Blocks within which a leaf block counts as part of a tree." :spec (a/num-in 0 nil)}
   ::axe-saved-s {:default 1.5 :doc "Seconds an axe saves on one log over bare hands (3 s by hand, 1.5 s with a wooden axe)." :spec (a/num-in 0 nil)}
   ::axe-fetch-minutes {:default 1 :doc "The longest an axe is fetched for, in minutes (an axe is a speed-up: chests and crafting only)." :spec (a/num-in 0.1 nil)}
   ::max-logs {:default 1024 :doc "The most logs one scan reads." :spec (a/int-in 1 nil)}})

(def default-radius 16)

(defn axe-worth-fetching?
  "Whether an axe pays for felling n logs: the seconds it saves beat a fetch trip (the axe itself lasts for later trees)."
  [n]
  (> (* n (settings/get settings ::axe-saved-s)) (cost/trip)))
(defn axe-fetch-limits [] {:how #{:chest :craft} :minutes (settings/get settings ::axe-fetch-minutes)})
(defn leaf-reach [] (settings/get settings ::leaf-reach))
(def max-partials
  "Consecutive partial walks toward one tree before it counts as unreachable."
  3)

(def log-names
  "The overworld log blocks (the stem woods are not logs)."
  (mapv #(str % "_log") ["oak" "spruce" "birch" "jungle" "acacia" "dark_oak" "mangrove" "cherry" "pale_oak"]))

(def species
  "Every species a sapling grows (the logs' species and the two stem woods)."
  (conj (mapv #(subs % 0 (- (count %) 4)) log-names) "crimson" "warped"))

(defn log-name? [n] (boolean (some-> n (str/ends-with? "_log"))))
(defn leaves-name? [n] (boolean (some-> n (str/ends-with? "_leaves"))))
(defn species-of [log-name] (str/replace log-name #"_log$" ""))

(defn max-logs [] (settings/get settings ::max-logs))
(def max-leaves
  "The most leaves one scan reads: their own cap, so a dense forest's far trees keep theirs."
  4096)

(defn scan
  "The blocks the body has seen that match, nearest first, as {:name :pos}, at most max."
  [p radius match max]
  (mapv #(select-keys % [:name :pos]) (look/seen-blocks p {:radius radius :max max :match match :live? true})))

(defn scan-logs [p radius species]
  (scan p radius (if species #(= % (str species "_log")) log-name?) (max-logs)))

(defn find-trees
  "The log columns whose top log has leaves close by, nearest first (lazily), each as
  {:column {:x :z} :base pos :species name}. logs and leaves are scan results; columns in excluded (a set of [x z])
  are skipped."
  [logs leaves excluded]
  (let [logs (remove (fn [{:keys [pos]}] (excluded [(:x pos) (:z pos)])) logs)
        columns (group-by (fn [{:keys [pos]}] [(:x pos) (:z pos)]) logs)
        tree? (fn [col]
                (let [top (apply max-key #(get-in % [:pos :y]) col)]
                  (some #(<= (u/dist (:pos top) (:pos %)) (leaf-reach)) leaves)))]
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

(def dig-reach u/eye-reach)

(defn unreachable-set [memory]
  (set (map vec (:unreachable memory))))

(def replant-kind
  "Body memory kind of replant debts: {:pos base :species name} per felled
  tree, oldest first, until a sapling is planted there."
  :forestry/replant)

(def replant-policy {:cap 50 :ttl :forever})

(def unreachable-kind
  "Entries {:column [x z]} a fell-tree ending :tree-unreachable writes: the part-cut-tree trigger leaves those columns alone while they last."
  :forestry/tree-unreachable)

(def unreachable-policy {:cap 20 :ttl (* 10 60 1000)})

(defn recently-unreachable
  "The set of [x z] columns fell-tree gave up on lately, read from memory (a view or the body's)."
  [memory]
  (into #{} (map (comp :column :data)) (mem/entries memory unreachable-kind)))

(defn debts [c]
  (mapv :data (ctx/entries c replant-kind)))

(defn on-crop-plot?
  "Whether the debt cell pos is a planned crop cell or the soil under one: a tree is not put back into a field."
  [c {:keys [x y z]}]
  (let [crops (known/crop-cells c)]
    (boolean (or (crops [x y z]) (crops [x (inc y) z])))))

(defn forget-plot-debts!
  "Drop the debts that lie on a planned crop plot."
  [c]
  (ctx/forget-where! c replant-kind #(on-crop-plot? c (:pos %))))

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

(defn sapling-name?
  "Whether n names what a tree is planted as: a sapling, a mangrove propagule or a fungus (sapling-of's names)."
  [n]
  (boolean (and n (or (str/ends-with? n "_sapling") (= "mangrove_propagule" n) (str/ends-with? n "_fungus")))))

(defn sapling-for
  "The name of a sapling carried, of species when given. Without a species a fungus is not taken: it grows only on nylium."
  [items species]
  (let [want (when species (sapling-of species))]
    (->> items
         (map :name)
         (filter #(if want (= want %) (and (sapling-name? %) (not (str/ends-with? % "_fungus")))))
         first)))

(def unloaded-kind
  "Entries {:pos} of replant debts plant-sapling could not reach because their cell is not loaded: the replant-owed trigger
  leaves them alone while they last."
  :forestry/replant-unloaded)

(def unloaded-policy {:cap 50 :ttl (* 2 60 1000)})

(defn without-unloaded
  "The debts minus those plant-sapling found unloaded lately, read from memory (a view or the body's)."
  [memory debts]
  (let [skip (into #{} (map (comp :pos :data)) (mem/entries memory unloaded-kind))]
    (remove (comp skip :pos) debts)))

(defn with-sapling
  "The debts whose species' sapling is among items (a fungus included when its species wants one)."
  [debts items]
  (let [have (set (map :name items))]
    (filterv #(have (sapling-of (:species %))) debts)))

(def all-drops
  (vec (concat log-names (map #(sapling-of %) species) ["stick" "apple"])))

(defn drop-filter
  "The items a felled tree of the species (any species when nil) leaves: its log, planting item, sticks and apples."
  [sp]
  (if sp
    [(str sp "_log") (sapling-of sp) "stick" "apple"]
    all-drops))