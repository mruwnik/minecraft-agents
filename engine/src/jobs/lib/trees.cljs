(ns jobs.lib.trees
  "Helpers the forestry jobs (jobs.forestry.*) share: finding trees, the
  replant debts in body memory, saplings. Not a job namespace."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.cost :as cost]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(a/defargs settings
  {::leaf-reach {:default 3.5 :doc "Blocks within which a leaf block counts as part of a tree." :spec (a/num-in 0 nil)}
   ::axe-saved-s {:default 1.5 :doc "Seconds an axe saves on one log over bare hands (3 s by hand, 1.5 s with a wooden axe)." :spec (a/num-in 0 nil)}
   ::axe-fetch-minutes {:default 1 :doc "The longest an axe is fetched for, in minutes (an axe is a speed-up: chests and crafting only)." :spec (a/num-in 0.1 nil)}
   ::replant-reach {:default 96 :doc "Blocks (flat) from the body within which a replant debt is acted on; a farther one waits until the body is near." :spec (a/num-in 0 nil)}
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

(def log-names facts/log-names)

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
  {:column {:x :z} :base pos :species name}. With leafless? and none such in logs, the leafless columns instead (a stump,
  a log pile, a village's frame are still logs to cut). logs and leaves are scan results; columns in excluded (a set of
  [x z]) are skipped."
  [logs leaves excluded leafless?]
  (let [logs (remove (fn [{:keys [pos]}] (excluded [(:x pos) (:z pos)])) logs)
        columns (group-by (fn [{:keys [pos]}] [(:x pos) (:z pos)]) logs)
        leafy? (fn [col]
                 (let [top (apply max-key #(get-in % [:pos :y]) col)]
                   (some #(<= (u/dist (:pos top) (:pos %)) (leaf-reach)) leaves)))
        trees (fn [keep-col?]
                (keep (fn [[x z :as k]]
                        (let [col (get columns k)]
                          (when (keep-col? col)
                            (let [base (apply min-key #(get-in % [:pos :y]) col)]
                              {:column {:x x :z z}
                               :base (:pos base)
                               :species (species-of (:name base))}))))
                      (distinct (map (fn [{:keys [pos]}] [(:x pos) (:z pos)]) logs))))]
    (or (seq (trees leafy?)) (when leafless? (trees (complement leafy?))))))

(defn trees-near
  "The trees of species (any when nil) within radius, nearest first (find-trees)."
  ([p radius species] (trees-near p radius species #{} false))
  ([p radius species excluded] (trees-near p radius species excluded false))
  ([p radius species excluded leafless?]
   (find-trees (scan-logs p radius species) (scan p (+ radius 4) leaves-name? max-leaves) excluded leafless?)))

(defn tree-near
  ([p radius species] (tree-near p radius species #{} false))
  ([p radius species excluded leafless?]
   (first (trees-near p radius species excluded leafless?))))

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

(def need-kind
  "Entries {:column [x z] or :any, :any-of [names] :count n, :trees [[x z]..] (with :any: the part-cut columns in sight)}, one per column, a fell-tree ending :need writes: the part-cut-tree trigger leaves the column alone until the body carries what the need named."
  :forestry/tree-need)

(def need-policy {:cap 20 :ttl :forever})

(defn needing
  "The set of [x z] columns (or :any) a fell-tree need ending named items for that the inventory inv still lacks, read from
  memory. An :any need counts only while every column of cols (the part-cut columns in sight now) is one it was written with."
  [memory inv cols]
  (let [carried (fn [names] (transduce (comp (filter (comp (set names) :name)) (map :count)) + 0 inv))
        known? (fn [{:keys [column trees]}] (or (not= :any column) (every? (set trees) cols)))]
    (into #{} (comp (map :data) (filter #(< (carried (:any-of %)) (:count % 1))) (filter known?) (map :column)) (mem/entries memory need-kind))))

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

(defn debt-near-body?
  "True when the debt's cell is within ::replant-reach of the body (flat distance)."
  [p pos]
  (<= (u/flat-dist (u/pos-of (.-pos (.self p))) pos) (settings/get settings ::replant-reach)))

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

(def stalled-kind
  "Entries {:pos} of replant debts plant-sapling failed to plant (the walk ended :not-loaded, :unreachable ..): the replant-owed trigger
  leaves them alone while they last."
  :forestry/replant-stalled)

(def stalled-policy {:cap 50 :ttl (* 2 60 1000)})

(defn without-stalled
  "The debts minus those plant-sapling failed on lately, read from memory (a view or the body's)."
  [memory debts]
  (let [skip (into #{} (map (comp :pos :data)) (mem/entries memory stalled-kind))]
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

(def tree-plants
  #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "vine" "glow_lichen" "dandelion" "poppy"
    "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip" "oxeye_daisy"
    "cornflower" "lily_of_the_valley" "torchflower" "sunflower" "lilac" "rose_bush" "peony" "pink_petals"
    "wildflowers" "leaf_litter" "short_dry_grass" "tall_dry_grass" "bush" "firefly_bush"})

(defn tree-free?
  "Whether a tree's growth may take the place of the block: air, leaves, saplings, plants, snow, vines, invisible light blocks."
  [n]
  (boolean (or (bs/air n) (= "light" n) (str/ends-with? n "_leaves") (str/ends-with? n "_sapling") (tree-plants n))))

(defn cell-filled?
  "Whether the cell seen at pos holds a block growth cannot take the place of and that is no tree log: a debt there cannot be planted."
  [p pos]
  (let [n (u/seen-name p pos)]
    (boolean (and n (not (tree-free? n)) (not (log-name? n))))))
