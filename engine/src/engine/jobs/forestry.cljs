(ns engine.jobs.forestry
  "Wood jobs: fell a tree, collect what dropped, replant, and the composite
  that does all three. Contracts in README.md, section Job library."
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

(defn find-tree
  "The nearest log column whose top log has leaves close by, as
  {:column {:x :z} :base pos :species name}, or nil. logs and leaves come
  nearest first from scan; columns in excluded (a set of [x z]) are skipped."
  [logs leaves excluded]
  (let [logs (remove (fn [{:keys [pos]}] (excluded [(:x pos) (:z pos)])) logs)
        columns (group-by (fn [{:keys [pos]}] [(:x pos) (:z pos)]) logs)
        tree? (fn [col]
                (let [top (apply max-key #(get-in % [:pos :y]) col)]
                  (some #(<= (u/dist (:pos top) (:pos %)) leaf-reach) leaves)))]
    (some (fn [{:keys [pos]}]
            (let [col (get columns [(:x pos) (:z pos)])]
              (when (tree? col)
                (let [base (apply min-key #(get-in % [:pos :y]) col)]
                  {:column {:x (:x pos) :z (:z pos)}
                   :base (:pos base)
                   :species (species-of (:name base))}))))
          logs)))

(defn tree-near
  ([p radius species] (tree-near p radius species #{}))
  ([p radius species excluded]
   (find-tree (scan-logs p radius species) (scan p (+ radius 4) leaves-name?) excluded)))

(defn unreachable-set [memory]
  (set (map vec (:unreachable memory))))

(def replant-kind
  "Body memory kind of replant debts: {:pos base :species name} per felled
  tree, oldest first, until a sapling is planted there."
  :forestry/replant)

(def replant-policy {:cap 50 :ttl :forever})

(defn debts [c]
  (mapv :data (ctx/entries c replant-kind)))

;; ---------------------------------------------------------------- fell-tree

(defn column-logs
  "Logs standing in the chosen column, lowest first."
  [p radius {:keys [column species]}]
  (->> (scan-logs p radius species)
       (filter #(and (= (:x column) (get-in % [:pos :x])) (= (:z column) (get-in % [:pos :z]))))
       (sort-by #(get-in % [:pos :y]))))

(defn record-debt!
  "Write the replant debt for the tree this job chose, once."
  [c]
  (let [{:keys [base species]} (ctx/mem c)]
    (when-not (some #(= base (:pos %)) (debts c))
      (ctx/remember! c replant-kind {:pos base :species species} replant-policy))))

(defn ^:async dig-up!
  "Dig the logs in order, walking in reach first. Commits the replant debt
  when the base log is dug. Resolves to :ok, :partial (the walk made progress
  but is not in reach yet; call again) or a non-ok walk or dig status for the
  caller to count as a failure (:unreachable, :cannot and :out-of-reach mean
  the tree cannot be dug from here and are marked unreachable by the round)."
  [c logs]
  (loop [[l & more] logs]
    (if-not l
      :ok
      (let [w (await (u/walk-near! c (:pos l) 3))]
        (case w
          :blocked :blocked
          :partial :partial
          (let [r (await (ctx/act c :dig (clj->js {:pos (:pos l)})))]
            (if (#{"dug" "missing"} (.-status r))
              (do (when (and (= "dug" (.-status r)) (= (:pos l) (:base (ctx/mem c))))
                    (record-debt! c))
                  (recur more))
              (keyword (.-status r)))))))))

(defn choose-tree!
  "Commit the column, species and base of the nearest tree not marked
  unreachable; nil when no candidate is in sight."
  [c radius species]
  (when-let [t (tree-near (:primitives c) radius species (unreachable-set (ctx/mem c)))]
    (ctx/update-mem! c #(-> % (merge (select-keys t [:column :species :base])) (assoc :partials 0)))
    t))

(defn mark-unreachable!
  "Remember the chosen column as unreachable and forget the choice."
  [c]
  (ctx/update-mem! c (fn [m] (-> m
                             (update :unreachable (fnil conj []) [(get-in m [:column :x]) (get-in m [:column :z])])
                             (dissoc :column :species :base)
                             (assoc :partials 0)))))

(defn walk-failed!
  "Book a walk result of :blocked or :partial against the chosen tree."
  [c r]
  (let [partials (inc (:partials (ctx/mem c) 0))]
    (if (or (= :blocked r) (>= partials max-partials))
      (mark-unreachable! c)
      (ctx/update-mem! c assoc :partials partials))
    :continue))

(defn ^:async fell-tree-round
  "args {:species name-or-nil :radius 16}. Picks a tree (log column with
  leaves) the first round and remembers the column, then digs up to two logs
  bottom-up per round. Writes the replant debt {:pos base :species} to body
  memory kind :forestry/replant when the base log is dug. A tree whose walk is
  :blocked, or partial three times in a row, is remembered as unreachable and
  the next candidate is chosen; with none left it warns tree_blocked and
  finishes. Done when the column holds no logs."
  [c]
  (let [{:keys [species radius] :or {radius default-radius}} (:args c)
        chosen (or (:column (ctx/mem c)) (choose-tree! c radius species))]
    (cond
      (and (not chosen) (seq (:unreachable (ctx/mem c))))
      (do (ctx/emit! c :tree_blocked :warn {:text "no reachable tree"})
          :done)

      (not chosen) :continue

      :else
      (let [logs (column-logs (:primitives c) radius (ctx/mem c))]
        (if (empty? logs)
          :done
          (let [r (await (dig-up! c (take logs-per-round logs)))]
            (case r
              :ok (do (ctx/update-mem! c assoc :partials 0) :continue)
              (:partial :blocked) (walk-failed! c r)
              (:unreachable :cannot :out-of-reach) (do (mark-unreachable! c) :continue)
              (u/fail! c :tree_blocked (str "cannot dig the tree: " (name r))))))))))

(defn fell-tree-check
  "A tree is chosen, or every candidate was unreachable (the round warns and
  finishes), or a tree is in sight."
  [c]
  (let [m (ctx/mem c)
        {:keys [radius species] :or {radius default-radius}} (:args c)]
    (boolean (or (:column m)
                 (seq (:unreachable m))
                 (tree-near (:primitives c) radius species)))))

(def fell-tree {:name :fell-tree :check fell-tree-check :round fell-tree-round})

;; ------------------------------------------------------------ collect-drops

(defn ^:async collect-drops-round
  "args {:radius 16 :filter [item names] or nil}. Collects the nearest
  matching dropped item, one per round. Items that could not be reached are
  remembered in job memory and skipped. Done when none are left in radius."
  [c]
  (let [{:keys [radius] :or {radius default-radius}} (:args c)
        wanted (some-> (:filter (:args c)) set)
        skipped (set (:skipped (ctx/mem c)))
        item (->> (array-seq (.entities (:primitives c) #js {:radius radius :kind "item" :max 32}))
                  (remove #(skipped (.-id %)))
                  (filter #(or (nil? wanted) (wanted (some-> (.-item %) .-name))))
                  first)]
    (if-not item
      :done
      (let [r (await (ctx/act c :collect #js {:id (.-id item)}))]
        (when (#{"unreachable" "timeout"} (.-status r))
          (ctx/update-mem! c update :skipped (fnil conj []) (.-id item)))
        :continue))))

(def collect-drops {:name :collect-drops :check (constantly true) :round collect-drops-round})

;; ------------------------------------------------------------ plant-sapling

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

(defn plant-sapling-check
  "Nothing to plant (the round finishes), or a matching sapling is carried
  and the spot no longer holds a log."
  [c]
  (let [p (:primitives c)
        t (target-of (debts c) (:args c))]
    (cond
      (nil? t) true
      (nil? (sapling-for (u/inventory p) (:species t))) false
      (log-name? (some-> (.blockAt p (clj->js (:pos t))) .-name)) false
      :else true)))

(defn ^:async plant-sapling-round
  "args {:at pos-or-nil :species name-or-nil}. Without :at, plants at the
  oldest replant debt in body memory (of species, when given). Equips a
  sapling, places it and clears the debt. Done at once when there is nothing
  to plant."
  [c]
  (let [t (target-of (debts c) (:args c))
        sapling (when t (sapling-for (u/inventory (:primitives c)) (:species t)))]
    (cond
      (nil? t) :done
      (nil? sapling) :continue
      :else
      (let [w (await (u/walk-near! c (:pos t) 3))]
        (case w
          :partial :continue
          :blocked (u/fail! c :plant_blocked "cannot reach the planting spot")
          (do (await (ctx/act c :equip (clj->js {:item sapling})))
              (let [r (await (ctx/act c :place (clj->js {:pos (:pos t) :item sapling})))]
                (if (#{"placed" "occupied"} (.-status r))
                  (do (ctx/forget-where! c replant-kind #(= (:pos t) (:pos %)))
                      :done)
                  (u/fail! c :plant_blocked (str "cannot plant: " (.-status r)))))))))))

(def plant-sapling {:name :plant-sapling :check plant-sapling-check :round plant-sapling-round})

;; -------------------------------------------------------------- harvest-wood

(defn drop-filter [species]
  (when species [(str species "_log") (str species "_sapling") "stick" "apple"]))

(defn phases
  "The children of harvest-wood in order: [phase slot def args]."
  [{:keys [species radius filter] :or {radius default-radius}}]
  [[:fell :fell fell-tree {:species species :radius radius}]
   [:collect :collect collect-drops {:radius radius :filter (or filter (drop-filter species))}]
   [:plant :plant plant-sapling {:species species}]])

(defn current-phase
  "The [phase slot def args] harvest-wood is in, from its memory."
  [c]
  (let [phase (:phase (ctx/mem c) :fell)]
    (some #(when (= phase (first %)) %) (phases (:args c)))))

(defn harvest-wood-check
  "The current phase's child would run: its check, against its sub-map."
  [c]
  (let [[_ slot def args] (current-phase c)]
    (boolean (ctx/check-child c slot def args))))

(defn ^:async harvest-wood-round
  "args {:species name-or-nil :radius 16 :filter names-or-nil}. Phases :fell,
  :collect and :plant, each one child in the slot of the same name. A round
  steps the current phase's child once; when the child is done the phase
  advances. Done when the :plant child is done. A declined child is
  :continue (its check normally keeps the round from running at all)."
  [c]
  (let [[phase slot def args] (current-phase c)
        r (await (ctx/call-child c slot def args))
        next-phase (second (drop-while #(not= phase %) (map first (phases (:args c)))))]
    (cond
      (not= :done r) :continue
      (nil? next-phase) :done
      :else (do (ctx/update-mem! c assoc :phase next-phase) :continue))))

(def harvest-wood {:name :harvest-wood :check harvest-wood-check :round harvest-wood-round})
