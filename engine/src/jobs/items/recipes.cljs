(ns jobs.items.recipes
  "Which crafts make an item from what is carried. Pure: recipes come from minecraft-data for the body's version, the
  carried counts and whether a crafting table is at hand are arguments.

  (plan version have item n opts) -> {:steps [step ...]} or nil when no chain of crafts gets n more of item.
  A step is {:op :craft :item name :count n :table? bool}; a crafting table is a craft step before the first that
  needs one. Ingredients are taken from what is carried first, then crafted; a recipe
  whose ingredients are carried is tried before one that needs more crafts. opts: :table? (a table is at hand: no table
  steps), :max-depth (nested crafts, default 6), :gather? (items that are gatherable? and not carried count as
  gathered, not crafted: the plan then has :gather {name n}, what must be mined or felled before its crafts can run),
  :materials (the stone-materials that count as gatherable, default all; the cheapest recipe among them wins),
  :raw (more items that count as gathered: ore drops), :smelts ({product [input ..]}: a product is also made by a
  step {:op :smelt :item product :count n :input name}, one input per item; without it no smelt is planned).
  The plan also has :uses {name n}, the carried units its crafts take.
  (lacking version have item n) says why a craft cannot start."
  (:require ["minecraft-data" :as minecraft-data]
            [jobs.lib.game-facts :as facts]
            [clojure.string]
            [clojure.set]))

(def table-item "crafting_table")

(defn id-of [x]
  (cond (number? x) x (and (some? x) (number? (.-id x))) (.-id x)))

(defn needs-table?
  "Whether a recipe's grid is bigger than the 2x2 of the inventory."
  [r]
  (if-let [shape (.-inShape r)]
    (or (> (alength shape) 2) (some #(> (alength %) 2) (array-seq shape)))
    (> (alength (.-ingredients r)) 4)))

(defn recipes-of
  "{result name [{:count n made per craft :needs {name n} :table? bool}]} for a minecraft-data version."
  [version]
  (let [md (minecraft-data version)
        name-of (fn [id] (some-> (aget (.-items md) id) .-name))]
    (reduce (fn [acc [_ rs]]
              (reduce (fn [acc r]
                        (let [ids (if (.-inShape r) (mapcat array-seq (array-seq (.-inShape r))) (array-seq (.-ingredients r)))
                              needs (frequencies (keep #(some-> (id-of %) name-of) ids))
                              out (name-of (id-of (.-result r)))]
                          (if (and out (seq needs))
                            (update acc out (fnil conj []) {:count (max 1 (or (some-> (.-result r) .-count) 1))
                                                           :needs needs :table? (boolean (needs-table? r))})
                            acc)))
                      acc (array-seq rs)))
            {} (js/Object.entries (.-recipes md)))))

(defn slots-of
  "The ingredient ids of a recipe, in slot order (-1 for an empty cell)."
  [r]
  (if (.-inShape r)
    (map #(or (id-of %) -1) (mapcat array-seq (array-seq (.-inShape r))))
    (map #(or (id-of %) -1) (array-seq (.-ingredients r)))))

(defn interchange-classes
  "{name #{names}} for a version: items that stand in the same slot of recipes making the same result in the same layout
  (every plank species in a chest). Vanilla takes any member of such a tag in each slot, so carried members add up."
  [version]
  (let [md (minecraft-data version)
        name-of (fn [id] (some-> (aget (.-items md) id) .-name))
        groups (group-by (fn [r] [(id-of (.-result r)) (map #(= -1 %) (slots-of r))])
                         (mapcat (comp array-seq second) (js/Object.entries (.-recipes md))))
        pairs (for [rs (vals groups) :when (> (count rs) 1)
                    slot (apply map vector (map slots-of rs))
                    :let [names (set (keep name-of (remove #{-1} slot)))]
                    :when (> (count names) 1)]
                names)
        all (mapcat (comp array-seq second) (js/Object.entries (.-recipes md)))
        plank-logs (set (for [r all :let [out (name-of (id-of (.-result r)))]
                              :when (and out (clojure.string/ends-with? out "_planks") (= 1 (count (slots-of r))))]
                          (name-of (first (slots-of r)))))
        wood-of (into {} (for [r all :when (and (.-inShape r) (= 4 (count (slots-of r))) (apply = (slots-of r)))]
                           [(name-of (first (slots-of r))) (name-of (id-of (.-result r)))]))
        has-item? #(some? (aget (.-itemsByName md) %))
        ;; the planks tag takes a log, its wood and the stripped forms of both
        log-pairs (for [l plank-logs :let [sl (str "stripped_" l) forms (cond-> #{l} (wood-of l) (conj (wood-of l))
                                                                              (has-item? sl) (conj sl)
                                                                              (and (has-item? sl) (wood-of sl)) (conj (wood-of sl)))]
                        :when (> (count forms) 1)]
                    forms)
        ;; a campfire takes any log, but planks take only their own species' forms: the broad class would let an oak log pass for a cherry one
        pairs (concat (remove #(some plank-logs %) pairs) log-pairs)
        merged (reduce (fn [classes names]
                         (let [hit (filter #(seq (clojure.set/intersection % names)) classes)]
                           (conj (remove (set hit) classes) (apply clojure.set/union names hit))))
                       [] pairs)]
    (into {} (for [cls merged n cls] [n cls]))))

(def interchange (memoize interchange-classes))

(defn pooled
  "have with every member of an interchange class counted at the class total (also a member none is carried of): a
  recipe asking one species of plank, or the log of a stripped log carried, is satisfied by a mix. Enough to judge
  whether a craft can start, not to plan several crafts from one stock."
  [version have]
  (let [classes (interchange version)
        totals (into {} (keep (fn [[n cls]] (when (contains? have n) [cls (reduce + 0 (map #(get have % 0) cls))]))) classes)]
    (reduce (fn [acc [cls total]] (reduce #(assoc %1 %2 total) acc cls)) have totals)))

(def recipes-for (memoize recipes-of))

(def stone-materials
  "The blocks stone tools are made from for a version (cobblestone, cobbled_deepslate, blackstone), read off the
  stone_pickaxe recipes: every ingredient but the stick. Each is gathered like cobblestone."
  (memoize
   (fn [version]
     (->> (get (recipes-for version) "stone_pickaxe") (mapcat (comp keys :needs)) (remove #{"stick"}) set))))

(def material-ranks
  "{material rank} for a version, 0 the one the most recipes use (cobblestone): the tie-break between materials
  that gather equally."
  (memoize
   (fn [version]
     (let [uses (frequencies (for [rs (vals (recipes-for version)) r rs k (keys (:needs r))] k))]
       (into {} (map-indexed (fn [i m] [m i]) (sort-by #(- (uses % 0)) (stone-materials version))))))))

(defn ceil-div [a b] (js/Math.ceil (/ a b)))

(declare make lacking)

(defn gathered?
  "Whether item counts as gathered, not crafted, in plan state st."
  [st item]
  (boolean (and (:gather? st) (or (facts/gatherable? item) (contains? (:materials st) item) (contains? (:raw st) item)))))

(defn acquire
  "State st with n of item taken from what is carried, the rest crafted; nil when it cannot be had, or when item is
  one being made up the chain (a carried ingot is not crafted down to nuggets to make ingots)."
  [rs st item n chain depth]
  (when-not (chain item)
    (let [have (get-in st [:have item] 0)
          use (min have n)
          st (cond-> (update-in st [:have item] (fnil - 0) use)
               (pos? use) (update-in [:used item] (fnil + 0) use))
          deficit (- n use)]
      (cond
        (zero? deficit) st
        (gathered? st item) (if-let [crafted (make rs (assoc st :gather? false) item deficit chain depth)]
                              (-> crafted (assoc :gather? true) (update-in [:have item] - deficit))
                              (update-in st [:gather item] (fnil + 0) deficit))
        :else (when-let [st (make rs st item deficit chain depth)]
                (update-in st [:have item] - deficit))))))

(defn short-count
  "How many of the recipe's ingredients (for batches crafts) the state lacks."
  [st {:keys [needs]} batches]
  (reduce + 0 (map (fn [[k v]] (max 0 (- (* v batches) (get-in st [:have k] 0)))) needs)))

(defn make
  "State st with n more of item crafted (added to :have, steps appended), nil when no recipe works."
  [rs st item n chain depth]
  (when (and (pos? depth) (not (chain item)))
    (let [chain (conj chain item)
          smelts (for [input (get (:smelts st) item)] {:count 1 :needs {input 1} :smelt input})
          options (sort-by (fn [r] (short-count st r (ceil-div n (:count r)))) (concat (get rs item) smelts))
          try-option (fn [{:keys [needs] :as r}]
                       (let [batches (ceil-div n (:count r))
                             st' (reduce (fn [st [k v]] (or (acquire rs st k (* v batches) chain (dec depth)) (reduced nil)))
                                         st needs)]
                         (when st'
                           (-> st'
                               (update-in [:have item] (fnil + 0) (* batches (:count r)))
                               (update :steps conj (if (:smelt r)
                                                     {:op :smelt :item item :count (* batches (:count r)) :input (:smelt r)}
                                                     {:op :craft :item item :count (* batches (:count r)) :table? (:table? r)}))))))]
      (if (:gather? st)
        ;; the option with fewest smelts (a furnace needs fuel and time), then that gathers least (a carried species of log wins over a new one)
        (first (sort-by (juxt #(count (filter (comp #{:smelt} :op) (:steps %))) #(reduce + 0 (vals (:gather %))) #(reduce + 0 (map (:ranks st) (keys (:gather %)))))
                        (keep try-option options)))
        (some try-option options)))))

(defn lacking
  "Why no craft of item (n more) can start, as text: the cheapest recipe's ingredients short of what is carried
  (\"needs 3 stick, have 1\"; on a tie the commonest material, cobblestone), or that no recipe makes item. With no table
  carried, the table's own ingredients are taken from what is carried first. Pure."
  [version have item n]
  (let [rs (get (recipes-for version) item)
        ranks (material-ranks version)
        cheapest (fn [rs have batches-of] (first (sort-by (juxt #(short-count {:have have} % (batches-of %)) #(reduce + 0 (map (fn [k] (get ranks k 0)) (keys (:needs %))))) rs)))
        short-of (fn [r have n] (let [batches (ceil-div n (:count r))]
                                  (keep (fn [[k v]] (let [h (get have k 0)] (when (< h (* v batches)) (str "needs " (* v batches) " " k ", have " h))))
                                        (:needs r))))]
    (if (empty? rs)
      (str "no recipe makes " item)
      (let [r (cheapest rs have #(ceil-div n (:count %)))
            short (short-of r have n)
            table? (and (:table? r) (not= item table-item) (zero? (get have table-item 0)))
            tr (when table? (cheapest (get (recipes-for version) table-item) have (constantly 1)))
            after-table (when tr (reduce (fn [h [k v]] (update h k #(max 0 (- (or % 0) v)))) have (:needs tr)))
            short-after (when (and tr (empty? (short-of tr have 1))) (short-of r after-table n))]
        (str "crafting " item " "
             (cond
               (seq short) (clojure.string/join "; " short)
               (seq short-after) (str "needs a crafting_table, none carried, and making one leaves "
                                      (clojure.string/join "; " (map #(clojure.string/replace % "needs" "short:") short-after)))
               table? (str "needs a crafting_table, none carried; " (lacking version have table-item 1))
               :else "has no ingredient short, but no chain works"))))))

(defn plan
  "See the ns doc. n is the amount still lacking: a gatherable item is planned as {:gather {item n}} whatever is carried."
  [version have item n {:keys [table? max-depth gather? materials raw smelts] :or {max-depth 6}}]
  (let [rs (recipes-for version)
        st0 {:have have :steps [] :gather? gather? :materials (or materials (stone-materials version)) :ranks (material-ranks version)
             :raw (set raw) :smelts smelts}
        with-gather (fn [pl st] (cond-> pl (seq (:gather st)) (assoc :gather (:gather st)) (seq (:used st)) (assoc :uses (:used st))))
        main (make rs st0 item n #{} max-depth)
        needs-table (and main (some :table? (:steps main)) (not table?))]
    (cond
      (gathered? st0 item) {:steps [] :gather {item n}}
      (nil? main) nil
      (not needs-table) (with-gather {:steps (:steps main)} main)
      :else (when-let [t (acquire rs st0 table-item 1 #{} max-depth)]
              (when-let [m (make rs t item n #{} max-depth)]
                (with-gather {:steps (vec (concat (:steps t) (subvec (:steps m) (count (:steps t)))))} m))))))
