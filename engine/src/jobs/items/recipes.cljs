(ns jobs.items.recipes
  "Which crafts make an item from what is carried. Pure: recipes come from minecraft-data for the body's version, the
  carried counts and whether a crafting table is at hand are arguments.

  (plan version have item n opts) -> {:steps [step ...]} or nil when no chain of crafts gets n more of item.
  A step is {:op :craft :item name :count n :table? bool} or {:op :place :item \"crafting_table\"} (put the table down
  before the first craft that needs one). Ingredients are taken from what is carried first, then crafted; a recipe
  whose ingredients are carried is tried before one that needs more crafts. opts: :table? (a table is at hand: no table
  steps), :max-depth (nested crafts, default 6), :gather? (items that are gatherable? and not carried count as
  gathered, not crafted: the plan then has :gather {name n}, what must be mined or felled before its crafts can run).
  (lacking version have item n) says why a craft cannot start."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string]))

(def table-item "crafting_table")

(defn gatherable?
  "Items with no recipe that the body gets by mining or felling: logs, cobblestone, coal."
  [item]
  (boolean (or (#{"cobblestone" "coal"} item) (clojure.string/ends-with? item "_log"))))

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

(def recipes-for (memoize recipes-of))

(defn ceil-div [a b] (js/Math.ceil (/ a b)))

(declare make)

(defn acquire
  "State st with n of item taken from what is carried, the rest crafted; nil when it cannot be had."
  [rs st item n chain depth]
  (let [have (get-in st [:have item] 0)
        use (min have n)
        st (update-in st [:have item] (fnil - 0) use)
        deficit (- n use)]
    (cond
      (zero? deficit) st
      (and (:gather? st) (gatherable? item)) (update-in st [:gather item] (fnil + 0) deficit)
      :else (when-let [st (make rs st item deficit chain depth)]
              (update-in st [:have item] - deficit)))))

(defn short-count
  "How many of the recipe's ingredients (for batches crafts) the state lacks."
  [st {:keys [needs]} batches]
  (reduce + 0 (map (fn [[k v]] (max 0 (- (* v batches) (get-in st [:have k] 0)))) needs)))

(defn make
  "State st with n more of item crafted (added to :have, steps appended), nil when no recipe works."
  [rs st item n chain depth]
  (when (and (pos? depth) (not (chain item)))
    (let [chain (conj chain item)
          options (sort-by (fn [r] (short-count st r (ceil-div n (:count r)))) (get rs item))
          try-option (fn [{:keys [needs] :as r}]
                       (let [batches (ceil-div n (:count r))
                             st' (reduce (fn [st [k v]] (or (acquire rs st k (* v batches) chain (dec depth)) (reduced nil)))
                                         st needs)]
                         (when st'
                           (-> st'
                               (update-in [:have item] (fnil + 0) (* batches (:count r)))
                               (update :steps conj {:op :craft :item item :count (* batches (:count r)) :table? (:table? r)})))))]
      (if (:gather? st)
        ;; the option that gathers least (a carried species of log wins over a new one)
        (first (sort-by #(reduce + 0 (vals (:gather %))) (keep try-option options)))
        (some try-option options)))))

(defn lacking
  "Why no craft of item (n more) can start, as text: the cheapest recipe's ingredients short of what is carried
  (\"needs 3 stick, have 1\"), or that no recipe makes item. Pure."
  [version have item n]
  (let [rs (get (recipes-for version) item)
        st {:have have}]
    (if (empty? rs)
      (str "no recipe makes " item)
      (let [r (first (sort-by #(short-count st % (ceil-div n (:count %))) rs))
            batches (ceil-div n (:count r))
            short (keep (fn [[k v]] (let [h (get have k 0)] (when (< h (* v batches)) (str "needs " (* v batches) " " k ", have " h))))
                        (:needs r))]
        (str "crafting " item " " (if (seq short) (clojure.string/join "; " short) "has no ingredient short, but no chain works"))))))

(defn plan
  "See the ns doc."
  [version have item n {:keys [table? max-depth gather?] :or {max-depth 6}}]
  (let [rs (recipes-for version)
        st0 {:have have :steps [] :gather? gather?}
        with-gather (fn [pl st] (cond-> pl (seq (:gather st)) (assoc :gather (:gather st))))
        main (make rs st0 item n #{} max-depth)
        needs-table (and main (some :table? (:steps main)) (not table?))]
    (cond
      (nil? main) nil
      (not needs-table) (with-gather {:steps (:steps main)} main)
      :else (when-let [t (acquire rs st0 table-item 1 #{} max-depth)]
              (when-let [m (make rs t item n #{} max-depth)]
                (with-gather {:steps (vec (concat (:steps t) [{:op :place :item table-item}] (subvec (:steps m) (count (:steps t)))))} m))))))
