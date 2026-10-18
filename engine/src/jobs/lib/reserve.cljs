(ns jobs.lib.reserve
  "The body's register of reserved items: body memory kind :reserve, entries {:id :item|:tag :count|:all :strength 0..1
  :by \"agent\"|job instance id :why :until ms|nil :not #{chooser..} the choosers it does not bind}. Writing an id replaces its entry. An entry counts while :until has
  not passed and its :by job still lives. It covers carried units only.
  (entries view) the live entries. (rows c chooser) [{:name :count :strength}] of the carried units reserved, strongest
  first: the register plus the built-in rules (the bait foods, the 3-day food reserve, precious food, a chooser's
  :choose/override :avoid names), each at strength 1. (held c chooser) {name count} of those units, the one answer to
  \"what may this chooser not use\". (price c name n) the extra cost of using n units of name: the units over the free
  surplus, lowest strength first, each (cost/reserve-cost value strength); ##Inf when one is strength 1.
  (free c chooser) the inventory with the held units taken out: what a job may spend without weighing a price."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.register :as register]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.food :as food]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]
            [clojure.string :as str]))

(def kind :reserve)

(def policy {:cap 100 :ttl :forever})

(def tags
  "Tag -> predicate over an item name."
  {:food foods/edible?
   :ore #(or (str/ends-with? % "_ore") (str/starts-with? % "raw_") (= "ancient_debris" %))
   :metal #(boolean (re-find #"_ingot$|^netherite_scrap$" %))
   :bed #(str/ends-with? % "_bed")
   :seeds #(boolean (re-find #"_seeds$|^(carrot|potato)$" %))})

(def live? register/live?)

(defn entries [view]
  (filterv #(live? view %) (map :data (mem/entries view kind))))

(defn- carried-of [inv]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inv))

(defn- units
  "{name n} the entry covers in carried: its item or the carried names of its tag, up to :count (all with :all)."
  [carried {:keys [item tag count]}]
  (let [names (cond item (filter #{item} (keys carried))
                    (tags tag) (sort (filter (tags tag) (keys carried)))
                    :else [])
        left (atom (if (number? count) count ##Inf))]
    (into {}
          (keep (fn [n] (let [k (min @left (carried n))]
                          (when (pos? k) (swap! left - k) [n k]))))
          names)))

(defn overrides
  "The body's :choose/override data for chooser, merged: {:avoid #{...} :prefer [...]}."
  [c chooser]
  (let [mine (filter #(= chooser (:chooser %)) (map :data (ctx/entries c :choose/override)))]
    {:avoid (into #{} (mapcat :avoid) mine)
     :prefer (vec (distinct (mapcat :prefer mine)))}))

(defn- builtin
  "{name n} the built-in rules keep off limits for chooser."
  [c chooser inv carried]
  (let [whole (fn [names] (into {} (keep (fn [n] (when (carried n) [n (carried n)]))) names))]
    (merge-with max
                (whole (foods/reserved (ctx/view c)))
                (whole (filter foods/precious (map :name inv)))
                (food/food-reserve inv)
                (whole (:avoid (overrides c chooser))))))

(defn rows
  "See the ns doc."
  [c chooser]
  (let [inv (u/inventory (:primitives c))
        carried (carried-of inv)
        asked (concat (map (fn [[n k]] [n k 1]) (builtin c chooser inv carried))
                      (mapcat (fn [e] (map (fn [[n k]] [n k (:strength e 1)]) (units carried e)))
                              (remove #(or (zero? (:strength % 1)) (contains? (:not %) chooser)) (entries (ctx/view c)))))]
    (->> (group-by first asked)
         (mapcat (fn [[n rs]]
                   (loop [[[_ k s] & more] (sort-by #(- (nth % 2)) rs) left (carried n) out []]
                     (if (or (nil? k) (<= left 0))
                       out
                       (let [take-n (min k left)]
                         (recur more (- left take-n) (conj out {:name n :count take-n :strength s})))))))
         (sort-by (juxt #(- (:strength %)) :name))
         vec)))

(defn held
  "See the ns doc."
  [c chooser]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (rows c chooser)))

(defn price
  "See the ns doc."
  [c chooser name n]
  (let [carried (get (carried-of (u/inventory (:primitives c))) name 0)
        rs (filter #(= name (:name %)) (rows c chooser))
        free (- carried (reduce + 0 (map :count rs)))
        each (:value (cost/item-value [{:name name :count 1}]))]
    (loop [need (- n (max 0 free)) [r & more] (reverse rs) total 0]
      (if (or (<= need 0) (nil? r))
        total
        (let [k (min need (:count r))]
          (recur (- need k) more (+ total (* k (cost/reserve-cost each (:strength r))))))))))

(defn free
  "See the ns doc."
  [c chooser]
  (loop [[it & more] (u/inventory (:primitives c)) left (held c chooser) out []]
    (if-not it
      out
      (let [k (min (:count it) (get left (:name it) 0))]
        (recur more (update left (:name it) (fnil - 0) k)
               (cond-> out (> (:count it) k) (conj (assoc it :count (- (:count it) k)))))))))
