(ns jobs.lib.choose
  "Choosers: what a body may use for a purpose, in order. (tools-for c [:dig block] amount) answers
  {:list [{:item name :uses-left n}] :short n}: the carried tools that harvest block and are not reserved, cheapest
  material first (opts {:fast true}: best first), and the uses the amount still lacks once the list is used up (the
  caller's :fetch). `reserved` is the one filter over the reserves that name an item: the bait reserve, the food
  reserve, the never-spent items and a body's :choose/override entries {:chooser :tools :avoid [names] :prefer [names]}.
  Zones judge cells, not items: the caller asks them for the cell it digs."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost.food :as food]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.foods :as foods]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.util :as u]))

(def whole "Uses of a tool with no known durability: counts as whole." 1000000)

(defn overrides
  "The body's :choose/override data for chooser, merged: {:avoid #{...} :prefer [...]}."
  [c chooser]
  (let [mine (filter #(= chooser (:chooser %)) (map :data (ctx/entries c :choose/override)))]
    {:avoid (into #{} (mapcat :avoid) mine)
     :prefer (vec (distinct (mapcat :prefer mine)))}))

(defn reserved
  "{name count}: how much of each carried item is off limits to a chooser (the bait foods, the food reserve and the
  never-spent items whole stacks), plus the override's :avoid names, whole."
  [c chooser]
  (let [inv (u/inventory (:primitives c))
        carried (fn [n] (transduce (comp (filter #(= n (:name %))) (map :count)) + 0 inv))
        bait (foods/reserved (ctx/view c))
        whole-of (fn [names] (into {} (map (fn [n] [n (carried n)])) names))]
    (merge-with max
                (whole-of bait)
                (whole-of (filter foods/precious (map :name inv)))
                (food/food-reserve inv)
                (whole-of (:avoid (overrides c chooser))))))

(defn tools-for
  "See the ns doc. Only the purpose [:dig block] exists; any other throws."
  ([c purpose amount] (tools-for c purpose amount nil))
  ([c [what block :as purpose] amount opts]
   (when-not (and (= :dig what) (string? block))
     (throw (js/Error. (str "tools-for: unknown purpose " (pr-str purpose)))))
   (let [p (:primitives c)
         harvest (some-> (.harvestTools p block) js->clj)
         ok (set harvest)
         wanted (kind/tool-kind block)
         held (reserved c :tools)
         prefer (:prefer (overrides c :tools))
         rank-in (fn [name] (let [i (.indexOf prefer name)] (if (neg? i) (count prefer) i)))
         order (fn [t] [(rank-in (:name t))
                        (if (:fast opts) (- (weapon/weapon-rank (:name t))) (weapon/cheap-rank (:name t)))
                        (or (:durability t) whole)])
         fit (fn [k] (->> (u/inventory p)
                          (filter #(and (kind/of-kind? (:name %) k) (or (empty? ok) (ok (:name %)))))
                          (remove #(contains? held (:name %)))
                          (remove #(zero? (or (:durability %) whole)))))
         usable (sort-by order (or (seq (fit wanted)) (some-> (kind/fallback-kind wanted) fit)))
         uses (mapv (fn [t] {:item (:name t) :uses-left (or (:durability t) whole)}) usable)]
     {:list uses
      :short (max 0 (- amount (transduce (map :uses-left) + 0 uses)))})))
