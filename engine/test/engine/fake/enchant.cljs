(ns engine.fake.enchant
  "The fake world's `enchant` primitive: an enchanting table's three offers and enchanting with one of them, as pure
  functions over world data {:blocks :self {:pos :experience {:level}} :inventory :enchant-tables {pos spec}}. Mirrors
  js/enchant.mjs and js/fake-enchant.mjs. A table's offers come from its bookshelves (none: 2/3/5 levels, fifteen:
  10/20/30) unless its spec gives them: {:shelves n :offers [l l l] :hints [[name level] | nil ...] :busy bool}.
  (enchant w args) answers [world' result]. Test-only."
  (:require [engine.fake.pockets :as pockets]))

(def reach 4.5)
(def lapis "lapis_lazuli")
(def result {"book" "enchanted_book"})
(def enchantable
  #"_(sword|pickaxe|axe|shovel|hoe|helmet|chestplate|leggings|boots)$|^(bow|crossbow|fishing_rod|trident|shears|book|elytra|shield|mace)$")
(def gives [[#"_sword$|_axe$" "sharpness"] [#"_(pickaxe|shovel|hoe)$" "efficiency"]
            [#"_(helmet|chestplate|leggings|boots)$" "protection"] [#"^bow$" "power"]])

(defn plain? [i] (empty? (:enchants i)))

(defn check [{:keys [pos op item choice levelCost] :as a}]
  (cond
    (not (and (map? a) (vector? pos) (= 3 (count pos)) (every? number? pos))) (throw (pockets/bad-args "enchant needs pos [x y z]"))
    (not (#{"offers" "enchant"} op)) (throw (pockets/bad-args "enchant op must be offers or enchant"))
    (not (and (string? item) (seq item))) (throw (pockets/bad-args "enchant needs item, a name"))
    (and (= op "enchant") (not (#{0 1 2} choice))) (throw (pockets/bad-args "enchant needs choice 0, 1 or 2"))
    (and (some? levelCost) (not (integer? levelCost))) (throw (pockets/bad-args "enchant levelCost must be an integer"))))

(defn levels-for [table item]
  (cond
    (not (re-find enchantable item)) [0 0 0]
    (:offers table) (:offers table)
    :else (let [top (max 5 (* 2 (get table :shelves 0)))]
            [(max 1 (Math/round (/ top 3))) (Math/round (/ (* top 2) 3)) top])))

(defn gives-for [item] (or (some (fn [[re name]] (when (re-find re item) name)) gives) "unbreaking"))
(defn level-for [cost] (max 1 (Math/ceil (/ cost 8))))

(defn hints-for [table item levels]
  (vec (map-indexed (fn [i cost]
                      (cond
                        (<= cost 0) nil
                        (:hints table) (when-let [[name level] (nth (:hints table) i nil)] {:enchant name :level level})
                        :else {:enchant (gives-for item) :level (level-for cost)}))
                    levels)))

(defn enchants-for [item cost]
  (into [{:name (gives-for item) :level (level-for cost)}]
        (when (>= cost 15) [{:name "unbreaking" :level (Math/ceil (/ cost 10))}])))

(defn enchant-one
  "One plain stack of item loses one and an enchanted one is added at the end."
  [w item cost]
  (let [i (first (keep-indexed #(when (and (= item (:name %2)) (plain? %2)) %1) (:inventory w)))
        made {:name (get result item item) :count 1 :enchants (enchants-for item cost)}
        inv (update-in (:inventory w) [i :count] dec)]
    [(assoc w :inventory (conj (filterv #(pos? (:count %)) inv) made)) made]))

(defn enchant [w {:keys [pos op item choice levelCost] :as a}]
  (check a)
  (let [block (get-in w [:blocks pos])
        distance (pockets/distance (get-in w [:self :pos]) pos)
        mine (filter #(= item (:name %)) (:inventory w))
        table (get-in w [:enchant-tables pos] {})
        xp (get-in w [:self :experience :level])
        levels (levels-for table item)
        hints (hints-for table item levels)
        offers (vec (map-indexed (fn [index level-cost] {:index index :levelCost level-cost :lapisCost (inc index) :hint (nth hints index)}) levels))
        have (pockets/carried w lapis)
        chosen (when (= op "enchant") (nth offers choice))
        need (when chosen (max (:levelCost chosen) (:lapisCost chosen)))]
    (cond
      (or (nil? block) (= block "air")) [w {:status "missing"}]
      (not= block "enchanting_table") [w {:status "cannot" :reason "not-a-table"}]
      (> distance reach) [w {:status "unreachable" :reason "too-far" :distance (pockets/round2 distance)}]
      (empty? mine) [w {:status "no-item" :item item}]
      (not-any? plain? mine) [w {:status "cannot" :reason "already-enchanted" :item item}]
      (:busy table) [w {:status "failed" :reason "window-did-not-open"}]
      (= op "offers") [w {:status "ok" :item item :xpLevel xp :lapis have :offers offers}]
      (< have (inc choice)) [w {:status "no-lapis" :have have :need (inc choice)}]
      (every? #(<= % 0) levels) [w {:status "cannot" :reason "not-enchantable"}]
      (<= (:levelCost chosen) 0) [w {:status "cannot" :reason "no-such-offer" :offers levels}]
      (and (some? levelCost) (not= levelCost (:levelCost chosen))) [w {:status "cannot" :reason "offer-changed" :offers levels}]
      (< xp need) [w {:status "no-levels" :need need :have xp}]
      :else
      (let [[w' done] (enchant-one w item (:levelCost chosen))
            spent (:lapisCost chosen)
            w'' (-> w' (pockets/take-from lapis spent) (assoc-in [:self :experience :level] (- xp spent)))]
        [w'' {:status "enchanted" :item item :choice choice :enchants (:enchants done) :lapisSpent spent :levelsSpent spent
              :xpLevel (- xp spent)}]))))
