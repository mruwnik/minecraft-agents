(ns engine.fake.anvil
  "The fake world's `anvil` primitive: put a carried item and an enchanted book holding an enchantment together, as a
  pure function over world data {:blocks :self {:pos :experience {:level}} :inventory}. Mirrors js/anvil.mjs. The level
  cost is 2 per level of the book's enchantment (a rare enchantment's book multiplier) and nothing for prior work.
  (anvil w args) answers [world' result]. Test-only."
  (:require [engine.fake.pockets :as pockets]))

(def reach 4.5)
(def book "enchanted_book")

(defn holds? [stack enchant] (boolean (some #(= enchant (:name %)) (:enchants stack))))

(defn check [{:keys [pos item enchant] :as a}]
  (cond
    (not (and (map? a) (vector? pos) (= 3 (count pos)) (every? number? pos))) (throw (pockets/bad-args "anvil needs pos [x y z]"))
    (not (and (string? item) (seq item))) (throw (pockets/bad-args "anvil needs item, a name"))
    (not (and (string? enchant) (seq enchant))) (throw (pockets/bad-args "anvil needs enchant, a name"))))

(defn index-of [w pred] (first (keep-indexed #(when (pred %2) %1) (:inventory w))))

(defn anvil [w {:keys [pos item enchant] :as a}]
  (check a)
  (let [block (get-in w [:blocks pos])
        mine (filter #(= item (:name %)) (:inventory w))
        b (index-of w #(and (= book (:name %)) (holds? % enchant)))
        t (index-of w #(and (= item (:name %)) (not (holds? % enchant))))
        level (or (some #(when (= enchant (:name %)) (:level %)) (:enchants (get-in w [:inventory b]))) 1)
        cost (* 2 level)
        xp (get-in w [:self :experience :level] 0)]
    (cond
      (or (nil? block) (= block "air")) [w {:status "missing"}]
      (not (re-find #"^(chipped_|damaged_)?anvil$" block)) [w {:status "cannot" :reason "not-an-anvil"}]
      (> (pockets/distance (get-in w [:self :pos]) pos) reach) [w {:status "unreachable" :reason "too-far"}]
      (empty? mine) [w {:status "no-item" :item item}]
      (nil? t) [w {:status "cannot" :reason "already-has" :item item}]
      (nil? b) [w {:status "no-book" :enchant enchant}]
      (< xp cost) [w {:status "no-levels" :need cost :have xp}]
      :else
      (let [made (-> (get-in w [:inventory t]) (assoc :count 1) (update :enchants (fnil conj []) {:name enchant :level level}))
            inv (-> (:inventory w) (update-in [t :count] dec) (update-in [b :count] dec))
            w' (-> w
                   (assoc :inventory (conj (filterv #(pos? (:count %)) inv) made))
                   (assoc-in [:self :experience :level] (- xp cost)))]
        [w' {:status "combined" :item item :enchant enchant :enchants (:enchants made) :levelCost cost :levelsSpent cost :xpLevel (- xp cost)}]))))
