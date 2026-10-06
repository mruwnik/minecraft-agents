(ns jobs.items.shortfall
  "What a craft that ran out of ingredients reports. The primitive hands back every candidate recipe
  ({name n} each) and the counts carried; the choice of the recipe closest to done, what it lacks and
  the names the other recipes use in its place is made here.")

(def preferred
  "The common base items, earliest first: when recipes tie on what is missing, the one asking for these wins."
  ["cobblestone" "oak_planks" "oak_log" "stick" "iron_ingot" "string" "coal" "wheat" "diamond" "gold_ingot" "redstone" "leather" "sand" "dirt"])

(defn missing-for
  "{name n} of what recipe asks for beyond have."
  [recipe have]
  (into {} (keep (fn [[name n]] (let [lack (- n (get have name 0))] (when (pos? lack) [name lack])))) recipe))

(defn rank-of
  "The earliest preferred index among the missing names; Infinity for none of them."
  [missing]
  (transduce (map #(let [i (.indexOf preferred %)] (if (neg? i) js/Infinity i))) min js/Infinity (keys missing)))

(defn total [counts] (reduce + (vals counts)))

(defn closer?
  "Whether candidate a (with :missing) beats b: fewer items missing, then the more common base item."
  [a b]
  (let [ta (total (:missing a)) tb (total (:missing b))]
    (or (< ta tb) (and (= ta tb) (< (rank-of (:missing a)) (rank-of (:missing b)))))))

(defn closest
  "{:recipe r :missing m} for the recipe closest to being satisfied by have, nil without recipes."
  [recipes have]
  (->> recipes
       (map (fn [recipe] {:recipe recipe :missing (missing-for recipe have)}))
       (reduce (fn [best c] (if (or (nil? best) (closer? c best)) c best)) nil)))

(defn shortfall
  "What the closest recipe is missing, {name n}."
  [recipes have]
  (or (:missing (closest recipes have)) {}))

(defn alternatives
  "For each missing name of the chosen recipe, the names the other recipes use instead of it."
  [recipes have]
  (let [{:keys [recipe missing]} (closest recipes have)
        others (remove #(identical? % recipe) recipes)
        cousins (fn [name]
                  (->> others
                       (remove #(contains? % name))
                       (mapcat keys)
                       (remove #(contains? recipe %))
                       distinct
                       vec))]
    (if-not recipe
      {}
      (into {} (keep (fn [name] (let [c (cousins name)] (when (seq c) [name c])))) (keys missing)))))

(defn no-item
  "{:short {name n}} and, only when there are some, :alternatives {name [cousins]}."
  [recipes have]
  (let [alts (alternatives recipes have)]
    (cond-> {:short (shortfall recipes have)}
      (seq alts) (assoc :alternatives alts))))
