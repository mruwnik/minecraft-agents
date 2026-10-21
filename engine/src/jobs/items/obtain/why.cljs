(ns jobs.items.obtain.why
  "The texts of a source that has nothing: why no source, what a failed try adds."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.items.obtain.base :refer [animal-radius gathers? makes? seen-radius way?]]
            [jobs.items.obtain.need :refer [explore-opts]]
            [jobs.items.obtain.plan :refer [gather-plan]]
            [jobs.items.obtain.seen :refer [seen-blocks-of seen-of]]
            [jobs.items.obtain.table :refer [animal? gather-needs material-blocks]]
            [jobs.items.recipes :as recipes]
            [jobs.items.smelt.plan :as smelt-plan]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]
            [clojure.string]))

(defn other-logs-seen
  "The log names of other species seen for an unseen log need (oak_log wanted, birch_log seen), or nil."
  [c {:keys [key]}]
  (when (trees/log-name? key)
    (not-empty (filter #(seq (look/seen-blocks (:primitives c) {:names [%] :radius (seen-radius) :max 1}))
                       (remove #{key} trees/log-names)))))

(defn unseen-why
  "Text for the gather source: the raw needs of the chain whose blocks are not seen, those seen but not taken (a zone,
  claim or plan footprint refuses them), or the general text."
  [c names n]
  (let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))))
        seen (filter #(and (not (animal? %)) (not (seen-of c %)) (seq (seen-blocks-of c % true))) needs)
        unseen (remove #(or (seen-of c %) (seq (seen-blocks-of c % true))) needs)
        label (fn [{:keys [key seen sheep mob] :as need}]
                (cond
                  sheep (str key " (from " sheep " sheep within " (animal-radius) " blocks)")
                  mob (str key " (from a " mob " within " (animal-radius) " blocks)")
                  (and (set? seen) (not= #{key} seen)) (str key " (from " (clojure.string/join "/" (sort seen)) ")")
                  :else (str key (when-let [o (other-logs-seen c need)]
                                   (str " (only " (clojure.string/join "/" o) " seen: another species)")))))]
    (str "nothing seen to gather what a craft lacks"
         (when (some #(= "obsidian" (:key %)) needs)
           " (obsidian: seen obsidian, or lava with open air above and a water bucket carried; a bucket is 3 iron plus a seen water source)")
         (when (seq unseen) (str ": " (clojure.string/join ", " (map label unseen))))
         (when (and (seq unseen) (not (explore-opts c))) " (pass :explore true to search for it)")
         (when (seq seen)
           (str "; seen but not taken (a zone, claim or plan footprint may refuse it): "
                (clojure.string/join ", " (for [need seen :let [b (first (seen-blocks-of c need true))]]
                                            (str (label need) (when-let [p (some-> b :pos)] (str " at " (:x p) " " (:y p) " " (:z p)))))))))))

(defn pickaxe-why
  "For a stone material wanted with no pickaxe carried: that it needs one, and what crafting the cheapest lacks; else nil."
  [version have names]
  (when (and (some #(material-blocks version %) names)
             (not-any? (fn [[k v]] (and (pos? v) (clojure.string/ends-with? k "_pickaxe"))) have))
    (str "mining it needs a pickaxe, none carried; " (recipes/lacking version have "wooden_pickaxe" 1))))

(defn make-why
  "Why item cannot be made: a smelted product (iron_ingot) lacks its input, whatever packed-block recipe also makes it."
  [c version have item n]
  (cond
    (= "water_bucket" item)
    (str "a water bucket is a bucket filled at a seen water source; "
         (if (pos? (get have "bucket" 0)) "a bucket is carried" (recipes/lacking version have "bucket" 1)))

    :else
    (if-let [input (and (way? c :smelt) (smelt-plan/made-from item))]
      (str "smelting " item " needs " input ", have " (get have input 0))
      (recipes/lacking version have item n))))

(defn chest-why
  "Text for the chest source: no chest seen, or the chests already looked in or taken from (their stock is carried now)."
  [c names have]
  (let [looked (count (:done-chests (ctx/mem c)))
        label (clojure.string/join "/" names)]
    (if (zero? looked)
      (str "no seen chest that may hold " label)
      (str "the " looked " chest(s) already looked in or taken from hold no more " label
           " (" (reduce + (keep #(get have %) names)) " carried)"))))

(defn no-source-why
  "Text for the no-source wait: what each allowed source lacks."
  [c names n]
  (let [have (storage/carried-totals (u/inventory (:primitives c)))
        version (game/version-of (:primitives c))]
    (clojure.string/join
     "; "
     (concat (when (way? c :chest) [(chest-why c names have)])
             (when (makes? c) [(make-why c version have (first names) n)])
             (when (gathers? c) [(unseen-why c names n)])
             (when-let [w (and (gathers? c) (pickaxe-why version have names))] [w])))))

(defn tried-text
  "What a gather that was tried and failed adds to the no-source text (\"; gathering was tried and ended ..\", with the
  child's reason when it gave one), or nil when none was tried."
  ([tried] (tried-text tried nil))
  ([tried why] (tried-text tried why nil))
  ([tried why craft-detail]
   (not-empty
    (str (when-let [g (:gather tried)]
           (str "; gathering was tried and ended " (pr-str g) (when why (str " (" why ")"))))
         (when (and craft-detail (:craft tried))
           (str "; crafting ended " (let [k (:craft tried)] (if (keyword? k) (name k) k)) ": " craft-detail))))))
