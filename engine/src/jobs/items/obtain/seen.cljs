(ns jobs.items.obtain.seen
  "A gather need against the body: the blocks and sheep seen for it, and the need made ready to run."
  (:require [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [animal-radius seen-radius]]
            [jobs.lib.access :as access]
            [jobs.lib.animals :as animals]
            [jobs.lib.foods :as foods]
            [jobs.lib.stock :as stock]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]))

(defn sheep-in-range
  "The adult unsheared sheep within the sheep radius, as JS entities."
  [c]
  (remove #(or (true? (.-baby %)) (true? (.-sheared %))) (animals/herd (:primitives c) "sheep" (animal-radius))))

(def wool-keep "Sheep of a colour combat.hunt spares (its :keep default for animals); a colour with no more is no source." 2)

(defn shears-source
  "How the body gets shears for wool: :carried, :craft (2 iron ingots carried), :chest (shears or 2 iron ingots in a
  booked chest), else nil."
  [c]
  (let [inv (u/inventory (:primitives c))
        in-chest (fn [item n] (some #(>= (get % item 0) n) (vals (stock/stock-of (ctx/view c)))))]
    (cond
      (pos? (storage/carried inv "shears")) :carried
      (>= (storage/carried inv "iron_ingot") 2) :craft
      (or (in-chest "shears" 1) (in-chest "iron_ingot" 2)) :chest)))

(defn gather-carried [p {:keys [key]}]
  (reduce + 0 (for [[k v] (storage/carried-totals (u/inventory p)) :when (if (= "log" key) (trees/log-name? k) (= key k))] v)))

(defn cut-off?
  "Whether the block b stands in a column of a tree an earlier gather found cut off."
  [c b]
  (contains? (get (ctx/mem c) :cut-off #{}) [(:x (:pos b)) (:z (:pos b))]))

(defn seen-blocks-of
  "The seen blocks of a need (within the seen radius), nearest first, with those a zone, claim or plan footprint bars
  taking left out unless all? is true."
  [c need all?]
  (when (:seen need)
    (let [in (access/rules-input c)
          blocks (if (= "water_bucket" (:key need))
                   (look/seen-water-sources (:primitives c) (seen-radius))
                   (look/seen-blocks (:primitives c) {:match #((:seen need) %) :radius (seen-radius) :max (if all? 1 64)}))]
      (if all? blocks (filter #(and (:ok (access/may? in :harvest (:pos %))) (not (cut-off? c %))) blocks)))))

(defn seen-of
  "The nearest seen block of a need that may be taken, or for a wool need a sheep of its colour in range, for a drop of an animal one adult in range, else nil."
  [c need]
  (cond
    (:sheep need) (first (filter #(= (:sheep need) (.-color %)) (sheep-in-range c)))
    (:mob need) (first (remove #(true? (.-baby %)) (animals/herd (:primitives c) (:mob need) (animal-radius))))
    :else (first (seen-blocks-of c need false))))

(defn with-block
  "The need with the nearest seen water as the :pos a bucket is filled at, :block of its args the kind of block seen when it could be either (a coal ore, dirt or grass); a wool need shears when shears are carried, craftable from 2 iron ingots or in a booked chest, else hunts."
  [c need]
  (cond
    (and (:sheep need) (shears-source c))
    (assoc need :job 'jobs.animals.shear :args {:color (:sheep need) :radius (animal-radius)})

    (and (= "obsidian" (:key need))
         (empty? (look/seen-blocks (:primitives c) {:names ["obsidian"] :radius (seen-radius) :max 1})))
    (assoc need :job 'jobs.gather.cast-obsidian :args {:count (:count need) :radius (seen-radius) :fetch #{:tool}})

    (= "water_bucket" (:key need))
    (cond-> need (seen-of c need) (assoc-in [:args :on] (:pos (seen-of c need))))

    :else
    (if-let [name (when (or (:ore? need) (= "dirt" (:key need))) (:name (seen-of c need)))]
      (assoc-in need [:args :block] name)
      need)))

(defn with-count
  "The need with the items to carry in all (the :count of harvest-wood, fish, get-seeds, hunt and mine) in its args: those carried and those it lacks."
  [p need]
  (case (:job need)
    jobs.gather.get-seeds (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    jobs.gather.mine (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    jobs.forestry.harvest-wood (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    jobs.food.fish (assoc-in need [:args :count] (+ (foods/fish-carried p) (:count need)))
    jobs.combat.hunt (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    need))

(defn with-avoid
  "The need with the columns of trees found cut off in its args when it fells trees."
  [c need]
  (if-let [cut (seq (:cut-off (ctx/mem c)))]
    (cond-> need (= 'jobs.forestry.harvest-wood (:job need)) (assoc-in [:args :avoid] (vec cut)))
    need))

(defn gather-slot
  "The child slot of a gather need: one per source, since a slot's memory outlives its job while it runs and another job must not start over it."
  [need]
  (keyword (str "gather-" (:key need))))
