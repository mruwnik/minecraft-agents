(ns jobs.items.obtain.seen
  "A gather need against the body: the blocks and sheep seen for it, and the need made ready to run."
  (:require [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [given-worth search-worth seen-radius]]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.items.obtain.chest :as chest]
            [jobs.items.obtain.table :refer [grass-seeds]]
            [jobs.lib.animals :as animals]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]))

(defn animals-worth-hunting
  "The adult animals of the need's mob in sight (colour-matching when given), nearest first, whose trip may pay: the call's
  given-worth, else the larger of the animal's meat and the call's search-worth, against fetch-cost of the straight walk to the
  animal, a lower bound (its danger is priced from go-to's quote when one is walked to: jobs.lib.walk-pick)."
  [c mob pred]
  (let [p (:primitives c)
        self (u/self-pos c)
        worth (or (given-worth c) (max (cost/animal-worth mob) (search-worth c)))]
    (filter #(< (:cost (cost/fetch-cost {:distance (u/dist self (u/pos-of (.-pos %))) :danger 0})) worth)
            (filter pred (animals/herd p mob (seen-radius))))))

(defn sheep-in-range
  "The adult unsheared sheep in sight whose trip pays, as JS entities."
  [c]
  (animals-worth-hunting c "sheep" #(not (or (true? (.-baby %)) (true? (.-sheared %))))))

(defn shears-source
  "How the body gets shears for wool: :carried, :craft (2 iron ingots carried), :chest (shears or 2 iron ingots in a
  booked chest the chest source would walk to: allowed, not given up, not beyond far-chest), else nil."
  [c]
  (let [in-chest (fn [item n] (some #(>= (get (:stock %) item 0) n) (chest/candidates c [item])))]
    (or (spare/shears-at-hand (u/inventory (:primitives c)))
        (when (or (in-chest "shears" 1) (in-chest "iron_ingot" 2)) :chest))))

(defn gather-carried [p {:keys [key]}]
  (reduce + 0 (for [[k v] (storage/carried-totals (u/inventory p)) :when (if (= "log" key) (trees/log-name? k) (= key k))] v)))

(defn cut-off?
  "Whether the block b stands in a column of a tree an earlier gather found cut off."
  [c b]
  (contains? (get (ctx/mem c) :cut-off #{}) [(:x (:pos b)) (:z (:pos b))]))

(defn seen-blocks-of
  "The seen blocks of a need (within the seen radius), nearest first, with those a zone, claim or plan footprint bars
  taking left out unless all? is true. The nearest n are read and filtered, n doubling until one may be taken or
  none is left, so barred blocks never crowd out a free one."
  [c need all?]
  (when (:seen need)
    (let [in (access/rules-input c)
          water? (= "water_bucket" (:key need))
          read (fn [n] (if water?
                         (look/seen-water-sources (:primitives c) (seen-radius))
                         (look/seen-blocks (:primitives c) {:match #((:seen need) %) :radius (seen-radius) :max n})))
          takeable (fn [blocks] (filter #(and (:ok (access/may? in :harvest (:pos %))) (not (cut-off? c %))) blocks))]
      (if all?
        (read 1)
        (loop [n 64]
          (let [blocks (read n)
                ok (takeable blocks)]
            (if (or (seq ok) water? (< (count blocks) n)) ok (recur (* 2 n)))))))))

(defn growable?
  "Whether a crop need can be grown from nothing: the crop's seed is broken out of grass (wheat)."
  [need]
  (boolean (and (:crop need) (grass-seeds (crops/seed-of (:crop need))))))

(defn ripe-crop
  "The nearest ripe crop of the need's kind that may be cut and is no cut-off column, else nil."
  [c need]
  (let [in (access/rules-input c)]
    (first (filter #(and (some-> (:age %) (>= (crops/ripe-age (:crop need))))
                         (:ok (access/may? in :harvest (:pos %)))
                         (not (cut-off? c %)))
                   (crops/seen-crops (:primitives c) [(:crop need)] (seen-radius) 256)))))

(defn animals-of
  "The animals in sight a hunt or shear of an animal need may go for, nearest first: sheep of its colour in range, or adults of its mob worth the trip."
  [c need]
  (if (:sheep need)
    (filter #(= (:sheep need) (.-color %)) (sheep-in-range c))
    (animals-worth-hunting c (:mob need) #(not (true? (.-baby %))))))

(defn seen-of
  "The nearest seen block of a need that may be taken, or for a wool need a sheep of its colour in range, for a drop of an animal one adult in range; a growable crop need with none seen is sourced by growing it ({:grow true}); else nil."
  [c need]
  (cond
    (growable? need) (or (first (seen-blocks-of c need false)) {:grow true})
    (or (:sheep need) (:mob need)) (first (animals-of c need))
    :else (first (seen-blocks-of c need false))))

(defn with-block
  "The need with the nearest seen water as the :pos a bucket is filled at, :block of its args the kind of block seen when it could be either (a coal ore, dirt or grass); a wool need shears when shears are carried, craftable from 2 iron ingots or in a booked chest, else hunts."
  [c need]
  (cond
    (and (:sheep need) (shears-source c))
    (assoc need :job 'jobs.animals.shear :args {:color (:sheep need) :radius (seen-radius)})

    (and (= "obsidian" (:key need))
         (empty? (look/seen-blocks (:primitives c) {:names ["obsidian"] :radius (seen-radius) :max 1})))
    (assoc need :job 'jobs.gather.cast-obsidian :args {:count (:count need) :radius (seen-radius) :ways #{:carried}})

    (and (growable? need) (not (ripe-crop c need)))
    (assoc need :job 'jobs.farm.grow :args {:crop (:crop need) :count (:count need)})

    (:crop need)
    (cond-> need (ripe-crop c need) (assoc-in [:args :area] {:pos (:pos (ripe-crop c need)) :radius crops/cut-radius}))

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
    jobs.farm.grow (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    jobs.gather.get-seeds (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    jobs.gather.mine (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    jobs.forestry.harvest-wood (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    jobs.food.fish (assoc-in need [:args :count] (+ (foods/fish-carried p) (:count need)))
    jobs.combat.hunt (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    need))

(defn with-avoid
  "The need with the columns of trees found cut off in its args when it fells trees, and the cells a mine gave up on when it mines."
  [c need]
  (let [cut (seq (:cut-off (ctx/mem c)))
        skip (seq (:skip-cells (ctx/mem c)))]
    (case (:job need)
      jobs.forestry.harvest-wood (cond-> need cut (assoc-in [:args :avoid] (vec cut)))
      jobs.gather.mine (cond-> need skip (assoc-in [:args :skip] (vec skip)))
      need)))

(defn gather-slot
  "The child slot of a gather need: one per source, since a slot's memory outlives its job while it runs and another job must not start over it."
  [need]
  (keyword (str "gather-" (:key need))))
