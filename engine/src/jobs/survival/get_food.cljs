(ns jobs.survival.get-food
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.triggers.hungry :as hungry]
            [jobs.survival.eat :as eat]))

(def doc
  "Keep the body fed. Each round takes the first step of this ladder that
  has something to do: (1) eat what is carried (the eat job); (2) use the
  latest :food-source body-memory entry {:pos :kind} within :source-radius:
  a :farm is walked to and its mature crops dug and collected (replanting
  is not this job's business), a :chest is walked to and its best food
  withdrawn, :animals centres the hunt there; (3) hunt the nearest passive
  food animal within :hunt-radius, or else dig mature crops or sweet berry
  bushes in it, and collect the drops; (4) nothing found: emit a food.none
  warn naming what was searched and the nearest known source, write a
  :hungry entry and finish, then stay quiet and idle for :ask-cooldown-ms.
  Hungry is food below :food (default 6), or below :food-when-hurt (default
  14) while health is below full; the same test as the hungry trigger.")

(def args
  {:food {:doc "hungry below this much food (of 20)" :default hungry/default-food}
   :food-when-hurt {:doc "hungry below this much food while health is below full" :default hungry/default-food-when-hurt}
   :source-radius {:doc "how far away a remembered food source still counts, in blocks" :default 64}
   :hunt-radius {:doc "how far to look for animals and wild crops, in blocks" :default 24}
   :farm-radius {:doc "how far around a known farm to harvest, in blocks" :default 6}
   :take {:doc "most items to withdraw from a chest in one go" :default 16}
   :attack-gap-ms {:doc "least time between two swings at an animal, so a swing lands at full strength" :default 600}
   :ask-cooldown-ms {:doc "after finding nothing, how long before saying so (and searching) again" :default (* 10 60 1000)}})

(def food-animals #{"cow" "pig" "sheep" "chicken" "rabbit"})

(def ripe-age
  "The growth stage at which each crop block can be harvested."
  {"wheat" 7 "carrots" 7 "potatoes" 7 "beetroots" 3})

(def berry-bush {"sweet_berry_bush" 2})

(def harvest-items
  "What the drops of a harvest or a kill may be called."
  (into eat/edible #{"wheat" "wheat_seeds" "beetroot_seeds" "poisonous_potato"}))

(def hungry-policy {:cap 10 :ttl (* 60 60 1000)})

(def drop-radius 8)

(def reach 3)

(defn hungry-now? [c]
  (let [self (.self (:primitives c))]
    (hungry/hungry? (.-food self) (.-health self) (:args c))))

(defn check
  "Hungry, or in the middle of a meal that began when it was."
  [c]
  (or (hungry-now? c) (boolean (:eating (ctx/mem c)))))

;; ------------------------------------------------------------------ sources

(defn known-source
  "The latest :food-source entry's data, when within :source-radius and not
  already found unusable in this job."
  [c]
  (let [{:keys [pos] :as source} (:data (ctx/latest c :food-source))]
    (when (and source
               (<= (u/dist (u/self-pos c) pos) (:source-radius (:args c)))
               (not= pos (:dead-source (ctx/mem c))))
      source)))

(defn bury-source! [c pos]
  (ctx/update-mem! c assoc :dead-source pos)
  nil)

(defn ^:async go-near!
  "Walk to pos by the go-to child. :continue while walking, :there, or :far
  when the walk gave up."
  [c pos]
  (let [r (await (ctx/call-child c :goto 'jobs.movement.go-to {:pos pos :range reach}))]
    (cond
      (= :continue r) :continue
      (<= (u/dist (u/self-pos c) pos) (inc reach)) :there
      :else :far)))

;; ------------------------------------------------------------------ gathering

(defn ^:async collect-drops!
  "One collect-drops round for harvest items within radius: :continue when it
  did something, else nil."
  [c radius]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius radius :filter (vec harvest-items)}))]
    (when (= :continue r) :continue)))

(defn ripe-blocks
  "The ripe blocks named in ripe-ages within radius of center, nearest to the
  body first, minus the ones that proved undiggable."
  [c ripe-ages center radius]
  (let [skipped (set (:skipped-blocks (ctx/mem c)))]
    (->> (array-seq (.blocks (:primitives c) #js {:radius radius :names (clj->js (keys ripe-ages)) :max 64}))
         (filter #(some-> (.-age %) (>= (ripe-ages (.-name %)))))
         (map #(u/pos-of (.-pos %)))
         (filter #(<= (u/dist % center) radius))
         (remove skipped)
         first)))

(defn ^:async dig-ripe!
  "Walk to and dig the nearest ripe block, one per round: :continue, or nil
  when there is none."
  [c ripe-ages center radius]
  (let [pos (ripe-blocks c ripe-ages center radius)]
    (when pos
      (let [walked (await (u/walk-near! c pos reach))]
        (if (not= :there walked)
          (do (when (= :blocked walked) (ctx/update-mem! c update :skipped-blocks (fnil conj []) pos))
              :continue)
          (let [r (await (ctx/act c :dig (clj->js {:pos pos})))]
            (when-not (= "dug" (.-status r))
              (ctx/update-mem! c update :skipped-blocks (fnil conj []) pos))
            :continue))))))

(defn ^:async farm!
  [c {:keys [pos]}]
  (let [radius (:farm-radius (:args c))
        there (await (go-near! c pos))]
    (case there
      :continue :continue
      :far (bury-source! c pos)
      (or (await (collect-drops! c (+ radius 2)))
          (await (dig-ripe! c ripe-age pos radius))))))

(defn best-in-chest [items]
  (->> items
       (filter #(eat/food-points (.-name %)))
       (sort-by #(- (eat/food-points (.-name %))))
       first))

(defn ^:async chest!
  [c {:keys [pos]}]
  (let [there (await (go-near! c pos))]
    (case there
      :continue :continue
      :far (bury-source! c pos)
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))
            best (when (= "ok" (.-status seen)) (best-in-chest (array-seq (.-items seen))))]
        (if-not best
          (bury-source! c pos)
          (let [r (await (ctx/act c :transfer (clj->js {:pos pos :direction "withdraw" :item (.-name best)
                                                        :count (min (.-count best) (:take (:args c)))})))]
            (if (= "ok" (.-status r))
              :continue
              (bury-source! c pos))))))))

(defn nearest-animal [c]
  (let [skipped (set (:skipped-animals (ctx/mem c)))]
    (->> (array-seq (.entities (:primitives c) #js {:radius (:hunt-radius (:args c)) :kind "passive"
                                                    :names (clj->js food-animals) :max 16}))
         (remove #(skipped (.-id %)))
         first)))

(defn ^:async swing!
  "One swing at animal, unless the last was less than :attack-gap-ms ago."
  [c animal]
  (let [last-swing (:last-swing (ctx/mem c))]
    (when-not (and last-swing (< (- (ctx/now c) last-swing) (:attack-gap-ms (:args c))))
      (ctx/update-mem! c assoc :last-swing (ctx/now c))
      (let [r (await (ctx/act c :attack #js {:id (.-id animal)}))]
        (when (= "gone" (.-status r))
          (ctx/update-mem! c update :skipped-animals (fnil conj []) (.-id animal)))))
    :continue))

(defn ^:async hunt!
  "Step 3: loot lying about, else the nearest animal, else wild crops."
  [c]
  (let [{:keys [hunt-radius]} (:args c)
        animal (nearest-animal c)]
    (or (await (collect-drops! c drop-radius))
        (when animal
          (let [apos (u/pos-of (.-pos animal))
                walked (await (u/walk-near! c apos 2))]
            (case walked
              :there (await (swing! c animal))
              :partial :continue
              (do (ctx/update-mem! c update :skipped-animals (fnil conj []) (.-id animal))
                  :continue))))
        (await (dig-ripe! c (merge ripe-age berry-bush) (u/self-pos c) hunt-radius)))))

;; ------------------------------------------------------------------ nothing found

(defn nearest-known-source [c]
  (let [here (u/self-pos c)]
    (->> (ctx/entries c :food-source)
         (map :data)
         (sort-by #(u/dist here (:pos %)))
         first)))

(defn none-text [c]
  (let [{:keys [hunt-radius]} (:args c)
        near (nearest-known-source c)]
    (str "no food: carried none, searched for animals and ripe crops within " hunt-radius " blocks"
         (if near
           (str "; nearest known source is a " (name (:kind near)) " at " (pr-str (:pos near))
                ", " (js/Math.round (u/dist (u/self-pos c) (:pos near))) " blocks away")
           "; no food source is known"))))

(defn none! [c]
  (let [food (.-food (.self (:primitives c)))]
    (ctx/emit! c :food.none :warn {:food food :text (none-text c)})
    (ctx/remember! c :hungry {:food food} hungry-policy)
    :done))

(defn gave-up-recently? [c]
  (pos? (ctx/count-in c :hungry (:ask-cooldown-ms (:args c)))))

;; ------------------------------------------------------------------ round

(defn ^:async use-source!
  "Step 2: :continue when the source gave something to do, else nil."
  [c source]
  (case (:kind source)
    :farm (await (farm! c source))
    :chest (await (chest! c source))
    :animals (let [there (await (go-near! c (:pos source)))]
               (when (= :continue there) :continue))
    nil))

(defn ^:async search! [c]
  (let [source (known-source c)]
    (or (when source (await (use-source! c source)))
        (await (hunt! c))
        (none! c))))

(defn ^:async round [c]
  (let [eaten (await (ctx/call-child c :eat 'jobs.survival.eat {}))]
    (ctx/update-mem! c assoc :eating (= :continue eaten))
    (cond
      (= :continue eaten) :continue
      (not (hungry-now? c)) :done
      (gave-up-recently? c) :done
      :else (await (search! c)))))
