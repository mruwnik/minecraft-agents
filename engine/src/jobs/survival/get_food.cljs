(ns jobs.survival.get-food
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [engine.triggers.hungry :as hungry]
            [engine.foods :as foods]
            [jobs.survival.eat :as eat]))

(def doc
  "Keep the body fed. Each round takes the first step of this ladder that
  has something to do: (1) eat what is carried (the eat job); (1b) when none is carried but 3 or more wheat is, bake bread
  (the craft job, enough loaves for the hunger, within 32 blocks of a table)
  and eat it next round; a failed bake is remembered as :no-bake in body
  memory for 10 minutes, so no firing of this job tries again meanwhile and
  the ladder goes on; (2) use the
  latest :food-source body-memory entry {:pos :kind} within :source-radius:
  a :farm is walked to and its mature crops dug and collected (replanting
  is not this job's business), a :chest is walked to and its best food
  withdrawn (with no edible food in it but 3 or more wheat, wheat is
  withdrawn, in multiples of 3 and enough for the hunger, for step 1b), :animals centres the hunt there; (3) hunt the nearest passive
  food animal within :hunt-radius, or else dig mature crops or sweet berry
  bushes in it, and collect the drops; (4) nothing found: emit a food.none
  warn naming what was searched and the nearest known source, write a
  :hungry entry and finish. For :ask-cooldown-ms after that the round still
  eats and still does step 3 (what is in sight), but does not walk to
  remembered sources it already knew when it gave up (one learned since is
  tried first), say food.none or write :hungry again; with nothing to do it
  returns :declined. Wheat is not harvested. A remembered chest in another's zone or claim is skipped, never taken
  from, in every mode and when starving (one get-food.skipped warn): survival jobs never take other people's stuff
  (:ignore-zones? lifts it). A source found empty or unreachable is forgotten.
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
   :ask-cooldown-ms {:doc "after finding nothing, how long before saying so (and searching) again" :default (* 10 60 1000)}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def food-animals #{"cow" "pig" "sheep" "chicken" "rabbit"})

(def ripe-age
  "The growth stage at which each crop block can be harvested. Wheat is left
  out: it is not harvested here."
  {"carrots" 7 "potatoes" 7 "beetroots" 3})

(def berry-bush {"sweet_berry_bush" 2})

(def harvest-items
  "What the drops of a harvest or a kill may be called."
  (into (set (filter foods/edible? (keys (foods/table)))) #{"wheat" "wheat_seeds" "beetroot_seeds" "poisonous_potato"}))

(def hungry-policy {:cap 10 :ttl (* 60 60 1000)})

(def drop-radius 8)

(def no-bake-policy {:cap 1 :ttl (* 10 60 1000)})

(def no-bake-ms (* 10 60 1000))

(def reach 3)

(defn hungry-now? [c]
  (let [self (.self (:primitives c))]
    (hungry/hungry? (.-food self) (.-health self) (:args c))))

(defn check
  "Hungry, or in the middle of a meal that began when it was."
  [c]
  (let [self (.self (:primitives c))]
    (or (hungry-now? c)
        (hungry/top-up? (.-food self) (.-health self) (hungry/carried-names self))
        (boolean (:eating (ctx/mem c))))))

;; ------------------------------------------------------------------ sources

(defn foreign-chest?
  "A chest source in another's zone or claim: survival jobs never take other people's stuff, in any mode and even
  starving, so it is skipped (one get-food.skipped warn per chest). :ignore-zones? lifts it."
  [c {:keys [pos kind]}]
  (let [v (when (= :chest kind) (access/container-refusal c :take pos))]
    (when v
      (let [fields (access/refusal-fields [v])]
        (ctx/warn-once! c [:skipped pos] :get-food.skipped
                        (assoc fields :pos pos :text (str "skipped the chest at " (pr-str pos) ": " (access/refusal-text fields))))))
    (boolean v)))

(defn usable-entries
  "The :food-source entries, newest first, within :source-radius, not found unusable in this job and not another's
  chest."
  [c]
  (->> (ctx/entries c :food-source)
       reverse
       (filter (fn [{:keys [data]}]
                 (and (<= (u/dist (u/self-pos c) (:pos data)) (:source-radius (:args c)))
                      (not= (:pos data) (:dead-source (ctx/mem c)))
                      (not (foreign-chest? c data)))))))

(defn known-source
  "The newest usable :food-source entry's data (see usable-entries)."
  [c]
  (:data (first (usable-entries c))))

(defn fresh-source
  "known-source, when its entry is strictly newer than the latest :hungry
  entry: a source the last fruitless search never had in front of it."
  [c]
  (let [{source :data learned :t} (first (usable-entries c))
        gave-up (:t (ctx/latest c :hungry))]
    (when (and source learned gave-up (> learned gave-up))
      source)))

(defn bury-source!
  "Give up on the source at pos: not tried again this job, and forgotten."
  [c pos]
  (ctx/update-mem! c assoc :dead-source pos)
  (ctx/forget-where! c :food-source #(= pos (:pos %)))
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
  body first, minus the ones that proved undiggable (a lazy seq)."
  [c ripe-ages center radius]
  (let [skipped (set (:skipped-blocks (ctx/mem c)))]
    (->> (array-seq (.blocks (:primitives c) #js {:radius radius :names (clj->js (keys ripe-ages)) :max 64}))
         (filter #(some-> (.-age %) (>= (ripe-ages (.-name %)))))
         (map #(u/pos-of (.-pos %)))
         (filter #(<= (u/dist % center) radius))
         (remove skipped))))

(defn ^:async dig-ripe!
  "Walk to and dig the nearest ripe block, one per round: :continue, or nil
  when there is none."
  [c ripe-ages center radius]
  (let [{pos :option trespass :trespass} (access/choose c :dig (ripe-blocks c ripe-ages center radius) vector)]
    (when pos
      (access/trespass! c "get-food" trespass)
      (let [walked (await (near/walk-near! c pos reach))]
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

(defn carried-count [c name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn loaves-wanted
  "How many loaves fill the body to full."
  [c]
  (int (js/Math.ceil (/ (- 20 (.-food (.self (:primitives c)))) 5))))

(defn best-in-chest [items]
  (->> items
       (filter #(foods/edible? (.-name %)))
       (sort-by #(- (foods/points (.-name %))))
       first))

(defn wheat-to-take
  "How much wheat to withdraw: the loaves wanted worth, no more than the chest
  holds or :take allows, all in multiples of 3; 0 when under 3."
  [c wheat]
  (let [down3 #(* 3 (quot % 3))]
    (min (* 3 (loaves-wanted c)) (down3 wheat) (down3 (:take (:args c))))))

(defn chest-wheat [items]
  (transduce (comp (filter #(= "wheat" (.-name %))) (map #(.-count %))) + 0 items))

(defn ^:async withdraw!
  "Withdraw n of item from the chest at pos: :continue, or the source buried."
  [c pos item n]
  (let [r (await (ctx/act c :transfer (clj->js {:pos pos :direction "withdraw" :item item :count n})))]
    (if (= "ok" (.-status r))
      :continue
      (bury-source! c pos))))

(defn ^:async chest!
  [c {:keys [pos]}]
  (let [there (await (go-near! c pos))]
    (case there
      :continue :continue
      :far (bury-source! c pos)
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))
            items (when (= "ok" (.-status seen)) (array-seq (.-items seen)))
            best (best-in-chest items)
            wheat (wheat-to-take c (chest-wheat items))]
        (cond
          best (await (withdraw! c pos (.-name best) (min (.-count best) (:take (:args c)))))
          (pos? wheat) (await (withdraw! c pos "wheat" wheat))
          :else (bury-source! c pos))))))

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
                walked (await (near/walk-near! c apos 2))]
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

(defn no-bake-reason
  "The reason of the live :no-bake entry, or nil."
  [c]
  (when (pos? (ctx/count-in c :no-bake no-bake-ms))
    (:reason (:data (ctx/latest c :no-bake)))))

(defn none-text [c]
  (let [{:keys [hunt-radius]} (:args c)
        near (nearest-known-source c)
        wheat (carried-count c "wheat")
        reason (no-bake-reason c)]
    (str "no food: carried none, searched for animals and ripe crops within " hunt-radius " blocks"
         (when (and (>= wheat 3) reason)
           (str "; carrying " wheat " wheat but cannot bake (" reason ")"))
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

;; ------------------------------------------------------------------ bread

(defn can-bake?
  "No edible food carried, 3 or more wheat, and no live :no-bake entry."
  [c]
  (and (nil? (eat/carried-best c))
       (>= (carried-count c "wheat") 3)
       (zero? (ctx/count-in c :no-bake no-bake-ms))))

(defn ^:async bake!
  "Rung 1b: :continue while the craft goes on or made bread to eat, else nil
  with :no-bake remembered."
  [c]
  (let [loaves (min (loaves-wanted c) (quot (carried-count c "wheat") 3))
        r (await (ctx/call-child c :bake 'jobs.items.craft {:item "bread" :count loaves :radius 32}))
        res (when (= :done r) (ctx/child-result c :bake))]
    (cond
      (= :continue r) :continue
      (>= (:made res 0) 1) :continue
      :else (let [reason (or (:reason res) "short")]
              (ctx/remember! c :no-bake {:reason reason} no-bake-policy)
              (ctx/emit! c :food.no-bake :info {:text (str "cannot bake bread: " reason)})
              nil))))

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

(defn ^:async decline-or-hunt!
  "The ask cooldown: a source learned since giving up, else what is in sight,
  else :declined."
  [c]
  (let [source (fresh-source c)]
    (or (when source (await (use-source! c source)))
        (await (hunt! c))
        :declined)))

(defn ^:async below-bake!
  "What follows rung 1b: the cooldown or the search."
  [c]
  (if (gave-up-recently? c)
    (await (decline-or-hunt! c))
    (await (search! c))))

(defn ^:async round [c]
  (let [eaten (await (ctx/call-child c :eat 'jobs.survival.eat {}))]
    (ctx/update-mem! c assoc :eating (= :continue eaten))
    (cond
      (= :continue eaten) :continue
      (not (hungry-now? c)) :done
      :else (or (when (can-bake? c) (await (bake! c)))
                (await (below-bake! c))))))
