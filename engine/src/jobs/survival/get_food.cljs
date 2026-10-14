(ns jobs.survival.get-food
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.crops :as crops]
            [jobs.lib.look :as look]
            [jobs.lib.result :as r]
            [jobs.lib.threats :as threats]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.foods :as foods]
            [jobs.survival.eat :as eat]))

(def doc
  "Keep the body fed. Hungry means food below :food plus one per missing hp, at most 18 (the hungry trigger's test).
  One round is one whole attempt. It eats what is carried (the eat job), up to 18, or up to 20 while health is below
  :health; fed, it ends {:food n}. Still hungry, it takes the first way that has something to do, then eats again:
  1. No food carried but 3 or more wheat: bake bread (the craft job, enough loaves for the hunger, within 32 blocks of
     a table). A failed bake is remembered as :no-bake for 10 minutes and the ladder goes on.
  1b. Raw food carried (chicken, beef ...) with a furnace or smoker seen or remembered (the :furnace place) within ::cook-far-radius (64), or no furnace but enough raw food
     to pay for making one (jobs.lib.cost/cook-worth?): cook it (the smelt job, which fetches a furnace and fuel, waiting out the cook in
     the round), then eat it. Done before eating unless a danger is sensed or food is 0 and health would not outlast the cook plus ::cook-margin-hp. Not at night nor while the night
     handler holds. A failed cook is remembered as :no-cook for 10 minutes.
  2. The newest :food-source entry {:pos :kind} within :source-radius, or a chest it booked food in (memory :fetch/stock) within ::stock-radius (256), reached by go-to:
     :farm: dig its mature crops (blocks.dig, which collects the drops). It does not replant.
     :chest: withdraw its best food (storage.withdraw); with no food but 3 or more wheat, wheat in multiples of 3,
     enough for the hunger, for way 1.
     :animals: hunt there (way 4).
     A chest found empty or a source go-to cannot reach is forgotten; a farm with nothing ripe is passed over.
  3. Harvest items lying within 8 blocks (collect-drops).
  4. The nearest passive food animal within :hunt-radius (combat.attack, then collect-drops).
  5. Mature crops, sweet berry bushes or melons within :hunt-radius (blocks.dig).
  Nothing left: with a fishing rod carried and a bank beside seen water two deep within ::fish-radius (48), once per job it fishes two (jobs.food.fish) and eats or cooks them. Then, once per job (:explore, default on; a starving body again after foods/::starving-rest-ms) it searches for food animals and crops (jobs.explore.search, the
  ::explore setting; not at night nor while the night handler holds) and goes on if it found some; at ::eat-bad-food (6) or below it then eats harmful food carried (rotten
  flesh, with :allow-bad). Still nothing: it stops :no-food with a food.none warning (what was searched, the nearest known source) and writes
  :hungry. For :ask-cooldown-ms after that a round still eats and still does ways 1, 3, 4 and 5, but not a source it
  already knew when it gave up (one learned since is tried first; a starving body, food 0 or health below 7, rests only foods/::starving-rest-ms); with nothing to do (harmful food above ::eat-bad-food does not count) the check waits :no-food-found, else the round declines.
  :stock n: a fed body also takes the ways until the pack holds n food points, else stops :no-stock; while the pack is short of it the ask cooldown does not block the search.
  Never :continue: a go-to that waits on the world, or a child still going after jobs.lib.child's call cap, is waited out in the round.
  More than 12 ways in one round without getting fed stops :still-hungry.
  Wheat is never harvested. Babies, animals in another's zone or claim, animals in a zone or plan of the body's own or inside a closed enclosure (:spare-pens, default true), kinds in :off-limits (default the jobs.lib.animal-spare setting), and the last :keep-adults (2) adults of a kind in sight (for breeding; never hunted, even when starving, unless ::kept-lifted-below lifts it) are never hunted (:ignore-zones? lifts the others' zone only). Each animal attacked is a food.killed info with its name, cell, adults counted, :keep-adults, food and the rule that allowed it; the result carries :killed. A chest in another's zone or claim is skipped and never taken from, even when starving
  (one get-food.skipped warning). A crop in another's zone or claim is dug only when no other is (one
  get-food.trespass-last-resort warning). :ignore-zones? lifts both.
  A cut round starts again from the world: carried food first, then the ways.
  Memory: reads :food-source and :hungry. Writes :hungry {:food} (cap 10, one hour) and :no-bake. Job memory (hints for
  this run): :eating (a meal under way), :tried-sources, :skipped-blocks, :skipped-animals.")

(a/defargs args
  {:food {:doc "hungry below this much food (of 20)" :spec (a/num-in 0 20) :default foods/default-food}
   :health {:doc "below this health eat up to a full bar" :spec (a/num-in 0 20) :default foods/default-health}
   :source-radius {:doc "how far away a remembered food source still counts, in blocks" :spec (a/num-in 0 nil) :default 64}
   :hunt-radius {:doc "how far to look for animals and wild crops, in blocks" :spec (a/num-in 0 nil) :default 24}
   :farm-radius {:doc "how far around a known farm to harvest, in blocks" :spec (a/num-in 0 nil) :default 6}
   :take {:doc "most items to withdraw from a chest in one go" :spec (a/int-in 1 nil) :default 16}
   :attack-gap-ms {:doc "least time between two swings at an animal, so a swing lands at full strength" :spec (a/num-in 0 nil) :default 600}
   :explore {:doc "with nothing known or seen, walk and look for food animals and crops once per job (a starving body again after foods/::starving-rest-ms; not underground unless starving): false, true (the ::explore setting) or jobs.explore.search options over it" :spec (a/or-of boolean? map?) :default true}
   :stock {:doc "also carry food worth this many hunger points: a fed body takes the ways below until the pack holds it (0: only feed)" :spec (a/num-in 0 nil) :default 0}
   :ask-cooldown-ms {:doc "after finding nothing, how long before saying so (and searching) again" :spec (a/num-in 0 nil) :default (* 10 60 1000)}
   :spare-pens {:doc "never hunt animals in a zone or plan of the body's own (false: a job asked to get food may take them)" :spec boolean? :default true}
   :keep-adults {:doc "adults of a kind in sight that are never hunted, so they can breed (hunted only when ::kept-lifted-below lifts it)" :spec (a/int-in 0 nil) :default 2}
   :off-limits {:doc "animal kinds never hunted; nil: the jobs.lib.animal-spare/off-limits setting" :spec (a/or-of (a/pred "nil" nil?) (a/coll-of a/item?)) :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(def food-animals #{"cow" "pig" "sheep" "chicken" "rabbit"})

(def ripe-age
  "The growth stage at which each crop block can be harvested. Wheat is left
  out: it is not harvested here."
  {"carrots" 7 "potatoes" 7 "beetroots" 3})

(def berry-bush {"sweet_berry_bush" 2})

(def melon-block "A melon has no growth stage: it is always ripe." {"melon" 0})

(def wild-food "Blocks dug for food besides the crops." (merge berry-bush melon-block))

(def harvest-items
  "What the drops of a harvest or a kill may be called."
  (into (set (filter foods/edible? (keys (foods/table)))) #{"wheat" "wheat_seeds" "beetroot_seeds" "poisonous_potato"}))

(def hungry-policy {:cap 10 :ttl (* 60 60 1000)})

(def widen-max
  "The search grows with each recent give-up (:hungry entry) up to this many times its size."
  3)

(defn widen
  "How many times the search radius is multiplied: 1 plus the :hungry give-ups within the policy ttl, at most widen-max."
  [c]
  (min widen-max (inc (ctx/count-in c :hungry (:ttl hungry-policy)))))

(defn hunt-radius [c] (* (widen c) (:hunt-radius (:args c))))

(a/defargs settings
  {::drop-radius {:default 8 :doc "Blocks around the body in which dropped food is picked up." :spec (a/int-in 0 nil)}
   ::reach {:default 3 :doc "How near a food source the body stands to use it, in blocks." :spec (a/int-in 0 nil)}
   ::explore {:default {:max-distance 64 :max-legs 12 :timeout-s 120} :doc "jobs.explore.search options of the search for food (:explore true)." :spec map?}
   ::fish-radius {:default 48 :doc "Blocks within which seen water two deep with a bank makes fishing a way, with a fishing rod carried." :spec (a/int-in 0 nil)}
   ::kept-lifted-below {:default -1 :doc "At this much food or less, with no food carried, the last adults of a kind (kept for breeding) are hunted too; -1 (default) never." :spec (a/int-in -1 20)}
   ::eat-bad-food {:default 6 :doc "At this much food or less the last resort eats harmful food carried (rotten flesh); above it harmful food is not food." :spec (a/num-in 0 20)}
   ::stock-radius {:default 256 :doc "Blocks within which a chest the body booked food in (memory :fetch/stock, what it saw when it opened it) counts as a food source." :spec (a/int-in 0 nil)}
   ::cook-far-radius {:default 64 :doc "Blocks within which a furnace or smoker the body has seen (perception's memory) or the one it remembers (:furnace place) counts for cooking." :spec (a/int-in 0 nil)}
   ::shelter-cook-reach {:default 5 :doc "Blocks within which a furnace counts for cooking while the night handler holds the body in its shelter." :spec (a/int-in 0 nil)}
   ::starve-s-per-hp {:default 4 :doc "Seconds of starvation (food 0) that cost one hp." :spec (a/num-in 0 nil)}
   ::cook-s-per-item {:default 10 :doc "Seconds a furnace takes to cook one raw food, to tell whether health outlasts a cook at food 0." :spec (a/num-in 0 nil)}
   ::cook-margin-hp {:default 3 :doc "Hp health must exceed the starvation over the expected cook by, else the body eats raw at food 0." :spec (a/num-in 0 nil)}
   ::furnace-cost {:default 8 :doc "Hunger points a furnace to make and place is counted to cost when cooking is weighed against eating raw." :spec (a/num-in 0 nil)}
   ::place-cost {:default 2 :doc "Hunger points putting down a furnace already carried is counted to cost." :spec (a/num-in 0 nil)}
   ::walk-cost {:default 0.2 :doc "Hunger points one block of walking to a furnace is counted to cost." :spec (a/num-in 0 nil)}
   ::cook-kinds {:default #{"chicken" "beef" "porkchop" "mutton" "rabbit" "cod" "salmon" "potato"} :doc "Raw foods the cook step puts in a furnace." :spec (a/set-of a/item?)}
   ::max-ways {:default 12 :doc "Ways one round tries without getting fed before it stops :still-hungry." :spec (a/int-in 0 nil)}})
(defn shelter-cook-reach [] (settings/get settings ::shelter-cook-reach))

(defn drop-radius [] (settings/get settings ::drop-radius))

(def no-bake-policy {:cap 1 :ttl (* 10 60 1000)})

(def no-bake-ms (* 10 60 1000))

(defn reach [] (settings/get settings ::reach))

(defn max-ways [] (settings/get settings ::max-ways))


(defn cook-kinds [] (settings/get settings ::cook-kinds))

(defn cook-costs [] {:furnace-cost (settings/get settings ::furnace-cost)
                     :place-cost (settings/get settings ::place-cost)
                     :walk-cost (settings/get settings ::walk-cost)})

(defn eat-bad-food [] (settings/get settings ::eat-bad-food))

(defn food-level [c] (.-food (.self (:primitives c))))

(defn hungry-now? [c]
  (let [self (.self (:primitives c))]
    (foods/hungry? (.-food self) (.-health self) (:args c))))

(defn tryable-food?
  "Food worth a round: any edible item, and harmful food (rotten flesh) only at or below the eat-bad line."
  [c item]
  (and (foods/food? (:name item))
       (or (not (contains? foods/harmful (:name item))) (<= (food-level c) (eat-bad-food)))))

(defn stock-short?
  "A :stock is asked for and the pack holds fewer food points than that."
  [c]
  (< (foods/stock-points (u/inventory (:primitives c))) (:stock (:args c))))

(defn wants-food? [c] (or (hungry-now? c) (stock-short? c)))

(declare gave-up-recently? can-cook? night-holds? fresh-source animals-in-sight ripe-blocks can-bake?)

(defn nothing-to-try?
  "Hungry inside the ask cooldown with no food carried, no bread to bake, no source learned since giving up, no
  animal to hunt and no ripe crop in range: a round would only decline."
  [c]
  (and (gave-up-recently? c)
       (not-any? #(tryable-food? c %) (u/inventory (:primitives c)))
       (not (can-bake? c))
       (not (can-cook? c))
       (nil? (fresh-source c))
       (empty? (remove :why (animals-in-sight c)))
       (empty? (ripe-blocks c (merge ripe-age wild-food) (u/self-pos c) (hunt-radius c)))))

(defn check
  "Hungry, or in the middle of a meal that began when it was. Hungry with nothing to try waits :no-food-found."
  [c]
  (let [self (.self (:primitives c))]
    (if (and (hungry-now? c) (not (:eating (ctx/mem c))) (nothing-to-try? c))
      (ctx/wait c {:reason :no-food-found
                   :why (str "hungry (food " (.-food self) ") but no food is carried, known or in sight within "
                             (hunt-radius c) " blocks; searched recently")})
    (or (wants-food? c)
        (foods/top-up? (.-food self) (.-health self) (foods/carried-names self (foods/reserved (ctx/view c))))
        (foods/eat-now? self (:args c) (foods/reserved (ctx/view c)))
        (boolean (:eating (ctx/mem c)))
        (ctx/wait c {:reason :not-hungry
                     :why (str "food " (.-food self) " at health " (.-health self) ", hungry below " (foods/hungry-line (.-health self) (:args c)))})))))

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

(defn stock-entries
  "The chests the body booked food in (memory :fetch/stock, latest per chest) within ::stock-radius, as :food-source-shaped
  entries {:t :data {:pos {:x :y :z} :kind :chest}}: its own stores count as sources though nothing wrote a :food-source."
  [c]
  (let [here (u/self-pos c)]
    (->> (ctx/entries c fetch/stock-kind)
         (reduce (fn [m e] (assoc m (:pos (:data e)) e)) {})
         vals
         (filter (fn [{:keys [data]}] (and (some foods/edible? (keys (:items data))) (<= (u/dist here (zipmap [:x :y :z] (:pos data))) (settings/get settings ::stock-radius)))))
         (map (fn [{:keys [t data]}] {:t t :data {:pos (zipmap [:x :y :z] (:pos data)) :kind :chest}})))))

(defn usable-entries
  "The :food-source entries within :source-radius and the chests booked with food within ::stock-radius, newest first,
  not tried in this job and not another's chest."
  [c]
  (let [tried (set (:tried-sources (ctx/mem c)))
        noted (filter (fn [{:keys [data]}] (<= (u/dist (u/self-pos c) (:pos data)) (:source-radius (:args c)))) (ctx/entries c :food-source))
        known (set (map (comp :pos :data) noted))]
    (->> (concat noted (remove #(known (:pos (:data %))) (stock-entries c)))
         (sort-by :t >)
         (filter (fn [{:keys [data]}]
                   (and (not (tried (:pos data)))
                        (not (foreign-chest? c data))))))))

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

(defn pass-over!
  "Not try the source at pos again in this job."
  [c pos]
  (ctx/update-mem! c update :tried-sources (fnil conj []) pos))

(defn bury-source!
  "Give up on the source at pos: not tried again this job, and forgotten. :again, for the next way."
  [c pos]
  (pass-over! c pos)
  (ctx/forget-where! c :food-source #(= pos (:pos %)))
  :again)

(defn ^:async go-near!
  "Walk within reach of pos by one go-to call: :there, :far (it gave up) or :continue (go-to waits on the world)."
  [c pos]
  (let [r (await (child/run! c :goto 'jobs.movement.go-to {:pos pos :range (reach)} {:max-calls 1}))]
    (cond
      (= :continue r) :continue
      (<= (u/dist (u/self-pos c) pos) (inc (reach))) :there
      :else :far)))

;; ------------------------------------------------------------------ gathering

(defn carried-count [c name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn ^:async collect-drops!
  "Pick up the harvest items within radius (collect-drops): :again when some came in, else nil."
  [c radius]
  (let [st (await (child/run! c :collect 'jobs.forestry.collect-drops {:radius radius :filter (vec harvest-items)}))]
    (case st
      :continue :continue
      :done (when (pos? (:collected (ctx/child-result c :collect) 0)) :again)
      nil)))

(defn ripe-blocks
  "The ripe blocks named in ripe-ages within radius of center, nearest to the
  body first, minus the ones that proved undiggable (a lazy seq)."
  [c ripe-ages center radius]
  (let [skipped (set (:skipped-blocks (ctx/mem c)))]
    (->> (crops/seen-crops (:primitives c) (keys ripe-ages) radius 64)
         (filter #(let [need (ripe-ages (:name %))] (or (zero? need) (some-> (:age %) (>= need)))))
         (map :pos)
         (filter #(<= (u/dist % center) radius))
         (remove skipped))))

(defn ^:async dig-ripe!
  "Dig the nearest ripe block, permitted ones first (blocks.dig walks there and collects the drops): :again, or nil
  when there is none. A block not dug is skipped from then on."
  [c ripe-ages center radius]
  (when (empty? (ripe-blocks c ripe-ages center radius))
    (await (look/survey! c :cell)))
  (let [{pos :option trespass :trespass} (access/choose c :dig (ripe-blocks c ripe-ages center radius) vector)]
    (when pos
      (access/trespass! c "get-food" trespass)
      ;; access/choose made the zone call (a missing zone list never blocks survival); the dig notes a trespass for restore-broken
      (let [st (await (child/run! c :dig 'jobs.blocks.dig {:pos pos :ignore-zones? true}))]
        (cond
          (= :continue st) :continue
          (and (= :done st) (:dug (ctx/child-result c :dig))) :again
          :else (do (ctx/update-mem! c update :skipped-blocks (fnil conj []) pos) :again))))))

(defn ^:async farm!
  [c {:keys [pos]}]
  (case (await (go-near! c pos))
    :continue :continue
    :far (bury-source! c pos)
    (or (await (dig-ripe! c ripe-age pos (:farm-radius (:args c))))
        (do (pass-over! c pos) nil))))

(defn loaves-wanted
  "How many loaves fill the body to full."
  [c]
  (int (js/Math.ceil (/ (- 20 (food-level c)) 5))))

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
  "Withdraw n of item from the chest at pos (storage.withdraw): :again; a chest that gave nothing is forgotten."
  [c pos item n]
  (let [before (carried-count c item)
        st (await (child/run! c :withdraw 'jobs.storage.withdraw
                              {:chest pos :items {item (+ before n)} :ignore-zones? (:ignore-zones? (:args c))}))]
    (cond
      (= :continue st) :continue
      (> (carried-count c item) before) :again
      :else (bury-source! c pos))))

(defn ^:async chest!
  "Look in the chest (the inventory may have changed since it was learned) and withdraw its best food, or wheat."
  [c {:keys [pos]}]
  (case (await (go-near! c pos))
    :continue :continue
    :far (bury-source! c pos)
    (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))
          status (.-status seen)
          items (when (= "ok" status) (array-seq (.-items seen)))
          best (best-in-chest items)
          wheat (wheat-to-take c (chest-wheat items))]
      (cond
        (not (#{"ok" "missing"} status)) (do (pass-over! c pos) :again)
        best (await (withdraw! c pos (.-name best) (min (.-count best) (:take (:args c)))))
        (pos? wheat) (await (withdraw! c pos "wheat" wheat))
        :else (bury-source! c pos)))))

(defn animals-in-sight
  "The food animals the entity scan lists within :hunt-radius, nearest first, as {:entity e :why r :adults n :rule k}: r is nil for one
  to hunt, else :tried (attacked this job), :baby, :refused (another's zone or claim), or what jobs.lib.animal-spare/why
  says (:off-limits, :pen: a zone or plan of its own or a closed enclosure, :kept: the last :keep-adults of a kind are lifted only at ::kept-lifted-below
  food or less with no food carried, by default never); n counts the adults of its kind in sight and k is the spare rule that allowed the hunt."
  [c]
  (let [skipped (set (:skipped-animals (ctx/mem c)))
        seen (array-seq (.entities (:primitives c) #js {:radius (hunt-radius c) :kind "passive"
                                                        :names (clj->js food-animals) :max 16}))
        adults (frequencies (map #(.-name %) (remove #(true? (.-baby %)) seen)))
        self (.self (:primitives c))
        pens (atom [])
        starved? (and (<= (.-food self) (settings/get settings ::kept-lifted-below)) (not (foods/carries-food? self)))]
    (->> seen
         (mapv (fn [e]
                 (let [n (adults (.-name e))
                       penned? (and (not (skipped (.-id e))) (not (true? (.-baby e)))
                                    (spare/penned? c pens e))
                       why (cond (skipped (.-id e)) :tried
                                 (true? (.-baby e)) :baby
                                 (access/container-refusal c :take (u/pos-of (.-pos e))) :refused
                                 :else (spare/why (:args c) (.-name e) n penned? starved?))]
                   {:entity e :why why :adults n
                    :rule (when-not why (spare/allowed-by (:args c) n penned?))}))))))

(defn nearest-animal
  "The nearest adult food animal within :hunt-radius not skipped in this job and not in another's zone or claim, as an animals-in-sight entry."
  [c]
  (->> (animals-in-sight c) (remove :why) first))

(defn ^:async animal-in-sight
  "The nearest food animal in sight (an animals-in-sight entry); with none, the body looks around once from where it stands and tries again."
  [c]
  (or (nearest-animal c)
      (await (look/survey-until! c :cell #(nearest-animal c)))))

(defn note-kill!
  "Say which animal the hunt attacked and where, with the adults of its kind counted, the keep threshold, the food level
  and the rule that allowed it, and keep it for the result."
  [c {animal :entity :keys [adults rule]}]
  (let [p (u/pos-of (.-pos animal))
        cell (mapv #(js/Math.floor %) [(:x p) (:y p) (:z p)])
        why {:adults adults :keep (:keep-adults (:args c)) :food (food-level c) :rule rule}]
    (ctx/update-mem! c update :killed (fnil conj []) (merge {:name (.-name animal) :pos cell} why))
    (ctx/emit! c :food.killed :info (merge {:name (.-name animal) :pos cell
                                            :text (str "hunted a " (.-name animal) " at " (pr-str cell)
                                                       " (" adults " adults, keep " (:keep why) ", food " (:food why) ", " (name rule) ")")}
                                           why))))

(defn ^:async hunt!
  "Kill the nearest food animal (combat.attack) and pick up its drops: :again, or nil when there is none (after a
  look around). An animal is attacked once per job, killed or not."
  [c]
  (when-let [{animal :entity :as target} (await (animal-in-sight c))]
    (let [{:keys [attack-gap-ms]} (:args c)
          hunt-radius (hunt-radius c)
          st (await (child/run! c :attack 'jobs.combat.attack
                                {:targets [(.-id animal)] :radius hunt-radius :attack-gap-ms attack-gap-ms
                                 :lost-s 0 :timeout-s 60}))]
      (if (= :continue st)
        :continue
        (do (ctx/update-mem! c update :skipped-animals (fnil conj []) (.-id animal))
            (note-kill! c target)
            (or (await (collect-drops! c (drop-radius))) :again))))))

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

(defn harmful-carried-text
  "\"; carrying 2 chicken, 5 rotten_flesh, skipped on purpose (...)\" for the harmful foods in the inventory, or nil."
  [inventory]
  (let [bad (->> inventory (filter #(contains? foods/harmful (:name %))) (map (juxt :name :count)))]
    (when (seq bad)
      (str "; carrying " (str/join ", " (map (fn [[n k]] (str k " " n)) bad))
           ", skipped on purpose (raw or rotten food hurts; eat with :allow-bad via the eat job)"))))

(defn passed-over
  "The animals in sight that were not hunted, as {:name :id :why}."
  [c]
  (->> (animals-in-sight c) (filter :why) (mapv (fn [{:keys [entity why]}] {:name (.-name entity) :id (.-id entity) :why why}))))

(defn nearby
  "The mobs of any kind the body sees or heard within :hunt-radius, as {:name :kind :distance}: tells a hunt filter from
  an empty scan when no animal qualified."
  [c]
  (->> (look/seen-entities (:primitives c) {:radius (hunt-radius c) :max 16})
       (remove #(#{"item" "player"} (:kind %)))
       (mapv (fn [e] {:name (:name e) :kind (:kind e) :distance (/ (js/Math.round (* 10 (:distance e))) 10)}))))

(defn none-text [c around explored]
  (let [hunt-radius (hunt-radius c)
        near (nearest-known-source c)
        passed (passed-over c)
        wheat (carried-count c "wheat")
        reason (no-bake-reason c)]
    (str "no food: carried none" (harmful-carried-text (u/inventory (:primitives c))) ", searched for animals and ripe crops within " hunt-radius " blocks"
         (when explored (str ", explored: " explored))
         (when (and (>= wheat 3) reason)
           (str "; carrying " wheat " wheat but cannot bake (" reason ")"))
         (when (seq passed)
           (str "; passed over " (count passed) " animal" (when (> (count passed) 1) "s") " ("
                (str/join ", " (map (fn [a] (str (:name a) ": " (if (= :kept (:why a)) "kept for breeding" (name (:why a))))) passed)) ")"))
         (when (and (empty? passed) (seq around))
           (str "; nothing hunted; in range: " (str/join ", " (map (fn [a] (str (:name a) " (" (:kind a) ")")) around))))
         (if near
           (str "; nearest known source is a " (name (:kind near)) " at " (pr-str (:pos near))
                ", " (js/Math.round (u/dist (u/self-pos c) (:pos near))) " blocks away")
           "; no food source is known"))))

(defn none!
  "Stop :no-food with a food.none warn and write :hungry, which starts the ask cooldown."
  [c]
  (let [food (food-level c)
        around (nearby c)
        text (none-text c around (:explored (ctx/mem c)))]
    (ctx/emit! c :food.none :warn {:food food :animals (passed-over c) :nearby around
                                   :text (str text "; the hungry reflex rests for "
                                              (js/Math.round (/ (:ask-cooldown-ms (:args c)) 60000))
                                              " min unless food is carried, wheat to bake is, or a food source is learned")})
    (ctx/remember! c :hungry {:food food} hungry-policy)
    (r/stop! c :no-food text :food food)))

(defn stock-stop!
  "Fed but the pack cannot be filled: stop :no-stock (no :hungry entry, the body is not hungry)."
  [c]
  (let [have (foods/stock-points (u/inventory (:primitives c)))]
    (r/stop! c :no-stock (str "fed (food " (food-level c) ") but carries " have " of " (:stock (:args c))
                              " food points and found no more within " (hunt-radius c) " blocks")
             :food (food-level c) :stock (:stock (:args c)) :have have)))

(defn starving?
  "Food 0, or health at or below 6 and hungry: the rest after a fruitless search is only foods/::starving-rest-ms."
  [c]
  (foods/starving? (.self (:primitives c)) (:args c)))

(defn gave-up-recently?
  "Whether a fruitless search is still inside the ask cooldown; an explicit :stock ask not yet met is never blocked by it."
  [c]
  (and (not (stock-short? c))
       (pos? (ctx/count-in c :hungry (if (starving? c)
                                       (min (:ask-cooldown-ms (:args c)) (foods/starving-rest-ms))
                                       (:ask-cooldown-ms (:args c)))))))

;; ------------------------------------------------------------------ bread

(defn can-bake?
  "No edible food carried, 3 or more wheat, and no live :no-bake entry."
  [c]
  (and (nil? (eat/carried-best c))
       (>= (carried-count c "wheat") 3)
       (zero? (ctx/count-in c :no-bake no-bake-ms))))

(defn ^:async bake!
  "Way 1: :again when bread was made, else nil with :no-bake remembered."
  [c]
  (let [loaves (min (loaves-wanted c) (quot (carried-count c "wheat") 3))
        st (await (child/run! c :bake 'jobs.items.craft {:item "bread" :count loaves :radius 32 :fetch false}))
        res (when (= :done st) (ctx/child-result c :bake))]
    (cond
      (= :continue st) :continue
      (>= (:made res 0) 1) :again
      :else (let [reason (or (some-> (:reason res) name) "short")]
              (ctx/remember! c :no-bake {:reason reason} no-bake-policy)
              (ctx/emit! c :food.no-bake :info {:text (str "cannot bake bread: " reason)})
              nil))))

;; ------------------------------------------------------------------ cooking

(defn raw-carried
  "The cookable raw foods carried, as {:name :count}."
  [c]
  (filter #(contains? (cook-kinds) (:name %)) (u/inventory (:primitives c))))

(defn seen-cooker
  "The nearest furnace or smoker the body has seen within radius (perception's memory, still there), as a position, or nil."
  [c radius]
  (->> (look/seen-blocks (:primitives c) {:names #{"furnace" "smoker"} :radius radius :max 8 :live? true})
       first :pos))

(defn nearest-cooker
  "The nearest furnace or smoker seen within ::cook-far-radius, else the furnace the body remembers (the :furnace
  place) within it, as a position, or nil."
  [c]
  (let [far (settings/get settings ::cook-far-radius)]
    (or (seen-cooker c far)
        (let [known (mem/place (ctx/view c) :furnace)]
          (when (and known (<= (u/dist (u/self-pos c) known) far))
            known)))))

(defn starving-too-soon?
  "At food 0, health cannot outlast the starvation over the expected cook (::cook-s-per-item per raw food carried,
  at ::starve-s-per-hp) plus ::cook-margin-hp."
  [c]
  (let [cook-s (* (settings/get settings ::cook-s-per-item) (reduce + (map :count (raw-carried c))))
        lost (/ cook-s (settings/get settings ::starve-s-per-hp))]
    (<= (.-health (.self (:primitives c))) (+ lost (settings/get settings ::cook-margin-hp)))))

(defn rushed?
  "No time to cook: a real danger is sensed near, or food is 0 and health would not outlast the cook (starving-too-soon?)."
  [c]
  (boolean (or (seq (threats/sensed-mobs (:primitives c) {}))
               (and (<= (food-level c) 0) (starving-too-soon? c)))))

(defn worth-cooking?
  "Cooking the raw food carried beats eating it now (jobs.lib.cost/cook-worth?): the walk to the furnace known, or
  the one smelt makes (or, carried, only puts down) when none is."
  [c]
  (let [cooker (nearest-cooker c)]
    (cost/cook-worth? (raw-carried c) (food-level c) (when cooker (u/dist (u/self-pos c) cooker))
                      (boolean (some #(contains? #{"furnace" "smoker"} (:name %)) (u/inventory (:primitives c))))
                      (cook-costs))))

(defn can-cook?
  "Mid-cook, or raw food carried and no live :no-cook entry, with a furnace known or cooking worth a furnace of its
  own (worth-cooking?); never at night, while the night handler holds or when rushed? (a cook under way is dropped then)."
  [c]
  (boolean
   (and (not (sh/night? (:primitives c)))
        (not (night-holds? c))
        (not (rushed? c))
        (or (:cooking (ctx/mem c))
            (and (seq (raw-carried c))
                 (zero? (ctx/count-in c :no-cook no-bake-ms))
                 (or (nearest-cooker c) (worth-cooking? c)))))))

(defn cook-first?
  "Cook before eating: can-cook? and the cook worth its time now (a cook under way goes on)."
  [c]
  (boolean (and (can-cook? c) (or (:cooking (ctx/mem c)) (worth-cooking? c)))))

(defn ^:async cook!
  "Way 1b: smelt the raw food carried (the smelt job, fuel as it takes it) in the furnace known (or the given one);
  the cooking wait is spent in the round. :again when it ended, :continue while it goes on, else nil with :no-cook
  remembered."
  ([c] (await (cook! c nil)))
  ([c cooker]
  (let [{:keys [name count]} (or (first (raw-carried c)) (:cooking (ctx/mem c)))
        furnace (or (:furnace (:cooking (ctx/mem c))) cooker (nearest-cooker c))
        args (cond-> {:item name :count count} furnace (assoc :furnace furnace))
        _ (ctx/update-mem! c assoc :cooking {:name name :count count :furnace furnace})]
    (loop [n 0]
      (let [st (await (child/run! c :cook 'jobs.items.smelt args))]
        (cond
          (not (ctx/alive? c)) :continue
          (= :continue st) :continue
          (and (= :declined st) (< n (child/default-max-calls))) (do (await (child/pace!)) (recur (inc n)))
          (= :declined st) :continue
          :else (let [res (ctx/child-result c :cook)]
                  (ctx/update-mem! c dissoc :cooking)
                  (if (pos? (:smelted res 0))
                    :again
                    (let [reason (or (:reason res) "short")]
                      (ctx/remember! c :no-cook {:reason reason} no-bake-policy)
                      (ctx/emit! c :food.no-cook :info {:text (str "cannot cook " name ": " reason)})
                      nil)))))))))

;; ------------------------------------------------------------------ round

(defn ^:async use-source!
  "Way 2: :again or :continue when the source gave something to do, else nil."
  [c source]
  (case (:kind source)
    :farm (await (farm! c source))
    :chest (await (chest! c source))
    :animals (case (await (go-near! c (:pos source)))
               :continue :continue
               :far (bury-source! c (:pos source))
               (do (pass-over! c (:pos source)) (await (hunt! c))))
    nil))

(defn ^:async next-way!
  "The first way with something to do: :again (try eating, then the ways, again), :continue (yield), or nil when
  none is left. Inside the ask cooldown only a source learned since counts."
  [c]
  (let [source (if (gave-up-recently? c) (fresh-source c) (known-source c))]
    (or (when (can-bake? c) (await (bake! c)))
        (when (can-cook? c) (await (cook! c)))
        (when source (await (use-source! c source)))
        (await (collect-drops! c (drop-radius)))
        (await (hunt! c))
        (await (dig-ripe! c (merge ripe-age wild-food) (u/self-pos c) (hunt-radius c))))))

(defn night-holds?
  "Whether the night handler would take the body now (jobs.lib.shelter/night-holds?): no exploring then."
  [c]
  (boolean (sh/night-holds? (:primitives c) (ctx/view c) {} (sh/bed-permit (:primitives c) (:world (:engine c))))))

(defn night-blocks-search?
  "Whether the night stops the search: it does, unless the body is starving and no danger is near."
  [c]
  (and (or (sh/night? (:primitives c)) (night-holds? c))
       (not (and (starving? c)
                 (not (danger-q/danger-near? (:primitives c) (danger-q/hostile-radius) (danger-q/ranged-radius)))))))

(defn explore-done?
  "Whether this job's search is over: it ran, and a starving body's foods/::starving-rest-ms has not passed since."
  [mem starving? now rest-ms]
  (boolean (and (:explored mem) (not (and starving? (>= (- now (:explored-at mem 0)) rest-ms))))))

(def search-max-distance "The largest :max-distance jobs.explore.search accepts." 128)

(defn widen-explore [opts k]
  (-> opts (update :max-distance #(some-> % (* k) (min search-max-distance))) (update :max-legs #(some-> % (* k))) (update :timeout-s #(some-> % (* k)))))

(defn ^:async explore!
  "Once per job, with nothing known or seen: search for food animals and ripe crops (jobs.explore.search, :explore),
  not at night (a body sheltered for the night stays in) nor while the night handler holds, unless starving with no
  danger near (jobs.lib.danger/danger-near?), nor underground (sh/buried?) unless starving. :again when the search found something to hunt or dig, else nil with what it
  covered remembered."
  [c]
  (let [e (:explore (:args c))]
    (cond
      (or (not e) (explore-done? (ctx/mem c) (starving? c) (ctx/now c) (foods/starving-rest-ms))) nil
      (night-blocks-search? c)
      (do (ctx/update-mem! c assoc :explored "not searched at night or while the night handler holds") nil)
      (and (sh/buried? (:primitives c)) (not (starving? c)))
      (do (ctx/update-mem! c assoc :explored "not searched underground: the caves are dark and full of mobs; surface or eat first") nil)
      :else
      (do
        (ctx/update-mem! c assoc :explored "started" :explored-at (ctx/now c))
        (let [opts (widen-explore (merge (settings/get settings ::explore) (when (map? e) e)) (widen c))
              st (await (child/run! c :explore 'jobs.explore.search
                                    (assoc opts :target (vec (concat (sort food-animals) (keys ripe-age) (keys wild-food))))
                                    {:max-calls 1}))]
          (if (= :continue st)
            :continue
            (let [res (ctx/child-result c :explore)]
              (ctx/update-mem! c assoc :explored
                               (str (count (:found res)) " found, " (get-in res [:coverage :legs] 0) " legs"
                                    (when (:why res) (str ", " (name (:why res))))))
              (when (= :found (:reason res)) :again))))))))

(defn fish-spot?
  "Whether a fishing rod is carried and a bank beside seen water two deep lies within ::fish-radius."
  [c]
  (let [p (:primitives c)]
    (boolean (and (some #(= "fishing_rod" (:name %)) (u/inventory p))
                  (some #(= "water" (u/seen-name p (update (:water %) :y dec)))
                        (shore/spots p (u/self-pos c) {:radius (settings/get settings ::fish-radius)}))))))

(defn ^:async fish!
  "With nothing else to do, a rod carried and a bank in sight: jobs.food.fish for two fish, once per job (:fished), not at
  night unless starving with no danger near. :again when it caught any (they are eaten or cooked next), :continue while
  the child waits on the world, else nil."
  [c]
  (when (and (not (:fished (ctx/mem c))) (not (night-blocks-search? c)) (fish-spot? c))
    (let [st (await (child/run! c :fish 'jobs.food.fish {:count 2 :radius (settings/get settings ::fish-radius) :fetch false} {:max-calls 1}))]
      (if (= :continue st)
        :continue
        (do (ctx/update-mem! c assoc :fished true)
            (when (pos? (:got (ctx/child-result c :fish) 0)) :again))))))

(defn ^:async eat-bad!
  "Last resort when food is at or below the ::eat-bad-food line: eat what hurts (rotten flesh) with :allow-bad.
  :again when the food level rose, else nil."
  [c]
  (when (and (<= (food-level c) (eat-bad-food))
             (some #(contains? foods/harmful (:name %)) (u/inventory (:primitives c))))
    (let [before (food-level c)
          st (await (child/run! c :eat-bad 'jobs.survival.eat {:until foods/top-up-food :allow-bad true}))]
      (if (= :continue st) :continue (when (> (food-level c) before) :again)))))

(defn ^:async eat-carried!
  "Eat what is carried (the eat job) up to 18, or 20 below the :health line: :continue when the eat job is still
  going after the call cap, else nil."
  [c]
  (let [low? (< (.-health (.self (:primitives c))) (:health (:args c)))
        _ (ctx/update-mem! c assoc :eating true)
        st (await (child/run! c :eat 'jobs.survival.eat {:until (if low? 20 foods/top-up-food)}))]
    (if (= :continue st)
      :continue
      (do (ctx/update-mem! c dissoc :eating) nil))))

(defn ^:async round
  "One whole attempt: eat, then while hungry take the next way and eat again. A step that yields (:continue) is
  waited out in the round (a reflex never yields); a cut round ends :done and starts again from the world."
  [c]
  (loop [n 0]
    (let [cooked (when (cook-first? c) (await (cook! c)))
          eaten (if (or (= :continue cooked) (= :again cooked)) cooked (await (eat-carried! c)))
          way (when-not (or (= :continue eaten) (= :again cooked))
                (cond
                  (not (wants-food? c)) ::fed
                  (>= n (max-ways)) ::enough
                  :else (await (next-way! c))))]
      (cond
        (not (ctx/alive? c)) :done
        (= :again cooked) (recur n)
        (or (= :continue eaten) (= :continue way)) (do (await (child/pace!)) (recur n))
        (= ::fed way) (r/finish! c (cond-> {:food (food-level c)} (seq (:killed (ctx/mem c))) (assoc :killed (:killed (ctx/mem c)))))
        (= ::enough way) (r/stop! c :still-hungry (str "still hungry (food " (food-level c) ") after " n " ways")
                                  :food (food-level c))
        (= :again way) (recur (inc n))
        :else (let [fished (when-not (gave-up-recently? c) (await (fish! c)))
                    more (cond fished fished (or (gave-up-recently? c) (not (:explore (:args c)))) nil :else (await (explore! c)))
                    bad (when-not more (await (eat-bad! c)))]
                (cond
                  (or (= :continue more) (= :continue bad)) (do (await (child/pace!)) (recur n))
                  (or more bad) (recur (inc n))
                  (not (hungry-now? c)) (stock-stop! c)
                  (gave-up-recently? c) :declined
                  :else (none! c)))))))
