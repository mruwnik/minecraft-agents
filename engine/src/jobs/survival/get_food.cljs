(ns jobs.survival.get-food
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.crops :as crops]
            [jobs.lib.look :as look]
            [jobs.lib.result :as r]
            [jobs.lib.util :as u]
            [jobs.lib.foods :as foods]
            [jobs.survival.eat :as eat]))

(def doc
  "Keep the body fed. Hungry means food below :food plus one per missing hp, at most 18 (the hungry trigger's test).
  One round is one whole attempt. It eats what is carried (the eat job), up to 18, or up to 20 while health is below
  :health; fed, it ends {:food n}. Still hungry, it takes the first way that has something to do, then eats again:
  1. No food carried but 3 or more wheat: bake bread (the craft job, enough loaves for the hunger, within 32 blocks of
     a table). A failed bake is remembered as :no-bake for 10 minutes and the ladder goes on.
  2. The newest :food-source entry {:pos :kind} within :source-radius, reached by go-to:
     :farm: dig its mature crops (blocks.dig, which collects the drops). It does not replant.
     :chest: withdraw its best food (storage.withdraw); with no food but 3 or more wheat, wheat in multiples of 3,
     enough for the hunger, for way 1.
     :animals: hunt there (way 4).
     A chest found empty or a source go-to cannot reach is forgotten; a farm with nothing ripe is passed over.
  3. Harvest items lying within 8 blocks (collect-drops).
  4. The nearest passive food animal within :hunt-radius (combat.attack, then collect-drops).
  5. Mature crops or sweet berry bushes within :hunt-radius (blocks.dig).
  Nothing left: it stops :no-food with a food.none warning (what was searched, the nearest known source) and writes
  :hungry. For :ask-cooldown-ms after that a round still eats and still does ways 1, 3, 4 and 5, but not a source it
  already knew when it gave up (one learned since is tried first); with nothing to do it declines, quietly.
  Never :continue: a go-to that waits on the world, or a child still going after jobs.lib.child's call cap, is waited out in the round.
  More than 12 ways in one round without getting fed stops :still-hungry.
  Wheat is never harvested. Babies and animals in another's zone or claim are never hunted (:ignore-zones? lifts the zone). A chest in another's zone or claim is skipped and never taken from, even when starving
  (one get-food.skipped warning). A crop in another's zone or claim is dug only when no other is (one
  get-food.trespass-last-resort warning). :ignore-zones? lifts both.
  A cut round starts again from the world: carried food first, then the ways.
  Memory: reads :food-source and :hungry. Writes :hungry {:food} (cap 10, one hour) and :no-bake. Job memory (hints for
  this run): :eating (a meal under way), :tried-sources, :skipped-blocks, :skipped-animals.")

(def args
  {:food {:doc "hungry below this much food (of 20)" :default foods/default-food}
   :health {:doc "below this health eat up to a full bar" :default foods/default-health}
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

(def max-ways "Ways one round tries without getting fed before it stops :still-hungry." 12)

(defn food-level [c] (.-food (.self (:primitives c))))

(defn hungry-now? [c]
  (let [self (.self (:primitives c))]
    (foods/hungry? (.-food self) (.-health self) (:args c))))

(defn check
  "Hungry, or in the middle of a meal that began when it was."
  [c]
  (let [self (.self (:primitives c))]
    (or (hungry-now? c)
        (foods/top-up? (.-food self) (.-health self) (foods/carried-names self))
        (foods/eat-now? self (:args c))
        (boolean (:eating (ctx/mem c)))
        (ctx/wait c {:reason :not-hungry}))))

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
  "The :food-source entries, newest first, within :source-radius, not tried in this job and not another's chest."
  [c]
  (let [tried (set (:tried-sources (ctx/mem c)))]
    (->> (ctx/entries c :food-source)
         reverse
         (filter (fn [{:keys [data]}]
                   (and (<= (u/dist (u/self-pos c) (:pos data)) (:source-radius (:args c)))
                        (not (tried (:pos data)))
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
  (let [r (await (child/run! c :goto 'jobs.movement.go-to {:pos pos :range reach} {:max-calls 1}))]
    (cond
      (= :continue r) :continue
      (<= (u/dist (u/self-pos c) pos) (inc reach)) :there
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
         (filter #(some-> (:age %) (>= (ripe-ages (:name %)))))
         (map :pos)
         (filter #(<= (u/dist % center) radius))
         (remove skipped))))

(defn ^:async dig-ripe!
  "Dig the nearest ripe block, permitted ones first (blocks.dig walks there and collects the drops): :again, or nil
  when there is none. A block not dug is skipped from then on."
  [c ripe-ages center radius]
  (when (and (empty? (ripe-blocks c ripe-ages center radius)) (not (look/looked-here? c)))
    (await (look/look-around! c)))
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
  "The food animals the entity scan lists within :hunt-radius, nearest first, as {:entity e :why r}: r is nil for one
  to hunt, else :tried (attacked this job), :baby or :refused (another's zone or claim)."
  [c]
  (let [skipped (set (:skipped-animals (ctx/mem c)))]
    (->> (array-seq (.entities (:primitives c) #js {:radius (:hunt-radius (:args c)) :kind "passive"
                                                    :names (clj->js food-animals) :max 16}))
         (mapv (fn [e] {:entity e
                        :why (cond (skipped (.-id e)) :tried
                                   (true? (.-baby e)) :baby
                                   (access/container-refusal c :take (u/pos-of (.-pos e))) :refused)})))))

(defn nearest-animal
  "The nearest adult food animal within :hunt-radius not skipped in this job and not in another's zone or claim."
  [c]
  (->> (animals-in-sight c) (remove :why) first :entity))

(defn ^:async animal-in-sight
  "The nearest food animal in sight; with none, the body looks around once from where it stands and tries again."
  [c]
  (or (nearest-animal c)
      (when-not (look/looked-here? c)
        (await (look/look-around! c))
        (nearest-animal c))))

(defn ^:async hunt!
  "Kill the nearest food animal (combat.attack) and pick up its drops: :again, or nil when there is none (after a
  look around). An animal is attacked once per job, killed or not."
  [c]
  (when-let [animal (await (animal-in-sight c))]
    (let [{:keys [hunt-radius attack-gap-ms]} (:args c)
          st (await (child/run! c :attack 'jobs.combat.attack
                                {:targets [(.-id animal)] :radius hunt-radius :attack-gap-ms attack-gap-ms
                                 :lost-s 0 :timeout-s 60}))]
      (if (= :continue st)
        :continue
        (do (ctx/update-mem! c update :skipped-animals (fnil conj []) (.-id animal))
            (or (await (collect-drops! c drop-radius)) :again))))))

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
  (->> (look/seen-entities (:primitives c) {:radius (:hunt-radius (:args c)) :max 16})
       (remove #(#{"item" "player"} (:kind %)))
       (mapv (fn [e] {:name (:name e) :kind (:kind e) :distance (/ (js/Math.round (* 10 (:distance e))) 10)}))))

(defn none-text [c around]
  (let [{:keys [hunt-radius]} (:args c)
        near (nearest-known-source c)
        passed (passed-over c)
        wheat (carried-count c "wheat")
        reason (no-bake-reason c)]
    (str "no food: carried none" (harmful-carried-text (u/inventory (:primitives c))) ", searched for animals and ripe crops within " hunt-radius " blocks"
         (when (and (>= wheat 3) reason)
           (str "; carrying " wheat " wheat but cannot bake (" reason ")"))
         (when (seq passed)
           (str "; passed over " (count passed) " animal" (when (> (count passed) 1) "s") " ("
                (str/join ", " (map (fn [a] (str (:name a) ": " (name (:why a)))) passed)) ")"))
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
        text (none-text c around)]
    (ctx/emit! c :food.none :warn {:food food :animals (passed-over c) :nearby around
                                   :text (str text "; the hungry reflex rests for "
                                              (js/Math.round (/ (:ask-cooldown-ms (:args c)) 60000))
                                              " min unless food is carried, wheat to bake is, or a food source is learned")})
    (ctx/remember! c :hungry {:food food} hungry-policy)
    (r/stop! c :no-food text :food food)))

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
  "Way 1: :again when bread was made, else nil with :no-bake remembered."
  [c]
  (let [loaves (min (loaves-wanted c) (quot (carried-count c "wheat") 3))
        st (await (child/run! c :bake 'jobs.items.craft {:item "bread" :count loaves :radius 32 :fetch false}))
        res (when (= :done st) (ctx/child-result c :bake))]
    (cond
      (= :continue st) :continue
      (>= (:made res 0) 1) :again
      :else (let [reason (or (:reason res) "short")]
              (ctx/remember! c :no-bake {:reason reason} no-bake-policy)
              (ctx/emit! c :food.no-bake :info {:text (str "cannot bake bread: " reason)})
              nil))))

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
        (when source (await (use-source! c source)))
        (await (collect-drops! c drop-radius))
        (await (hunt! c))
        (await (dig-ripe! c (merge ripe-age berry-bush) (u/self-pos c) (:hunt-radius (:args c)))))))

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
    (let [eaten (await (eat-carried! c))
          way (when-not (= :continue eaten)
                (cond
                  (not (hungry-now? c)) ::fed
                  (>= n max-ways) ::enough
                  :else (await (next-way! c))))]
      (cond
        (not (ctx/alive? c)) :done
        (or (= :continue eaten) (= :continue way)) (do (await (child/pace!)) (recur n))
        (= ::fed way) (r/finish! c {:food (food-level c)})
        (= ::enough way) (r/stop! c :still-hungry (str "still hungry (food " (food-level c) ") after " n " ways")
                                  :food (food-level c))
        (= :again way) (recur (inc n))
        (gave-up-recently? c) :declined
        :else (none! c)))))
