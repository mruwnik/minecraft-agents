(ns jobs.storage.deposit
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.cost :as cost]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.near :as near]
            [jobs.lib.places :as places]))

(def doc
  "Walk to the chest and deposit one stack per round: the named :items in the order named, or everything but tools
  and armour (saplings included) and the body's 3-day food reserve (jobs.lib.cost/food-reserve) in inventory order.
  :keep leaves at least that many of a name carried. With explicit :items the reserve is ignored.
  Ends with {:gave-up false} when nothing is left to put away, or {:gave-up true :reason r} after three failed
  transfers (warn chest_unusable). r is the transfer status or \"unreachable\".
  Memory: a :chest argument that took at least one item and finished clean is offered to the :chest place. It is
  recorded when none is recorded or the recorded one is gone, never over a different live one (place.kept
  event; jobs.memory.set-place moves it). A transfer that finds the recorded chest missing retracts it, with one
  chest_missing warn.
  Zones: a chest in another owner's zone or claim that does not allow :put is refused before the walk and again
  before the transfer. The job ends {:gave-up true :reason :refused :zones [..] :claims [..]} after one
  deposit.refused warn and puts nothing in. :ignore-zones? true skips the check.
  Stock: a chest whose stock is booked in body memory :fetch/stock gets what was put in added (jobs.lib.fetch).")

(def args
  {:chest {:doc "chest position [x y z] or {:x :y :z}; the known :chest place when nil" :default nil}
   :items {:doc "item names to put away, in this order (the first name with something to spare goes first); everything but tools and armour when nil" :default nil}
   :keep {:doc "{item-name count}: leave at least this many of the name carried" :default {}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def gear-suffixes ["_pickaxe" "_axe" "_shovel" "_hoe" "_sword" "_helmet" "_chestplate" "_leggings" "_boots"])
(def gear-names #{"shears" "bow" "crossbow" "fishing_rod" "flint_and_steel" "shield" "trident"})

(defn tool? [n]
  (or (contains? gear-names n)
      (some #(str/ends-with? n %) gear-suffixes)))

(defn chest-of
  "The chest position: args :chest as {:x :y :z} (read from [x y z] or {:x :y :z}; nil when unreadable), else the
  known :chest place."
  [view args]
  (if (some? (:chest args))
    (:pos (places/parse-pos (:chest args)))
    (mem/place view :chest)))

(defn carried
  "Total carried of name over all stacks."
  [items name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 items))

(defn to-deposit
  "The next carried stack to put away and how many of it: {:stack s :count n}
  or nil. With wanted names, the first carried stack of the first name (in
  wanted's order) that has spare; with none (nil), the first stack in
  inventory order that is not a tool or armour and has spare. A name has spare
  when its carried total exceeds its keep (a map of name to count, default 0);
  n is the stack count, cut so the keep stays carried."
  ([items wanted] (to-deposit items wanted {}))
  ([items wanted keep]
   (let [spare (fn [stack] (- (carried items (:name stack)) (get keep (:name stack) 0)))
         pick (fn [stack] (when (pos? (spare stack))
                            {:stack stack :count (min (:count stack) (spare stack))}))]
     (if wanted
       (some (fn [n] (some #(when (= n (:name %)) (pick %)) items)) wanted)
       (some #(when-not (tool? (:name %)) (pick %)) items)))))

(defn check
  "A chest is known."
  [c]
  (boolean (chest-of (ctx/view c) (:args c))))

(defn give-up!
  "u/fail!, and when it gives up hand the parent the reason."
  [c kind text reason]
  (let [r (u/fail! c kind text)]
    (when (= :done r) (ctx/result! c {:gave-up true :reason reason}))
    r))

(defn refuse!
  "End refused: one deposit.refused warn naming the zones and claims, the refusal as the result."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])]
    (ctx/emit! c :deposit.refused :warn (assoc fields :text (str "deposit refused: " (access/refusal-text fields))))
    (ctx/result! c (access/refused-result verdict))
    :done))

(defn ^:async round
  "Done when nothing is left to put away. Three failed transfers in a row (full,
  missing, unreachable) give up with a chest_unusable warn."
  [c]
  (let [{:keys [items keep]} (:args c)
        chest (chest-of (ctx/view c) (:args c))
        inventory (u/inventory (:primitives c))
        pick (to-deposit inventory items (if items keep (merge-with max (cost/food-reserve inventory) keep)))]
    (cond
      (nil? pick) (do (when (and (:chest (:args c)) (pos? (:deposited (ctx/mem c) 0)))
                        (places/offer! c :chest (:chest (:args c))))
                      (ctx/result! c {:gave-up false})
                      :done)
      (nil? chest) :continue
      (access/container-refusal c :put chest) (refuse! c (access/container-refusal c :put chest))
      :else
      (let [w (await (near/walk-near! c chest 3))]
        (case w
          :partial :continue
          :blocked (give-up! c :chest_unusable "cannot reach the chest" "unreachable")
          (if-let [v (access/container-refusal c :put chest)]
            (refuse! c v)
            (let [r (await (ctx/act c :transfer (clj->js {:pos chest :direction "deposit"
                                                           :item (:name (:stack pick)) :count (:count pick)})))]
              (if (= "ok" (.-status r))
                (if (pos? (or (.-moved r) 0))
                  (do (ctx/update-mem! c update :deposited (fnil inc 0))
                      (u/progress! c)
                      (fetch/note-moved! c chest (:name (:stack pick)) (.-moved r))
                      :continue)
                  (give-up! c :chest_unusable "nothing moved into the chest" "nothing-moved"))
                (do (places/retract-if-missing! c :chest chest (.-status r))
                    (give-up! c :chest_unusable (str "chest not usable: " (.-status r)) (.-status r)))))))))))
