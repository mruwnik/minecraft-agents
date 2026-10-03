(ns jobs.storage.deposit
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def doc
  "Walk to the chest and deposit one stack per round: the named items, or
  everything but tools and armour (saplings included). :keep leaves at least
  that many of a name carried (the stack moved is cut short to respect it).
  Ends with a result {:gave-up false} when nothing is left to put away, or
  {:gave-up true :reason r} (the transfer status, or \"unreachable\") when
  the failed attempts used it up and the warn was emitted.")

(def args
  {:chest {:doc "chest position; the known :chest place when nil" :default nil}
   :items {:doc "item names to put away; everything but tools and armour when nil" :default nil}
   :keep {:doc "{item-name count}: leave at least this many of the name carried" :default {}}})

(def gear-suffixes ["_pickaxe" "_axe" "_shovel" "_hoe" "_sword" "_helmet" "_chestplate" "_leggings" "_boots"])
(def gear-names #{"shears" "bow" "crossbow" "fishing_rod" "flint_and_steel" "shield" "trident"})

(defn tool? [n]
  (or (contains? gear-names n)
      (some #(str/ends-with? n %) gear-suffixes)))

(defn chest-of
  "The chest position: args :chest, else the known :chest place."
  [view args]
  (or (:chest args) (mem/place view :chest)))

(defn carried
  "Total carried of name over all stacks."
  [items name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 items))

(defn to-deposit
  "The first carried stack to put away and how many of it: {:stack s :count n}
  or nil. The stack is of the wanted names when given, otherwise anything that
  is not a tool or armour, and its name's carried total must exceed its keep
  (a map of name to count, default 0); n is the stack count, cut so the keep
  stays carried."
  ([items wanted] (to-deposit items wanted {}))
  ([items wanted keep]
   (let [wanted (some-> wanted set)
         spare (fn [stack] (- (carried items (:name stack)) (get keep (:name stack) 0)))]
     (some (fn [stack]
             (when (and (if wanted (wanted (:name stack)) (not (tool? (:name stack))))
                        (pos? (spare stack)))
               {:stack stack :count (min (:count stack) (spare stack))}))
           items))))

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

(defn ^:async round
  "Done when nothing is left to put away. Three failed transfers (full,
  missing, unreachable) give up with a chest_unusable warn."
  [c]
  (let [{:keys [items keep]} (:args c)
        chest (chest-of (ctx/view c) (:args c))
        pick (to-deposit (u/inventory (:primitives c)) items keep)]
    (cond
      (nil? pick) (do (ctx/result! c {:gave-up false}) :done)
      (nil? chest) :continue
      :else
      (let [w (await (u/walk-near! c chest 3))]
        (case w
          :partial :continue
          :blocked (give-up! c :chest_unusable "cannot reach the chest" "unreachable")
          (let [r (await (ctx/act c :transfer (clj->js {:pos chest :direction "deposit"
                                                         :item (:name (:stack pick)) :count (:count pick)})))]
            (if (= "ok" (.-status r))
              :continue
              (give-up! c :chest_unusable (str "chest not usable: " (.-status r)) (.-status r)))))))))
