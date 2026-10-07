(ns jobs.storage.deposit
  (:require [jobs.lib.args :as jargs]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.cost :as cost]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]))

(def doc
  "Walk to the chest (one go-to) and deposit every stack in one call: the named :items in the order named, or everything but tools
  and armour (saplings included) and the body's 3-day food reserve (jobs.lib.cost/food-reserve) in inventory order.
  :keep leaves at least that many of a name carried. :free ends the call, clean, once that many slots are free. With explicit :items the reserve is ignored.
  Ends with {:gave-up false} when nothing is left to put away, or stopped {:gave-up true :status :stopped :reason r
  :moved n} (n items put in) after three failed transfers (warn chest_unusable) or at once when go-to cannot reach.
  r is the transfer status or \"unreachable\".
  Memory: a :chest argument that took at least one item and finished clean is offered to the :chest place. It is
  recorded when none is recorded or the recorded one is gone, never over a different live one (place.kept
  event; jobs.memory.set-place moves it). A transfer that finds the recorded chest missing retracts it, with one
  chest_missing warn, and ends at once with reason \"missing\". No chest known: waits, reason :no-chest; no chest
  at round time gives up \"no-chest\".
  Zones: a chest in another owner's zone or claim that does not allow :put is refused before the walk and again
  before the transfer. The job ends {:gave-up true :reason :refused :zones [..] :claims [..]} after one
  deposit.refused warn and puts nothing in. :ignore-zones? true skips the check.
  Stock: a chest whose stock is booked in body memory :fetch/stock gets what was put in added (jobs.lib.fetch).")

(def args
  {:chest {:doc "chest position [x y z] or {:x :y :z}; the known :chest place when nil" :type :pos :default nil}
   :items {:doc "item names to put away, in this order (the first name with something to spare goes first); everything but tools and armour when nil" :default nil}
   :keep {:doc "{item-name count}: leave at least this many of the name carried" :default {}}
   :free {:doc "stop once this many inventory slots are free; all of it when nil" :type :int :min 0 :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :type :bool :default false}})

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

(defn check-run
  "A chest is known; else waits with reason :no-chest."
  [c]
  (or (boolean (chest-of (ctx/view c) (:args c)))
      (ctx/wait c {:reason :no-chest})))

(defn stop!
  "End stopped: {:gave-up true :reason reason :status :stopped :moved n}, n the items put in so far."
  [c reason]
  (ctx/result! c {:gave-up true :reason reason :status :stopped :moved (:moved (ctx/mem c) 0)})
  :done)

(defn give-up!
  "u/fail!: :again until the third failure in a row, then stop with the reason (a warn of kind first)."
  [c kind text reason]
  (let [r (u/fail! c kind text)]
    (if (= :done r) (stop! c reason) :again)))

(defn retracted?
  "retract-if-missing!, and true when status is \"missing\" at the recorded chest place (so the job ends at once)."
  [c chest status]
  (let [recorded (mem/place (ctx/view c) :chest)]
    (places/retract-if-missing! c :chest chest status)
    (and (= "missing" status) (some? recorded) (= chest recorded))))

(defn refuse!
  "End refused: one deposit.refused warn naming the zones and claims, the refusal as the result."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])]
    (ctx/emit! c :deposit.refused :warn (assoc fields :text (str "deposit refused: " (access/refusal-text fields))))
    (ctx/result! c (access/refused-result verdict))
    :done))

(defn ^:async walk!
  "Within 3 of the chest: nil. Else one go-to call: :continue while it waits on the world, :again once there,
  :unreachable when it gave up."
  [c chest]
  (if (u/within? (u/self-pos c) chest 3)
    nil
    (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos chest :range 3 :escalate false :warn false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
      (cond
        (= :continue r) :continue
        (:arrived (ctx/child-result c :walk)) :again
        :else :unreachable))))

(defn ^:async step!
  "Put the next stack away, walking to the chest first: :again, :continue (go-to waits), or :done."
  [c]
  (let [{:keys [items keep free]} (:args c)
        chest (chest-of (ctx/view c) (:args c))
        inventory (u/inventory (:primitives c))
        pick (when-not (and free (>= (u/free-slots (:primitives c)) free))
               (to-deposit inventory items (if items keep (merge-with max (cost/food-reserve inventory) keep))))]
    (cond
      (nil? pick) (do (when (and (:chest (:args c)) (pos? (:deposited (ctx/mem c) 0)))
                        (places/offer! c :chest (:chest (:args c))))
                      (ctx/result! c {:gave-up false})
                      :done)
      (nil? chest) (give-up! c :chest_unusable "no chest is known" "no-chest")
      (access/container-refusal c :put chest) (refuse! c (access/container-refusal c :put chest))
      :else
      (let [w (await (walk! c chest))]
        (cond
          (= :unreachable w) (give-up! c :chest_unusable "cannot reach the chest" "unreachable")
          w w
          :else
        (if-let [v (access/container-refusal c :put chest)]
          (refuse! c v)
          (let [r (await (ctx/act c :transfer (clj->js {:pos chest :direction "deposit"
                                                         :item (:name (:stack pick)) :count (:count pick)})))]
            (if (= "ok" (.-status r))
              (if (and (pos? (or (.-moved r) 0))
                       (< (carried (u/inventory (:primitives c)) (:name (:stack pick))) (carried inventory (:name (:stack pick)))))
                (do (ctx/update-mem! c #(-> % (update :deposited (fnil inc 0)) (update :moved (fnil + 0) (.-moved r))))
                    (u/progress! c)
                    (fetch/note-moved! c chest (:name (:stack pick)) (.-moved r))
                    :again)
                (give-up! c :chest_unusable "nothing moved into the chest" "nothing-moved"))
              (if (retracted? c chest (.-status r))
                (stop! c (.-status r))
                (give-up! c :chest_unusable (str "chest not usable: " (.-status r)) (.-status r)))))))))))

(defn ^:async round
  "The whole put-away in one call: stacks one after another (the inventory is read again before each, so a restart
  moves nothing twice) until nothing is left or it stops. :continue only while go-to waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async deposit-step [] (await (step! c))))))

(def bad-lists
  "Args checked by jobs.lib.args."
  {:items :names :keep :counts})

(defn check
  "check-run once the list args are well formed, else declines :bad-args."
  [c]
  (jargs/guard c bad-lists check-run))
