(ns jobs.items.equip
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            ["minecraft-data" :as minecraft-data]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.armour :as armour]
            [jobs.lib.checks :as checks]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Put a carried item where it belongs: armour in its armour slot, a shield in the off-hand, anything else in the main hand
  (:hand \"main\"/\"off\" overrides that for a non-armour item). Without :item, the best carried armour piece goes into
  each slot that is empty or worn with a weaker one, and a carried shield into an empty off-hand (leather < golden < turtle helmet < chainmail < iron < diamond < netherite);
  a worn piece the new one replaces comes back to the pockets.
  With :off true the item (a worn armour piece, the shield or the held item) goes back to the pockets; without :item every worn armour piece does. The pockets need room for it (condition-failed :no-free-slot, nothing taken off).
  Needs the item carried; a missing one is fetched (jobs.lib.need) from the ways the call allows. done? holds when it is in its slot (without :item: when nothing carried beats what is worn; :off true: when it is out of its slot, without :item when no armour is worn).
  Ends :done {:item :slot :worn [{:item :slot}] :text} (:off true: the pieces taken off are in :off, :worn is empty). Impossible: :unknown-item (no such item for the body's version). Conditions (a listed root job waits): :no-item (not carried, the call's :ways #{:carried}). A failed fetch ends as {:reason :need} with its ending as
  the :because. Nothing-possible :failed (the server did not take a piece; data :equip the primitive's status, :worn the pieces done so far). Quote: a fixed time per piece to put on.")

(a/defargs args
  {:item {:doc "the item to put on or hold; nil puts on the best carried armour for each slot" :spec a/item? :default nil}
   :hand {:doc "\"main\" or \"off\" (a keyword works too) for a non-armour item; nil follows from the item" :spec #{"main" "off" :main :off nil} :default nil}
   :off {:doc "true takes the item (nil item: all worn armour) off into the pockets instead of putting it on" :spec boolean? :default false}})

(a/defargs settings
  {::job-time-s {:default 10 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::equip-s {:default 0.5 :doc "Seconds to put one item in place (the quote)." :spec (a/num-in 0 nil)}})

(def hand-dests {"main" "hand" "off" "off-hand"})
(def equipment-keys {"hand" :mainHand "off-hand" :offHand})

(defn known-item? [p name]
  (.hasOwnProperty (.-itemsByName (minecraft-data (game/version-of p))) name))

(defn carried? [p item] (boolean (some #(= item (:name %)) (u/inventory p))))

(defn in-slot
  "The name of the item in slot (an armour slot, \"hand\" or \"off-hand\")."
  [p slot]
  (let [eq (.-equipment (.self p))]
    (if-let [k (equipment-keys slot)]
      (some-> eq (aget (name k)) .-name)
      (get (armour/worn-of eq) slot))))

(defn slot-of
  "Where the named item goes: its armour slot, else the hand asked for, else the off-hand for a shield, else the main hand."
  [item hand]
  (or (:slot (armour/parse item))
      (hand-dests (some-> hand name))
      (if (= "shield" item) "off-hand" "hand")))

(defn steps
  "[{:item :slot}] still to do for c: the named item unless it is in its slot, else the best armour for each weaker slot."
  [c]
  (let [p (:primitives c)
        {:keys [item hand off]} (:args c)]
    (cond
      (and item off)
      (let [slot (slot-of item hand)]
        (if (= item (in-slot p slot)) [{:item item :slot slot}] []))
      off
      (vec (for [[slot worn] (armour/worn-of (.-equipment (.self p)))] {:item worn :slot slot}))
      item
      (let [slot (slot-of item hand)]
        (if (= item (in-slot p slot)) [] [{:item item :slot slot}]))
      :else
      (cond-> (armour/plan (armour/worn-of (.-equipment (.self p))) (map :name (u/inventory p)))
        (and (carried? p "shield") (nil? (in-slot p "off-hand"))) (conj {:item "shield" :slot "off-hand"})))))

(def target [:item :hand])

(def safe? ready/ok)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn unknown
  "The cause :unknown-item when the item is not in the body's version."
  [c]
  (let [item (:item (:args c))]
    (when (and item (not (known-item? (:primitives c) item)))
      {:reason :unknown-item :item item})))

(defn lacking
  "The cause :no-item when the item is neither carried nor in its slot and nothing fetches it."
  [c]
  (let [p (:primitives c)
        {:keys [item]} (:args c)]
    (when (and item (not (:off (:args c))) (seq (steps c)) (not (carried? p item)) (empty? (need/fetch-how c)))
      {:reason :no-item :item item})))

(defn can-start? [c]
  (contract/all [(checks/cause-status :never (unknown c)) (checks/cause-status :later (lacking c))]))

(defn done?
  "The item is in its slot (no item: nothing carried beats what is worn) and no fetch of it is under way."
  [c]
  (let [{:keys [item off]} (:args c)
        left (count (steps c))
        data [:units :items :done (count (:worn (ctx/mem c))) :left left
              :result (cond-> {:worn (vec (:worn (ctx/mem c)))} off (assoc :off (vec (:off (ctx/mem c)))) item (assoc :item item :slot (slot-of item (:hand (:args c)))))]]
    (cond
      (and off (pos? left)) (apply contract/later :still-in-slot (if item (str item " is still in its slot") (str left " armour piece(s) still worn")) data)
      off (apply contract/ok :off (if item (str item " is off") "no armour worn") data)
      (pos? left) (apply contract/later :not-held (if item (str item " is not in place") (str left " armour piece(s) to put on")) data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up (str "holding " item ", the fetch wraps up") data)
      :else (apply contract/ok :held (if item (str "holding " item) "nothing better to wear") data))))

(defn quote-of [c]
  (let [t (* (max 1 (count (steps c))) (settings/get settings ::equip-s))]
    {:uses {:job-time [0 t t]}}))

(def ways need/fetch-ways)

(defn fetch-opts [c] (when-let [how (seq (need/fetch-how c))] {:how (set how)}))

(defn refuse!
  [c kind reason text data]
  (ctx/end c kind reason (assoc data :text text)))

(defn ^:async put-on!
  "Equip (or, with :off, unequip) each step in turn, booking the ones the server took in mem :worn (:off); the failed step's primitive result, or nil."
  [c todo]
  (loop [todo todo]
    (when-let [{:keys [item slot] :as step} (first todo)]
      (let [off (:off (:args c))
            r (await (if off
                       (ctx/act c :unequip (clj->js {:dest slot}))
                       (ctx/act c :equip (clj->js {:item item :dest slot}))))]
        (if (contains? #{"equipped" "ok" "empty"} (.-status r))
          (do (ctx/update-mem! c update (if off :off :worn) (fnil conj []) step)
              (recur (rest todo)))
          (assoc step :status (.-status r)))))))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        {:keys [item off]} (:args c)
        opts (fetch-opts c)
        e (when (and item (not off) (seq (steps c)) (not (carried? p item)) opts) (await (need/item! c item 1 opts)))]
    (cond
      (= :continue e) :continue
      (and e (not= :done (:kind e))) (ctx/end c (:kind e) (:cause e) {:item item :worn []})
      (and item (not off) (seq (steps c)) (not (carried? p item)))
      (refuse! c :condition-failed :no-item (str "cannot hold " item ": not carried") {:item item :worn []})
      :else
      (let [todo (steps c)
            failed (await (put-on! c todo))
            worn (vec (:worn (ctx/mem c)))
            data (cond-> {:item item :slot (:slot (first todo)) :worn worn}
                   off (assoc :off (vec (:off (ctx/mem c)))))]
        (cond
          (= "full" (:status failed))
          (refuse! c :condition-failed :no-free-slot (str "cannot take off " (:item failed) ": no free slot") (assoc data :equip "full"))
          failed
          (refuse! c :nothing-possible :failed (str "cannot hold " (:item failed) ": " (:status failed)) (assoc data :equip (:status failed)))
          :else
          (ctx/end c :done nil (assoc data :text (cond
                                                   off (str "took off " (count (:off data)) " piece(s)")
                                                   item (str "holding " item)
                                                   :else (str "wore " (count worn) " armour piece(s)")))))))))
