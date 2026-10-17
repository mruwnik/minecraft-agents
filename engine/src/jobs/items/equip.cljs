(ns jobs.items.equip
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            ["minecraft-data" :as minecraft-data]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Hold a carried item in the main hand (:hand \"main\", default) or the off-hand (:hand \"off\"). Needs the item
  carried; a missing one is fetched (jobs.lib.need) unless :fetch is false. done? holds when it is held there.
  Ends :done {:item :hand :text}. Impossible: :bad-args (no :item), :unknown-item (no such item for the body's version). Conditions (a listed root job waits): :no-item (not carried, :fetch false). A failed fetch ends as {:reason :need} with its ending as
  the :because. Nothing-possible :failed (the server did not take it, data :equip the primitive's status; warn
  equip.refused). Quote: A, a fixed time.")

(a/defargs args
  {:item {:doc "the item to hold" :spec a/item? :default nil}
   :hand {:doc "\"main\" or \"off\" (a keyword works too)" :spec #{"main" "off" :main :off} :default "main"}
   :fetch {:doc "get a missing item (jobs.lib.need): true, a set of kinds or a map of limits; false stops :no-item instead" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 10 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::equip-s {:default 0.5 :doc "Seconds to put one item in hand (the quote)." :spec (a/num-in 0 nil)}})

(def dests {"main" "hand" "off" "off-hand"})
(def slots {"main" :mainHand "off" :offHand})

(defn known-item? [p name]
  (.hasOwnProperty (.-itemsByName (minecraft-data (game/version-of p))) name))

(defn carried? [p item] (boolean (some #(= item (:name %)) (u/inventory p))))

(defn held-in [p hand]
  (some-> (.-equipment (.self p)) (aget (name (slots (name hand)))) .-name))

(def target [:item :hand])

(def safe? ready/ok)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn problem
  "The cause when no item is given."
  [c]
  (when (nil? (:item (:args c)))
    {:reason :bad-args :why "equip needs :item"}))

(defn unknown
  "The cause :unknown-item when the item is not in the body's version."
  [c]
  (let [item (:item (:args c))]
    (when (and item (not (known-item? (:primitives c) item)))
      {:reason :unknown-item :item item})))

(defn lacking
  "The cause :no-item when the item is neither carried nor held and nothing fetches it."
  [c]
  (let [p (:primitives c)
        {:keys [item hand fetch]} (:args c)]
    (when (and item (not (carried? p item)) (not= item (held-in p hand)) (not (need/parse-arg fetch)))
      {:reason :no-item :item item})))

(defn can-start? [c]
  (contract/all [(checks/cause-status :never (problem c)) (checks/cause-status :never (unknown c)) (checks/cause-status :later (lacking c))]))

(defn done?
  "The item is in the hand asked for and no fetch of it is under way."
  [c]
  (let [{:keys [item hand]} (:args c)
        held (and (some? item) (= item (held-in (:primitives c) hand)))
        data [:units :items :done (if held 1 0) :left (if held 0 1) :result {:item item :hand hand}]]
    (cond
      (not held) (apply contract/later :not-held (str item " is not in the " hand " hand") data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up (str "holding " item ", the fetch wraps up") data)
      :else (apply contract/ok :held (str "holding " item) data))))

(defn quote-of [_c]
  (let [t (settings/get settings ::equip-s)]
    {:uses {:job-time [0 t t]}}))

(defn fetch-opts [c] (need/opts c 'jobs.items.equip))

(defn refuse!
  [c kind reason text data]
  (ctx/emit! c :equip.refused :warn (assoc data :reason reason :text text))
  (ctx/end c kind reason data))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        {:keys [item hand]} (:args c)
        opts (fetch-opts c)
        e (when (and (not (carried? p item)) opts) (await (need/item! c item 1 opts)))]
    (cond
      (= :continue e) :continue
      (and e (not= :done (:kind e))) (ctx/end c (:kind e) (:cause e) {:item item})
      (not (carried? p item)) (refuse! c :condition-failed :no-item (str "cannot hold " item ": not carried") {:item item})
      :else
      (let [r (await (ctx/act c :equip (clj->js {:item item :dest (dests (name hand))})))
            status (.-status r)]
        (if (= "equipped" status)
          (ctx/end c :done nil {:item item :hand hand :text (str "holding " item)})
          (refuse! c :nothing-possible :failed (str "cannot hold " item ": " status) {:item item :equip status}))))))
