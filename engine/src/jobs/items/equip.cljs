(ns jobs.items.equip
  (:require ["minecraft-data" :as minecraft-data]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]))

(def doc
  "Hold a carried item in the main hand (:hand \"main\", default) or the off-hand (:hand \"off\"). Done at once when
  it is held there already. A missing item is fetched (jobs.lib.fetch) only with :fetch.
  Ends {:status :done :item name :hand h} (plus :held true when nothing had to move) or {:status :stopped :reason r}:
  :no-item (not carried), :unknown-item (no such item for the body's version), :bad-args (bad :hand), :failed (the
  server did not take it, with :equip the primitive's status), warn equip.refused.")

(def args
  {:item {:doc "the item to hold" :default nil}
   :hand {:doc "\"main\" or \"off\"" :default "main"}
   :fetch {:doc "get a missing item (jobs.lib.fetch): true, a set of kinds or a map of limits" :default false}})

(def dests {"main" "hand" "off" "off-hand"})
(def slots {"main" :mainHand "off" :offHand})

(defn known-item? [p name]
  (.hasOwnProperty (.-itemsByName (minecraft-data (game/version-of p))) name))

(defn carried? [p item] (boolean (some #(= item (:name %)) (u/inventory p))))

(defn held-in [p hand]
  (some-> (.-equipment (.self p)) (aget (name (slots hand))) .-name))

(defn problem
  "The need wait of an item not carried, else nil."
  [c]
  (let [{:keys [item hand]} (:args c)]
    (when (and (string? item) (contains? dests (or hand "main")) (not (carried? (:primitives c) item))
               (not= item (held-in (:primitives c) (or hand "main"))))
      {:reason :need :item item})))

(defn check [c]
  (if-let [w (problem c)]
    (if (fetch/opts c 'jobs.items.equip) (fetch/check c 'jobs.items.equip w) true)
    true))

(defn stop! [c reason text extra]
  (ctx/emit! c :equip.refused :warn (assoc extra :reason reason :text text))
  (ctx/result! c (merge {:status :stopped :reason reason :text text} extra))
  :done)

(defn ^:async round
  [c]
  (let [p (:primitives c)
        {:keys [item]} (:args c)
        hand (or (:hand (:args c)) "main")]
    (cond
      (not (and (string? item) (contains? dests hand)))
      (stop! c :bad-args "equip needs :item and :hand main or off" {})
      (not (known-item? p item))
      (stop! c :unknown-item (str "no such item " item) {:item item})
      (= item (held-in p hand))
      (do (ctx/result! c {:status :done :item item :hand hand :held true}) :done)
      (and (not (carried? p item)) (fetch/opts c 'jobs.items.equip))
      (or (await (fetch/fetch! c 'jobs.items.equip problem)) :continue)
      (not (carried? p item))
      (stop! c :no-item (str "cannot hold " item ": not carried") {:item item})
      :else
      (let [r (await (ctx/act c :equip (clj->js {:item item :dest (dests hand)})))
            status (.-status r)]
        (if (= "equipped" status)
          (do (ctx/emit! c :equip.done :info {:item item :hand hand :text (str "holding " item)})
              (ctx/result! c {:status :done :item item :hand hand})
              :done)
          (stop! c :failed (str "cannot hold " item ": " status) {:item item :equip status}))))))
