(ns jobs.items.wear
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [jobs.lib.armour :as armour]
            [jobs.lib.checks :as checks]
            [jobs.lib.need :as need]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(def doc
  "Put armour on. With :item, that carried piece goes into its slot, and a worn piece of the slot comes back to
  the pockets. Without it, the best carried piece goes into each slot that is empty or worn with a weaker one
  (leather < golden < turtle helmet < chainmail < iron < diamond < netherite).
  Needs nothing, but a named :item not carried is fetched (jobs.lib.need) unless :fetch is false; done? holds when nothing carried is better than what is worn.
  Ends :done with {:worn [{:item :slot}]}. A named item that is no armour piece is impossible :not-armour; one not carried with :fetch false is a condition (a listed call waits; a started call ends
  condition-failed :no-item). A failed fetch ends as {:reason :need} with its ending as the :because. A piece the server does not take ends
  nothing-possible :failed with the pieces worn so far and :server-status.
  Quote: time per piece to put on.")

(a/defargs args
  {:item {:doc "the armour piece to wear; nil wears the best carried piece for each slot" :spec a/item? :default nil}
   :fetch {:doc "get a missing :item (jobs.lib.need): true, a set of kinds or a map of limits; false stops with :no-item instead" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 10 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::piece-s {:default 0.5 :doc "Seconds to put one armour piece on (the quote)." :spec (a/num-in 0 nil)}})

(def target [:item])

(def safe? ready/ok)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn plan-of
  "armour/plan for c: worn pieces and carried item names read from the body."
  [c]
  (let [p (:primitives c)]
    (armour/plan (armour/worn-of (.-equipment (.self p))) (map :name (u/inventory p)) (:item (:args c)))))

(defn refused
  "The reason (:not-armour, :no-item) when the named :item cannot be put on."
  [c]
  (let [{:keys [ok reason]} (plan-of c)]
    (when-not ok reason)))

(defn can-start? [c]
  (let [reason (refused c)]
    (cond
      (= :not-armour reason) (contract/never :not-armour (str (:item (:args c)) " is no armour piece"))
      (and (= :no-item reason) (need/parse-arg (:fetch (:args c)))) (checks/holds)
      reason (contract/later reason (str "cannot wear " (:item (:args c)) ": " (name reason)))
      :else (checks/holds))))

(defn done?
  "Nothing to put on (a refused item is not done)."
  [c]
  (let [{:keys [ok put]} (plan-of c)
        left (count put)
        data [:units :pieces :done (count (:worn (ctx/mem c))) :left left :result {:worn (vec (:worn (ctx/mem c)))}]]
    (if (and ok (zero? left))
      (apply contract/ok :worn "nothing better to wear" data)
      (apply contract/later :to-wear (if ok (str left " armour piece(s) to put on") (str "cannot wear " (:item (:args c)))) data))))

(defn ^:async quote-of [c]
  (let [n (count (:put (plan-of c)))
        t (* n (settings/get settings ::piece-s))]
    {:uses {:job-time [(min t (settings/get settings ::piece-s)) t t]}}))

(defn ^:async fetch!
  "Get the named :item when it is not carried and :fetch allows: nil when nothing is owed or it came, :continue while the fetch waits, else its ending."
  [c]
  (let [item (:item (:args c))
        opts (need/opts c 'jobs.items.wear)
        e (when (and item opts (not-any? #(= item (:name %)) (u/inventory (:primitives c)))) (await (need/item! c item 1 opts)))]
    (when-not (or (nil? e) (= :done (:kind e))) e)))

(defn ^:async wear-round!
  [c]
  (let [p (:primitives c)
        worn (armour/worn-of (.-equipment (.self p)))
        carried (map :name (u/inventory p))
        r (await (armour/wear! (fn ^:async equip-piece [i slot]
                                     (let [r (await (ctx/act c :equip (clj->js {:item i :dest slot})))]
                                       (when (= "equipped" (:status (js->clj r :keywordize-keys true))) (ctx/update-mem! c update :worn (fnil conj []) {:item i :slot slot}))
                                       r))
                                   worn carried (:item (:args c))))
        data {:worn (:worn r)}]
    (cond
      (:ok r)
      (ctx/end c :done nil (assoc data :text (str "wore " (count (:worn r)) " armour piece(s)")))
      (= :failed (:reason r))
      (ctx/end c :nothing-possible :failed (assoc data :server-status (:status r) :text (str "cannot wear " (:item r) ": failed")))
      :else
      (ctx/end c :condition-failed (:reason r) (assoc data :text (str "cannot wear " (:item r) ": " (name (:reason r))))))))

(defn ^:async round
  [c]
  (if-let [e (await (fetch! c))]
    (if (= :continue e) :continue (ctx/end c (:kind e) (:cause e) {:worn []}))
    (await (wear-round! c))))
