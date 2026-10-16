(ns jobs.storage.withdraw-entity
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.stock :as stock]))

(def doc
  "Move every stack of a container entity in reach (a chest minecart, :id) into the bag, the entity form of
  jobs.storage.withdraw. The caller walks within reach and checks zones first.
  Ends done {:full bool}: :full true when a transfer moved nothing for lack of room, what fitted is taken.
  Else condition-failed :missing (the entity is gone) or nothing-possible with the inspect or transfer status as the cause
  (:unreachable, ..). Spends :time (default ::job-time-s).")

(a/defargs args
  {:id {:doc "entity id of the container" :spec number? :default nil}})

(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds the emptying may take." :spec (a/num-in 0 nil)}})

(def target [:id])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn done?
  [c]
  (if (:emptied (ctx/mem c))
    (contract/ok :emptied "withdraw-entity: the container is empty or the bag full")
    (contract/later :emptying "withdraw-entity: stacks still to move")))

(defn quote-of [_c]
  {:uses {:job-time [1 5 (settings/get settings ::job-time-s)]}})

(defn ^:async round
  "Inspect the container and move its stacks one after another; a transfer that moves nothing ends it :full, any other failure nothing-possible."
  [c]
  (let [id (:id (:args c))
        seen (await (ctx/act c :inspectContainer (clj->js {:entity id})))
        status (.-status seen)]
    (if (not= "ok" status)
      (ctx/end c (if (= "missing" status) :condition-failed :nothing-possible) (keyword status))
      (loop [stacks (vec (stock/stacks->items (.-items seen)))]
        (if-let [[name n] (first stacks)]
          (let [r (await (ctx/act c :transfer (clj->js {:entity id :direction "withdraw" :item name :count n})))]
            (cond
              (and (= "ok" (.-status r)) (pos? (.-moved r))) (recur (rest stacks))
              (contains? #{"ok" "full"} (.-status r)) (do (ctx/update-mem! c assoc :emptied true) (ctx/end c :done nil {:full true}))
              :else (ctx/end c :nothing-possible (keyword (.-status r)))))
          (do (ctx/update-mem! c assoc :emptied true) (ctx/end c :done nil {:full false})))))))
