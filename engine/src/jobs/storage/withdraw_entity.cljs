(ns jobs.storage.withdraw-entity
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.look :as look]
            [jobs.lib.stock :as stock]))

(def doc
  "Move every stack of a container entity in reach (a chest minecart, :id) into the bag, the entity form of
  jobs.storage.withdraw. The caller walks within reach.
  Needs (can-start? and can-run?): an :id, else never :bad-args; the entity, when in sight, not in a zone or claim of another that refuses :take,
  else waits :refused {:zones :claims} (:ignore-zones? skips it).
  Ends done {:full bool :moved {item n}}: :full true when a later transfer moved nothing for lack of room, what fitted is taken.
  Else condition-failed :bag-full (the first transfer found no room) or :missing (the entity is gone), or nothing-possible with the inspect or transfer status as the cause
  (:unreachable, ..). Spends :job-time (default ::job-time-s).")

(a/defargs args
  {:id {:doc "entity id of the container" :spec number? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds the emptying may take." :spec (a/num-in 0 nil)}})

(def target [:id])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn entity-positions [c]
  (let [id (:id (:args c))]
    (->> (look/seen-entities (:primitives c) {:radius 16 :max 32})
         (filter #(= id (:id %)))
         (map :pos))))

(defn can-start? [c]
  (contract/all [((checks/args-fit "withdraw-entity: no :id" #(some? (:id (:args %)))) c)
                 ((checks/container-refused every? :take entity-positions) c)]))
(def can-run? can-start?)

(defn done?
  [c]
  (if (:emptied (ctx/mem c))
    (contract/ok :emptied "withdraw-entity: the container is empty or the bag full" :units :container :done 1 :left 0)
    (contract/later :emptying "withdraw-entity: stacks still to move" :units :container :done 0 :left 1)))

(defn quote-of [_c]
  {:uses {:job-time [1 5 (settings/get settings ::job-time-s)]}})

(defn ^:async round
  "Inspect the container and move its stacks one after another; a transfer that moves nothing ends it done :full once something moved, else :bag-full or nothing-possible; any other failure nothing-possible."
  [c]
  (let [id (:id (:args c))
        seen (await (ctx/act c :inspectContainer (clj->js {:entity id})))
        status (.-status seen)]
    (if (not= "ok" status)
      (ctx/end c (if (= "missing" status) :condition-failed :nothing-possible) (keyword status))
      (loop [stacks (vec (stock/stacks->items (.-items seen))) moved {}]
        (if-let [[name n] (first stacks)]
          (let [r (await (ctx/act c :transfer (clj->js {:entity id :direction "withdraw" :item name :count n})))
                status (.-status r)]
            (cond
              (and (= "ok" status) (pos? (.-moved r))) (recur (rest stacks) (assoc moved name (.-moved r)))
              (and (seq moved) (contains? #{"ok" "full"} status)) (do (ctx/update-mem! c assoc :emptied true) (ctx/end c :done nil {:full true :moved moved}))
              (= "full" status) (ctx/end c :condition-failed :bag-full)
              (= "ok" status) (ctx/end c :nothing-possible :nothing-moved)
              :else (ctx/end c :nothing-possible (keyword status))))
          (do (ctx/update-mem! c assoc :emptied true) (ctx/end c :done nil {:full false :moved moved})))))))
