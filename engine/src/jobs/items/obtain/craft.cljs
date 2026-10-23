(ns jobs.items.obtain.craft
  "The craft source: the steps of a craft chain, run as children."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.items.craft :as craft]
            [jobs.items.obtain.base :refer [carried-of child-ways search-worth seen-radius tried!]]
            [jobs.items.obtain.plan :refer [craft-plan]]
            [jobs.items.smelt :as smelt]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]
            [jobs.lib.need :as need]
            [jobs.lib.waits :as waits]))

(defn fruitless! [c why]
  (child/fruitless! c :craft)
  (ctx/update-mem! c assoc-in [:craft :why] why)
  :again)

(defn stop-why
  "What a craft ending e that made nothing says for tried-text: the short ingredients (and alternatives) of a :need
  stop, else its cause."
  [e]
  (if (get-in e [:data :short])
    (select-keys (:data e) [:short :alternatives])
    (contract/cause-key e)))

(defn wood-fuel
  "The planks kind to burn: a smelt wood fuel carried, else that of a log species carried, else of the nearest log
  seen, else the first. A kind whose logs are at hand is not searched for far away."
  [c]
  (let [p (:primitives c)
        inv (u/inventory p)
        fuels (smelt/fetch-wood-fuels)
        of-log (fn [n] (let [f (str (trees/species-of n) "_planks")] (when (some #{f} fuels) f)))
        carried-log (some #(when (and (trees/log-name? (:name %)) (pos? (:count %))) (of-log (:name %))) inv)
        seen-log (some-> (look/seen-blocks p {:names trees/log-names :radius (seen-radius) :max 1}) first :name of-log)]
    (or (first (filter #(pos? (carried-of inv [%])) fuels)) carried-log seen-log (first fuels))))

(defn ^:async smelt-step!
  "Run a smelt step of the plan as a jobs.items.smelt child (it fetches its own fuel and furnace) until it ends.
  While it cooks the child yields :continue.
  Charcoal burns wood (planks) as fuel: coal or charcoal as its fuel would ask for charcoal again."
  [c step]
  (ctx/update-mem! c assoc-in [:craft :step] step)
  (let [e (waits/waiting-out c (await (ctx/call c :smelt 'jobs.items.smelt
                           (cond-> {:item (:item step) :count (:count step) :ways (child-ways c) :explore (boolean (:explore (:args c))) :search-worth (search-worth c)}
                             (= "charcoal" (:item step)) (assoc :fuel (wood-fuel c))))))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c update :craft dissoc :step)
          (if (and (= :done (:kind e)) (pos? (get-in e [:data :smelted] 0)))
            :again
            (do (when-let [why (or (get-in e [:data :why]) (get-in e [:cause :why]))]
                  (ctx/update-mem! c assoc-in [:craft :detail] why))
                (fruitless! c (contract/cause-key e))))))))

(defn ^:async craft-step!
  "One round of the craft source: run the current step of the plan as a child, until it ends."
  [c names have target]
  (let [mem (:craft (ctx/mem c))
        step (or (:step mem)
                 (some-> (craft-plan c names (- target have)) :steps first))]
    (cond
      (child/exhausted? c :craft)
      (tried! c :craft (or (:why mem) :failed))

      ;; no plan without the seen table: try that table again (it may be reachable now); fruitless counts bound the retries
      (and (nil? step) (:table-unreachable mem))
      (do (ctx/update-mem! c update :craft dissoc :table-unreachable)
          (fruitless! c :table-unreachable))

      (nil? step)
      (tried! c :craft :no-plan)

      (= :smelt (:op step))
      (await (smelt-step! c step))

      :else
      (do (ctx/update-mem! c assoc-in [:craft :step] step)
          (let [before (carried-of (u/inventory (:primitives c)) [(:item step)])
                e (waits/waiting-out c (await (ctx/call c :craft 'jobs.items.craft {:item (:item step) :count (need/carry-count c :craft [(:item step)] (:count step)) :ways #{:carried} :avoid (:avoid (:args c)) :for-plan (:for-plan (:args c))
                                                                    :search-worth (search-worth c)
                                                                    :keep-table (nil? (:for-plan (:args c)))})))]
            (if (= :continue e)
              :continue
              (do
                (ctx/update-mem! c update :craft dissoc :step)
                (cond
                  (and (= :done (:kind e)) (pos? (get-in e [:data :made] 0))) :again

                  ;; a craft cut by its time limit (the walk to a table used it) after it made some: the rest is a new call
                  (> (carried-of (u/inventory (:primitives c)) [(:item step)]) before) :again

                  ;; the seen table cannot be reached: plan again without it, so a carried or new table is put down
                  (and (#{:unreachable :no-table} (contract/cause-key e)) (not (:table-unreachable mem)))
                  (do (ctx/update-mem! c assoc-in [:craft :table-unreachable] true) :again)

                  ;; the craft child ran its own rounds and made nothing: another child of the same step would repeat them
                  :else (tried! c :craft (stop-why e))))))))))
