(ns jobs.items.obtain.craft
  "The craft source: the steps of a craft chain, run as children."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.items.craft :as craft]
            [jobs.items.obtain.base :refer [carried-of old-how tried!]]
            [jobs.items.obtain.plan :refer [craft-plan]]
            [jobs.items.smelt :as smelt]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.world :as known]))

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

(defn smelt-fetch
  "The :fetch limits of a smelt child (its fuel and furnace): this obtain's sources."
  [c]
  (into {} (remove (comp nil? val))
        {:how (old-how c) :fail-minutes (:fail-minutes (:args c)) :explore (:explore (:args c))}))

(defn wood-fuel
  "The planks kind to burn: the first of the smelt's wood fuels carried, else the first."
  [inv]
  (let [fuels (smelt/fetch-wood-fuels)]
    (or (first (filter #(pos? (carried-of inv [%])) fuels)) (first fuels))))

(defn ^:async smelt-step!
  "Run a smelt step of the plan as a jobs.items.smelt child (it fetches its own fuel and furnace) until it ends.
  While it cooks the child yields :continue.
  Charcoal burns wood (planks) as fuel: coal or charcoal as its fuel would ask for charcoal again."
  [c step]
  (ctx/update-mem! c assoc-in [:craft :step] step)
  (let [e (await (ctx/call c :smelt 'jobs.items.smelt
                           (cond-> {:item (:item step) :count (:count step) :fetch (smelt-fetch c)}
                             (= "charcoal" (:item step)) (assoc :fuel (wood-fuel (u/inventory (:primitives c)))))))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c update :craft dissoc :step)
          (if (and (= :done (:kind e)) (pos? (get-in e [:data :smelted] 0)))
            :again
            (do (when-let [why (or (get-in e [:data :why]) (get-in e [:cause :why]))]
                  (ctx/update-mem! c assoc-in [:craft :detail] why))
                (fruitless! c (contract/cause-key e))))))))

(defn plan-table
  "The table this obtain put down in a cell of its :for-plan plan, else nil: the build keeps tables, so it is dug up once the count is carried."
  [c]
  (let [{:keys [x y z] :as spot} (:plan-table (ctx/mem c))
        plan (:for-plan (:args c))]
    (when (and spot plan (= plan (get (known/footprints c) (mapv #(js/Math.floor %) [x y z]))))
      spot)))

(defn ^:async pick-plan-table!
  "Dig up the table in a plan cell (a jobs.blocks.dig child): :continue while it waits, else :again (a failed dig is not retried)."
  [c spot]
  (let [r (await (ctx/call c :pickup 'jobs.blocks.dig {:pos spot :collect true :need-drop false :fetch false :on-fluid :fail :for-plan (:for-plan (:args c))}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :plan-table)
          :again))))

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

      (= :place (:op step))
      (let [spot (or (:spot mem) (await (craft/spot! c)))]
        (cond
          (= :continue spot) :continue
          (not spot) (fruitless! c :no-table-spot)
          :else
          (do (ctx/update-mem! c assoc-in [:craft :step] step)
              (ctx/update-mem! c assoc-in [:craft :spot] spot)
              (let [place #(ctx/call c :place 'jobs.blocks.place {:item (:item step) :pos spot :fetch false})
                    e (await (if (= "crafting_table" (:item step))
                               (ctx/atomic c :urgent :place-table (fn ^:async place-table [] (await (place))))
                               (place)))]
                (if (= :continue e)
                  :continue
                  (do (ctx/update-mem! c update :craft dissoc :step :spot)
                      (if (and (= :done (:kind e)) (get-in e [:data :placed]))
                        (do (ctx/update-mem! c assoc :table spot :plan-table spot)
                            (ctx/update-mem! c update :craft dissoc :table-unreachable)
                            :again)
                        (fruitless! c (contract/cause-key e)))))))))

      (= :smelt (:op step))
      (await (smelt-step! c step))

      :else
      (do (ctx/update-mem! c assoc-in [:craft :step] step)
          (let [e (await (ctx/call c :craft 'jobs.items.craft (cond-> {:item (:item step) :count (storage/plus-carried (u/inventory (:primitives c)) (:item step) (:count step)) :fetch false :avoid (:avoid (:args c)) :for-plan (:for-plan (:args c))}
                                                                (and (:table? step) (:table (ctx/mem c))) (assoc :table (:table (ctx/mem c))))))]
            (if (= :continue e)
              :continue
              (do
                (ctx/update-mem! c update :craft dissoc :step)
                (cond
                  (and (= :done (:kind e)) (pos? (get-in e [:data :made] 0))) :again

                  ;; the seen table cannot be reached: plan again without it, so a carried or new table is put down
                  (and (#{:unreachable :no-table} (contract/cause-key e)) (not (:table-unreachable mem)))
                  (do (ctx/update-mem! c assoc-in [:craft :table-unreachable] true) :again)

                  ;; the craft child ran its own rounds and made nothing: another child of the same step would repeat them
                  :else (tried! c :craft (stop-why e))))))))))
