(ns jobs.items.obtain.craft
  "The craft source: the steps of a craft chain, run as children."
  (:require [engine.ctx :as ctx]
            [jobs.items.craft :as craft]
            [jobs.items.obtain.base :refer [carried-of limits max-fruitless tried!]]
            [jobs.items.obtain.plan :refer [craft-plan]]
            [jobs.items.smelt :as smelt]
            [jobs.lib.declined :as declined]
            [jobs.lib.util :as u]))

(defn fruitless! [c why]
  (ctx/update-mem! c update-in [:craft :fruitless] (fnil inc 0))
  (ctx/update-mem! c assoc-in [:craft :why] why)
  :again)

(defn stop-why
  "What a craft that made nothing says for tried-text: the short ingredients (and alternatives) of a :need stop, else its reason."
  [res]
  (if (:short res)
    (select-keys res [:short :alternatives])
    (or (:reason res) :declined)))

(defn smelt-fetch
  "The :fetch limits of a smelt child (its fuel and furnace): this obtain's, one level deeper."
  [c]
  (let [o (limits c)]
    (into {} (remove (comp nil? val))
          {:depth (max 0 (dec (:depth o))) :how (:how o) :minutes (:minutes o) :fail-minutes (:fail-minutes o)
           :explore (:explore (:args c))})))

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
  (let [r (await (declined/call-child! c :smelt 'jobs.items.smelt
                                                 (cond-> {:item (:item step) :count (:count step) :fetch (smelt-fetch c)}
                                                   (= "charcoal" (:item step)) (assoc :fuel (wood-fuel (u/inventory (:primitives c)))))))]
    (if (#{:continue :declined} r)
      r
      (let [res (ctx/child-result c :smelt)]
        (ctx/update-mem! c update :craft dissoc :step)
        (if (and (= :done r) (pos? (:smelted res 0)))
          :again
          (do (when (:why res) (ctx/update-mem! c assoc-in [:craft :detail] (:why res)))
              (fruitless! c (or (:reason res) :declined))))))))

(defn ^:async craft-step!
  "One round of the craft source: run the current step of the plan as a child, until it ends."
  [c names have target]
  (let [mem (:craft (ctx/mem c))
        step (or (:step mem)
                 (some-> (craft-plan c names (- target have)) :steps first))]
    (cond
      (>= (:fruitless mem 0) (max-fruitless))
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
              (let [place #(ctx/call-child c :place 'jobs.blocks.place {:item (:item step) :pos spot :fetch false})
                    r (await (if (= "crafting_table" (:item step))
                               (ctx/atomic c :urgent :place-table (fn ^:async place-table [] (await (place))))
                               (place)))]
                (if (= :continue r)
                  :continue
                  (let [res (ctx/child-result c :place)]
                    (ctx/update-mem! c update :craft dissoc :step :spot)
                    (if (and (= :done r) (:placed res))
                      (do (ctx/update-mem! c assoc :table spot)
                          (ctx/update-mem! c update :craft dissoc :table-unreachable)
                          :again)
                      (fruitless! c (or (:reason res) :place-declined)))))))))

      (= :smelt (:op step))
      (await (smelt-step! c step))

      :else
      (do (ctx/update-mem! c assoc-in [:craft :step] step)
          (let [r (await (ctx/call-child c :craft 'jobs.items.craft (cond-> {:item (:item step) :count (:count step) :fetch false :avoid (:avoid (:args c))}
                                                                        (and (:table? step) (:table (ctx/mem c))) (assoc :table (:table (ctx/mem c))))))]
            (if (= :continue r)
              :continue
              (let [res (ctx/child-result c :craft)]
                (ctx/update-mem! c update :craft dissoc :step)
                (cond
                  (and (= :done r) (pos? (:made res 0))) :again

                  ;; the seen table cannot be reached: plan again without it, so a carried or new table is put down
                  (and (#{:unreachable :no-table} (:reason res)) (not (:table-unreachable mem)))
                  (do (ctx/update-mem! c assoc-in [:craft :table-unreachable] true) :again)

                  ;; the craft child ran its own rounds and made nothing: another child of the same step would repeat them
                  :else
                  (let [why (stop-why res)]
                    (if (= :declined r) (fruitless! c why) (tried! c :craft why)))))))))))
