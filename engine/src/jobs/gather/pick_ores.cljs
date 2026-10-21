(ns jobs.gather.pick-ores
  "Dig the ore a mining job passes (the :ore-seen trigger's job)."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.checks :as checks]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.ready :as ready]))

(def doc
  "The :ore-seen trigger's job: dig the ore a mining job passes. It reads what the live mining jobs dig themselves
  (jobs.lib.ore-pick marks), then picks the seen ores exposed within reach one after another (jobs.lib.ore-pick/pick!,
  a blocks.dig child each, back to the stand between). Cells the trigger saw and this run could not take are
  remembered (:ore-left, 5 min), so the trigger does not fire for them again.
  Ends :done with data {:picked [{:cell :block :got}]} when it dug ore, else nothing-possible :none-picked. A call that
  finds no mark has nothing to leave to a job and takes every seen ore in reach. Quote: a few seconds an ore.")

(a/defargs args {})

(def target [])

(def meta {:tags [:mine :dig :ore]})

(def fit? ready/ok)

(def defaults {})

(def s-per-ore 4)

(defn opts-of [c]
  (let [m (ore-pick/merged (ore-pick/marks (ctx/view c)))]
    (assoc m :left (ore-pick/left-cells (ctx/view c)))))

(defn seen-cells
  "The cells the trigger sees from here (no zone verdict)."
  [c opts]
  (into #{} (map (comp blocks/cell :pos)) (ore-pick/candidates c (assoc opts :skipped (:left opts)))))

(defn done?
  [c]
  (contract/later :picking "pick-ores: the round ends the call" :units :ores :done (count (ore-pick/picked c))
                  :result {:picked (ore-pick/picked c)}))

(defn place "The nearest seen ore cell to pick, nil for none." [c]
  (some-> (ore-pick/candidates c (assoc (opts-of c) :skipped (:left (opts-of c)))) first :pos (update-vals js/Math.floor)))

(def safe? (ready/safe-at-or-roaming place))

(defn can-start?
  [c]
  (checks/cause-status :later
                       (when (empty? (seen-cells c (opts-of c)))
                         {:reason :no-ore :text "pick-ores: no seen ore within reach to pick up"})))

(defn quote-of
  [c]
  (let [t (* s-per-ore (count (seen-cells c (opts-of c))))]
    {:uses {:job-time [0 t (* 3 t)]} :done [0 1 1]}))

(defn ^:async round
  [c]
  (let [opts (opts-of c)
        c (update c :args merge (select-keys opts [:ignore-zones? :for-plan]))
        first-seen (or (:first-seen (ctx/mem c)) (seen-cells c opts))]
    (ctx/update-mem! c assoc :first-seen (vec first-seen))
    (if (= :continue (await (ore-pick/pick! c opts)))
      :continue
      (let [picked (ore-pick/picked c)
            taken (into #{} (map :cell) picked)]
        (ore-pick/left! c (remove taken (:first-seen (ctx/mem c))))
        (if (seq picked)
          (ctx/end c :done nil {:picked picked :text (str "picked up " (count picked) " ore block" (when (> (count picked) 1) "s"))})
          (ctx/end c :nothing-possible :none-picked {:picked [] :text "pick-ores: no ore was dug"}))))))
