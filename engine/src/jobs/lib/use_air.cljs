(ns jobs.lib.use-air
  "The air kind of jobs.items.use: look at a spot and use the held item without a block target (throw an egg, snowball,
  ender pearl). `ops` is what the job calls."
  (:require [jobs.lib.ready :as ready]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [engine.args :as a]))

(defn aim
  "The point looked at: :on as given, a whole-number cell taken at its centre."
  [on]
  (let [{:keys [x y z] :as p} (a/cell on)]
    (when p
      (if (every? integer? [x y z]) {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)} p))))

(defn bad-args [c]
  (let [{:keys [on item]} (:args c)]
    (cond (not (a/cell on)) "items.use :air needs :on as the spot to look at, [x y z] or {:x :y :z}"
          (not (string? item)) "items.use :air needs the :item to throw")))

(def zones (checks/refusing :allowed "an air use changes no block" (fn [_] nil)))

(defn quote-of [_c] {:uses {:job-time [0.5 1 2]}})

(def condition-statuses #{:no-item :cannot})

(defn ^:async attempt-air! [c]
  (let [{:keys [on item]} (:args c)
        r (await (ctx/act c :useOn (clj->js {:pos (aim on) :item item :air true})))
        status (keyword (.-status r))
        used (= :used status)]
    (when used (ctx/update-mem! c assoc :used true))
    (ctx/end c (cond used :done (condition-statuses status) :condition-failed :else :nothing-possible)
             (when-not used status)
             {:used used :pos (a/cell on) :item item :reason status :raw (js->clj r :keywordize-keys true)
              :text (str (if used "used " "did not use ") item " toward " (pr-str (a/cell on)) (when-not used (str ": " (name status))))})))

(defn ^:async run-round [c]
  (await (pace/steps! c #(attempt-air! c))))

(def ops {:safe? ready/safe-here-by-night :quote quote-of :bad-args bad-args :zones zones :round run-round})
