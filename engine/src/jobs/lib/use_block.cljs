(ns jobs.lib.use-block
  "The block kind of jobs.items.use: walk into reach, click the block or use the bucket on its cell. `ops` is what the job calls."
  (:require [jobs.lib.ready :as ready]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [engine.args :as a]))

(defn block-args
  "The args as the block jobs read them: :on becomes :pos."
  [c]
  (-> (:args c) (assoc :pos (:on (:args c))) (dissoc :on)))

(def safe? (ready/safe-at-by-night (ready/near-unseen-here (fn [c] (a/cell (:on (:args c)))))))

(def faces #{"up" "down" "north" "south" "east" "west"})

(def condition-reasons
  "The block click statuses that name a missing target or item; any other failed status is nothing-possible."
  #{:no-item :missing :cannot :occupied})

(defn quote-of [c]
  (let [{:keys [pos error]} (b/parse (block-args c))
        walk (if (or error (b/in-reach? c pos (:see (:args c)))) 0 (u/eye-dist (u/self-pos c) pos))]
    {:uses {:job-time [0.5 (+ 1 (/ walk 4)) (+ 10 walk)]}}))

(defn bucket? [item] (and (string? item) (boolean (re-find #"bucket$" item))))

(defn bad-args [c]
  (let [{:keys [face] :or {face "up"}} (:args c)
        error (or (:error (b/parse (block-args c))) (when-not (faces face) (str "face is one of " (pr-str (sort faces)))))]
    (when error (str "items.use " error))))

(def zones
  (checks/refusing :allowed "zones allow the bucket"
                   (fn [c]
                     (let [{:keys [pos error]} (b/parse (block-args c))]
                       (when (and (not error) (bucket? (:item (:args c))))
                         (b/not-allowed pos (access/may? (b/rules-in c) (if (= "bucket" (:item (:args c))) :harvest :place) pos)))))))

(defn finish-block!
  "End the call: done when used, else kind with the result's :reason (and :why); the data is the result with a :text."
  [c kind {:keys [used] :as result}]
  (let [result (update result :text #(or % (str (if used "clicked " "did not click ") (pr-str (b/cell (:pos result)))
                                                (when-not used (str ": " (name (:reason result)))))))]
    (ctx/end c kind (when-not used (select-keys result [:reason :why])) result)))

(defn ^:async click-block!
  "The click or bucket use at pos once in reach: the act's status keyword and its reply."
  [c {:keys [pos item face]}]
  (let [r (await (if (bucket? item)
                   (ctx/act c :place (clj->js {:pos pos :item item}))
                   (ctx/act c :useOn (clj->js (cond-> {:pos pos :face face} item (assoc :item item))))))
        status (keyword (.-status r))]
    [(if (and (bucket? item) (= :placed status)) :used status) r]))

(defn ^:async attempt-block!
  "One step: walk into reach (:again after a failed walk, until the limit), else click and end."
  [c pos]
  (let [{:keys [item face see range] :or {face "up"}} (:args c)
        w (when-not (b/in-reach? c pos see) (await (b/walk-into-reach! c pos see range)))]
    (cond
      (:unreachable w) (finish-block! c :nothing-possible {:used false :pos pos :item item :face face :reason :unreachable :why (:unreachable w)})
      w w
      :else
      (let [[status r] (await (click-block! c {:pos pos :item item :face face}))]
        (finish-block! c (cond (= :used status) :done (condition-reasons status) :condition-failed :else :nothing-possible)
                       (cond-> {:used (= :used status) :pos pos :item item :face face :reason status :raw r}
                         (.-spawnSet r) (assoc :spawn-set true)))))))


(defn ^:async run-round [c]
  (let [{:keys [pos]} (b/parse (block-args c))]
    (await (pace/steps! c #(attempt-block! c pos)))))

(def ops {:safe? safe? :quote quote-of :bad-args bad-args :zones zones :round run-round})
