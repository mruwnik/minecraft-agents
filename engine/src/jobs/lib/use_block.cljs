(ns jobs.lib.use-block
  "The block kind of jobs.items.use: walk into reach, click the block or use the bucket on its cell. `ops` is what the job calls."
  (:require [jobs.lib.ready :as ready]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.crops :as crops]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [engine.args :as a]))

(defn bucket? [item] (and (string? item) (boolean (re-find #"bucket$" item))))

(def face-steps {"up" [0 1 0] "down" [0 -1 0] "north" [0 0 -1] "south" [0 0 1] "east" [1 0 0] "west" [-1 0 0]})

(defn target
  "The cell a bucket fills or scoops: :on itself, or with a :face the cell beside that face of the block :on (a
  player clicks a block's face). nil when :on is not a position."
  [{:keys [on item face]}]
  (let [cell (a/cell on)]
    (if (and cell (bucket? item) (face-steps face))
      (zipmap [:x :y :z] (map + ((juxt :x :y :z) cell) (face-steps face)))
      on)))

(defn block-args
  "The args as the block jobs read them: :on becomes :pos (a bucket's target, see target)."
  [c]
  (-> (:args c) (assoc :pos (target (:args c))) (dissoc :on)))

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:on (:args c))))))

(def faces #{"up" "down" "north" "south" "east" "west"})

(def condition-reasons
  "The block click statuses that name a missing target or item; any other failed status is nothing-possible."
  #{:no-item :missing :cannot :occupied})

(defn quote-of [c]
  (let [{:keys [pos error]} (b/parse (block-args c))
        walk (if (or error (b/in-reach? c pos (:see (:args c)))) 0 (u/eye-dist (u/self-pos c) pos))]
    {:uses {:job-time [0.5 (+ 1 (/ walk 4)) (+ 10 walk)]}}))

(defn bad-args [c]
  (let [{:keys [face]} (:args c)
        error (or (:error (b/parse (block-args c))) (when-not (or (nil? face) (faces face)) (str "face is one of " (pr-str (sort faces)))))]
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

(defn ripe-before?
  "Whether the reply r says the block was a crop already at its ripe age before the click (bone meal does nothing to it)."
  [r]
  (let [before (.-before r)
        ripe (some-> before .-name crops/ripe-age)
        age (some-> before .-properties .-age)]
    (boolean (and ripe age (>= age ripe)))))

(defn ^:async click-block!
  "The click or bucket use at pos once in reach: the act's status keyword and its reply."
  [c {:keys [pos item face]}]
  (let [r (await (if (bucket? item)
                   (ctx/act c :place (clj->js {:pos pos :item item}))
                   (ctx/act c :useOn (clj->js (cond-> {:pos pos :face (or face "up")} item (assoc :item item))))))
        status (keyword (.-status r))]
    [(if (and (bucket? item) (= :placed status)) :used status) r]))

(defn ^:async attempt-block!
  "One step: walk into reach (:again after a failed walk, until the limit), else click and end."
  [c pos]
  (let [{:keys [item face see range]} (:args c)
        w (when-not (b/in-reach? c pos see) (await (b/walk-into-reach! c pos see range)))]
    (cond
      (:unreachable w) (finish-block! c :nothing-possible {:used false :pos pos :item item :face face :reason :unreachable :why (:unreachable w)})
      w w
      :else
      (let [[status r] (await (click-block! c {:pos pos :item item :face face}))]
        (finish-block! c (cond (= :used status) :done (condition-reasons status) :condition-failed :else :nothing-possible)
                       (cond-> {:used (= :used status) :pos pos :item item :face face :raw r
                                :reason (if (and (= :unchanged status) (= "bone_meal" item) (ripe-before? r)) :ripe status)}
                         (.-spawnSet r) (assoc :spawn-set true)))))))


(defn ^:async run-round [c]
  (let [{:keys [pos]} (b/parse (block-args c))]
    (await (pace/steps! c #(attempt-block! c pos)))))

(def ops {:safe? safe? :quote quote-of :bad-args bad-args :zones zones :round run-round})
