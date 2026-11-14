(ns jobs.items.get-levels
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.fetch-time :as fetch-time]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.xp :as xp]))

(def doc
  "Earn experience until the body has at least :levels levels. The ways (jobs.lib.xp/ways, each priced in seconds with
  jobs.lib.cost): smelt carried items that give points, or raw iron, copper or gold fetched when the call's ways fetch
  (jobs.items.smelt, which fetches the furnace and fuel too); dig an ore it has seen that gives points (jobs.blocks.dig
  with the drop kept, a harvesting tool fetched); kill a hostile it senses now (jobs.combat.attack). Points per act
  are the game's means (jobs.lib.xp). It takes the way cheapest a point first, one child at a time, and chooses again
  after each. A way whose child gave up, or ended done without a point, is not taken again in the call.
  Trading is no way yet: no offers are remembered to price one.
  Needs nothing of its own: every act is a child's, with its own checks.
  can-start?: never :bad-args without :levels; later :no-way while the known ways do not earn the points (data
  :short, the points lacking, :price), later :not-worth while their price (jobs.lib.xp/levels-price) is above the
  call's worth (data :price).
  Quote: that price. :spend :job-time: fetch-time/one-unit-s a level asked.
  Ends :done {:level :levels :earned} once the level is reached; else {:level :levels :earned :tried [way keys]} with
  condition-failed :no-way (the known ways do not earn the points, none tried), nothing-possible :no-way (the same after
  a way was tried) or :not-worth (the ways left cost
  more than the worth). done? holds once the body's level is at least :levels.")

(a/defargs args
  {:levels {:doc "the experience level to reach at least" :spec (a/int-in 1 nil) :default nil}})

(def target [:levels])
(def ways need/fetch-ways)

(def safe? ready/ok)
(def fit? ready/ok)

(defn default-time [{:keys [levels]}] (* (fetch-time/one-unit-s) (or levels 1)))
(def defaults {:spend {:job-time default-time}})

(defn level [c] (xp/level-of (:primitives c)))
(defn skip [c] (set (:tried (ctx/mem c))))

(defn can-start? [c]
  (let [levels (:levels (:args c))]
    (cond
      (nil? levels) (contract/never :bad-args "get-levels: no :levels")
      (>= (level c) levels) (contract/ok :has-levels "the body has the levels")
      :else (let [price (xp/levels-price c levels (xp/ways c levels (skip c)))
                  short (js/Math.ceil (xp/points-short (:primitives c) levels))]
              (cond
                (not (js/isFinite price)) (contract/later :no-way (str "no known way earns the " short " points to level " levels) :short short :price price)
                (not (cost/affords? price (ctx/worth c))) (contract/later :not-worth (str "level " levels " costs " (js/Math.round price) " s, above its worth") :price price)
                :else (contract/ok :has-way (str "level " levels " for about " (js/Math.round price) " s")))))))

(defn done? [c]
  (let [levels (:levels (:args c)) have (level c)]
    (if (>= have levels)
      (contract/ok :levels (str "level " have) :units :levels :done have :left 0 :result {:level have})
      (contract/later :earning (str "level " have " of " levels) :units :levels :done have :left (- levels have) :result {:level have}))))

(defn quote-of [c]
  (let [p (xp/levels-price c (:levels (:args c)))]
    {:uses {:job-time [p p p]}}))

(defn result [c]
  {:level (level c) :levels (:levels (:args c)) :earned (- (xp/points-of (:primitives c)) (:start (ctx/mem c) 0))})

(defn give-up! [c]
  (let [tried (vec (:tried (ctx/mem c)))]
    (ctx/end c (if (seq tried) :nothing-possible :condition-failed) :no-way
             (assoc (result c) :tried tried :text (str "get-levels: no known way left to level " (:levels (:args c)))))))

(defn child-args [c {:keys [job args]}]
  (cond-> args (not= 'jobs.combat.attack job) (assoc :ways (need/pass-ways c))))

(defn ^:async earn!
  "One round of the way w's child (slot :earn), at the worth left after the rest of the plan; once it ended, w's key is
  tried when it gave up or earned nothing."
  [c w rest-s]
  (let [r (await (ctx/call c :earn (:job w) (assoc (child-args c w) :worth (cost/worth-left c rest-s))))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c (fn [m] (cond-> (dissoc m :running)
                                       (or (not= :done (:kind r)) (<= (xp/points-of (:primitives c)) (:before w)))
                                       (update :tried (fnil conj []) (:key w)))))
          :again))))

(defn ^:async step!
  "The running way's child, else the cheapest way a point (the ways known do not earn the points: give up :no-way;
  their price is above the worth: :not-worth)."
  [c]
  (if-let [w (:running (ctx/mem c))]
    (await (earn! c w (:rest-s w)))
    (let [levels (:levels (:args c))
          ws (xp/ways c levels (skip c))
          price (xp/levels-price c levels ws)
          w (first ws)]
      (cond
        (not (js/isFinite price)) (give-up! c)
        (not (cost/affords? price (ctx/worth c)))
        (ctx/end c :nothing-possible :not-worth (assoc (result c) :tried (vec (:tried (ctx/mem c))) :price price
                                                       :text (str "get-levels: level " levels " costs " (js/Math.round price) " s, above its worth")))
        :else (let [w (-> (select-keys w [:way :key :points :price :job :args])
                          (assoc :before (xp/points-of (:primitives c)) :rest-s (- price (:price w))))]
                (ctx/update-mem! c assoc :running w)
                (await (earn! c w (:rest-s w))))))))

(defn ^:async round [c]
  (when-not (:start (ctx/mem c)) (ctx/update-mem! c assoc :start (xp/points-of (:primitives c))))
  (let [r (await (pace/steps! c #(step! c)))]
    (if (and (map? r) (= :done (:kind r)))
      (assoc r :data (merge (result c) (:data r)))
      r)))

(def meta {:tags [:goal]})
