(ns jobs.forestry.plant-sapling
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.gate :as gate]
            [jobs.lib.need :as need]
            [jobs.lib.trees :refer [debts target-of sapling-for sapling-of log-name? replant-kind species]
             :rename {species all-species}]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]))

(def doc
  "Plant a sapling at :at, or at the oldest replant debt (then at the next, until none is owed), and clear that debt. With :bone-meal n it then uses up to
  n bone meal on it.
  With :near and :within only the debts within :within blocks of :near count; the others stay owed.
  A missing sapling is got with jobs.lib.need (a seen chest, or the leaves of its tree broken for the drop) unless :fetch
  is false or leaves out :item. The sapling kind is the species' (mangrove propagule, fungus too); the same one already on the spot counts as
  planted.
  Zones: a spot in another owner's zone or claim, or in another body's plan footprint (but :for-plan's own, or the body's own plan), is not planted.
  The job warns plant-sapling.declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.

  The planting is a jobs.blocks.place child (fetch off: the sapling is got here) and each bone meal a jobs.blocks.use-on child.

  Start conditions: :refused or :no-zones (the spot may not be planted; the debt stays owed), :no-sapling (none carried and
  :fetch off for items; data :species); a log on the spot (data :pos) is waited out. A spot that holds its sapling, or no debt owed, is done at once. Ends :done with data
  {:pos} once planted (and the bone meal used up, grown or gone); condition-failed with the need cause when the fetch
  failed, :refused when the zone rule changed on the way; a failed place child ends it with that ending
  (:occupied, :unreachable, :not-loaded ..); a failed bone meal use (extra) still ends :done, with {:meal-failed {:kind :cause}}. Quote: the walk to the spot.")

(a/defargs args
  {:at {:doc "where to plant; the oldest :forestry/replant debt when nil" :spec ::a/pos :default nil}
   :near {:doc "{:x :z}: only replant debts within :within blocks of it are taken; nil: any" :spec ::a/pos :default nil}
   :within {:doc "radius for :near" :spec (a/num-in 0 nil) :default nil}
   :species {:doc "sapling species; any when nil" :spec a/name? :default nil}
   :bone-meal {:doc "bone meal uses after planting, 0 for none" :spec (a/int-in 0 nil) :default 0}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :fetch {:doc "get a missing sapling with jobs.lib.need: true, a set of kinds or a map of limits; false or without :item waits :no-sapling" :spec need/option? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 180 :doc "Seconds a planting may take: the walk, a fetch, the bone meal." :spec (a/num-in 0 nil)}
   ::max-steps {:default 400 :doc "Pieces of work of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))
(defn max-steps [] (settings/get settings ::max-steps))

(def target [:at :near])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (or (:at (:args c)) (:near (:args c)))))))

(def fit? ready/fit?)

(defn sapling-at?
  "True when the block at pos is a sapling, mangrove propagule or fungus."
  [p pos]
  (boolean (some-> (u/seen-name p pos)
                   (as-> n (or (.endsWith n "_sapling") (= "mangrove_propagule" n) (.endsWith n "_fungus"))))))

(defn has-meal? [p]
  (some #(= "bone_meal" (:name %)) (u/inventory p)))

(def saplings (mapv sapling-of all-species))

(defn current-target [c] (target-of (debts c) (:args c)))

(defn refusal
  "Why the job may not plant at pos (:refused or :no-zones; one warn per job), or nil."
  [c pos]
  (:refusal (gate/judge c :plant-sapling.declined "plant-sapling" :place [pos] {:except (:for-plan (:args c)) :own-plans-ok? true})))

(defn problem
  "The cause {:reason :no-sapling :species} while there is a spot to plant and no matching sapling is carried; else nil.
  The :item field (the species' sapling) is what a fetch gets."
  [c]
  (let [t (when-not (:meal (ctx/mem c)) (current-target c))
        species (:species t)]
    (when (and t (nil? (sapling-for (u/inventory (:primitives c)) species)))
      (cond-> {:reason :no-sapling :species species}
        species (assoc :item (sapling-of species))))))

(defn fetch-opts
  "The limits {:how :explore :minutes} for fetching a sapling, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c 'jobs.forestry.plant-sapling)]
    (when (contains? (:what o) :item) (select-keys o [:how :explore :minutes]))))

(def spot-refused
  (checks/refusing :spot-allowed "the spot may be planted"
                   (fn [c]
                     (let [t (when-not (:meal (ctx/mem c)) (current-target c))]
                       (when t (refusal c (:pos t)))))))

(def no-sapling
  (checks/refusing :sapling "a sapling is carried or fetched"
                   (fn [c]
                     (when-not (fetch-opts c)
                       (some-> (problem c) (dissoc :item))))))

(def log-on-spot
  (checks/refusing :spot-clear "no log on the spot"
                   (fn [c]
                     (let [t (when-not (:meal (ctx/mem c)) (current-target c))]
                       (when (and t (log-name? (some-> (u/seen-block (:primitives c) (:pos t)) .-name)))
                         {:reason :log-on-spot :pos (:pos t)})))))

(def can-start? (cond/and spot-refused no-sapling))

(def can-run? log-on-spot)

(defn planted?
  "Whether the call's spot holds its sapling (any when no :species is given), or with no :at whether no debt is owed."
  [c]
  (let [{:keys [at species]} (:args c)]
    (cond
      (nil? at) (nil? (current-target c))
      species (= (sapling-of species) (u/seen-name (:primitives c) at))
      :else (sapling-at? (:primitives c) at))))

(defn done?
  "The spot holds its sapling (with no :at: no debt is owed), no bone meal is being used and no child is under way."
  [c]
  (let [at (:at (:args c))
        meal (:meal (ctx/mem c))
        owed? (or meal (not (planted? c)))
        data [:units :saplings :done (if owed? 0 1) :left (if owed? 1 0) :result (if at {:pos at} {})]]
    (cond
      owed? (apply contract/later :owed (if meal "plant-sapling: bone meal under way" "plant-sapling: a sapling to plant") data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up "plant-sapling: planted, wrapping up" data)
      :else (apply contract/ok :planted (if at (str "planted at " (pr-str at)) "nothing to plant") data))))

(defn quote-of [c]
  (let [t (current-target c)
        d (if t (u/dist (u/self-pos c) (:pos t)) 0)
        fetch? (and (fetch-opts c) (problem c))]
    {:uses {:job-time [1 (+ 2 (* 0.3 d)) (if fetch? (max (time-s) (+ 4 (* 0.6 d))) (+ 4 (* 0.6 d)))]}}))

(defn ^:async child!
  "One call of job in slot with args: :continue while it waits on the world, :done once it is done (the slot is freed for the
  next call), else its ending."
  [c slot job args]
  (let [e (await (ctx/call c slot job (assoc args :ignore-zones? (boolean (:ignore-zones? (:args c))))))]
    (cond (= :continue e) :continue
          (not= :done (:kind e)) e
          :else (do (ctx/update-mem! c update :children dissoc slot) :done))))

(defn ^:async meal-round
  "One bone meal use on the planted sapling (a use-on child): :again; the :done ending when it grew, the uses are spent or
  no bone meal is carried; :continue while the child waits."
  [c {:keys [pos left]}]
  (let [p (:primitives c)]
    (if (or (zero? left) (not (sapling-at? p pos)) (not (has-meal? p)))
      (do (ctx/update-mem! c dissoc :meal) (ctx/end c :done nil {:pos pos}))
      (let [e (await (child! c :meal 'jobs.blocks.use-on {:pos pos :item "bone_meal" :face "up"}))]
        (cond
          (contract/ending? e) (do (ctx/update-mem! c dissoc :meal)
                                   (ctx/end c :done nil {:pos pos :meal-failed (select-keys e [:kind :cause])}))
          (not= :done e) e
          :else (do (ctx/update-mem! c update-in [:meal :left] dec) :again))))))

(defn ^:async fetch-sapling!
  "Get the missing sapling w names with jobs.lib.need: :continue while it works, :again once got, else the ending."
  [c w]
  (let [e (await (if (:item w) (need/item! c (:item w) 1 (fetch-opts c)) (need/blocks! c saplings 1 (fetch-opts c))))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) :again
          :else e)))

(defn ^:async place!
  "Plant the sapling at the target's spot with a blocks.place child: :again (bone meal follows) or the ending."
  [c t sapling]
  (let [e (await (child! c :place 'jobs.blocks.place {:pos (:pos t) :item sapling :fetch false :for-plan (:for-plan (:args c))}))]
    (if (not= :done e)
      e
      (let [n (:bone-meal (:args c))]
        (ctx/forget-where! c replant-kind (fn [d] (= (:pos t) (:pos d))))
        (cond
          (and (pos? n) (has-meal? (:primitives c))) (do (ctx/update-mem! c assoc :meal {:pos (:pos t) :left n}) :again)
          (and (nil? (:at (:args c))) (current-target c)) :again
          :else (ctx/end c :done nil {:pos (:pos t)}))))))

(defn ^:async step
  "One piece of the planting: :again, :continue (a child waits on the world) or the ending."
  [c]
  (let [t (current-target c)
        sapling (when t (sapling-for (u/inventory (:primitives c)) (:species t)))
        meal (:meal (ctx/mem c))
        refused #(refusal c (:pos t))]
    (cond
      meal (await (meal-round c meal))
      (nil? t) (ctx/end c :done nil {})
      (refused) (ctx/end c :condition-failed (refused) {:pos (:pos t)})
      (nil? sapling) (await (fetch-sapling! c (problem c)))
      :else (await (place! c t sapling)))))

(defn ^:async round
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue)))))
