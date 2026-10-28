(ns jobs.forestry.plant-sapling
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.gate :as gate]
            [jobs.lib.need :as need]
            [jobs.lib.look :as look]
            [jobs.lib.forestry :as lib]
            [jobs.lib.shelter :as sh]
            [jobs.forestry.prepare-rule :as rule]
            [jobs.lib.trees :refer [debt-near-body? debts forget-plot-debts! stalled-kind stalled-policy without-stalled target-of with-sapling sapling-for sapling-of sapling-name? log-name? replant-kind species tree-free? cell-filled?]
             :rename {species all-species}]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]))

(def doc
  "Plant a sapling at :at, or at the oldest replant debt (then at the next, until none is owed), and clear that debt. A debt on a planned crop plot, or on a cell another block fills, is dropped, never planted.
  With no :species a debt whose sapling is carried comes before older ones.
  With :near and :within only the debts within :within blocks of :near count; the others stay owed.
  A missing sapling is got with jobs.lib.need (a seen chest, or the leaves of its tree broken for the drop) unless the call's :ways are #{:carried}
  is false or leaves out :item. The sapling kind is the species' (mangrove propagule, fungus too); the same one already on the spot counts as
  planted.
  Zones: a spot in another owner's zone or claim, or in another body's plan footprint (but :for-plan's own, or the body's own plan), is not planted.
  The job warns plant-sapling.declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.

  The planting is a jobs.blocks.place child (:ways #{:carried}: the sapling is got here). Bone meal for it is jobs.farm.fertilize's.

  Start conditions: :refused or :no-zones (the spot may not be planted; the debt stays owed), the spot's soil, light and room (checked
  before any sapling is fetched: :no-soil with :found, :cramped with :at and :block, the species' growth space of
  jobs.forestry.prepare-rule/headroom, :too-dark when the cell's best light, sky or block, is under ::min-light (9); an unseen cell or light is no refusal, nor is light in a cell holding a solid block (:occupied is the place child's);
  jobs.forestry.prepare readies the ground), :no-sapling (none carried and
  :ways #{:carried}; data :species); a log on the spot (data :pos) is waited out. A spot that holds its sapling, or no debt owed, is done at once. Ends :done with data
  {:pos} once planted; condition-failed with the need cause when the fetch
  failed, :refused when the zone rule changed on the way; a failed place child ends it with that ending
  (:occupied, :unreachable, :not-loaded ..). Quote: the walk to the spot.")

(a/defargs args
  {:at {:doc "where to plant; the oldest :forestry/replant debt when nil" :spec ::a/pos :default nil}
   :near {:doc "{:x :z}: only replant debts within :within blocks of it are taken; nil: any" :spec ::a/pos :default nil}
   :within {:doc "radius for :near" :spec (a/num-in 0 nil) :default nil}
   :species {:doc "sapling species; any when nil" :spec a/name? :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(a/defargs settings
  {::min-light {:default 9 :doc "The light (the better of sky and block light, the clock left out) a planted sapling's cell needs to grow." :spec (a/int-in 0 15)}
   ::job-time-s {:default 180 :doc "Seconds a planting may take: the walk, a fetch." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:at :near])

(def defaults {:spend {:job-time ::job-time-s}})

(def fit? (ready/fit-for :click))

(defn sapling-at?
  "True when the block at pos is a sapling, mangrove propagule or fungus."
  [p pos]
  (sapling-name? (u/seen-name p pos)))

(def saplings (mapv sapling-of all-species))

(defn current-target
  "The spot to plant: :at, else the oldest debt; of the debts whose sapling is carried first when no species is asked.
  Debts whose cell is unloaded, that failed lately or where the body would be due home before night (sh/home-due-at?)
  come last: the replant-owed trigger holds for the others only."
  [c]
  (let [all (debts c)
        p (:primitives c)
        reachable (filterv #(and (u/block-at p (:pos %)) (debt-near-body? p (:pos %)) (not (sh/home-due-at? p (ctx/view c) (:pos %)))) (without-stalled (ctx/view c) all))
        ds (if (seq reachable) reachable all)
        carried (when-not (:species (:args c)) (seq (with-sapling ds (u/inventory (:primitives c)))))]
    (target-of (or carried ds) (:args c))))

(def safe?
  (price/staying :work (ready/safe-at-by-night (fn [c] (some-> (current-target c) :pos a/cell)))))

(defn refusal
  "Why the job may not plant at pos (:refused or :no-zones; one warn per job), or nil."
  [c pos]
  (:refusal (gate/judge c :plant-sapling.declined "plant-sapling" :place [pos] {:except (:for-plan (:args c)) :own-plans-ok? true})))

(defn problem
  "The cause {:reason :no-sapling :species} while there is a spot to plant and no matching sapling is carried; else nil.
  The :item field (the species' sapling) is what a fetch gets."
  [c]
  (let [t (current-target c)
        species (:species t)]
    (when (and t (nil? (sapling-for (u/inventory (:primitives c)) species)))
      (cond-> {:reason :no-sapling :species species}
        species (assoc :item (sapling-of species))))))

(defn fetch-opts
  "The limits {:how} for fetching a sapling, nil when the call's ways leave only :carried."
  [c]
  (when-let [how (seq (need/fetch-how c))]
    {:how (set how)}))

(defn light-at
  "The better of the sky and block light at pos, from the raw world; nil when it is unknown."
  [p pos]
  (when-let [raw (look/raw-of p)]
    (let [{:keys [x y z]} pos]
      (when (look/light-known? raw x y z)
        (let [v (.lightAt raw x y z)]
          (max (bit-and v 15) (bit-shift-right v 4)))))))

(defn unfit
  "Why a sapling of species cannot grow at pos, as a cause {:reason ..}, or nil (unseen ground, room or light is no refusal)."
  [c pos species]
  (let [p (:primitives c)
        under (u/seen-name p (lib/down pos))
        space (when (rule/headroom-of species) (rule/growth-space p pos species))
        here (u/seen-name p pos)
        light (when (and here (tree-free? here)) (light-at p pos))]
    (cond
      (and under (not ((lib/soil-for species) under))) {:reason :no-soil :pos pos :found under}
      (= :cramped (:state space)) (assoc (select-keys space [:at :block]) :reason :cramped :pos pos)
      (and light (< light (settings/get settings ::min-light))) {:reason :too-dark :pos pos :light light})))

(defn target-unfit
  "Why the current target's spot cannot grow its sapling (unfit), or nil."
  [c]
  (let [t (current-target c)]
    (when (and t (not (#{"crimson" "warped"} (:species t)))) (unfit c (:pos t) (:species t)))))

(def spot-unfit
  (checks/refusing :spot-fit "the spot has soil, light and room" target-unfit))

(def spot-refused
  (checks/refusing :spot-allowed "the spot may be planted"
                   (fn [c]
                     (let [t (current-target c)]
                       (when t (refusal c (:pos t)))))))

(def no-sapling
  (checks/refusing :sapling "a sapling is carried or fetched"
                   (fn [c]
                     (when-not (fetch-opts c)
                       (some-> (problem c) (dissoc :item))))))

(def log-on-spot
  (checks/refusing :spot-clear "no log on the spot"
                   (fn [c]
                     (let [t (current-target c)]
                       (when (and t (log-name? (some-> (u/seen-block (:primitives c) (:pos t)) .-name)))
                         {:reason :log-on-spot :pos (:pos t)})))))

(def can-start? (cond/and spot-refused spot-unfit no-sapling))

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
  "The spot holds its sapling (with no :at: no debt is owed) and no child is under way."
  [c]
  (let [at (:at (:args c))
        owed? (not (planted? c))
        data [:units :saplings :done (if owed? 0 1) :left (if owed? 1 0) :result (if at {:pos at} {})]]
    (cond
      owed? (apply contract/later :owed "plant-sapling: a sapling to plant" data)
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

(defn ^:async fetch-sapling!
  "Get the missing sapling w names with jobs.lib.need: :continue while it works, :again once got, else the ending."
  [c w]
  (let [e (await (if (:item w) (need/item! c (:item w) 1 (fetch-opts c)) (need/blocks! c saplings 1 (fetch-opts c))))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) :again
          :else e)))

(defn ^:async place!
  "Plant the sapling at the target's spot with a blocks.place child: :again (more debts) or the ending."
  [c t sapling]
  (let [e (await (child! c :place 'jobs.blocks.place {:pos (:pos t) :item sapling :ways #{:carried} :for-plan (:for-plan (:args c))}))]
    (if (not= :done e)
      e
      (do (ctx/forget-where! c replant-kind (fn [d] (= (:pos t) (:pos d))))
          (if (and (nil? (:at (:args c))) (current-target c))
            :again
            (ctx/end c :done nil {:pos (:pos t)}))))))

(defn ^:async step
  "One piece of the planting: :again, :continue (a child waits on the world) or the ending. A failed ending leaves the debt
  noted in stalled-kind so the replant-owed trigger does not refire it at once."
  [c]
  (forget-plot-debts! c)
  (ctx/forget-where! c replant-kind #(cell-filled? (:primitives c) (:pos %)))
  (let [t (current-target c)
        sapling (when t (sapling-for (u/inventory (:primitives c)) (:species t)))
        refused #(refusal c (:pos t))
        e (cond
            (nil? t) (ctx/end c :done nil {})
            (refused) (ctx/end c :condition-failed (refused) {:pos (:pos t)})
            (target-unfit c) (ctx/end c :condition-failed (target-unfit c) {:pos (:pos t)})
            (nil? sapling) (await (fetch-sapling! c (problem c)))
            :else (await (place! c t sapling)))]
    (when (and t (map? e) (not= :cut-off (:kind e)) (not= :done (:kind e)))
      (ctx/remember! c stalled-kind {:pos (:pos t)} stalled-policy))
    e))

(defn ^:async round
  [c]
  (await (pace/steps! c #(step c))))
