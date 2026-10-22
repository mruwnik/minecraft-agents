(ns jobs.strip.all
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.need :as need]
            [jobs.strip.site :as site]
            [jobs.strip.site-plan :refer [shears?]]
            [jobs.strip.site-tree :refer [settled vec3]]
            [jobs.strip.steps :as steps :refer [run-all!]]))

(def doc
  "Strip all stuff from one abandoned mineshaft (a site), from a field base, and leave it sealed: the parent of the strip
  siblings, each usable alone. Submit it listed with :survive-death? true: its state lives in body memory
  :strip/runs {site {:phase :was ..}} (and the site's in :strip/sites), so a restart, a stop or a death resumes where it was.
  Every step is a registered job run as a child, re-reading the world first. Phases:
  :base   jobs.strip.base (build the plan :base, move the respawn to its bed and :home there); the old :home and :spawn-bed are kept in :was.
  :shears items.get-tool shears, when none are carried.
  :site   jobs.strip.site (descent, corridors, seals, trips to the base chests) with the site args. Ending :trips-exhausted,
  :all-skipped or :unreachable, smelt and haul still run and the call then ends with the site's ending.
  :smelt  jobs.strip.smelt (the raw ore carried and in the base chests, smelted at the base furnaces).
  :haul   jobs.strip.haul (the loot banked in the base chests, then hauled to the main storage).
  The cleanup export puts :home and the respawn bed back to :was however the call ends (a cut included).
  A child that waits on the night or the light is waited out with :continue. A call that wants no smelt or no haul runs
  the siblings it wants itself.
  Needs :site and :base (a new site also :at), else impossible :bad-args. Its safe? is ready/safe-at-by-night :at.
  Ends :done with data {:segments {state n} :taken {class n} :hauled {item n} :smelted {raw-item n} :smelt-failed [{:furnace :reason}]},
  or a failed child's ending: its kind and its reason (:bad-plan, :no-torches, :all-skipped, ..), or :no-shears (condition-failed)
  when no shears can be had, with :phase, :child and, for a :child-failed child, :because. Event: strip.all.stopped (warn).")

(a/defargs args
  {:site {:doc "the site's name, the key of its memory" :spec a/name? :default nil}
   :at {:doc "[x y z] of a rail, plank or fence of the site the body has seen; kept once given" :spec ::a/pos :default nil}
   :base {:doc "id of the field-base plan (blueprint field-base)" :spec a/name? :default nil}
   :shaft {:doc "id of a mineshaft plan down from the base; nil: the nearest usable one; false: walk" :spec (a/or-of a/name? boolean?) :default nil}
   :take {:doc "classes stripped" :spec (a/coll-of #{:wood :cobweb :rail :ore}) :default [:wood :cobweb :rail :ore]}
   :spawner {:doc "a spawner in a segment: :light lights its spawn box and leaves it, :break lights and digs it, :seal seals the segment's mouth at once (its subtree skipped), :skip leaves it unlit" :spec #{:light :break :seal :skip} :default :light}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(a/defargs settings
  {::job-time-s {:default 10800 :doc "Seconds one mineshaft strip may take: base, strip, smelt and haul." :spec (a/num-in 0 nil)}
   ::max-runs {:default 4 :doc "Runs kept in memory; finished ones go first once there are more." :spec (a/int-in 1 nil)}})

(def kind :strip/runs)
(def policy {:cap 1 :ttl :forever})
(defn runs [c] (or (:data (ctx/latest c kind)) {}))
(defn run [c] (get (runs c) (:site (:args c))))

(defn save!
  "Write the run's state as (f state & xs); runs finished earlier go once there are more than ::max-runs."
  [c f & xs]
  (let [id (:site (:args c))
        all (update (runs c) id #(apply f % xs))
        old (take (- (count all) (settings/get settings ::max-runs)) (filter #(= :done (:phase (all %))) (remove #{id} (keys all))))]
    (ctx/remember! c kind (apply dissoc all old) policy)))

(defn result-of
  "What the call reports: the site's segments and what it took, what was smelted and hauled."
  [c]
  (let [r (run c)]
    (merge (site/result-of (site/site c)) {:hauled (:hauled r {}) :smelted (:smelted r {}) :smelt-failed (:smelt-failed r [])})))

(defn site-args [c]
  (select-keys (:args c) [:site :at :base :shaft :take :spawner :ways :ignore-zones?]))

(defn base-args [c] (select-keys (:args c) [:base :ignore-zones?]))

(defn end-child!
  "End the call with the failed child's ending e: its kind and reason, the child's fields (a :child-failed one's own)."
  [c job e]
  (let [res (merge (:data e) (when (map? (:cause e)) (:cause e)))
        reason (steps/reason-of e)
        fields (if (= :child-failed reason) (select-keys res [:child :because]) {:child (str job) :because reason})
        data (merge (result-of c) fields {:phase (:phase (run c))})]
    (ctx/emit! c :strip.all.stopped :warn (assoc (merge data {:reason reason})
                                                 :text (str "strip all " (:site (:args c)) " stopped " (pr-str reason))))
    (ctx/end c (if (#{:condition-failed :limit-reached :impossible} (:kind e)) (:kind e) :nothing-possible) reason data)))

(defn ^:async child!
  "Run the child of this phase; then (f e) with its :done ending, or the call's end for any other, :continue while it waits."
  [c job args f]
  (let [e (await (steps/call! c :child job args))]
    (cond (= :continue e) :continue
          (not= :done (:kind e)) (end-child! c job e)
          :else (do (f e) :again))))

(defn ^:async base! [c r]
  (if (nil? (:was r))
    (do (save! c assoc :was {:home (mem/place (ctx/view c) :home)
                             :spawn-bed (:pos (:data (sh-spawn/spawn-bed-live (ctx/view c))))})
        :again)
    (await (child! c 'jobs.strip.base (base-args c) (fn [_] (save! c assoc :phase :shears))))))

(defn ^:async shears! [c]
  (if (shears? c)
    (do (save! c assoc :phase :site) :again)
    (let [f (await (run-all! c [[:shears 'jobs.items.get-tool {:kind "shears"}]]))]
      (cond (= :continue f) :continue
            f (end-child! c 'jobs.items.get-tool (:ending f))
            (not (shears? c)) (ctx/end c :condition-failed :no-shears (merge (result-of c) {:phase :shears}))
            :else (do (save! c assoc :phase :site) :again)))))

(defn short-ending?
  "A site that gave up (:trips-exhausted :all-skipped :unreachable): the base chests and the bag still hold loot, so smelt and haul run,
  then the call reports the ending. A failed child or a missing need (torches, food) stops at once."
  [e]
  (and (= :nothing-possible (:kind e)) (not= :child-failed (steps/reason-of e))))

(defn ^:async site! [c]
  (let [e (await (steps/call! c :child 'jobs.strip.site (site-args c)))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) (do (save! c assoc :phase :smelt) :again)
          (short-ending? e) (do (save! c assoc :phase :smelt :site-end (select-keys e [:kind :cause :data])) :again)
          :else (end-child! c 'jobs.strip.site e))))

(defn ^:async step! [c]
  (let [r (run c)]
    (case (:phase r)
      :base (await (base! c r))
      :shears (await (shears! c))
      :site (await (site! c))
      :smelt (await (child! c 'jobs.strip.smelt (base-args c)
                            (fn [e] (save! c assoc :phase :haul :smelted (:smelted (:data e) {}) :smelt-failed (:smelt-failed (:data e) [])))))
      :haul (await (child! c 'jobs.strip.haul (base-args c)
                           (fn [e] (save! c assoc :phase :done :hauled (:hauled (:data e) {})))))
      :done (if-let [e (:site-end r)] (end-child! c 'jobs.strip.site e) (or (ctx/done c) :continue)))))

(defn ^:async round [c]
  (when-not (run c)
    (save! c (constantly {:phase :base})))
  (await (pace/steps! c #(step! c))))

(defn restore-steps [{:keys [home spawn-bed]}]
  (cond-> [(if home
             [:home 'jobs.memory.set-place {:name "home" :pos (vec3 home)}]
             [:home 'jobs.memory.forget-place {:name "home"}])]
    spawn-bed (conj [:spawn 'jobs.survival.set-spawn {:bed (vec3 spawn-bed)}])))

(defn ^:async cleanup
  "However the call ends: :home and the respawn bed go back to :was. Done, or nothing-possible with the failed child's
  reason."
  [c _]
  (let [was (:was (run c))
        f (when was (await (run-all! c (restore-steps was))))]
    (cond (= :continue f) :continue
          f (ctx/end c :nothing-possible (steps/reason-of (:ending f)) (select-keys f [:child]))
          :else (ctx/end c :done nil))))

;; ------------------------------------------------------------------ the contract

(def target [:site])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (price/staying :long (ready/safe-at-by-night #(a/cell (or (:at (:args %)) (:at (site/site %)))))))

(def fit? ready/fit?)

(defn can-start?
  "Never :bad-args: no :site or :base, and a new site without :at."
  [c]
  (let [{:keys [at base] id :site} (:args c)]
    (cond (not (and id base)) (contract/never :bad-args "needs :site and :base")
          (and (nil? at) (nil? (site/site c))) (contract/never :bad-args "a new site needs :at")
          :else (contract/ok :args "site, base and at given"))))

(defn done?
  "The run is over (phase :done, the site ended done); the site's settled segments are the progress."
  [c]
  (let [states (map :state (vals (:segments (site/site c))))
        data [:units :segments :done (count (filter settled states)) :left (count (remove settled states)) :result (result-of c)]]
    (if (and (= :done (:phase (run c))) (not (:site-end (run c))))
      (apply contract/ok :stripped (str "strip all " (:site (:args c)) " done " (pr-str (:segments (result-of c)))) data)
      (apply contract/later :stripping "strip all: segments left to strip" data))))

(defn quote-of [c]
  {:uses {:job-time [1 (* 120 (max 1 (count (:segments (site/site c))))) (settings/get settings ::job-time-s)]}})

(def meta {:tags [:mine]})
