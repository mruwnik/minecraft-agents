(ns jobs.gather.shaft-descent
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.waits :as waits]
            [jobs.lib.access-tool :as atool]
            [jobs.lib.child :as child]
            [jobs.lib.look :refer [cell-of]]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shafts :as shafts]
            [jobs.lib.util :as u]))

(def doc
  "Go down a mineshaft (a plan of jobs.lib.shafts) and stand at the start of its next branch: the mineshaft way of
  jobs.gather.dig-for's descent, a stair without the cutting of one. Picks the shaft (:shaft names it, else the nearest usable one
  not failed lately, foreign-zone ones only with :ignore-zones?), walks to its top, calls jobs.access.stair :through the plan down
  to its depth, then walks to the shaft's next branch (jobs.lib.shafts/next-for). A body already on the shaft's stair continues it
  from there. Digs nothing but the stair's steps; the caller digs the branch.

  Needs: a read zone list, torches and a pickaxe as the stair needs them (the stair is called with all ways). Impossible: nothing.

  Ends :done with data {:shaft id :to-y y :n branch number :branch {:from :heading} :heading name-or-nil :steps stair steps}. A shaft
  that cannot be used ends :condition-failed (:no-shaft :failed-lately :zone :claim :footprint, no stair part), one whose walk or stair
  failed :nothing-possible (:unreachable, or the stair's stop; condition-failed for its fact stops), data {:shaft id-or-nil :steps n}.
  A shaft whose walk or stair failed is left alone by later picks for a day (memory :mine-shaft-failed) unless named by :shaft.")

(a/defargs args
  {:shaft {:doc "the shaft plan id to use; nil: the nearest usable one not failed lately" :spec (a/pred "a shaft plan id or nil" #(or (nil? %) (string? %))) :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def target [])

(def safe? ready/safe-roaming)

(def fit? (ready/fit-for :long))

(def defaults {:spend {:job-time ::job-time-s}})

(def failed-shaft-policy
  "Body memory policy of :mine-shaft-failed: a shaft whose stair stopped is left alone for a day unless :shaft names it."
  {:cap 20 :ttl (* 24 60 60 1000)})

(defn failed-shafts [c]
  (set (map (comp :id :data) (ctx/entries c :mine-shaft-failed))))

(defn pick-shaft
  "The usable mineshaft the :shaft arg names, else the nearest not failed lately (jobs.lib.shafts/lookup; :ignore-zones? uses a foreign one): the shaft,
  or {:none reason}."
  [c]
  (let [{:keys [shaft ignore-zones?]} (:args c)
        {:keys [usable refused]} (shafts/lookup c (u/self-pos c) {:foreign-zone? ignore-zones?})
        usable (if (string? shaft) (filter #(= shaft (:id %)) usable) usable)
        failed (when-not (string? shaft) (failed-shafts c))
        fresh (remove #(contains? failed (:id %)) usable)]
    (or (first fresh)
        (when-let [stale (first usable)] {:none :failed-lately :shaft (:id stale)})
        {:none (or (get-in (first refused) [:verdict :reason]) :no-shaft)})))

(defn xyz [[x y z]] {:x x :y y :z z})

(def no-pick
  "Reasons that name a fact about the shaft (condition-failed), not a failed try, so the shaft is not marked failed."
  #{:failed-lately :zone :claim :footprint :no-shaft})

(defn fail!
  "End without a shaft used: kind by reason (a fact of the pick or one of the stair's facts is condition-failed). A shaft whose
  walk or stair failed is remembered as failed."
  [c reason shaft steps]
  (when (and shaft (not (no-pick reason)))
    (ctx/remember! c :mine-shaft-failed {:id shaft} failed-shaft-policy))
  (ctx/update-mem! c assoc :finished true)
  (ctx/end c (if (or (no-pick reason) (atool/facts reason) (nil? shaft)) :condition-failed :nothing-possible)
           {:reason reason :shaft shaft :text (str "shaft-descent stopped" (some->> shaft (str " at ")) ": " (name reason))}
           {:shaft shaft :steps (or steps 0)}))

(defn ^:async step
  "One step of a descent by mineshaft: pick it, walk to its top, the stair child down to its depth, walk to the next branch."
  [c]
  (let [{:keys [state id top heading to-y bottom] :as sh} (:shaft (ctx/mem c))
        steps (get-in (ctx/mem c) [:descent :steps])]
    (case state
      nil (let [p (pick-shaft c)]
            (if (:none p)
              (fail! c (:none p) (:shaft p) 0)
              (let [feet (cell-of (u/self-pos c))]
                (ctx/update-mem! c assoc :shaft (assoc (select-keys p [:id :top :heading :to-y :bottom])
                                                       :state (if (shafts/on-line? p [(:x feet) (:y feet) (:z feet)]) :down :going)))
                :again)))
      :going (let [r (await (child/call-walk* c :shaft-walk {:pos top :range 0}))]
               (cond
                 (= :continue r) :continue
                 (u/within? (u/self-pos c) (cell-of (xyz top)) 1) (do (ctx/update-mem! c assoc-in [:shaft :state] :down) :again)
                 :else (fail! c :unreachable id 0)))
      :down (let [r (waits/waiting-out c (await (ctx/call c :shaft-stair 'jobs.access.stair
                                     {:dir :down :heading heading :y to-y :through id :need-drop true
                                      :ignore-zones? (:ignore-zones? (:args c))})))
                  e (when (map? r) (assoc (:data r) :status (if (= :done (:kind r)) :done :stopped) :reason (contract/cause-key r)))]
              (cond
                (= :continue r) :continue
                (= :done (:status e))
                (let [shaft (assoc sh :to-y to-y)
                      n (shafts/branch-count c id)
                      branch (shafts/next-for c shaft)]
                  (ctx/update-mem! c #(-> % (assoc-in [:descent :steps] (or (:steps e) 1)) (assoc-in [:shaft :state] :walking)
                                          (assoc-in [:shaft :n] (inc n)) (assoc-in [:shaft :branch] branch)))
                  :again)
                :else (fail! c (or (:reason e) :stair-stopped) id (:steps e))))
      :walking (let [{:keys [from heading]} (:branch sh)
                     r (when from (await (child/call-walk* c :shaft-branch {:pos from :range 0})))]
                 (cond
                   (= :continue r) :continue
                   (and from (not (u/within? (u/self-pos c) (cell-of (xyz from)) 1))) (fail! c :unreachable id steps)
                   :else (do (ctx/update-mem! c assoc :finished true)
                             (ctx/end c :done nil {:shaft id :to-y to-y :n (:n sh) :branch (:branch sh) :heading (some-> heading name) :steps (or steps 0)
                                                   :text (str "took shaft " id " down to y " to-y (when heading (str ", branch " (:n sh) " " (name heading))))})))))))

(defn done?
  [c]
  (if (:finished (ctx/mem c))
    (contract/ok :done "the shaft is descended" :units :shafts :done 1 :left 0)
    (contract/later :descending "the shaft is being descended" :units :shafts :done 0 :left 1)))

(defn quote-of [_c] {:uses {:job-time [5 60 (settings/get settings ::job-time-s)]}})

(defn ^:async round [c] (await (pace/steps! c #(step c))))

(def meta {:tags [:mine :dig]})
