(ns jobs.strip.base
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.work-place :as work-place]
            [jobs.strip.site-plan :refer [plan-of spot]]
            [jobs.strip.steps :as steps :refer [run-all!]]))

(def doc
  "Set up a field base for a trip: build the plan :base (build.from-plan), move the respawn to its bed (survival.set-spawn)
  and :home to the same bed (memory.set-place). Usable for any trip, not only a mineshaft strip. The old :home and
  respawn bed are not saved or restored here: the caller keeps them (jobs.strip.all does).
  A night set-spawn is waited out with :continue. Needs the plan to be readable and to hold a \"bed\" spot, else
  condition-failed :bad-plan. Done at once when :home is already at the bed. Ends :done {:bed pos}, or the failing child's kind
  with :child-failed and :child, :because (the child's reason).")

(a/defargs args
  {:base {:doc "id of the field-base plan (blueprint field-base)" :spec a/name? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds the base setup may take: the build, the spawn and the home." :spec (a/num-in 0 nil)}})

(defn bed [c]
  (let [plan (plan-of c (:base (:args c)))]
    (when-not (or (nil? plan) (:broken plan)) (spot plan "bed"))))

(defn cell-of [p] (mapv #(js/Math.floor (get p %)) [:x :y :z]))

(defn ^:async round [c]
  (let [bed (bed c)
        zones (select-keys (:args c) [:ignore-zones?])]
    (if-not bed
      (ctx/end c :condition-failed :bad-plan {:base (:base (:args c))})
      (let [f (await (run-all! c [[:build 'jobs.build.from-plan (merge {:plan (:base (:args c))} zones)]
                                  [:spawn 'jobs.survival.set-spawn {:bed bed}]
                                  [:home 'jobs.memory.set-place {:name "home" :pos bed}]]))]
        (cond (= :continue f) :continue
              f (steps/end-failed c (steps/child-fields f))
              :else (ctx/end c :done nil {:bed bed}))))))

(def target [:base])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night #(work-place/plan-spot {:plan (:base (:args %))} %)))

(def fit? ready/fit?)

(defn can-start? [c]
  (if (:base (:args c))
    (contract/ok :args "base given")
    (contract/never :bad-args "needs :base")))

(defn done?
  "Done once :home is at the plan's bed."
  [c]
  (let [bed (bed c)
        home (mem/place (ctx/view c) :home)
        set-up? (and bed home (= (cell-of home) (cell-of bed)))
        data [:units :steps :done (if set-up? 1 0) :left (if set-up? 0 1) :result {:bed bed}]]
    (if set-up?
      (apply contract/ok :set-up "home is at the base bed" data)
      (apply contract/later :setting-up "the base is not set up" data))))

(defn quote-of [_] {:uses {:job-time [1 300 (settings/get settings ::job-time-s)]}})

(def meta {:tags [:build]})
