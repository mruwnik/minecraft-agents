(ns jobs.strip.haul
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.pace :as pace]
            [jobs.lib.work-place :as work-place]
            [jobs.strip.site-plan :refer [chests]]
            [jobs.strip.steps :as steps]))

(def doc
  "Bank the loot at a field base: deposit what the bag holds (storage.deposit, torches kept) into the base plan's first chest,
  then move the base chests' contents to the main storage (storage.haul). Usable after any trip. A haul that waits on the light is
  waited out with :continue. Needs the :base plan to be readable and to hold a chest, else waits :no-chest (a called child ends
  condition-failed with it). Ends :done {:hauled {item n}}, or the failing child's kind with :child-failed and :child, :because.")

(a/defargs args
  {:base {:doc "id of the field-base plan whose chests are emptied" :spec a/name? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds the banking and the haul may take." :spec (a/num-in 0 nil)}})

(defn base-chests [c] (chests c (:base (:args c))))

(defn ^:async step! [c]
  (let [zones (select-keys (:args c) [:ignore-zones?])
        m (ctx/mem c)]
    (if (:banked m)
      (let [e (await (steps/call! c :haul 'jobs.storage.haul (merge {:from (base-chests c)} zones)))]
        (cond (= :continue e) :continue
              (not= :done (:kind e)) (steps/end-failed c (steps/child-fields {:child "jobs.storage.haul" :ending e}))
              :else (do (ctx/update-mem! c assoc :hauled (:moved (:data e) {})) (or (ctx/done c) :continue))))
      (let [f (await (steps/run-all! c [[:deposit 'jobs.storage.deposit (merge {:chest (first (base-chests c)) :keep {"torch" 64} :make-chests? false} zones)]]))]
        (cond (= :continue f) :continue
              f (steps/end-failed c (steps/child-fields f))
              :else (do (ctx/update-mem! c assoc :banked true) :again))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))

(def target [:base])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (price/staying :long (ready/safe-at-by-night #(work-place/plan-spot {:plan (:base (:args %))} %))))

(def fit? ready/fit?)

(defn can-start? [c]
  (cond (not (:base (:args c))) (contract/never :bad-args "needs :base")
        (empty? (base-chests c)) (contract/later :no-chest "the base plan holds no chest (or is unreadable)")
        :else (contract/ok :args "base with a chest")))

(defn done?
  "True once the haul child is done; the chests' contents are not read here."
  [c]
  (let [hauled (:hauled (ctx/mem c))
        data [:units :hauls :done (if hauled 1 0) :left (if hauled 0 1) :result {:hauled (or hauled {})}]]
    (if hauled
      (apply contract/ok :hauled "the base chests are hauled" data)
      (apply contract/later :hauling "the loot is banked and the base chests hauled" data))))

(defn quote-of [_] {:uses {:job-time [1 600 (settings/get settings ::job-time-s)]}})

(def meta {:tags [:storage]})
