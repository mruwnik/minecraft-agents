(ns jobs.survival.fetch-trip-food
  "A trip's food fetch (jobs.lib.ready/trip-fed): jobs.survival.get-food, and the holder's decision to go on short."
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]))

(def meta {:tags [:goal :food]})

(def doc
  "Fetch food for a trip: a jobs.survival.get-food child (:carry n, no search), which ends by its own goal.
  Done when the pack holds :carry food points (also at once), or when get-food ends otherwise and :go-on? says going on with the pack is
  cheaper than stopping (the caller priced it): done {:short true :have points :cause get-food's cause}, the child's
  ending as its :because. Without :go-on? get-food's ending is passed up.
  Quote: get-food's; done 1 fetch.")

(a/defargs args
  {:carry {:doc "food points the pack should hold" :spec (a/num-in 0 nil) :default 0}
   :go-on? {:doc "the caller priced going on with the pack as it is: a fetch that falls short ends done short" :spec boolean? :default false}})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds the fetch may take (get-food's own bound)." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(defn have [c] (foods/stock-points (u/inventory (:primitives c))))

(defn done? [c]
  (let [d (>= (have c) (:carry (:args c)))]
    (checks/goal d [:carried "the pack holds the food"] [:short "the pack holds less food than wanted"]
                 {:units :food :left (max 0 (- (:carry (:args c)) (have c)))})))

(defn ^:async quote-of [c]
  (assoc (select-keys (await (ctx/quote c 'jobs.survival.get-food {:carry (:carry (:args c)) :explore false})) [:uses :death :estimate])
         :done [0 1 1]))

(defn ^:async round [c]
  (let [e (await (ctx/call c :fetch 'jobs.survival.get-food {:carry (:carry (:args c)) :explore false}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (ctx/end c :done nil {:have (have c)})
      (:go-on? (:args c)) (contract/with-because
                            (ctx/end c :done nil {:short true :have (have c) :cause (contract/cause-key e)
                                                  :text (str "the fetch fell short (" (name (contract/cause-key e)) "): going on with " (have c) " food points")})
                            e)
      :else (contract/with-because (ctx/end c (:kind e) (:cause e) (:data e)) e))))
