(ns jobs.lib.use-entity
  "The entity kind of jobs.items.use: find it in sight, walk into reach, click it. `ops` is what the job calls."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/num-in 0 nil)}
   ::max-walks {:default 2 :doc "Failed walks of one call before it stops :unreachable." :spec (a/int-in 0 nil)}
   ::click-s {:default 0.5 :doc "Seconds one click takes (the quote)." :spec (a/num-in 0 nil)}
   ::walk-s {:default 10 :doc "Seconds one walk into reach takes (the quote)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-walks [] (settings/get settings ::max-walks))

(defn quote-of [_c]
  (let [click (settings/get settings ::click-s)
        walk (settings/get settings ::walk-s)]
    {:uses {:job-time [click (+ click walk) (+ click (* walk (max-walks)))]}}))

(defn bad-args [c]
  (when-not (pos-int? (:on (:args c))) "items.use needs a positive entity id as :on"))

(def zones (checks/refusing :allowed "an entity click asks no zone" (fn [_] nil)))

(defn entity-of
  "The sensed entity with this id, or nil."
  [p id]
  (first (filter #(= id (.-id %)) (array-seq (.entities p #js {:radius 64 :max 64})))))

(def condition-statuses "Entity click statuses that are a fact about the call, not a failed try." #{:no-item :full :cannot})

(defn finish-entity!
  "The ending, its text in the data: done when used, condition-failed for a fact, else nothing-possible."
  [c id item reason & [more]]
  (let [used (= :used reason)]
    (ctx/end c (cond used :done (or (= :gone reason) (condition-statuses reason)) :condition-failed :else :nothing-possible)
             (when-not used reason)
             (merge {:used used :id id :item item
                     :text (str (if used "used " "did not use ") (or item "the empty hand") " on entity " id
                                (when-not used (str ": " (name reason))))}
                    more))))

(defn ^:async attempt-entity!
  "One walk or the click; :again after a walk that arrived (the click follows whatever the reach, the server judges it,
  unless the target moved since: then it is walked to again) or failed below the limit."
  [c fails]
  (let [{id :on :keys [item]} (:args c)
        item (when (not= "none" item) item)
        p (:primitives c)
        seen (entity-of p id)
        _ (when seen (ctx/update-mem! c assoc :last-pos (u/pos-of (.-pos seen))))
        target (if seen (u/pos-of (.-pos seen)) (:last-pos (ctx/mem c)))]
    (cond
      (nil? target) (finish-entity! c id item :gone)
      (and (> (u/dist (u/self-pos c) target) (reach))
           (not (some-> (:arrived (ctx/mem c)) (u/dist target) (< 1))))
      (let [r (await (child/call-walk* c :walk {:pos target :range 2 :ways #{:walk :dig} :may-dig #{:own}}))]
        (cond
          (= :continue r) :continue
          (and (= :done (:kind r)) (:arrived (:data r))) (do (ctx/update-mem! c assoc :arrived target) :again)
          (>= (swap! fails inc) (max-walks)) (finish-entity! c id item :unreachable {:why (contract/cause-key r)})
          :else :again))
      :else
      (let [r (await (ctx/act c :interact (clj->js (cond-> {:id id} item (assoc :item item)))))
            status (keyword (.-status r))]
        (when (= :used status) (ctx/update-mem! c assoc :used true))
        (finish-entity! c id item status {:raw (js->clj r :keywordize-keys true)})))))

(defn ^:async run-round [c]
  (let [fails (atom 0)]
    (await (pace/steps! c #(attempt-entity! c fails)))))

(def ops {:safe? ready/safe-here-by-night :quote quote-of :bad-args bad-args :zones zones :round run-round})
