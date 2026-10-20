(ns jobs.items.interact
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Use :item (the empty hand when nil) on the entity :id once, as a right-click. One call is the whole attempt: find the
  entity in sight, walk into reach (a go-to child, range 2, no escalation), interact. It yields :continue only when
  the walk waits on the world. Quote: A, a click and the walks.

  An entity that drops out of sight (the pit rim, a wall) between the sighting and the click is walked to at its last
  seen place and clicked there (the click finds it by id); it is :gone when the click does not.

  Ends with data {:used true|false :id :item :text, and after a click :raw the primitive's answer as a map}. The primitive's status :used is done.
  Impossible: :bad-args (no :id). Condition-failed: :gone (no such entity in sight), :no-item, :full, :cannot. Nothing-possible:
  :unreachable (the walk failed twice, data :why), :no-effect, :out-of-reach, :failed.")

(a/defargs args
  {:id {:doc "the entity id (observe entities lists them)" :spec (a/int-in 1 nil) :default nil}
   :item {:doc "the item to hold for the click; nil: the empty hand" :spec a/item? :default nil}})

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/num-in 0 nil)}
   ::max-walks {:default 2 :doc "Failed walks of one call before it stops :unreachable." :spec (a/int-in 0 nil)}
   ::click-s {:default 0.5 :doc "Seconds one click takes (the quote)." :spec (a/num-in 0 nil)}
   ::walk-s {:default 10 :doc "Seconds one walk into reach takes (the quote)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-walks [] (settings/get settings ::max-walks))

(def target [:id :item])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn problem
  "The cause {:reason :bad-args} when there is no entity id."
  [c]
  (when-not (pos-int? (:id (:args c)))
    {:reason :bad-args :why "items.interact needs a positive entity :id"}))

(defn can-start? [c] (checks/cause-status :never (problem c)))

(defn done?
  "Ok once the click was made."
  [c]
  (if (:used (ctx/mem c))
    (contract/ok :used "interact: the click was made" :units :uses :done 1 :left 0 :result {:used true})
    (contract/later :not-used "interact: the round ends the call" :units :uses :done 0 :left 1 :result {:used false})))

(defn quote-of [_c]
  (let [click (settings/get settings ::click-s)
        walk (settings/get settings ::walk-s)]
    {:uses {:job-time [click (+ click walk) (+ click (* walk (max-walks)))]}}))

(defn entity-of
  "The sensed entity with this id, or nil."
  [p id]
  (first (filter #(= id (.-id %)) (array-seq (.entities p #js {:radius 64 :max 64})))))

(def condition-statuses "Click statuses that are a fact about the call, not a failed try." #{:no-item :full :cannot})

(defn finish!
  "The ending, its text in the data: done when used, condition-failed for a fact, else nothing-possible."
  [c id item reason & [more]]
  (let [used (= :used reason)]
    (ctx/end c (cond used :done (or (= :gone reason) (condition-statuses reason)) :condition-failed :else :nothing-possible)
             (when-not used reason)
             (merge {:used used :id id :item item
                     :text (str (if used "used " "did not use ") (or item "the empty hand") " on entity " id
                                (when-not used (str ": " (name reason))))}
                    more))))

(defn ^:async step!
  "One walk or the click; :again after a walk that arrived (the click follows whatever the reach, the server judges it) or
  failed below the limit."
  [c fails]
  (let [{:keys [id item]} (:args c)
        p (:primitives c)
        seen (entity-of p id)
        _ (when seen (ctx/update-mem! c assoc :last-pos (u/pos-of (.-pos seen))))
        target (if seen (u/pos-of (.-pos seen)) (:last-pos (ctx/mem c)))]
    (cond
      (nil? target) (finish! c id item :gone)
      (and (> (u/dist (u/self-pos c) target) (reach)) (not (:arrived (ctx/mem c))))
      (let [r (await (child/call-walk* c :walk {:pos target :range 2 :ways #{:walk :clear-seal}}))]
        (cond
          (= :continue r) :continue
          (and (= :done (:kind r)) (:arrived (:data r))) (do (ctx/update-mem! c assoc :arrived true) :again)
          (>= (swap! fails inc) (max-walks)) (finish! c id item :unreachable {:why (contract/cause-key r)})
          :else :again))
      :else
      (let [r (await (ctx/act c :interact (clj->js (cond-> {:id id} item (assoc :item item)))))
            status (keyword (.-status r))]
        (when (= :used status) (ctx/update-mem! c assoc :used true))
        (finish! c id item status {:raw (js->clj r :keywordize-keys true)})))))

(defn ^:async round [c]
  (let [fails (atom 0)]
    (await (pace/steps! c #(step! c fails)))))
