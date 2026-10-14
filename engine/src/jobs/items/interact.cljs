(ns jobs.items.interact
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]))

(def doc
  "Use :item (the empty hand when nil) on the entity :id once, as a right-click. One call is the whole attempt: find the
  entity in sight, walk into reach (a go-to child, range 2, no escalation), interact. It yields :continue only when
  the walk waits on the world. Quote: A, a click and the walks.

  Ends with info items.interact.done and data {:used true|false :id :item}. The primitive's status :used is done.
  Condition-failed: :bad-args (no :id), :gone (no such entity in sight), :no-item, :full, :cannot. Nothing-possible:
  :unreachable (the walk failed twice, data :why), :no-effect, :out-of-reach, :failed.")

(a/defargs args
  {:id {:doc "the entity id (observe entities lists them)" :spec (a/int-in 1 nil) :default nil}
   :item {:doc "the item to hold for the click; nil: the empty hand" :spec a/item? :default nil}})

(a/defargs settings
  {::reach {:default 2.5 :doc "How near the body stands to act on its target, in blocks." :spec (a/num-in 0 nil)}
   ::max-walks {:default 2 :doc "Failed walks of one call before it stops :unreachable." :spec (a/int-in 0 nil)}
   ::click-s {:default 0.5 :doc "Seconds one click takes (the quote)." :spec (a/num-in 0 nil)}
   ::walk-s {:default 10 :doc "Seconds one walk into reach takes (the quote)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-walks [] (settings/get settings ::max-walks))

(def target [:id :item])

(defn problem
  "The cause {:reason :bad-args} when there is no entity id."
  [c]
  (when-not (pos-int? (:id (:args c)))
    {:reason :bad-args :why "items.interact needs a positive entity :id"}))

(def conditions [[:fn problem]])

(defn done?
  "Counts only (the round ends the call with its data): whether the click was made."
  [c]
  (let [used (if (:used (ctx/mem c)) 1 0)]
    {:done? false :units :uses :done used :left (- 1 used)}))

(defn quote-of [_c]
  (let [click (settings/get settings ::click-s)
        walk (settings/get settings ::walk-s)]
    {:uses {:time [click (+ click walk) (+ click (* walk (max-walks)))]}}))

(defn entity-of
  "The sensed entity with this id, or nil."
  [p id]
  (first (filter #(= id (.-id %)) (array-seq (.entities p #js {:radius 64 :max 64})))))

(def condition-statuses "Click statuses that are a fact about the call, not a failed try." #{:no-item :full :cannot})

(defn finish!
  "Info and the ending: done when used, condition-failed for a fact, else nothing-possible."
  [c id item reason & [more]]
  (let [used (= :used reason)]
    (ctx/emit! c :items.interact.done :info
               {:used used :id id :item item :reason reason
                :text (str (if used "used " "did not use ") (or item "the empty hand") " on entity " id
                           (when-not used (str ": " (name reason))))})
    (ctx/end c (cond used :done (or (= :gone reason) (condition-statuses reason)) :condition-failed :else :nothing-possible)
             (when-not used reason)
             (merge {:used used :id id :item item} more))))

(defn ^:async round [c]
  (let [{:keys [id item]} (:args c)]
    (loop [fails 0]
      (let [p (:primitives c)
            e (entity-of p id)]
        (cond
          (nil? e) (finish! c id item :gone)
          (> (u/dist (u/self-pos c) (u/pos-of (.-pos e))) (reach))
          (let [r (await (child/call-walk* c :walk {:pos (u/pos-of (.-pos e)) :range 2 :escalate false}))]
            (cond
              (= :continue r) :continue
              (and (= :done (:kind r)) (:arrived (:data r))) (recur fails)
              (>= (inc fails) (max-walks)) (finish! c id item :unreachable {:why (contract/cause-key r)})
              :else (recur (inc fails))))
          :else
          (let [r (await (ctx/act c :interact (clj->js (cond-> {:id id} item (assoc :item item)))))
                status (keyword (.-status r))]
            (when (= :used status) (ctx/update-mem! c assoc :used true))
            (finish! c id item status)))))))
