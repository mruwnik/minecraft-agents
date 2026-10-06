(ns jobs.items.interact
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(def doc
  "Use :item (the empty hand when nil) on the entity :id once, as a right-click. One call is the whole attempt: find the
  entity in sight, walk into reach (one jobs.movement.go-to call, range 2, no escalation), interact. The check always
  passes. It yields :continue only when the walk waits on the world.

  Ends with info items.interact.done and {:used true|false :id :item :reason}. :reason is the primitive's status
  (:used, :no-effect, :no-item, :out-of-reach, :full, :cannot, :failed) or :gone (no such entity in sight) or
  :unreachable (the walk failed twice). :used is done; anything else is {:status :stopped}. A bad :id is :bad-args
  with an items.interact.declined warn.")

(def args
  {:id {:doc "the entity id (observe entities lists them)" :default nil}
   :item {:doc "the item to hold for the click; nil: the empty hand" :default nil}})

(def reach 2.5)
(def max-walks "Failed walks of one call before it stops :unreachable." 2)

(defn check [_c] true)

(defn entity-of
  "The sensed entity with this id, or nil."
  [p id]
  (first (filter #(= id (.-id %)) (array-seq (.entities p #js {:radius 64 :max 64})))))

(defn finish! [c {:keys [used] :as result}]
  (ctx/emit! c :items.interact.done :info
             (assoc result :text (str (if used "used " "did not use ") (or (:item result) "the empty hand") " on entity " (:id result)
                                      (when-not used (str ": " (name (:reason result)))))))
  (ctx/result! c result)
  :done)

(defn stop! [c id item reason & [more]]
  (finish! c (merge {:status :stopped :used false :id id :item item :reason reason} more)))

(defn ^:async round [c]
  (let [{:keys [id item]} (:args c)]
    (if-not (and (integer? id) (pos? id))
      (do (ctx/emit! c :items.interact.declined :warn {:reason :bad-args :text "items.interact needs a positive entity :id"})
          (ctx/result! c {:status :stopped :used false :reason :bad-args})
          :done)
      (loop [fails 0]
        (let [p (:primitives c)
              e (entity-of p id)]
          (cond
            (nil? e) (stop! c id item :gone)
            (> (u/dist (u/self-pos c) (u/pos-of (.-pos e))) reach)
            (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (u/pos-of (.-pos e)) :range 2 :escalate false}))
                  res (ctx/child-result c :walk)]
              (cond
                (= :continue r) :continue
                (and (= :done r) (:arrived res)) (recur fails)
                (>= (inc fails) max-walks) (stop! c id item :unreachable {:why (:reason res)})
                :else (recur (inc fails))))
            :else
            (let [r (await (ctx/act c :interact (clj->js (cond-> {:id id} item (assoc :item item)))))
                  status (keyword (.-status r))]
              (finish! c (cond-> {:used (= :used status) :id id :item item :reason status}
                           (not= :used status) (assoc :status :stopped))))))))))
