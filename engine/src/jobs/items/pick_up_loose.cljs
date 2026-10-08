(ns jobs.items.pick-up-loose
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.loose :as loose]))

(def doc
  "Pick up the loose items the body sees nearby that are worth it (jobs.lib.loose: seen, within :radius, worth the
  detour or on :always, not thrown, owed to another live job or in another's zone or claim). The :loose-items reflex runs it.
  One jobs.forestry.collect-drops child takes them by entity id; its go-to leaves what it cannot reach. Items not
  picked up are remembered (:loose-left) for :left-s; so are the seen items when it finds none worth it, so a
  reflex that judged otherwise cannot fire it again for them. One info event loose-items.picked {:collected :items :left}.
  Result {:collected n :items [names] :left [ids]}. :declined when none came in, or (the reflex) nothing is worth it;
  run by the agent with nothing worth it, it ends stopped :nothing-worth-it at once (a held run never blocks the queue).")

(a/defargs args
  {:radius {:doc "blocks from the body; nil: the jobs.lib.loose setting" :spec (a/num-in 0 32) :default nil}
   :min-ratio {:doc "worth over detour cost to pick up; nil: the setting" :spec (a/num-in 0 nil) :default nil}
   :always {:doc "item names always picked up; nil: the setting" :spec (a/coll-of a/item?) :default nil}
   :never {:doc "item names never picked up; nil: the setting" :spec (a/coll-of a/item?) :default nil}
   :value {:doc "jobs.lib.cost item-value options (:overrides :prices ...)" :spec map? :default nil}
   :cost {:doc "jobs.lib.cost detour-cost options (:per-block :per-climb)" :spec map? :default nil}})

(defn check
  "Always: with nothing worth it the round ends the job, so a held run never blocks the queue."
  [_c]
  true)

(defn names-of [items] (vec (distinct (keep #(some-> (.-item %) .-name) items))))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        view (ctx/view c)
        o (loose/options (:args c))
        items (loose/candidates p view (:world (:engine c)) (:args c) (:root c))
        ids (mapv #(.-id %) items)]
    (if (empty? ids)
      (do (loose/left! c (into [] (comp (map #(.-id %)) (remove (loose/left-ids view))) (loose/seen p o)) o)
          (if (:reflex c)
            (do (ctx/result! c {:collected 0 :reason :nothing-worth-it})
                :declined)
            (do (ctx/result! c {:status :stopped :reason :nothing-worth-it :collected 0
                                :text "pick-up-loose stopped: no loose item in sight is worth picking up"})
                :done)))
      (let [r (await (child/run! c :collect 'jobs.forestry.collect-drops {:ids ids :radius (+ (:radius o) (:collect-pad o))}))
            n (or (:collected (ctx/child-result c :collect)) 0)
            still (set (map #(.-id %) (look/seen-items p {:radius (:check-radius o) :ids ids})))
            left (filterv still ids)
            out {:collected n :items (names-of items) :left left}]
        (loose/left! c left o)
        (ctx/emit! c :loose-items.picked :info
                   (assoc out :text (str "picked up " n " loose item" (when (not= 1 n) "s") " (" (str/join ", " (:items out)) ")"
                                         (when (seq left) (str "; left " (count left))))))
        (ctx/result! c (cond-> out (= :continue r) (assoc :reason :walk-waiting)))
        (if (pos? n) :done :declined)))))
