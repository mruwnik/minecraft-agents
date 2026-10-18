(ns jobs.movement.boat-recover
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]
            [jobs.movement.boat-launch :as launch]))

(def doc
  "Take a boat back: the boat is :id, else the nearest boat or raft in sight (:radius). When aboard, it gets off first (jobs.movement.leave-vehicle),
  walks into reach (jobs.movement.go-to), hits it until it breaks and collects the dropped item (jobs.items.collect-drops).
  Condition at the start: :no-boat (none in sight); a listed call waits on it.
  Done once the boat is gone and its item is carried (data {:id :item name :collected n}), info boat.recovered, or
  condition-failed :gone (the boat is no longer in sight), or
  nothing-possible: :leave-failed, :unreachable (the walk failed twice), :not-collected (the item was not picked up), or
  limit-reached :job-time (the engine ends it when the call's :spend :job-time ran out; data {:id :hits}).

  Never :continue except when a child waits on the world.")

(a/defargs args
  {:id {:doc "the entity id of the boat; nil: the nearest in sight" :spec (a/int-in 1 nil) :default nil}
   :radius {:doc "how far to look for a boat" :spec (a/num-in 0 nil) :default 12}})

(def target [:id])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a recovery may take (a boat that does not break is hit until then)." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def reach 3)
(def recover-s "Seconds a recovery takes without a walk, for the quote." 8)

(defn pick-boat [c]
  (let [id (:id (:args c))
        boats (launch/boats-in-sight c)]
    (if id (first (filter #(= id (:id %)) boats)) (first boats))))

(defn no-boat
  "At the start: later :no-boat while none is in sight."
  [c]
  (if (nil? (pick-boat c))
    (contract/later :no-boat "no boat in sight" :hint "no boat in sight")
    (contract/ok :boat "a boat in sight")))

(def can-start? no-boat)

(defn carried [c item]
  (reduce + (for [s (u/inventory (:primitives c)) :when (= item (:name s))] (:count s 1))))

(defn done?
  "Ok once the boat being broken is out of sight and more of its item is carried than before the first hit."
  [c]
  (let [{:keys [hits target]} (ctx/mem c)
        {:keys [id item before]} target
        gone (and target (not-any? #(= id (:id %)) (launch/boats-in-sight c)))
        taken (and gone (> (carried c item) before))]
    (if taken
      (contract/ok :recovered "the boat is back in the inventory" :units :boats :done 1 :left 0 :result {:id id :item item})
      (contract/later :recovering "the boat is not back yet" :units :boats :done 0 :left 1
                      :result {:id (or id (:id (:args c))) :hits (or hits 0)}))))

(defn quote-of [c]
  (let [most (or (get-in (ctx/frozen-call c) [:spend :job-time]) (time-s))
        mean (min most recover-s)]
    {:uses {:job-time [(min 1 mean) mean most]}}))

(defn ^:async round [c]
  (let [p (:primitives c)
        id (:id (:args c))
        aboard (when (vehicle/mounted? p) (await (ctx/call c :leave 'jobs.movement.leave-vehicle {})))]
    (cond
      (= :continue aboard) :continue
      (and (some? aboard) (not= :done (:kind aboard))) (ctx/end c :nothing-possible :leave-failed {:id id})
      :else
      (loop [hits 0 failed 0]
        (let [boat (pick-boat c)]
          (cond
            (nil? boat) (ctx/end c :condition-failed :gone {:id id})
            (> (u/dist (u/self-pos c) (:pos boat)) reach)
            (let [w (await (child/call-walk* c :walk {:pos (:pos boat) :range 2 :ways #{:walk :clear-seal}}))]
              (cond
                (= :continue w) :continue
                (= :done (:kind w)) (recur hits failed)
                (>= (inc failed) 2) (ctx/end c :nothing-possible :unreachable {:id (:id boat) :why (contract/cause-key w)})
                :else (recur hits (inc failed))))
            :else
            (let [item (:name boat)
                  _ (when-not (= (:id boat) (:id (:target (ctx/mem c))))
                      (ctx/update-mem! c assoc :target {:id (:id boat) :item item :before (carried c item)}))
                  r (await (ctx/act c :attack #js {:id (:id boat)}))]
              (if (= "killed" (.-status r))
                (let [k (await (ctx/call c :collect 'jobs.items.collect-drops
                                         {:near (:pos boat) :radius 4 :items [item]}))
                      n (:collected (:data k) 0)]
                  (cond
                    (= :continue k) :continue
                    (zero? n) (ctx/end c :nothing-possible :not-collected {:id (:id boat) :item item})
                    :else (do (ctx/emit! c :boat.recovered :info {:id (:id boat) :item item :collected n :text (str "took back " item)})
                              (ctx/end c :done nil {:id (:id boat) :item item :collected n}))))
                (do (ctx/update-mem! c assoc :hits (inc hits))
                    (recur (inc hits) failed))))))))))
