(ns jobs.movement.mount
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Get on the boat, raft, minecart or rideable mob :id, or the nearest one in sight named :name (e.g. oak_boat, pig). One call is the whole attempt: find the entity in sight, walk
  into reach (jobs.movement.go-to, range 2, no escalation), mount (a mob needs an empty hand, a boat or minecart does not), wait for the server to seat the body.
  Holds the vehicle (jobs.lib.vehicle) while the job lives, so the :mounted trigger does not step the body off:
  the body stays aboard only during an exclusive run, otherwise the hold ends with the job and the trigger steps it off ~30 s later.
  By :name it prefers a vehicle with no rider.
  Impossible: :bad-args. Conditions, asked before every round: :no-vehicle while none is in sight and :aboard-other while on another vehicle (a listed call waits, a called one ends condition-failed with that cause). The round yields :continue only when the walk waits on the world.

  Ends done (data {:id :vehicle name}, plus :already true when aboard it already and the root job did not yet hold it; cause :already when it did), or
  condition-failed: :gone (the server found no such entity), :not-mountable, :occupied, :hand-full
  (a mob, with an item in hand that cannot be put away), or
  nothing-possible: :timeout (the server never seated the body), :unreachable (the walk failed twice),
  :failed (any other status, data :mount the primitive's).")

(a/defargs args
  {:id {:doc "the entity id of the vehicle or mount (observe entities lists them)" :spec (a/int-in 1 nil) :default nil}
   :name {:doc "instead of :id: the entity name to mount, the nearest in sight" :spec a/name? :default nil}})

(def target [:id :name])

(def reach 2.5)
(def max-walks 2)
(def walk-speed "Blocks a second a walk covers, for the quote." 4)
(def seat-s "Seconds the server takes to seat the body, for the quote." 1)

(defn sensed
  "The entities in sight with a position (heard mobs have none)."
  [p]
  (filter #(some? (.-pos %)) (look/seen-objs p {:radius 64 :max 64})))

(defn entity-of [p id]
  (first (filter #(= id (.-id %)) (sensed p))))

(def bad-args
  "Never :bad-args unless an :id or a :name is given."
  (checks/args-fit "needs an :id or a :name"
                   (fn [c] (let [{:keys [id name]} (:args c)] (or (and (integer? id) (pos? id)) (string? name))))))

(defn no-vehicle
  "Later :no-vehicle while no entity of that :id or :name is in sight."
  [c]
  (let [{:keys [id name]} (:args c)
        p (:primitives c)]
    (if (if (and (nil? id) (string? name))
          (some #(= name (.-name %)) (sensed p))
          (some? (entity-of p id)))
      (contract/ok :vehicle "a vehicle in sight")
      (contract/later :no-vehicle "no such vehicle in sight"))))


(defn done?
  "Done when aboard the wanted vehicle (:id its entity id, :name its entity name) and the root job holds it; aboard
  without the hold stays later, so the round takes the hold before it ends."
  [c]
  (let [{:keys [id] wanted :name} (:args c)
        ride (vehicle/vehicle-of (.self (:primitives c)))
        aboard (and ride (if id (= id (:id ride)) (or (nil? wanted) (= wanted (:name ride)))))]
    (if (and aboard (vehicle/held-by-root? c))
      (contract/ok :aboard "aboard the vehicle" :units :vehicles :done 1 :left 0 :result {:id (:id ride) :vehicle (:name ride)})
      (if aboard
        (contract/later :holding "aboard, the vehicle is not yet held" :units :vehicles :done 1 :left 0)
        (contract/later :ashore "not aboard the vehicle" :units :vehicles :done 0 :left 1)))))

(defn nearest-named
  "The id of the nearest sensed entity named name, a free one before an occupied one, or nil."
  [c name]
  (let [p (:primitives c) me (u/self-pos c)]
    (some->> (sensed p)
             (filter #(= name (.-name %)))
             (sort-by (juxt #(pos? (count (.-passengers %))) #(u/dist me (u/pos-of (.-pos %)))))
             first .-id)))

(defn aboard-other
  "Later :aboard-other while the body sits in a vehicle other than the wanted one."
  [c]
  (let [{:keys [id name]} (:args c)
        v (vehicle/vehicle-of (.self (:primitives c)))
        wanted (if (and (nil? id) (string? name)) (nearest-named c name) id)]
    (if (and v (not= wanted (:id v)))
      (contract/later :aboard-other (str "aboard another vehicle, " (:name v)) :vehicle (:name v))
      (contract/ok :vehicle "not aboard another vehicle"))))

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(def can-start? (cond/and bad-args no-vehicle aboard-other))

(def can-run? (cond/and no-vehicle aboard-other))

(defn quote-of [c]
  (let [p (:primitives c)
        {:keys [id name]} (:args c)
        id (if (and (nil? id) (string? name)) (nearest-named c name) id)
        e (when id (entity-of p id))
        walk-s (if e (/ (max 0 (- (u/dist (u/self-pos c) (u/pos-of (.-pos e))) reach)) walk-speed) 0)]
    {:uses {:job-time [seat-s (+ seat-s walk-s) (+ (* 2 seat-s) (* 3 walk-s) 10)]}}))

(defn stop!
  "End the call without a seat: kind and reason, data the vehicle id and more."
  [c id kind reason & [more]]
  (ctx/end c kind reason (merge {:id id :text (str "cannot mount entity " id ": " (name reason))} more)))

(defn done! [c result]
  (ctx/end c :done nil result))

(defn ^:async round [c]
  (let [{:keys [name] :as a} (:args c)
        id (if (and (nil? (:id a)) (string? name)) (nearest-named c name) (:id a))
        p (:primitives c)]
    (cond
      (and (nil? (:id a)) (string? name) (nil? id)) :continue
      (not (and (integer? id) (pos? id))) (stop! c id :condition-failed :bad-args)
      (some? (vehicle/vehicle-of (.self p)))
      (let [v (vehicle/vehicle-of (.self p))]
        (if (= id (:id v))
          (do (vehicle/hold! c) (done! c {:id id :vehicle (:name v) :already true}))
          :continue))
      :else
      (loop [fails 0]
        (let [e (entity-of p id)]
          (cond
            (nil? e) :continue
            (> (u/dist (u/self-pos c) (u/pos-of (.-pos e))) reach)
            (let [r (await (child/call-walk* c :walk {:pos (u/pos-of (.-pos e)) :range 2 :ways #{:walk :clear-seal}}))]
              (cond
                (= :continue r) :continue
                (>= (inc fails) max-walks) (stop! c id :nothing-possible :unreachable {:why (contract/cause-key r)})
                :else (recur (inc fails))))
            :else
            (do (vehicle/hold! c)
                (let [r (await (ctx/act c :mount #js {:id id}))
                      status (.-status r)]
                  (case status
                    "mounted" (done! c {:id id :vehicle (.-name (.-vehicle r))})
                    "already-mounted" (done! c {:id id :vehicle (.-name (.-vehicle r)) :already true})
                    "gone" (stop! c id :condition-failed :gone)
                    "not-mountable" (stop! c id :condition-failed :not-mountable)
                    "occupied" (stop! c id :condition-failed :occupied)
                    "hand-full" (stop! c id :condition-failed :hand-full)
                    "timeout" (stop! c id :nothing-possible :timeout {:seen (js->clj (.-vehicle r) :keywordize-keys true)})
                    "out-of-reach" (if (>= (inc fails) max-walks) (stop! c id :nothing-possible :unreachable) (recur (inc fails)))
                    (stop! c id :nothing-possible :failed {:mount status}))))))))))
