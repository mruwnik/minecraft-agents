(ns jobs.blocks.use-on
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Right-click the block at :pos ([x y z] or {:x :y :z}) once, with :item in hand (nil: whatever is held stays in hand; none: the empty hand), on :face.
  A bucket :item (water_bucket pours, lava_bucket pours lava, bucket scoops) is used on the cell :pos itself, through the
  place act: the liquid goes there, or is taken from there; :face is ignored.
  One call is the whole attempt: walk into reach (one jobs.movement.go-to call, within :range, no escalation), click. Conditions (can-start? and can-run?): a bad call is
  never (:bad-args); a bucket :item changes the cell, so zones, claims and plans may refuse it (a pour asks :place, a scoop :harvest; {:reason :not-allowed :pos :by ...},
  :ignore-zones? skips the rule); a plain click changes no block by itself and asks no zone. It yields :continue only when the walk waits on the world.

  Ends with data {:used true|false :pos :item :face :reason}, plus :spawn-set true when a bed answered \"respawn point set\" (a bed is used by day to set the spawn; not in the Nether or End). :reason is the primitive's
  status (:used, :unchanged, :no-item, :no-room, :missing, :cannot, :occupied, :unreachable, :failed); only :used is done.
  condition-failed: :no-item, :missing, :cannot, :occupied (a bucket's cell is filled). Impossible: a bad :pos or :face (:bad-args, from can-start?). nothing-possible: :unreachable (a walk
  that failed twice, :why), :unchanged, :no-room, :failed.")

(a/defargs args
  {:pos {:doc "the block to click, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :item {:doc "the item to hold for the click; nil: keep the hand as it is; none: the empty hand" :spec a/item? :default nil}
   :see {:doc "true: click only from where the eye sees the block (jobs.lib.aim), walking to such a stand first (go-to :see; none: :unreachable); false: within reach is enough, though a wall in between refuses the click" :spec boolean? :default false}
   :ignore-zones? :shared
   :range {:doc "how many cells from :pos the walk may stop (go-to :range); a click the server checks by distance, a bed's 3 blocks, wants less" :spec (a/num-in 0 nil) :default 3}
   :face {:doc "the face clicked: up down north south east west" :spec #{"up" "down" "north" "south" "east" "west"} :default "up"}})

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))

(def fit? ready/ok)

(def target [:pos])
(def defaults {:spend {:job-time ::job-time-s}})


(def faces #{"up" "down" "north" "south" "east" "west"})
(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-walks {:default 2 :doc "Failed walks of one call before it ends :unreachable." :spec (a/int-in 0 nil)}})
(defn max-walks [] (settings/get settings ::max-walks))

(def condition-reasons
  "The click statuses that name a missing target or item; any other failed status is nothing-possible."
  #{:no-item :missing :cannot :occupied})

(defn quote-of [c]
  (let [{:keys [pos error]} (b/parse (:args c))
        walk (if (or error (b/in-reach? c pos (:see (:args c)))) 0 (u/eye-dist (u/self-pos c) pos))]
    {:uses {:job-time [0.5 (+ 1 (/ walk 4)) (+ 10 walk)]}}))

(defn finish!
  "End the call: done when used, else kind with the result's :reason (and :why); the data is the result with a :text."
  [c kind {:keys [used] :as result}]
  (let [result (update result :text #(or % (str (if used "clicked " "did not click ") (pr-str (b/cell (:pos result)))
                                                (when-not used (str ": " (name (:reason result)))))))]
    (ctx/end c kind (when-not used (select-keys result [:reason :why])) result)))

(defn ^:async walk!
  "The walk to pos: :arrived, :continue or {:unreachable why}."
  [c pos see? range]
  (let [[pens args] (await (b/walk-request c pos see? range))
        r (await (child/call-walk* c :walk args))
        arrived? (and (= :done (:kind r)) (:arrived (:data r)))]
    (cond
      (= :continue r) :continue
      (and arrived? (b/in-reach? c pos see?)) :arrived
      :else (do (b/warn-kept-out! c pens)
                {:unreachable (cond (not= :done (:kind r)) :declined
                                    arrived? :out-of-reach
                                    :else (get-in r [:data :why] :unreachable))}))))

(defn bucket? [item] (and (string? item) (boolean (re-find #"bucket$" item))))

(defn ^:async click!
  "The click or bucket use at pos once in reach: the act's status keyword and its reply."
  [c {:keys [pos item face]}]
  (let [r (await (if (bucket? item)
                   (ctx/act c :place (clj->js {:pos pos :item item}))
                   (ctx/act c :useOn (clj->js (cond-> {:pos pos :face face} item (assoc :item item))))))
        status (keyword (.-status r))]
    [(if (and (bucket? item) (= :placed status)) :used status) r]))

(defn bad-args [c]
  (let [{:keys [face] :or {face "up"}} (:args c)
        error (or (:error (b/parse (:args c))) (when-not (faces face) (str "face is one of " (pr-str (sort faces)))))]
    (if error
      (contract/never :bad-args (str "blocks.use-on " error))
      (contract/ok :args "a block to click"))))

(def bucket-zones
  (checks/refusing :allowed "zones allow the bucket"
                   (fn [c]
                     (let [{:keys [pos error]} (b/parse (:args c))]
                       (when (and (not error) (bucket? (:item (:args c))))
                         (b/not-allowed pos (access/may? (b/rules-in c) (if (= "bucket" (:item (:args c))) :harvest :place) pos)))))))

(def can-start? (cond/and bad-args bucket-zones))
(def can-run? bucket-zones)

(defn ^:async attempt!
  "One step: walk into reach (:again after a failed walk, until the limit), else click and end."
  [c pos fails]
  (let [{:keys [item face see range] :or {face "up"}} (:args c)
        w (when-not (b/in-reach? c pos see) (await (walk! c pos see range)))]
    (cond
      (= :continue w) :continue
      (map? w) (if (>= (inc @fails) (max-walks))
                 (finish! c :nothing-possible {:used false :pos pos :item item :face face :reason :unreachable :why (:unreachable w)})
                 (do (swap! fails inc) :again))
      :else
      (let [[status r] (await (click! c {:pos pos :item item :face face}))]
        (finish! c (cond (= :used status) :done (condition-reasons status) :condition-failed :else :nothing-possible)
                 (cond-> {:used (= :used status) :pos pos :item item :face face :reason status}
                   (.-spawnSet r) (assoc :spawn-set true)))))))

(defn ^:async round [c]
  (let [{:keys [pos]} (b/parse (:args c))
        fails (atom 0)]
    (await (pace/steps! c #(attempt! c pos fails)))))
