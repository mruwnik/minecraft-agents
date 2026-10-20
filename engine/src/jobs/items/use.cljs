(ns jobs.items.use
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
  "Right-click :on once, with :item in hand: a block (:on is a position, [x y z] or {:x :y :z}) or an entity (:on is its id,
  from observe entities). One call is the whole attempt. :continue only when the walk waits on the world.

  A block: walk into reach (one jobs.movement.go-to call, within :range, no escalation), click on :face with :item (nil: whatever is held stays
  in hand; none: the empty hand). A bucket :item (water_bucket pours, lava_bucket pours lava, bucket scoops) is used on the cell :on itself, through the
  place act: the liquid goes there, or is taken from there; :face is ignored. A bad call is never (:bad-args); a bucket changes the cell, so zones,
  claims and plans may refuse it (a pour asks :place, a scoop :harvest; {:reason :not-allowed :pos :by ...}, :ignore-zones? skips the rule); a plain
  click changes no block by itself and asks no zone. Ends with data {:used true|false :pos :item :face :reason :raw (the primitive's answer: before,
  after, consumed)}, plus :spawn-set true when a bed answered \"respawn point set\" (a bed is used by day to set the spawn; not in the Nether or End).
  :reason is the primitive's status (:used, :unchanged, :no-item, :no-room, :missing, :cannot, :occupied, :unreachable, :failed); only :used is done.
  condition-failed: :no-item, :missing, :cannot, :occupied (a bucket's cell is filled). nothing-possible: :unreachable (a walk that failed twice, :why),
  :unchanged, :no-room, :failed.

  An entity: find it in sight, walk into reach (a go-to child, range 2, no escalation; again when the entity moved since the walk arrived), click
  with :item (nil or none: the empty hand). An entity that drops out of sight (the pit rim, a wall) between the sighting and the click is walked to at
  its last seen place and clicked there (the click finds it by id); it is :gone when the click does not. Ends with data {:used true|false :id :item
  :text, and after a click :raw the primitive's answer as a map}. The primitive's status :used is done. Condition-failed: :gone (no such entity in
  sight), :no-item, :full, :cannot. Nothing-possible: :unreachable (the walk failed twice, data :why), :no-effect, :out-of-reach, :failed.")

(a/defargs args
  {:on {:doc "what to click: a block, [x y z] or {:x :y :z}, or an entity id (observe entities lists them)" :spec (a/or-of a/position? (a/int-in 1 nil)) :default nil}
   :item {:doc "the item to hold for the click; nil: a block keeps the hand as it is, an entity gets the empty hand; none: the empty hand" :spec a/item? :default nil}
   :see {:doc "block only. true: click only from where the eye sees the block (jobs.lib.aim), walking to such a stand first (go-to :see; none: :unreachable); false: within reach is enough, though a wall in between refuses the click" :spec boolean? :default false}
   :ignore-zones? :shared
   :range {:doc "block only: how many cells from :on the walk may stop (go-to :range); a click the server checks by distance, a bed's 3 blocks, wants less" :spec (a/num-in 0 nil) :default 3}
   :face {:doc "block only: the face clicked: up down north south east west" :spec #{"up" "down" "north" "south" "east" "west"} :default "up"}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "Entity: how near the body stands to act on its target, in blocks." :spec (a/num-in 0 nil)}
   ::max-walks {:default 2 :doc "Entity: failed walks of one call before it stops :unreachable." :spec (a/int-in 0 nil)}
   ::click-s {:default 0.5 :doc "Entity: seconds one click takes (the quote)." :spec (a/num-in 0 nil)}
   ::walk-s {:default 10 :doc "Entity: seconds one walk into reach takes (the quote)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-walks [] (settings/get settings ::max-walks))

(defn entity? [c] (number? (:on (:args c))))

(defn block-args
  "The args as the block jobs read them: :on becomes :pos."
  [c]
  (-> (:args c) (assoc :pos (:on (:args c))) (dissoc :on)))

(def target [:on :item])
(def defaults {:spend {:job-time ::job-time-s}})

(def safe-block (ready/safe-at-by-night (ready/near-unseen-here (fn [c] (a/cell (:on (:args c)))))))
(defn safe? [c] (if (entity? c) (ready/safe-here-by-night c) (safe-block c)))
(def fit? ready/ok)

(def faces #{"up" "down" "north" "south" "east" "west"})

(def condition-reasons
  "The block click statuses that name a missing target or item; any other failed status is nothing-possible."
  #{:no-item :missing :cannot :occupied})

(defn quote-of [c]
  (if (entity? c)
    (let [click (settings/get settings ::click-s)
          walk (settings/get settings ::walk-s)]
      {:uses {:job-time [click (+ click walk) (+ click (* walk (max-walks)))]}})
    (let [{:keys [pos error]} (b/parse (block-args c))
          walk (if (or error (b/in-reach? c pos (:see (:args c)))) 0 (u/eye-dist (u/self-pos c) pos))]
      {:uses {:job-time [0.5 (+ 1 (/ walk 4)) (+ 10 walk)]}})))

(defn bucket? [item] (and (string? item) (boolean (re-find #"bucket$" item))))

(defn bad-args [c]
  (let [{:keys [face] :or {face "up"}} (:args c)
        error (if (entity? c)
                (when-not (pos-int? (:on (:args c))) "needs a positive entity id as :on")
                (or (:error (b/parse (block-args c))) (when-not (faces face) (str "face is one of " (pr-str (sort faces))))))]
    (if error
      (contract/never :bad-args (str "items.use " error))
      (contract/ok :args "something to click"))))

(def bucket-zones
  (checks/refusing :allowed "zones allow the bucket"
                   (fn [c]
                     (let [{:keys [pos error]} (b/parse (block-args c))]
                       (when (and (not (entity? c)) (not error) (bucket? (:item (:args c))))
                         (b/not-allowed pos (access/may? (b/rules-in c) (if (= "bucket" (:item (:args c))) :harvest :place) pos)))))))

(def can-start? (cond/and bad-args bucket-zones))
(def can-run? bucket-zones)

(defn done?
  "Ok once the click was made."
  [c]
  (if (:used (ctx/mem c))
    (contract/ok :used "use: the click was made" :units :uses :done 1 :left 0 :result {:used true})
    (contract/later :not-used "use: the round ends the call" :units :uses :done 0 :left 1 :result {:used false})))

;; --- block

(defn finish-block!
  "End the call: done when used, else kind with the result's :reason (and :why); the data is the result with a :text."
  [c kind {:keys [used] :as result}]
  (let [result (update result :text #(or % (str (if used "clicked " "did not click ") (pr-str (b/cell (:pos result)))
                                                (when-not used (str ": " (name (:reason result)))))))]
    (ctx/end c kind (when-not used (select-keys result [:reason :why])) result)))

(defn ^:async click-block!
  "The click or bucket use at pos once in reach: the act's status keyword and its reply."
  [c {:keys [pos item face]}]
  (let [r (await (if (bucket? item)
                   (ctx/act c :place (clj->js {:pos pos :item item}))
                   (ctx/act c :useOn (clj->js (cond-> {:pos pos :face face} item (assoc :item item))))))
        status (keyword (.-status r))]
    [(if (and (bucket? item) (= :placed status)) :used status) r]))

(defn ^:async attempt-block!
  "One step: walk into reach (:again after a failed walk, until the limit), else click and end."
  [c pos]
  (let [{:keys [item face see range] :or {face "up"}} (:args c)
        w (when-not (b/in-reach? c pos see) (await (b/walk-into-reach! c pos see range)))]
    (cond
      (:unreachable w) (finish-block! c :nothing-possible {:used false :pos pos :item item :face face :reason :unreachable :why (:unreachable w)})
      w w
      :else
      (let [[status r] (await (click-block! c {:pos pos :item item :face face}))]
        (finish-block! c (cond (= :used status) :done (condition-reasons status) :condition-failed :else :nothing-possible)
                       (cond-> {:used (= :used status) :pos pos :item item :face face :reason status :raw r}
                         (.-spawnSet r) (assoc :spawn-set true)))))))

;; --- entity

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
      (let [r (await (child/call-walk* c :walk {:pos target :range 2 :ways #{:walk :clear-seal}}))]
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

(defn ^:async round [c]
  (if (entity? c)
    (let [fails (atom 0)]
      (await (pace/steps! c #(attempt-entity! c fails))))
    (let [{:keys [pos]} (b/parse (block-args c))]
      (await (pace/steps! c #(attempt-block! c pos))))))
