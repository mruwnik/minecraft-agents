(ns jobs.blocks.use-on
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]))

(def doc
  "Right-click the block at :pos ([x y z] or {:x :y :z}) once, with :item in hand (nil: whatever is held stays in hand; none: the empty hand), on :face.
  One call is the whole attempt: walk into reach (one jobs.movement.go-to call, range 3, no escalation), click. It has
  no conditions. It yields :continue only when the walk waits on the world. Zones are not consulted: a click
  changes no block by itself.

  Ends with info blocks.use-on.done and {:used true|false :pos :item :face :reason}, plus :spawn-set true when a bed answered \"respawn point set\" (a bed is used by day to set the spawn; not in the Nether or End). :reason is the primitive's
  status (:used, :unchanged, :no-item, :no-room, :missing, :cannot, :unreachable, :failed); only :used is done.
  condition-failed: :no-item, :missing, :cannot, a walk that would leave a night shelter (:night-sheltered, :at-night :go walks
  out). Impossible: a bad :pos or :face (:bad-args, with a blocks.use-on.declined warn). nothing-possible: :unreachable (a walk
  that failed twice, :why), :unchanged, :no-room, :failed.")

(a/defargs args
  {:pos {:doc "the block to click, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :item {:doc "the item to hold for the click; nil: keep the hand as it is; none: the empty hand" :spec a/item? :default nil}
   :see {:doc "true: click only from where the eye sees the block (jobs.lib.aim), walking to such a stand first (go-to :see; none: :unreachable); false: within reach is enough, though a wall in between refuses the click" :spec boolean? :default false}
   :at-night {:doc "go-to's :at-night for the walk: wait (default) stops the call :night-sheltered when the walk would leave a night shelter; go walks out" :spec #{"wait" "go" :wait :go} :default :wait}
   :enter-pens {:doc "true: the walk may go through the gate of a pen holding animals; false: it keeps out of those gates and, with no other way, declines (a :blocks.pen-kept-out warn names the pen)" :spec boolean? :default false}
   :face {:doc "the face clicked: up down north south east west" :spec #{"up" "down" "north" "south" "east" "west"} :default "up"}})

(def target [:pos])
(def defaults {:spend {:time ::time-s}})

(def conditions [])

(def faces #{"up" "down" "north" "south" "east" "west"})
(a/defargs settings
  {::time-s {:default 120 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-walks {:default 2 :doc "Failed walks of one call before it ends :unreachable." :spec (a/int-in 0 nil)}})
(defn max-walks [] (settings/get settings ::max-walks))

(def condition-reasons
  "The click statuses that name a missing target or item; any other failed status is nothing-possible."
  #{:no-item :missing :cannot})

(defn quote-of [c]
  (let [{:keys [pos error]} (b/parse (:args c))
        walk (if (or error (b/in-reach? c pos (:see (:args c)))) 0 (u/eye-dist (u/self-pos c) pos))]
    {:uses {:time [0.5 (+ 1 (/ walk 4)) (+ 10 walk)]}}))

(defn finish!
  "End the call with the info event and an ending: done when used, else kind with the result's :reason (and :why)."
  [c kind {:keys [used] :as result}]
  (ctx/emit! c :blocks.use-on.done :info
             (update result :text #(or % (str (if used "clicked " "did not click ") (pr-str (b/cell (:pos result)))
                                              (when-not used (str ": " (name (:reason result))))))))
  (ctx/end c kind (when-not used (select-keys result [:reason :why])) result))

(defn ^:async walk!
  "The walk to pos: :arrived, :continue, {:held reason} (go-to waits on a night shelter) or {:unreachable why}."
  [c pos see? at-night]
  (let [[pens args] (b/walk-request c pos see? at-night)
        [r held] (await (child/call-walk-reason* c :walk args))
        arrived? (and (= :done (:kind r)) (:arrived (:data r)))]
    (cond
      (= :continue r) (if held {:held held} :continue)
      (and arrived? (b/in-reach? c pos see?)) :arrived
      :else (do (b/warn-kept-out! c pens)
                {:unreachable (cond (not= :done (:kind r)) :declined
                                    arrived? :out-of-reach
                                    :else (get-in r [:data :why] :unreachable))}))))

(defn ^:async round [c]
  (let [{:keys [item face see at-night] :or {face "up"}} (:args c)
        {:keys [pos error]} (b/parse (:args c))
        error (or error (when-not (faces face) (str "face is one of " (pr-str (sort faces)))))]
    (if error
      (do (ctx/emit! c :blocks.use-on.declined :warn {:reason :bad-args :text (str "blocks.use-on " error)})
          (ctx/end c :impossible {:reason :bad-args :text error} {:used false :reason :bad-args :text error}))
      (loop [fails 0]
        (let [w (when-not (b/in-reach? c pos see) (await (walk! c pos see at-night)))]
          (cond
            (= :continue w) :continue
            (:held w) (finish! c :condition-failed {:used false :pos pos :item item :face face :reason (:held w)
                                                    :text (str "held: " (name (:held w)) "; pass :at-night :go to walk out anyway")})
            (map? w) (if (>= (inc fails) (max-walks))
                       (finish! c :nothing-possible {:used false :pos pos :item item :face face :reason :unreachable :why (:unreachable w)})
                       (recur (inc fails)))
            :else
            (let [r (await (ctx/act c :useOn (clj->js (cond-> {:pos pos :face face} item (assoc :item item)))))
                  status (keyword (.-status r))]
              (finish! c (cond (= :used status) :done (condition-reasons status) :condition-failed :else :nothing-possible)
                       (cond-> {:used (= :used status) :pos pos :item item :face face :reason status}
                         (.-spawnSet r) (assoc :spawn-set true))))))))))
