(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.walk :as walk]
            [engine.places :as places]
            [engine.triggers.stuck :as stuck]))

(def doc
  "Walk to :pos, [x y z] or {:x :y :z} (read by places/parse-pos, so fractional values are floored to the cell), until
  the body's cell is within :range cells of it (u/within?, measured cell to cell: range 0 is standing in that cell, range 1
  the cell next to it). A :pos that is not one is refused in the first round, before any walk: a :refused warn event
  with :reason :bad-pos and :text, and the result {:arrived false :reason :bad-pos}. A body with no pathWorld sensing
  is refused the same way as :unsupported.

  One round is one plan and one walk (engine.path.walk: plan within the executor's abilities from where the body
  stands, follow the plan once, at most 60 s). A plan that only gets part of the way (the goal unloaded or far) is
  walked as far as its steps can be undone, and the next round plans on from there. A round that ends more than 1
  closer than any before (:best) is progress and resets the count; any other round that does not arrive (a partial
  walk without a new best, no path, stuck) counts, and three in a row give up with an unreachable warn (the last
  status and the planner's :why and :kind). Hands over {:arrived true}, or {:arrived false :reason :unreachable}
  with the planner's reason as :why (:no-path reasons, or :abilities) and, when the executor cannot walk the way
  there, the :kind of step that cannot (ctx/result!), and emits it as a :result info event.

  Every round that walks writes a :moved memory entry {:from :to :status :target} (status \"arrived\", \"partial\" when
  the body got more than 1 closer, else \"blocked\"), which the stuck trigger and jobs.maintenance.unstick read.

  :doors is accepted: only :never has an effect in this version (a shut door or gate is a wall); :shut, the planned
  default, and :leave-open come with the next version, and any value now behaves as :never.")

(def args
  {:pos {:doc "target position {:x :y :z}" :default nil}
   :range {:doc "how close counts as there, in cells" :default 1}
   :doors {:doc "what to do at shut doors and gates: :never (walls); :shut and :leave-open are not in effect yet"
           :default :never}})

(def max-blocked 3)

(def walk-timeout-s 60)

(defn check [_c] true)

(defn finish!
  "Hand result over (ctx/result!), emit it as a :result info event (its :kind is the event's :refused-kind: an event's own
  :kind is what it is), end the job."
  [c result]
  (ctx/result! c result)
  (ctx/emit! c :result :info (cond-> (dissoc result :kind)
                               (:kind result) (assoc :refused-kind (:kind result))))
  :done)

(defn arrived! [c] (finish! c {:arrived true}))

(defn give-up! [c pos tries status {:keys [reason kind]}]
  (ctx/emit! c :unreachable :warn (cond-> {:target pos :tries tries :status status
                                           :text (str "gave up walking to " pos)}
                                    reason (assoc :why reason)
                                    kind (assoc :refused-kind kind)))
  (finish! c (cond-> {:arrived false :reason :unreachable}
               reason (assoc :why reason)
               kind (assoc :kind kind))))

(defn refuse! [c {:keys [reason message]}]
  (ctx/emit! c :refused :warn {:reason reason :text message})
  (finish! c {:arrived false :reason reason}))

(defn ^:async walk-once!
  "Plan from where the body stands and follow the plan once: the walk's result map (engine.path.walk), or the no-path
  result of a plan that is not walked."
  [c pw to range]
  (await (walk/settle! c))
  (let [plan (walk/plan-walk c pw to range walk/default-weight)]
    (if-let [no (walk/no-walk plan 0)]
      no
      (let [[done _] (await (walk/walk! c (:steps plan) walk-timeout-s))]
        (walk/partial-end done (:status plan) to range (:steps plan) (:stop plan))))))

(defn move-status
  "The :moved entry's status of a walk that began d from the target and ended left from it."
  [arrived? d left]
  (cond
    arrived? "arrived"
    (< left (dec d)) "partial"
    :else "blocked"))

(defn ^:async walk! [c pos range]
  (let [from (u/self-pos c)
        d (u/dist from pos)
        pw (walk/path-world (:primitives c))]
    (cond
      (u/within? from pos range)
      (arrived! c)

      (nil? pw)
      (refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      :else
      (let [result (await (walk-once! c pw [(:x pos) (:y pos) (:z pos)] range))
            to (u/self-pos c)
            left (u/dist to pos)
            arrived? (u/within? to pos range)
            status (move-status arrived? d left)
            best (:best (ctx/mem c) d)]
        (ctx/remember! c :moved {:from from :to to :status status :target pos} stuck/moved-policy)
        (if arrived?
          (arrived! c)
          (let [progress? (< left (dec best))
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (ctx/update-mem! c assoc :blocked tries :best (if progress? left best))
            (if (< tries max-blocked)
              :continue
              (give-up! c pos tries status result))))))))

(defn ^:async round [c]
  (let [parsed (places/parse-pos (:pos (:args c)))]
    (if (:reason parsed)
      (refuse! c parsed)
      (await (walk! c (:pos parsed) (:range (:args c)))))))
