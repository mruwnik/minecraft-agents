(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [engine.path.walk :as walk]
            [engine.places :as places]))

(def doc
  "Walk to :pos, [x y z] or {:x :y :z} (read by places/parse-pos, so fractional values are floored to the cell), until
  the body's cell is within :range cells of it (u/within?, measured cell to cell: range 0 is standing in that cell, range 1
  the cell next to it). A :pos that is not one is refused in the first round, before any walk: a :refused warn event
  with :reason :bad-pos and :text, and the result {:arrived false :reason :bad-pos}. A body with no pathWorld sensing
  is refused the same way as :unsupported.

  One round is one plan and one walk (engine.path.walk: plan within the executor's abilities from where the body
  stands, follow the plan once, at most 60 s). A plan that only gets part of the way (the goal unloaded or far) is
  walked as far as its steps can be undone, or past a step that cannot be undone (a drop of 2 or 3, a gap jump down) when
  the land past it runs on into unloaded land (engine.path.near; a pit whose cells are all loaded is never entered), and
  the next round plans on from there. A round that ends more than 1
  closer than any before (:best) is progress and resets the count; any other round that does not arrive (a partial
  walk without a new best, no path, stuck) counts, and three in a row give up with an unreachable warn (the last
  status and :why and :kind). Hands over {:arrived true}, or {:arrived false :reason :unreachable}
  with :why saying why (the planner's :no-path reason, or :abilities, or :no-path when it gave none; :stuck when the walk made
  no progress on a step, with the step's move as :kind and the executor's text as :detail; :off-plan; :steer-failed with
  the steer's reason as :detail; :no-progress when a walk ended no nearer) and, when the executor cannot walk the way there,
  the :kind of step that cannot (ctx/result!); :door-stuck adds the :cells that would not open, :one-way the :near and :one-way
  step the plan was cut at. It is emitted as a :result info event.

  Every round that walks writes a :moved memory entry {:from :to :status :target} (status \"arrived\", \"partial\" when
  the body got more than 1 closer, else \"blocked\"), which the stuck trigger and jobs.maintenance.unstick read.

  :doors says what to do at shut doors, gates and trapdoors (engine.path.pass). :shut, the default: the plan may go through
  them; the walk opens one with an empty hand, passes, and shuts it again if it was the walker that opened it (one that was
  open already is left open). :leave-open: the same but the walker leaves what it opened open (for leading an animal through);
  a gate or door in or next to a zone of another owner is shut whatever :doors says, and one that cannot be shut (an animal
  in its cell after a short wait, a click that does nothing) stays open with a :door-left-open warn. :never: a shut door or
  gate is a wall. Every block the walker opened has an :opened memory entry until it is shut; a round that was cut between
  the open and the shut shuts it at its next round. A block that will not open (a click that moves nothing) is a wall for
  the rest of that walk: the plan is made once more, and when no way is left the round's result is :no-path with the reason
  :door-stuck (so go-to gives up with :why :door-stuck after three such rounds). Iron doors and iron trapdoors, which a hand
  does not open, are walls.")

(def args
  {:pos {:doc "target position {:x :y :z}" :default nil}
   :range {:doc "how close counts as there, in cells" :default 1}
   :doors {:doc "what to do at shut doors, gates and trapdoors: :shut (open, pass, shut again what the walk opened), :leave-open (open and pass), :never (walls)"
           :default :shut}})

(def max-blocked 3)

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

(defn give-up-fields
  "What a fruitless round's result says about why: {:why :kind :detail ...}, only the keys it has. A :no-path says the planner's
  :reason (:why, :no-path when it has none), the :kind of step that cannot be walked, and what its reason carries (:door-stuck's
  :cells, :one-way's :near and :one-way); a walk that got stuck says :stuck, the :move it was stuck on as :kind and the
  executor's :why text as :detail; one that left its plan says :off-plan; a failed steer says :steer-failed with its reason
  as :detail; a walk that ended with the body no nearer says :no-progress."
  [{:keys [status reason kind move step] :as result}]
  (case status
    :stuck (cond-> {:why :stuck}
             move (assoc :kind move)
             (:why result) (assoc :detail (:why result)))
    :off-plan (cond-> {:why :off-plan}
                step (assoc :detail (str "left the plan at step " step)))
    :failed (cond-> {:why :steer-failed}
              reason (assoc :detail (str reason)))
    (cond
      (or reason (= :no-path status)) (merge {:why (if reason (keyword reason) :no-path)}
                                             (select-keys result [:kind :cells :near :one-way]))
      :else {:why :no-progress})))

(defn give-up! [c pos tries status result]
  (let [{:keys [why kind] :as fields} (give-up-fields result)
        extra (dissoc fields :why :kind)]
    (ctx/emit! c :unreachable :warn (cond-> (merge {:target pos :tries tries :status status :why why
                                                    :text (str "gave up walking to " pos)}
                                                   extra)
                                      kind (assoc :refused-kind kind)))
    (finish! c (merge {:arrived false :reason :unreachable} fields))))

(defn refuse! [c {:keys [reason message]}]
  (ctx/emit! c :refused :warn {:reason reason :text message})
  (finish! c {:arrived false :reason reason}))

(defn ^:async walk! [c pos range doors]
  (let [from (u/self-pos c)
        d (u/dist from pos)
        pw (walk/path-world (:primitives c))]
    (cond
      (u/within? from pos range)
      (arrived! c)

      (nil? pw)
      (refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      :else
      (let [{:keys [result status to]} (await (near/walk-round! c pw pos range doors))
            left (u/dist to pos)
            best (:best (ctx/mem c) d)]
        (if (= "arrived" status)
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
      (await (walk! c (:pos parsed) (:range (:args c)) (some-> (:doors (:args c)) keyword))))))
