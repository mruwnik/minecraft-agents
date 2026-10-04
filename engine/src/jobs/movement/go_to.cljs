(ns jobs.movement.go-to
  (:require [engine.access.click :as click]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.executor :as executor]
            [engine.path.pass :as pass]
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
  status and :why and :kind). Hands over {:arrived true}, or {:arrived false :reason :unreachable}
  with :why saying why (the planner's :no-path reason, or :abilities; :stuck when the walk made no progress on a step, with the
  step's move as :kind and the executor's text as :detail; :off-plan; :no-progress when a walk ended no nearer) and, when the
  executor cannot walk the way there, the :kind of step that cannot (ctx/result!), and emits it as a :result info event.

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

(defn give-up-fields
  "What a fruitless round's result says about why: {:why :kind :detail}, only the keys it has. A :no-path says the planner's
  :reason (:why) and the :kind of step that cannot be walked; a walk that got stuck says :stuck, the :move it was stuck on
  as :kind and the executor's :why text as :detail; one that left its plan says :off-plan; a failed steer says its reason;
  a walk that ended with the body no nearer says :no-progress."
  [{:keys [status reason kind move step] :as result}]
  (case status
    :stuck (cond-> {:why :stuck}
             move (assoc :kind move)
             (:why result) (assoc :detail (:why result)))
    :off-plan (cond-> {:why :off-plan}
                step (assoc :detail (str "left the plan at step " step)))
    (cond
      reason (cond-> {:why (keyword reason)}
               kind (assoc :kind kind))
      :else {:why :no-progress})))

(defn give-up! [c pos tries status result]
  (let [{:keys [why kind detail]} (give-up-fields result)]
    (ctx/emit! c :unreachable :warn (cond-> {:target pos :tries tries :status status :why why
                                             :text (str "gave up walking to " pos)}
                                      kind (assoc :refused-kind kind)
                                      detail (assoc :detail detail)))
    (finish! c (cond-> {:arrived false :reason :unreachable :why why}
                 kind (assoc :kind kind)
                 detail (assoc :detail detail)))))

(defn refuse! [c {:keys [reason message]}]
  (ctx/emit! c :refused :warn {:reason reason :text message})
  (finish! c {:arrived false :reason reason}))

(defn iron-cells
  "The cells {:x :y :z} among the blocks the steps open that a hand cannot open: iron doors and iron trapdoors."
  [c steps]
  (vec (distinct (for [s steps o (:opens s)
                       :let [cell (select-keys o [:x :y :z])]
                       :when (= :iron (click/kind-of (some-> (pass/block-at c cell) .-name)))]
                   cell))))

(defn door-stuck [cells] {:status :no-path :reason :door-stuck :cells cells})

(defn ^:async walk-once!
  "Plan from where the body stands and follow the plan once: the walk's result map (engine.path.walk), or the no-path
  result of a plan that is not walked. With doors other than :never, a plan may open blocks (iron ones are walls); one that
  will not open is a wall for one more plan, then the result is :no-path :door-stuck."
  [c pw to range doors]
  (await (walk/settle! c))
  (let [policy (if (= :never doors) executor/policy executor/door-policy)]
    (when-not (= :never doors) (await (pass/shut-leftovers! c doors)))
    (loop [walls [] stuck nil]
      (let [plan (walk/plan-walk c pw to range walk/default-weight {:policy policy :walls walls})
            iron (when-not (= :never doors) (iron-cells c (:steps plan)))]
        (if (seq iron)
          (recur (into walls iron) stuck)
          (if-let [no (walk/no-walk plan 0 policy)]
            (if stuck (door-stuck stuck) no)
            (let [[done _] (await (if (= :never doors)
                                    (walk/walk! c (:steps plan) walk-timeout-s)
                                    (pass/walk! c (:steps plan) {:timeout-s walk-timeout-s :doors doors})))]
              (cond
                (not= :door-stuck (:status done)) (walk/partial-end done (:status plan) to range (:steps plan) (:stop plan))
                stuck (door-stuck (:cells done))
                :else (recur (into walls (:cells done)) (:cells done))))))))))

(defn move-status
  "The :moved entry's status of a walk that began d from the target and ended left from it."
  [arrived? d left]
  (cond
    arrived? "arrived"
    (< left (dec d)) "partial"
    :else "blocked"))

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
      (let [result (await (walk-once! c pw [(:x pos) (:y pos) (:z pos)] range doors))
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
      (await (walk! c (:pos parsed) (:range (:args c)) (some-> (:doors (:args c)) keyword))))))
