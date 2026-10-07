(ns jobs.movement.go-to.result
  "How a go-to call ends: hand the result over, give up with the planner's reason in words, refuse."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(defn finish!
  "Hand result over (ctx/result!), emit it as a :result info event (its :kind is the event's :refused-kind: an event's own
  :kind is what it is), end the job."
  [c result]
  (ctx/result! c result)
  (ctx/emit! c :result :info (cond-> (dissoc result :kind)
                               (:kind result) (assoc :refused-kind (:kind result))))
  :done)

(defn feet-cell [c]
  (let [{:keys [x y z]} (reach/standing-cell (:primitives c))]
    [x y z]))

(defn give-up-fields
  "What a fruitless round's result says about why: {:why :kind :detail ...}, only the keys it has. A :no-path says the planner's
  :reason (:why, :no-path when it has none), the :kind of step that cannot be walked, and what its reason carries (:door-stuck's
  :cells, :one-way's :near and :one-way); a walk that got stuck says :stuck, the :move it was stuck on as :kind and the
  executor's :why text as :detail; one that left its plan says :off-plan; a failed steer says :steer-failed with its reason
  as :detail; a walk that ended with the body no nearer says :no-progress. A body sealed in its own dig-in shelter adds
  :inside-own-shelter (its cell) and a :hint (the shelter job lets it out by day, or run dig-in leave)."
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

(def kind-words
  {:corner-jump "needs a sprint jump past a corner and food is too low"
   :gap-up "needs a step up the body cannot make"
   :gap-down "needs a drop the body cannot survive"
   :gap "needs a jump across a gap the body cannot make"
   :gap-sprint "needs a gap jump that takes a sprint and food is too low"
   :gap-width "needs a jump across a gap wider than the body can jump"})

(def step-words {:stair "dig a stair out" :pillar "pillar out" :clear-path "dig through the wall"})

(def escalation-reason-words
  {:no-tool "no pickaxe" :no-dig "nothing it may dig" :no-headroom "no room above the head"
   :zone "a stair or dig is refused by another's zone, claim or plan (pass :ignore-zones? or ask)"})

(defn escalation-words
  "A failed way out of a shut-in body in words: {:step :reason|:why :n}."
  [{:keys [step reason why n]}]
  (let [r (some-> (or reason why) (as-> k (or (escalation-reason-words k) (str/replace (name k) "-" " "))))]
    (case step
      :spent (str "gave up after " n " ways out")
      :none (str "no way out" (some->> r (str ": ")))
      (str "could not " (or (step-words step) "make a way out") (some->> r (str ": "))))))

(defn give-up-words
  "The give-up reason in words, for the :text of the :unreachable event and the result: no map dumps."
  [pos {:keys [why kind detail escalation nearest-hostile mob mob-distance]}]
  (str "gave up walking to " (if (map? pos) (let [{:keys [x y z]} pos] [x y z]) pos) ": "
       (if escalation
         (str "shut in here; " (escalation-words escalation))
         (case why
           :abilities (str "it " (or (kind-words kind) (str "needs a " (some-> kind name) " move the body cannot make")))
           :goal-enclosed "the goal is walled in with no way through"
           :goal-cut-off "the goal is cut off by a drop: no walkable way leads to it"
           :start-enclosed "the body is shut in and nothing it can walk reaches out"
           :exhausted "no walkable way leads there from here"
           :goal-unloaded "the goal lies in land that is not loaded and the loaded land leads no nearer"
           :no-progress "the walk ended no nearer"
           :stuck (str "the body got stuck" (some->> kind name (str " on ")) (some->> detail (str ": ")))
           :off-plan (or detail "the walk left its plan")
           :steer-failed (str "steering failed" (some->> detail (str ": ")))
           :cut-again (str "the walk was cut again and again, each time with the body sent back, and got no nearer" (when nearest-hostile (str " (nearest hostile: " (str/replace (str nearest-hostile) "_" " ") ")")))
           :goal-dangerous (str "the goal is in danger: " (str/replace (str mob) "_" " ") " " mob-distance " blocks from it, a mob the body would flee")
           :moved-while-searching "the body was pushed about while the path was searched"
           :needs-health "every way costs more hp than the body may spend and it cannot heal first"
           :one-way "the way back is one-way"
           :door-stuck "a door in the way will not open"
           (str "no path (" (some-> why name) ")")))))

(defn warn-level [c] (if (false? (:warn (:args c))) :info :warn))

(defn give-up!
  ([c pos tries status result] (give-up! c pos tries status result nil))
  ([c pos tries status result extra]
   (let [{:keys [why kind] :as fields} (merge {:at (feet-cell c) :near (js/Math.round (u/dist (u/self-pos c) pos))}
                                              (give-up-fields result) (sh/shelter-hint c) extra)
         more (dissoc fields :why :kind)
         text (give-up-words pos fields)]
     (ctx/emit! c :unreachable (warn-level c) (cond-> (merge {:target pos :tries tries :status status :why why
                                                     :text text}
                                                    more)
                                       kind (assoc :refused-kind kind)))
     (finish! c (merge {:status :stopped :arrived false :reason :unreachable :text text} fields)))))

(defn refuse! [c {:keys [reason message]}]
  (ctx/emit! c :refused (warn-level c) {:reason reason :text message})
  (finish! c {:status :stopped :arrived false :reason reason :text message}))
