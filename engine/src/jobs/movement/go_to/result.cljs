(ns jobs.movement.go-to.result
  "How a go-to call ends: its ending, a give-up with the planner's reason in words, a refusal."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.movement.go-to.headroom :as headroom]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.solid :as solid]
            [jobs.lib.spent :as spent]
            [jobs.lib.util :as u]))

(defn dark-end?
  "True when a fruitless round with a :dark-best is given up as :unseen-floor-dark (go-to, its quote): :no-path for no reason,
  :exhausted, :goal-unloaded or :start-enclosed, or :off-plan with a partial; never when the search was damage-refused or
  the round failed for a reason of its own (stuck, health, air, ...)."
  [{:keys [status reason partial damage-refused dark-best]}]
  (boolean
    (and dark-best (not damage-refused)
         (or (and (= :no-path status) (contains? #{nil :exhausted :goal-unloaded :start-enclosed} (some-> reason keyword)))
             (and (= :off-plan status) partial)))))

(def why-kinds
  "The ending kind of a give-up by its :why, when not :nothing-possible: a fact about the goal, or a line of the call."
  {:goal-dangerous :condition-failed :goal-unloaded :condition-failed
   :needs-health :limit-reached :leg-search-spent :limit-reached})

(def why-lines "The limit line of a limit-reached give-up." {:needs-health :health :leg-search-spent :job-time})

(def refusal-kinds
  "The ending kind of a refusal other than :condition-failed: args or a body that can never do it are impossible."
  {:no-way :nothing-possible :bad-args :impossible :bad-pos :impossible :bad-name :impossible :unsupported :impossible})

(defn ending
  "The ending of result, the call's result map: arrived is :done; a leg walked (:leg) is limit-reached {:reason :leg
  :line :job-time}; a give-up (:reason :unreachable) is
  of the kind its :why maps to (why-kinds, else :nothing-possible) with the cause {:reason :unreachable :why ..} (and
  :line for a limit); a refusal is :condition-failed (refusal-kinds aside) with its reason as the cause. The result
  less :status and :reason is the ending's data, so an old caller (child-result) reads what it read before."
  [c {:keys [status reason why] :as result}]
  (let [data (dissoc result :status :reason)]
    (cond
      (:leg result) (ctx/end c :limit-reached {:reason :leg :line :job-time} data)
      (not= :stopped status) (ctx/end c :done nil data)
      (not= :unreachable reason) (ctx/end c (refusal-kinds reason :condition-failed) reason data)
      :else (ctx/end c (why-kinds why :nothing-possible)
                     (cond-> {:reason :unreachable :why why} (why-lines why) (assoc :line (why-lines why)))
                     data))))

(defn finish!
  "Emit result as a :result info event (its :kind is the event's :refused-kind: an event's own :kind is what it is) and
  end the call with its ending. What the walk placed (torches) is added as :spent and :spent-at."
  [c result]
  (let [result (merge result (some-> (spent/log c) spent/summary))]
    (ctx/emit! c :result :info (cond-> (dissoc result :kind)
                                 (:kind result) (assoc :refused-kind (:kind result))))
    (ending c result)))

(defn feet-cell [c]
  (let [{:keys [x y z]} (reach/standing-cell (:primitives c))]
    [x y z]))

(defn give-up-fields
  "What a fruitless round's result says about why: {:why :kind :detail ...}, only the keys it has. A :no-path says the planner's
  :reason (:why, :no-path when it has none), the :kind of step that cannot be walked, and what its reason carries (:door-stuck's
  :cells, :one-way's :near and :one-way); a walk that got stuck says :stuck, the :move it was stuck on as :kind and the
  executor's :why text as :detail; one that left its plan says :off-plan; a failed steer says :steer-failed with its reason
  as :detail; a walk that ended with the body no nearer says :no-progress. A body sealed in its own dig-in shelter adds
  :inside-own-shelter (its cell) and a :hint (the shelter job lets it out by day, or run dig-in leave). A failed waypoint
  walk of the call (go-to's mem :waypoint-why) rides along as :waypoint-why."
  [{:keys [status reason kind move step] :as result}]
  (merge
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
                                             (select-keys result [:kind :cells :near :one-way :dark-best :torches :heal :health :hp-cost :hp-budget :dip :rise]))
      :else {:why :no-progress}))
   (select-keys result [:waypoint-why])))

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
   :own-build "the only way out cuts a plan this body made (pass :spare-own-builds false or ask)"
   :own-pen "the only way out cuts the wall of a pen of its own with animals in it (pass :spare-own-builds false or ask)"
   :zone "a stair or dig is refused by another's zone, claim or plan (pass :ignore-zones? or ask)"})

(defn escalation-words
  "A failed way out of a shut-in body, or :into a walled-in goal, in words: {:step :reason|:why :n :into}; a child's
  wait (:wait) names its own reason."
  [{:keys [step reason why n wait into]}]
  (let [r (some-> (or (:reason wait) reason why) (as-> k (or (escalation-reason-words k) (str/replace (name k) "-" " "))))]
    (case step
      :spent (str "gave up after " n " ways " (if into "in" "out"))
      :none (str "no way " (if into "in" "out") (some->> r (str ": ")))
      (str "could not " (or (step-words step) (str "make a way " (if into "in" "out"))) (some->> r (str ": "))))))

(defn hostile-place-words
  "Where a hostile is, in words: \" at [x y z]\" for a cell, \" heard near, south\" for {:direction :band}, else nil."
  [at]
  (cond (vector? at) (str " at " (pr-str at))
        (:band at) (str " heard " (name (:band at)) ", " (name (:direction at)))))

(defn give-up-words
  "The give-up reason in words, for the :text of the :unreachable event and the result: no map dumps."
  [pos {:keys [why kind detail escalation shut-in nearest-hostile nearest-hostile-at resets-spent mob mob-distance explored walks torches waypoint-why heal hp-cost hp-budget low-passage dip rise route straight]}]
  (str "gave up walking to " (if (map? pos) (let [{:keys [x y z]} pos] [x y z]) pos) ": "
       (if (and escalation (not (#{:way-dives :detour} why)))
         (str (if (:into escalation) "the goal is walled in; " "shut in here; ") (escalation-words escalation))
         (case why
           :abilities (str (if shut-in "the body is shut in and " "it ") (or (kind-words kind) (str "needs a " (some-> kind name) " move the body cannot make")))
           :goal-enclosed (if low-passage (headroom/words low-passage) "the goal is walled in with no way through")
           :goal-cut-off "the goal is cut off by a drop: no walkable way leads to it"
           :goal-on-magma "every cell in range is on magma or in a magma bubble column, where a route never ends"
           :start-enclosed "the body is shut in and nothing it can walk reaches out"
           :exhausted (if shut-in
                        "the body is shut in and this walk may not dig or build out"
                        "no walkable way leads there from here")
           :out-of-sight "no stand within range that sees the block (:see) is reachable from here"
           :goal-unloaded "the goal lies in land that is not loaded and the loaded land leads no nearer"
           :no-progress "the walk ended no nearer"
           :explore-spent (str "walked " explored " blocks in " walks " walks to the edge of loaded land without coming nearer: more than the goal is worth (:tour-worth)")
           :stuck (str "the body got stuck" (some->> kind name (str " on ")) (some->> detail (str ": ")))
           :off-plan (or detail "the walk left its plan")
           :steer-failed (str "steering failed" (some->> detail (str ": ")))
           :cut-again (str "the walk was cut again and again, each time with the body sent back, and got no nearer" (when resets-spent " (its cut-count resets were spent)") (when nearest-hostile (str " (nearest hostile: " (str/replace (str nearest-hostile) "_" " ") (hostile-place-words nearest-hostile-at) ")")))
           :goal-dangerous (str "the goal is in danger: " (str/replace (str mob) "_" " ") " " mob-distance " blocks from it, a mob the body would flee")
           :moved-while-searching "the body was pushed about while the path was searched"
           :needs-health (str "a way exists but costs " hp-cost " hp, more than the " hp-budget " hp the body may spend"
                           (if (= :full heal) " at full health (healing cannot help; only a :floor {:health 1}, or a lower :floor :health, lets it go)" ", and it cannot heal first"))
           :air "every way there under water runs out of air"
           :air-lethal "every way there under water would drown the body to death"
           :one-way "the way back is one-way"
           :way-dives (str "the goal is " rise " blocks above and the only way found dives more than " dip " blocks below where the walk began under ground: "
                           (if escalation
                             (str "its own stair up failed (" (escalation-words escalation) "); cut one from another spot")
                             "cut a stair up")
                           ", or pass :y-margin to walk the deep way")
           :detour (str "the only way found is " route " blocks long for a goal that was " straight " blocks away, mostly over cells the body has never seen under ground: dig toward the goal, or pass :detour false to walk it")
           :door-stuck "a door in the way will not open"
           :unseen-floor-dark (str "the way goes down onto floors it cannot see in the dark: carry a torch (held in the off hand it sees deeper), light the way, or set :place-torches (go-to lights them)"
                                   (case torches
                                     :none "; it has no torch to light them and fetches none"
                                     :fetch-failed "; it has no torch to light them and could not fetch any"
                                     ""))
           (str "no path (" (some-> why name) ")")))
       (when waypoint-why (str "; a waypoint walk toward it failed: " (name waypoint-why)))))

(defn warn-level [c] (if (false? (:warn (:args c))) :info :warn))

(defn give-up!
  ([c pos tries status result] (give-up! c pos tries status result nil))
  ([c pos tries status result extra]
   (let [{:keys [why kind] :as fields} (merge {:at (feet-cell c) :near (js/Math.round (u/dist (u/self-pos c) pos))}
                                              (give-up-fields result) (sh-inside/shelter-hint c) extra)
         fields (cond-> fields
                  (= :goal-enclosed why) (merge (some->> (headroom/low-passage (:primitives c) (solid/cell (if (map? pos) pos (zipmap [:x :y :z] pos))))
                                                          (hash-map :low-passage))))
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

(defn known-frontier-result
  "A round's walk result as go-to judges it: a walk to a frontier in land earlier searches knew to their end
  (:frontier-known: there was no other edge) explores nothing, so it is the search's own answer, :exhausted, and never
  progress, unless it brings an edge seen earlier and now out of view nearer (:frontier-target)."
  [result]
  (if (:frontier-known result)
    (cond-> {:status :no-path :reason :exhausted :frontier-known true}
      (:frontier-target result) (assoc :frontier-target (:frontier-target result))
      (:damage-refused result) (assoc :damage-refused true))
    result))

(defn unloaded-end
  "A fruitless round's result as go-to gives it up when the goal cell reads unloaded: a search that ran out of loaded land
  (:exhausted) or a partial plan walked to its end (:off-plan :partial) says :goal-unloaded, the far side was never seen."
  [result unloaded?]
  (if (and unloaded? (or (= :exhausted (:reason result)) (and (= :off-plan (:status result)) (:partial result))))
    {:status :no-path :reason :goal-unloaded}
    result))
