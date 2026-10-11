(ns jobs.survival.retreat-goal
  "The :goal of a flight (jobs.survival.retreat): the destination of the walk it interrupted, when the flight walks to
  it and when it gives it up."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-home :as retreat-home]
            [jobs.survival.retreat-walk :as walk]))

(a/defargs settings
  {::drift {:default 3 :doc "Blocks a walk to the goal may take the body farther from the goal's height than where that walk began: a leg that does ends the walk to the goal (a plan that ends under or over the goal, nearer only in x and z, is no way to it)." :spec (a/num-in 0 nil)}})
(defn drift [] (settings/get settings ::drift))

(def near
  "Blocks from the goal at which the flight no longer walks to it."
  3)

(def leg-margin
  "Blocks past body and goal, up and down, a leg to the goal may plan in the open."
  16)

(defn goal-pos
  "The :goal arg ({:x :y :z} or [x y z]) as a position map, nil when none."
  [goal]
  (when goal
    (let [[x y z] (if (vector? goal) goal [(:x goal) (:y goal) (:z goal)])]
      {:x x :y y :z z})))

(defn active
  "The :goal as a position, nil when none or the flight gave it up (:goal-off)."
  [c]
  (when-not (:goal-off (ctx/mem c)) (goal-pos (:goal (:args c)))))

(defn safe
  "The active goal unless a threat (positions) lies on the way to it (walk/on-the-way?), else nil."
  [c threat-pos]
  (let [goal (active c)]
    (when (and goal (not (walk/on-the-way? (u/self-pos c) threat-pos goal))) goal)))

(defn under-floor?
  "Whether pos lies below the flight's floor (flight/floor) under rock: the body is under ground, or a block it has
  seen roofs pos."
  [c pos]
  (let [p (:primitives c)
        cell (update-vals (select-keys pos [:x :y :z]) #(js/Math.floor %))]
    (boolean (and (< (:y pos) (flight/floor c))
                  (or (sh/buried? p) (walk/roofed? #(u/seen-name p %) cell))))))

(defn margin
  "The :y-margin of a leg to the goal: under ground no deeper than walk/max-descent past body and goal, so the plan
  cannot dive through the caves below."
  [c]
  (if (sh/buried? (:primitives c)) (walk/max-descent) leg-margin))

(defn off!
  "Give the goal up (:goal-off) and say why (:lost-ground or :height) in a retreat.goal-off info."
  [c why]
  (ctx/update-mem! c #(-> % (assoc :goal-off true) (dissoc :goal-y0)))
  (ctx/emit! c :retreat.goal-off :info
             {:why why
              :text (str (case why
                           :lost-ground "the walk to the goal lost ground on the hostiles or did not near the goal"
                           :height "the walk to the goal left the goal's height or went down under rock")
                         ": away from them instead")}))

(defn leg
  "The goal (when no threat is on the way to it) as a go-to target while the body outruns its chasers
  (cost/outruns-at? at the flank turn): the planner's danger costs route the leg round the hostiles. Once the body is
  within near of it the goal is done for the flight (:goal-off), as it is after a leg that lost ground: the flight
  goes on away from the chasers; without that it walked back and forth between the goal and the way away for ever.
  Never a goal under rock below the flight's floor (under-floor?)."
  [c threats]
  (let [goal (safe c (mapv #(danger-q/mob-pos (:primitives c) %) threats))]
    (cond
      (nil? goal) nil
      (<= (u/dist (u/self-pos c) goal) near) (do (ctx/update-mem! c assoc :goal-off true) nil)
      (under-floor? c goal) nil
      (cost/outruns-at? (retreat-home/food (:primitives c)) (walk/flank-turn)) goal)))

(defn start-leg!
  "Note the feet height the walk to the goal began at (:goal-y0), once a walk."
  [c]
  (ctx/update-mem! c update :goal-y0 #(or % (:y (u/self-pos c)))))

(defn drifted?
  "Whether the body, after a leg to goal, is more than drift farther from the goal's height than where the walk to it
  began (:goal-y0), or stands under rock below the flight's floor."
  [c goal]
  (let [self (u/self-pos c)
        y0 (:goal-y0 (ctx/mem c) (:y self))
        off (fn [y] (js/Math.abs (- (:y goal) y)))]
    (or (> (off (:y self)) (+ (off y0) (drift)))
        (under-floor? c self))))
