(ns jobs.survival.respond.first-act
  "The body's act on the first call of a response, before any priced stage runs (Q202: the body reacts at once, the
  decision refines later): a swing when armed and a mob that is no creeper is within reach, else a step away from the
  nearest melee mob within close-radius over the cells the body senses."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.hostile-fight :as hostile]
            [jobs.lib.util :as u]
            [jobs.survival.respond.options :as options]))

(defn nearest-melee
  "The nearest seen mob of near that attacks at close range (no shooter, no enderman), nil when none."
  [p near]
  (->> near
       (remove #(or (combat/ranged? %) (combat/enderman? %) (not (.-visible %))))
       (sort-by #(danger-q/mob-distance p %))
       first))

(defn in-reach?
  "Whether mob e is within a swing's reach of the body's eyes."
  [c e]
  (let [me (u/self-pos c)]
    (<= (hostile/swing-gap (:x me) (+ (:y me) game/eye-height) (:z me) (danger-q/mob-pos (:primitives c) e)) hostile/swing-reach)))

(defn ^:async act!
  "Swing at (armed, no creeper, in reach) or step away from the nearest melee mob of near (within close-radius), once per
  mob (memory :first-acted under policy, which outlives the job: the call yields :continue and the reflex refires as a new
  job); nothing when none is near or no cell gains on it. true when the body acted."
  [c near policy]
  (let [p (:primitives c)
        mob (some-> (nearest-melee p near) (as-> m (when-not (= (.-id m) (:data (mem/latest (ctx/view c) :first-acted))) m)))]
    (when mob
      (ctx/remember! c :first-acted (.-id mob) policy)
      (let [weapon (combat/best-weapon p (combat/weapons c))
            at (danger-q/mob-pos p mob)
            cell (when-not (and weapon (not (combat/creeper? mob)) (in-reach? c mob)) (hostile/back-cell-of c mob))]
        (cond
          (and weapon (not (combat/creeper? mob)) (in-reach? c mob))
          (do (await (combat/equip-best! c weapon))
              (await (ctx/act c :look #js {:pos #js {:x (:x at) :y (+ 1 (:y at)) :z (:z at)}}))
              (await (ctx/act c :attack #js {:id (.-id mob)}))
              true)
          (and cell (<= (danger-q/mob-distance p mob) (options/close-radius)))
          (do (await (ctx/act c :moveTo (clj->js {:pos cell :range 0 :timeoutS (options/close-leg-s)})))
              true))))))
