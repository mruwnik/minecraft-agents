(ns jobs.survival.fight-back
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]))

(def doc
  "Equip the best weapon, step up to the nearest hostile within :range and hit
  it, at most one swing per :attack-gap-ms. Done when no hostile is within
  :range; declines when health is below :min-health.")

(def args
  {:range {:doc "hostiles within this many blocks are fought" :default 4}
   :min-health {:doc "decline below this health" :default 8}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}
   :attack-gap-ms {:doc "least time between swings" :default 600}})

(def reach 3)

(defn check [c]
  (let [{:keys [range min-health]} (:args c)
        p (:primitives c)]
    (and (>= (.-health (.self p)) min-health)
         (boolean (seq (combat/hostiles p range))))))

(defn ^:async equip-best! [c weapon]
  (when (and weapon (not= weapon (.-held (.self (:primitives c)))))
    (await (ctx/act c :equip #js {:item weapon :dest "hand"}))))

(defn ^:async swing!
  "Walk into reach if needed, then face and hit target once."
  [c target]
  (let [tpos (u/pos-of (.-pos target))
        r (if (> (u/dist (u/self-pos c) tpos) reach)
            (await (u/walk-near! c tpos 2))
            :there)]
    (when (= :there r)
      (ctx/update-mem! c assoc :last-attack (ctx/now c))
      (await (ctx/act c :look #js {:pos #js {:x (:x tpos) :y (+ 1 (:y tpos)) :z (:z tpos)}}))
      (await (ctx/act c :attack #js {:id (.-id target)})))))

(defn ^:async round [c]
  (let [{:keys [range weapons attack-gap-ms]} (:args c)
        p (:primitives c)
        target (first (combat/hostiles p range))
        last-attack (:last-attack (ctx/mem c))]
    (cond
      (nil? target) :done
      (and last-attack (< (- (ctx/now c) last-attack) attack-gap-ms)) :continue
      :else
      (do (await (equip-best! c (combat/best-weapon p weapons)))
          (await (swing! c target))
          (if (empty? (combat/hostiles p range)) :done :continue)))))
