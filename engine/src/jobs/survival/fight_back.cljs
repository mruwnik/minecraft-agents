(ns jobs.survival.fight-back
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]))

(def doc
  "Equip the best weapon, step up to the nearest hostile within :range and hit
  it, at most one swing per :attack-gap-ms. Done when no hostile is within
  :range; declines when health is below :min-health. A hostile the walk
  towards is blocked for three times is given up on (a warn of kind
  fight_unreachable) and no longer counts; with no other hostile in range the
  job declines, so respond-to-hostile retreats instead.")

(def args
  {:range {:doc "hostiles within this many blocks are fought" :default 4}
   :ranged-range {:doc "ranged hostiles (skeletons and the like) within this many blocks are fought" :default 16}
   :min-health {:doc "decline below this health" :default 8}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}
   :attack-gap-ms {:doc "least time between swings" :default 600}})

(def reach 3)

(defn given-up?
  "Whether the walk towards hostile e has been blocked u/max-failures times."
  [c e]
  (>= (get-in (ctx/mem c) [:blocked (.-id e)] 0) u/max-failures))

(defn in-range
  "The hostiles within :range (ranged ones within :ranged-range): the visible
  ones nearest first, then the hidden ones."
  [c]
  (let [{:keys [range ranged-range]} (:args c)]
    (combat/hostiles (:primitives c) range {:ranged-radius (max range ranged-range) :sight :prefer})))

(defn targets
  "The hostiles in range not given up on, nearest first."
  [c]
  (remove #(given-up? c %) (in-range c)))

(defn check [c]
  (and (>= (.-health (.self (:primitives c))) (:min-health (:args c)))
       (boolean (seq (targets c)))))

(defn ^:async equip-best! [c weapon]
  (when (and weapon (not= weapon (.-held (.self (:primitives c)))))
    (await (ctx/act c :equip #js {:item weapon :dest "hand"}))))

(defn note-blocked!
  "Count a blocked walk towards target; warn once when it is given up on."
  [c target]
  (let [n (inc (get-in (ctx/mem c) [:blocked (.-id target)] 0))]
    (ctx/update-mem! c assoc-in [:blocked (.-id target)] n)
    (when (= n u/max-failures)
      (ctx/emit! c :fight_unreachable :warn {:text (str "cannot reach the " (.-name target) ", giving up on it")}))))

(defn ^:async swing!
  "Walk into reach if needed, then face and hit target once. Resolves to the
  walk's outcome: :there, :partial or :blocked."
  [c target]
  (let [tpos (u/pos-of (.-pos target))
        r (if (> (u/dist (u/self-pos c) tpos) reach)
            (await (u/walk-near! c tpos 2))
            :there)]
    (when (= :blocked r) (note-blocked! c target))
    (when (= :there r)
      (ctx/update-mem! c assoc :last-attack (ctx/now c))
      (await (ctx/act c :look #js {:pos #js {:x (:x tpos) :y (+ 1 (:y tpos)) :z (:z tpos)}}))
      (await (ctx/act c :attack #js {:id (.-id target)})))
    r))

(defn ^:async round [c]
  (let [{:keys [weapons attack-gap-ms]} (:args c)
        p (:primitives c)
        target (first (targets c))
        last-attack (:last-attack (ctx/mem c))]
    (cond
      (nil? target) (if (empty? (in-range c)) :done :declined)
      (and last-attack (< (- (ctx/now c) last-attack) attack-gap-ms)) :continue
      :else
      (do (await (equip-best! c (combat/best-weapon p weapons)))
          (await (swing! c target))
          (cond
            (empty? (in-range c)) :done
            (empty? (targets c)) :declined
            :else :continue)))))
