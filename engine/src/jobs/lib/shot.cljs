(ns jobs.lib.shot
  "Shots from a shooter the body has not seen: where they came from (view-spots) and whether one has a line of fire to the body's feet (open?)."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::reach {:default 12 :doc "Blocks out along the way a shot came from that stand for the place of a shooter the body has not seen (cover, seal sides and back-off turn on it)." :spec (a/num-in 1 nil)}
   ::window-s {:default 5 :doc "Seconds a shot's direction stands for an unseen shooter." :spec (a/num-in 0 nil)}})

(defn shot?
  "Whether :hurt entry e is a ranged mob's hit: its attacker is one, or the damage is an arrow (the shooter untracked)."
  [e]
  (let [{:keys [attacker damageType]} (:data e)]
    (or (contains? cost/ranged-mobs (:name attacker))
        (= "arrow" (some-> damageType (str/replace #"^minecraft:" ""))))))

(defn shot-spots
  "Pure: for each :hurt entry that is a shot (shot?) and carries a :dir, the point reach blocks out from from along it,
  at the height of from."
  [entries {:keys [x y z]} reach]
  (->> entries
       (filter shot?)
       (keep (comp :dir :data))
       (mapv (fn [{dx :x dz :z}] {:x (+ x (* reach dx)) :y y :z (+ z (* reach dz))}))))

(defn view-spots
  "Where unseen shooters stand, as {:x :y :z}: shot-spots of the shots within ::window-s of the view's now, ::reach
  blocks out along the way each hit came from (as a player feels it), at the body's height. p: primitives."
  [p view]
  (shot-spots (mem/since view :hurt (- (:now view) (* 1000 (settings/get settings ::window-s))))
              (u/pos-of (.-pos (.self p))) (settings/get settings ::reach)))

(defn open?
  "Whether a shot within ::window-s came from a spot with a line of fire to the body's feet, as the hostile-near
  trigger and the retreat's dodge step read it. p: primitives, view: the memory view."
  [p view]
  (let [spots (view-spots p view)
        arrow-at (fn [x y z] (reach/arrow-kind-of (u/seen-block p {:x x :y y :z z})))]
    (boolean (and (seq spots) (not (reach/out-of-fire? arrow-at spots (sh-sense/feet p)))))))
