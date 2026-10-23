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

(def pitches
  "Degrees above the level at which a shot's way is tried: a hit shows the way it came from, not the height."
  [0 -30 30 -60 60])

(defn shot?
  "Whether :hurt entry e is a ranged mob's hit: its attacker is one, or the damage is an arrow (the shooter untracked)."
  [e]
  (let [{:keys [attacker damageType]} (:data e)]
    (or (contains? cost/ranged-mobs (:name attacker))
        (= "arrow" (some-> damageType (str/replace #"^minecraft:" ""))))))

(defn shot-spots
  "Pure: for each :hurt entry that is a shot (shot?) and carries a :dir, the points reach, 2/3 and 1/3 of reach blocks
  out from from along it, level and at each of pitches above or below, then the points those depths straight under
  from (a skeleton down a shaft under the body shoots up it); the first is the level one reach out."
  [entries {:keys [x y z]} reach]
  (let [outs [reach (* reach (/ 2 3)) (/ reach 3)]]
    (->> entries
         (filter shot?)
         (keep (comp :dir :data))
         (mapcat (fn [{dx :x dz :z}]
                   (concat (for [pitch pitches
                                 h outs]
                             {:x (+ x (* h dx)) :y (+ y (* h (js/Math.tan (* js/Math.PI (/ pitch 180))))) :z (+ z (* h dz))})
                           (for [d outs] {:x x :y (- y d) :z z}))))
         vec)))

(def max-drop "The most blocks a spot in seen open air is lowered to find what a shooter there stands on." 64)

(defn stand-spot
  "Where a shooter at spot ({:x :y :z}) can stand: nil when its feet or head cell is a seen block that stops arrows, else
  spot lowered onto the first cell under it that is not seen open (one not seen may be a floor), at most max-drop
  blocks. p: primitives."
  [p {:keys [x y z] :as spot}]
  (let [cx (js/Math.floor x) cz (js/Math.floor z) cy (js/Math.floor y)
        kind (fn [cy] (some-> (u/seen-block p {:x cx :y cy :z cz}) reach/arrow-kind-of))]
    (when-not (some #{:solid} [(kind cy) (kind (inc cy))])
      (assoc spot :y (loop [fy cy n 0]
                       (if (and (< n max-drop) (= :open (kind (dec fy)))) (recur (dec fy) (inc n)) fy))))))

(defn view-spots
  "Where unseen shooters may stand, as {:x :y :z}: shot-spots of the shots within ::window-s of the view's now (as a
  player feels a hit: its way, not its height), each where a shooter could stand there (stand-spot), one per cell. p:
  primitives."
  [p view]
  (->> (shot-spots (mem/since view :hurt (- (:now view) (* 1000 (settings/get settings ::window-s))))
                   (u/pos-of (.-pos (.self p))) (settings/get settings ::reach))
       (keep #(stand-spot p %))
       (reduce (fn [[seen out :as acc] s]
                 (let [cell (mapv js/Math.floor [(:x s) (:y s) (:z s)])]
                   (if (seen cell) acc [(conj seen cell) (conj out s)])))
               [#{} []])
       second))

(defn open?
  "Whether a shot within ::window-s came from a spot with a line of fire to the body's feet, as the hostile-near
  trigger and the retreat's dodge step read it. p: primitives, view: the memory view."
  [p view]
  (let [spots (view-spots p view)
        arrow-at (fn [x y z] (reach/arrow-kind-of (u/seen-block p {:x x :y y :z z})))]
    (boolean (and (seq spots) (not (reach/out-of-fire? arrow-at spots (sh-sense/feet p)))))))
