(ns jobs.survival.retreat
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]))

(def doc
  "Walk a short step away from the nearest hostile within :radius each round,
  leaning towards the latest :bed or :home when that is not through the
  hostile, and avoiding :hazard positions. Done when no hostile has been
  within :radius for :cooldown-ms.")

(def args
  {:radius {:doc "hostiles within this many blocks count" :default 8}
   :step {:doc "blocks per walk" :default 6}
   :cooldown-ms {:doc "done once no hostile was in radius for this long" :default 5000}})

(def hazard-clearance 2.5)

(def turns
  "Angles to try, in degrees from the preferred direction, best first."
  [0 30 -30 60 -60 90 -90])

(defn unit
  "[ux uz] for the vector (dx dz), or nil when it is zero."
  [dx dz]
  (let [n (js/Math.hypot dx dz)]
    (when (pos? n) [(/ dx n) (/ dz n)])))

(defn direction
  "The preferred unit [ux uz] from from: directly away from threat (+x when
  they coincide), blended with the direction of home when that lies on the
  away side (not through the threat)."
  [from threat home]
  (let [away (or (unit (- (:x from) (:x threat)) (- (:z from) (:z threat))) [1 0])
        to-home (when home (unit (- (:x home) (:x from)) (- (:z home) (:z from))))
        along (when to-home (+ (* (first away) (first to-home)) (* (second away) (second to-home))))]
    (if (and along (pos? along))
      (or (unit (+ (first away) (first to-home)) (+ (second away) (second to-home))) away)
      away)))

(defn rotate [[ux uz] degrees]
  (let [a (* degrees (/ js/Math.PI 180))
        c (js/Math.cos a)
        s (js/Math.sin a)]
    [(- (* ux c) (* uz s)) (+ (* ux s) (* uz c))]))

(defn point-along
  "The cell step blocks from from along [ux uz], same height."
  [from [ux uz] step]
  {:x (js/Math.round (+ (:x from) (* ux step)))
   :y (:y from)
   :z (js/Math.round (+ (:z from) (* uz step)))})

(defn near-hazard?
  "Whether the walk from from to target passes within clearance of a hazard
  (checked at the middle and the end)."
  [hazards from target]
  (let [mid {:x (/ (+ (:x from) (:x target)) 2) :y (:y from) :z (/ (+ (:z from) (:z target)) 2)}]
    (boolean (some #(or (< (u/dist % target) hazard-clearance) (< (u/dist % mid) hazard-clearance)) hazards))))

(defn choose-target
  "The first walk target, turning away from the preferred direction as needed,
  that avoids every hazard; nil when none does."
  [from threat home hazards step]
  (let [dir (direction from threat home)]
    (->> turns
         (map #(point-along from (rotate dir %) step))
         (remove #(near-hazard? hazards from %))
         first)))

(defn home-pos
  "The position of the latest :bed or :home entry, or nil."
  [c]
  (->> [(ctx/latest c :bed) (ctx/latest c :home)]
       (remove nil?)
       (sort-by :t >)
       first
       :data
       :pos))

(defn check [_c] true)

(defn ^:async round [c]
  (let [{:keys [radius step cooldown-ms]} (:args c)
        now (ctx/now c)
        last-seen (:last-seen (ctx/mem c))
        threat (first (combat/hostiles (:primitives c) radius))]
    (cond
      (and (nil? threat) (some? last-seen) (>= (- now last-seen) cooldown-ms)) :done
      (and (nil? threat) (some? last-seen)) :continue
      (nil? threat) (do (ctx/update-mem! c assoc :last-seen now) :continue)
      :else
      (let [target (choose-target (u/self-pos c) (u/pos-of (.-pos threat)) (home-pos c)
                                  (keep (comp :pos :data) (ctx/entries c :hazard)) step)]
        (ctx/update-mem! c assoc :last-seen now)
        (if (nil? target)
          (u/fail! c :retreat_blocked "no way away from the hostile avoids a hazard")
          (let [r (await (ctx/act c :moveTo (clj->js {:pos target :range 1})))]
            (if (= "blocked" (.-status r))
              (u/fail! c :retreat_blocked "the way away from the hostile is blocked")
              :continue)))))))
