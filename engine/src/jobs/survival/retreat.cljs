(ns jobs.survival.retreat
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]))

(def doc
  "Walk a short step away from the nearest hostile each round, leaning
  towards the latest :bed or :home when that is not through the hostile,
  avoiding :hazard positions and turning towards open ground when a wall is
  behind. A flight starts for a hostile within :radius (ranged ones within
  :ranged-radius) and keeps going while one is within :clear-radius, so a
  chasing mob does not catch up between steps. Cornered (no open direction,
  or the walk is blocked) it fights back with the best weapon whatever its
  health; unarmed it gives up after three tries. Once per flight, with at
  least :eat-gap blocks to the nearest hostile and food carried, it eats
  (jobs.survival.eat up to 20) so health can regenerate on the run. Done when
  no hostile has been within :clear-radius for :cooldown-ms.")

(def args
  {:radius {:doc "hostiles within this many blocks start a flight" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks start a flight" :default 16}
   :clear-radius {:doc "the flight goes on while a hostile is within this many blocks (zombies track to 35)" :default 40}
   :eat-gap {:doc "with at least this many blocks to the nearest hostile, eat once per flight" :default 12}
   :step {:doc "blocks per walk" :default 6}
   :cooldown-ms {:doc "done once no hostile was in the clear radius for this long" :default 5000}
   :weapons {:doc "item name substrings that count as weapons, for a cornered fight" :default combat/default-weapons}})

(def hazard-clearance 2.5)

(def turns
  "Angles to try, in degrees from the preferred direction, best first."
  [0 30 -30 60 -60 90 -90 120 -120])

(def min-open
  "Fewest clear cells a direction needs to be worth walking."
  2)

(def passable-names
  #{"air" "cave_air" "void_air" "short_grass" "tall_grass" "grass" "fern" "large_fern" "dead_bush" "snow"
    "water" "dandelion" "poppy" "torch" "sweet_berry_bush" "vine"})

(defn passable?
  "Whether a block name lets the body walk through; an unloaded cell (nil) counts as open."
  [block-name]
  (or (nil? block-name) (contains? passable-names block-name)
      (some #(.endsWith block-name %) ["_sapling" "_flower" "_carpet" "_tulip" "_orchid" "_button" "_pressure_plate"])))

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

(defn open-cells
  "How many cells along [ux uz] from from, up to n, are free at feet and head
  height before the first obstacle, per block-at (a cell -> block name or nil)."
  [block-at from dir n]
  (let [free? (fn [k] (let [{:keys [x z]} (point-along from dir k)
                            y (js/Math.floor (:y from))]
                        (and (passable? (block-at {:x x :y y :z z}))
                             (passable? (block-at {:x x :y (inc y) :z z})))))]
    (count (take-while free? (range 1 (inc n))))))

(defn worth?
  "Whether walking open cells along dir from from is worth it against threat:
  it must not end closer, and must either be a real walk (min-open + 1
  cells) or gain at least 2 blocks of distance. A short side step in a dead
  end is neither, so a body that only has those left is cornered."
  [from threat dir open]
  (let [now (u/dist from threat)
        then (u/dist (point-along from dir open) threat)]
    (and (>= open min-open)
         (>= then now)
         (or (> open min-open) (>= (- then now) 2)))))

(defn choose-target
  "The walk target: the first direction, turning away from the preferred one
  as needed, that avoids every hazard and is open for a full step; else the
  most open one that is still worth walking (see worth?). nil when cornered."
  [block-at from threat home hazards step]
  (let [dir (direction from threat home)
        options (->> turns
                     (map #(rotate dir %))
                     (map (fn [d] {:open (open-cells block-at from d step) :dir d}))
                     (remove #(near-hazard? hazards from (point-along from (:dir %) step)))
                     (filter #(worth? from threat (:dir %) (:open %))))
        pick (or (first (filter #(>= (:open %) step) options))
                 (last (sort-by :open options)))]
    (when pick (point-along from (:dir pick) (:open pick)))))

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

(defn block-at-fn [p]
  (fn [pos] (u/block-name p pos)))

(defn ^:async cornered!
  "Nowhere to go: fight back with the best weapon whatever the health, else
  count a failure."
  [c why]
  (let [{:keys [radius ranged-radius weapons]} (:args c)]
    (if (combat/best-weapon (:primitives c) weapons)
      (do (await (ctx/call-child c :cornered 'jobs.survival.fight-back
                                 {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons}))
          :continue)
      (u/fail! c :retreat_blocked why))))

(defn ^:async eat-on-the-run!
  "Once per flight, with the nearest hostile at least :eat-gap away, eat."
  [c threat]
  (when (and (not (:ate (ctx/mem c)))
             (>= (.-distance threat) (:eat-gap (:args c))))
    (let [r (await (ctx/call-child c :eat 'jobs.survival.eat {:until 20}))]
      (when (not= :declined r) (ctx/update-mem! c assoc :ate true)))))

(defn ^:async round [c]
  (let [{:keys [radius ranged-radius clear-radius step cooldown-ms]} (:args c)
        p (:primitives c)
        now (ctx/now c)
        fleeing? (some? (:last-seen (ctx/mem c)))
        threat (first (combat/hostiles p (if fleeing? (max clear-radius radius) radius)
                                       {:ranged-radius (if fleeing? (max clear-radius ranged-radius) ranged-radius)}))]
    (cond
      (and (nil? threat) fleeing? (>= (- now (:last-seen (ctx/mem c))) cooldown-ms)) :done
      (and (nil? threat) fleeing?) :continue
      (nil? threat) (do (ctx/update-mem! c assoc :last-seen now) :continue)
      :else
      (let [_ (await (eat-on-the-run! c threat))
            from (u/self-pos c)
            target (choose-target (block-at-fn p) from (u/pos-of (.-pos threat)) (home-pos c)
                                  (keep (comp :pos :data) (ctx/entries c :hazard)) step)]
        (ctx/update-mem! c assoc :last-seen now)
        (if (nil? target)
          (await (cornered! c "no open way away from the hostile"))
          (let [r (await (ctx/act c :moveTo (clj->js {:pos target :range 1})))]
            (if (= "blocked" (.-status r))
              (await (cornered! c "the way away from the hostile is blocked"))
              :continue)))))))
