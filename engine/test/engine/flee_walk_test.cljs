(ns engine.flee-walk-test
  "jobs.lib.flee-walk's choice of a flight target: water is slow, cover from a ranged mob comes first (pure,
  over block-at fns)."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.reach :as reach]
            [jobs.survival.respond.cover :as cover]
            [jobs.lib.flee-walk :as walk]))

(defn pond-world
  "Stone up to y 62; at y 63 water in the band x >= 1, |z| <= 2 (a pond straight ahead of a body fleeing +x), stone
  elsewhere; air above."
  [{:keys [x y z]}]
  (cond
    (<= y 62) "stone"
    (= y 63) (if (and (>= x 1) (<= (js/Math.abs z) 2)) "water" "stone")
    :else "air"))

(def from {:x 0.5 :y 64 :z 0.5})
(def threat {:x -5 :y 64 :z 0.5})

(deftest a-flight-takes-a-full-step-on-dry-ground-over-a-swim-straight-away
  (let [end (walk/choose-target pond-world from [threat] nil [] 6 {:swim-factor 2.2})]
    (is (not (walk/wet? pond-world end)) "dry ground beats swimming the pond")
    (is (< 2 (js/Math.abs (:z end))) "round the band, not through it"))
  (is (= {:x 6 :y 63 :z 0} (walk/choose-target pond-world from [threat] nil [] 6 {:swim-factor 1}))
      "with water as fast as ground the way straight away is kept"))

(defn walled-pond-world
  "pond-world with the pond |z| <= 3 wide and stone walls two high at |z| = 4: dry ground sideways is only 3 blocks."
  [{:keys [x y z]}]
  (cond
    (and (#{64 65} y) (= 4 (js/Math.abs z))) "stone"
    (and (= y 63) (>= x 1) (<= (js/Math.abs z) 3)) "water"
    :else (pond-world {:x x :y y :z (if (= y 63) 9 z)})))

(deftest a-flight-takes-a-short-dry-step-over-a-swim
  (is (contains? #{{:x 0 :y 64 :z 3} {:x 0 :y 64 :z -3}}
                 (walk/choose-target walled-pond-world from [threat] nil [] 6 {:swim-factor 2.2}))
      "a drowned or a lost breath waits in water: a dry step of 3 beats a full swim"))

(deftest a-refused-end-is-never-chosen-again
  (let [flat (fn [{:keys [y]}] (if (<= y 63) "stone" "air"))
        first-end (walk/choose-target flat from [threat] nil [] 6)
        refused #{[(:x first-end) (:y first-end) (:z first-end)]}
        again (walk/choose-target flat from [threat] nil [] 6 {:refused refused})]
    (is (some? again) "another heading is open")
    (is (not= first-end again) "the refused end is left out")))

(deftest a-flight-without-options-keeps-the-preferred-direction
  (is (= {:x 6 :y 63 :z 0} (walk/choose-target pond-world from [threat] nil [] 6))))

(deftest a-cell-is-wet-when-the-feet-or-the-floor-are-water
  (is (walk/wet? pond-world {:x 3 :y 63 :z 0}) "feet in water")
  (is (walk/wet? pond-world {:x 3 :y 64 :z 0}) "on water: the body sinks into it")
  (is (not (walk/wet? pond-world {:x 0 :y 64 :z 5}))))

(def flat-stone (fn [{:keys [y]}] (if (<= y 63) "stone" "air")))
(def logged-band (fn [{:keys [x y z]}] (and (= y 63) (>= x 1) (<= (js/Math.abs z) 2))))

(deftest a-waterlogged-block-is-wet-and-a-flight-swims-round-it
  (is (walk/wet? flat-stone logged-band {:x 3 :y 64 :z 0}) "on a waterlogged floor")
  (is (not (walk/wet? flat-stone logged-band {:x 3 :y 64 :z 5})))
  (let [end (walk/choose-target flat-stone from [threat] nil [] 6 {:swim-factor 2.2 :waterlogged? logged-band})]
    (is (< 2 (js/Math.abs (:z end))) "round the waterlogged band, as round a pond"))
  (is (= {:x 6 :y 64 :z 0} (walk/choose-target flat-stone from [threat] nil [] 6 {:swim-factor 2.2}))
      "unread, the band is dry"))

(deftest a-walk-is-priced-by-the-distance-it-gains
  (is (= 5.5 (/ (js/Math.round (* 10 (walk/gain from [threat] {:x 6 :y 64 :z 0}))) 10)) "straight away")
  (is (= 1 (walk/gain from [threat] {:x 0 :y 64 :z 1})) "a side step gains next to nothing: one")
  (is (= 1 (walk/gain from [threat] nil)))
  (is (= 1 (walk/gain from [] {:x 6 :y 64 :z 0})) "no threat: nothing to gain on"))

(deftest water-plants-and-a-bubble-column-are-wet
  (doseq [plant ["kelp" "kelp_plant" "seagrass" "tall_seagrass" "bubble_column"]]
    (is (walk/wet? (fn [{:keys [y]}] (if (= y 64) plant "stone")) {:x 0 :y 64 :z 0}) plant)))

(deftest a-flight-from-a-ranged-mob-takes-a-covered-end-first
  (let [flat (fn [{:keys [y]}] (if (<= y 63) "stone" "air"))
        covered? (fn [end] (neg? (:z end)))]
    (is (= {:x 6 :y 64 :z 0} (walk/choose-target flat from [threat] nil [] 6 {:swim-factor 2.2}))
        "no cover asked: straight away")
    (is (neg? (:z (walk/choose-target flat from [threat] nil [] 6 {:swim-factor 2.2 :covered? covered?})))
        "the first direction whose end is out of the line of fire")))

(defn leaf-wall-world
  "Flat stone floor at y 63; an oak_leaves wall at x 3, y 64-66, z -3..3."
  [x y z]
  (cond
    (<= y 63) :solid
    (and (= x 3) (<= 64 y 66) (<= -3 z 3)) (reach/arrow-kind-of #js {:name "oak_leaves"})
    :else (reach/arrow-kind-of #js {:name "air"})))

(deftest leaves-cover-a-body-from-arrows
  (let [skeleton {:x 0.5 :y 64 :z 0.5}]
    (is (reach/out-of-fire? leaf-wall-world [skeleton] {:x 5 :y 64 :z 0}) "behind the leaves")
    (is (not (reach/out-of-fire? leaf-wall-world [skeleton] {:x 0 :y 64 :z 6})) "in the open beside them")))

(def stair-floors (vec (for [i (range 1 9)] [(- 10 i) (- 70 i) 0])))

(deftest a-flight-takes-the-bodys-own-stair-up-away-from-the-mobs
  (let [at (fn [i] {:x (+ 0.5 (- 10 i)) :y (- 71 i) :z 0.5})
        mob {:x 0.5 :y 62 :z 0.5}]
    (is (= {:x 9 :y 70 :z 0} (walk/way-target (at 6) [mob] stair-floors 12))
        "the highest stair cell within reach, toward the entry, that is no nearer the mob")
    (is (nil? (walk/way-target (at 1) [mob] [] 12)) "no stair, no way target")
    (is (nil? (walk/way-target (at 8) [{:x 9.5 :y 70 :z 0.5}] stair-floors 12))
        "every stair cell above the body lies nearer the mob than the body: none")))

(defn deep-lake
  "Stone up to y 59, water at y 60 and 61 for x >= 0, air above: a lake two deep."
  [{:keys [x y]}]
  (cond (<= y 59) "stone" (<= 60 y 61) (if (>= x 0) "water" "air") :else "air"))

(deftest a-walk-over-deep-water-stays-at-the-surface
  (is (= [61 61 61 61] (mapv :y (walk/walk-cells deep-lake {:x 0.5 :y 61 :z 0.5} [1 0] 4)))
      "a swimmer is not walked down to the lake floor with its head under water"))

(defn slope-world
  "Stone floor sloping down one block per x from x 0 (feet y 64 - x); a stone roof from y 66 when roofed?; corridor? walls
  every column but z 0."
  [{:keys [roofed? corridor?]}]
  (fn [{:keys [x y z]}]
    (cond
      (and corridor? (not= z 0)) "stone"
      (<= y (- 63 (max 0 x))) "stone"
      (and roofed? (>= y 66)) "stone"
      :else "air")))

(deftest a-flight-does-not-run-down-into-a-roofed-place-below-its-floor
  (let [floor 61
        end (walk/choose-target (slope-world {:roofed? true}) from [threat] nil [] 6 {:floor floor})]
    (is (>= (:y end) floor) "a level way sideways is taken over the slope under the rock")
    (is (nil? (walk/choose-target (slope-world {:roofed? true :corridor? true}) from [threat] nil [] 6 {:floor floor}))
        "no way but down under the rock: cornered, not deeper")))

(deftest a-flight-may-descend-in-the-open
  (is (= 58 (:y (walk/choose-target (slope-world {}) from [threat] nil [] 6 {:floor 61})))
      "under open sky the slope is no cave")
  (is (= 58 (:y (walk/choose-target (slope-world {:roofed? true}) from [threat] nil [] 6)))
      "no floor given: unchanged"))

(deftest a-flight-may-descend-under-rock-out-of-a-ranged-mobs-fire
  (is (= 58 (:y (walk/choose-target (slope-world {:roofed? true}) from [threat] nil [] 6 {:floor 61 :covered? (constantly true)})))
      "cover from arrows outweighs depth"))

(deftest a-flight-may-descend-a-stair-the-body-cut
  (is (= 58 (:y (walk/choose-target (slope-world {:roofed? true}) from [threat] nil [] 6
                                    {:floor 61 :known (set (for [x (range 0 9)] [x (- 63 x) 0]))})))
      "known floors are no unknown cave"))

(deftest a-destination-lies-on-the-way-past-a-nearer-threat-in-its-direction
  (let [on-way? #(walk/on-the-way? from [%] {:x 40.5 :y 64 :z 0.5})]
    (is (on-way? {:x 10 :y 64 :z 0.5}) "ahead on the line")
    (is (on-way? {:x 10 :y 64 :z 8}) "ahead, 38 degrees off")
    (is (not (on-way? {:x 10 :y 64 :z 14})) "ahead but well to the side")
    (is (not (on-way? {:x -5 :y 64 :z 0.5})) "behind the body")
    (is (not (on-way? {:x 60 :y 64 :z 0.5})) "beyond the destination")))

(deftest a-wider-cone-puts-a-threat-off-to-the-side-on-the-way
  (let [threat {:x 3 :y 64 :z 4}
        dest {:x 40.5 :y 64 :z 0.5}]
    (is (not (walk/on-the-way? from [threat] dest)) "54 degrees off: outside the default cone")
    (is (walk/on-the-way? from [threat] dest 90) "inside a 90 degree cone")))

(defn corridor-world
  "Stone up to y 63 and walls at y 64-65 except along z 0: a corridor along x."
  [{:keys [y z]}]
  (if (or (<= y 63) (and (<= y 65) (not (zero? z)))) "stone" "air"))

(deftest a-flight-never-ends-within-keep-away-of-a-threat
  (let [close {:x -0.5 :y 64 :z 0.5}]
    (is (nil? (walk/choose-target corridor-world from [close] nil [] 3 {}))
        "a 3 block step gains 2.5 blocks but ends inside keep-away: cornered")
    (is (some? (walk/choose-target corridor-world from [close] nil [] 6 {}))
        "a 6 block step ends outside it")))

(deftest the-way-up-is-not-taken-to-a-cell-within-keep-away-of-a-threat
  (let [body {:x 0.5 :y 60 :z 0.5}
        floors [[3 62 0]]]
    (is (some? (walk/way-target body [{:x -9.5 :y 63 :z 0.5}] floors 12)) "far from the threat")
    (is (nil? (walk/way-target body [{:x 0.5 :y 63 :z 0.5}] floors 12))
        "no nearer the threat than the body, but 3 blocks from it")))

(def flat-world (fn [{:keys [y]}] (if (<= y 63) "stone" "air")))

(deftest a-flight-leaves-out-the-ends-it-is-asked-to-avoid-unless-nothing-else-is-open
  (let [ahead? (fn [end] (pos? (- (:x end) 3)))
        straight (walk/choose-target flat-world from [threat] nil [] 6 {})
        avoided (walk/choose-target flat-world from [threat] nil [] 6 {:avoid [ahead?]})]
    (is (= {:x 6 :y 64 :z 0} straight))
    (is (not= straight avoided) "the end straight away is avoided, another turn is taken")
    (is (= straight (walk/choose-target flat-world from [threat] nil [] 6 {:avoid [(constantly true)]}))
        "every end avoided: the preferred one stays")))

(deftest an-avoid-that-would-empty-the-options-gives-way-to-the-next
  (let [no-ahead (fn [end] (>= (:x end) 3))
        no-left (fn [end] (pos? (:z end)))
        end (walk/choose-target flat-world from [threat] nil [] 6 {:avoid [no-ahead (constantly true) no-left]})]
    (is (< (:x end) 3) "the first avoid holds")
    (is (not (pos? (:z end))) "the all-avoiding one is skipped, the last holds")))

(deftest a-meadow-flight-walks-through-flowers-and-rails
  (let [meadow (fn [{:keys [x y z]}]
                 (cond (<= y 63) "stone"
                       (not= y 64) "air"
                       (even? (+ x z)) "cornflower"
                       :else "rail"))]
    (is (< 3 (:x (walk/choose-target meadow from [threat] nil [] 6)))
        "flowers and rails are no walls: the flight runs straight away from the threat")))

(deftest the-places-recent-walks-began-are-ways-back-like-stairs
  (let [c {:view (fn [] {:now 1000 :data {:entries {:moved [{:t 900 :data {:from {:x 4.5 :y 15 :z -2.2} :to {:x 1 :y 3 :z 1}}}
                                                           {:t 950 :data {:from {:x 4.7 :y 15 :z -2.4} :to {:x 1 :y 3 :z 1}}}]}
                                           :policies {:moved {:cap 20 :ttl 600000}}}})}]
    (is (= #{[4 14 -3]} (cover/trail-floors c)) "the floor under each start, once")))

(deftest a-drowned-chaser-is-never-fled-into-water
  (let [all-water (fn [{:keys [y]}] (cond (<= y 62) "stone" (= y 63) "water" :else "air"))]
    (is (not (walk/wet? pond-world (walk/choose-target pond-world from [threat] nil [] 6 {:swim-factor 1 :dry-only? true})))
        "even with water as fast as ground, the dry way round the pond")
    (is (nil? (walk/choose-target all-water from [threat] nil [] 6 {:swim-factor 1 :dry-only? true}))
        "only water to flee through: cornered, not a swim")))

(def ledge-world
  "Stone up to y 63 west of x 1; east of it stone only to y 60 (a three-block ledge), lava under a second ledge at z >= 4."
  (fn [{:keys [x y z]}]
    (cond
      (<= y 60) "stone"
      (< x 1) (if (<= y 63) "stone" "air")
      (and (>= z 4) (= y 60)) "lava"
      :else "air")))

(deftest a-flight-jumps-down-a-three-block-ledge-it-sees-the-floor-of
  (is (= 3 (walk/open-cells ledge-world {:x 0.5 :y 64 :z 0.5} [1 0] 3)) "dropped onto the stone three blocks down, not blocked at the lip")
  (is (= {:x 6 :y 61 :z 0} (walk/choose-target ledge-world {:x 0.5 :y 64 :z 0.5} [{:x -5 :y 64 :z 0.5}] nil [] 6))
      "a creeper behind on the plateau: the flight runs off the ledge"))
