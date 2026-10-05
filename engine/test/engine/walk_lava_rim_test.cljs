(ns engine.walk-lava-rim-test
  "The lava-rim pocket (engine.planner-lava-rim-test's cave, card 74a8cee4) walked by the executor in prismarine-physics,
  the body's own client physics. The pocket joins the cave only by corner slides whose open side is a hole onto the lava;
  the planner takes them when they are the only way. Live, the walk out of and into the pocket burned the body 5 of 6
  times: the walker must carry the body past such a hole with its box never over the lava."
  (:require [cljs.test :refer [deftest is]]
            [engine.planner-lava-rim-test :as rim]
            [engine.stairs-physics-test :as sp]))

(def fills (mapv #(conj % {}) rim/cave))

(def lava-cells
  (set (for [[x0 y0 z0 x1 y1 z1 name] rim/cave :when (= "lava" name)
             x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))]
         [x y z])))

;; vanilla's lava test: the player's box (half-width 0.3) shrunk by 0.001 meets the fluid, a source's surface 8/9 up its
;; cell
(def half (- 0.3 0.001))
(def lava-top (/ 8 9))

(defn touches-lava?
  "The body's box at pose meets the lava of a lava cell (vanilla's in-lava test, which burns)."
  [{:keys [x y z]}]
  (some (fn [[lx ly lz]]
          (and (< (- x half) (inc lx)) (> (+ x half) lx)
               (< (- z half) (inc lz)) (> (+ z half) lz)
               (< (+ y 0.001) (+ ly lava-top)) (> (+ y 1.8) ly)))
        lava-cells))

(defn walk-report [from goal]
  (let [{:keys [done poses steps]} (sp/walk fills from goal)
        bad (filter touches-lava? poses)]
    {:status (:status done)
     :first-bad (some-> (first bad) (select-keys [:x :y :z]))
     :low (some->> (seq poses) (apply min-key :y) (#(select-keys % [:x :y :z])))
     :steps (mapv (juxt :x :y :z :move :free :hop) steps)}))

(deftest the-walk-out-of-the-pocket-never-dips-into-the-lava
  (let [r (walk-report [124 -54 49] [112 -53 63])]
    (is (= :arrived (:status r)) (pr-str r))
    (is (nil? (:first-bad r)) (pr-str r))))

(deftest the-walk-into-the-pocket-never-dips-into-the-lava
  (let [r (walk-report [118 -53 59] [124 -54 49])]
    (is (= :arrived (:status r)) (pr-str r))
    (is (nil? (:first-bad r)) (pr-str r))))

;; One corner slide in a 2-high tunnel, every way round: takeoff A [0 64 0], landing B [sx 64 sz], the blocked side W and
;; the open side H the other two cells of the square, H a hole onto lava (its ceiling one block higher when tall-h, as
;; in the cave). The slide is the only way between A and B.
(defn corner-fills [sx sz wall-on-x tall-h]
  (let [[wx wz] (if wall-on-x [sx 0] [0 sz])
        [hx hz] (if wall-on-x [0 sz] [sx 0])]
    (cond-> [[-3 62 -3 3 68 3 "deepslate" {}]
             [0 64 0 0 65 0 "air" {}]
             [sx 64 sz sx 65 sz "air" {}]
             [hx 64 hz hx 65 hz "air" {}]
             [hx 63 hz hx 63 hz "lava" {}]]
      tall-h (conj [hx 66 hz hx 66 hz "air" {}])
      true (conj [wx 64 wz wx 65 wz "deepslate" {}]))))

(defn corner-lava?
  "The body's box at pose meets the lava of the corner's hole (vanilla's in-lava test)."
  [fills {:keys [x y z]}]
  (let [[hx _ hz] (some (fn [[x0 y0 z0 _ _ _ name]] (when (= "lava" name) [x0 y0 z0])) fills)]
    (and (< (- x half) (inc hx)) (> (+ x half) hx)
         (< (- z half) (inc hz)) (> (+ z half) hz)
         (< (+ y 0.001) (+ 63 lava-top)))))

(deftest a-corner-slide-past-a-lava-hole-never-dips-whichever-way-it-turns
  (doseq [sx [-1 1] sz [-1 1] wall-on-x [true false] tall-h [true false] back [false true]]
    (let [fills (corner-fills sx sz wall-on-x tall-h)
          [from goal] (cond-> [[0 64 0] [sx 64 sz]] back reverse)
          {:keys [done poses]} (sp/walk fills from goal)
          bad (first (filter #(corner-lava? fills %) poses))]
      (is (= :arrived (:status done)) (pr-str [sx sz wall-on-x tall-h back done]))
      (is (nil? bad) (pr-str [sx sz wall-on-x tall-h back bad])))))
