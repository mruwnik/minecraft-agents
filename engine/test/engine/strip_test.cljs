(ns engine.strip-test
  "jobs.lib.strip: reading an abandoned mineshaft's corridors from seen blocks."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.strip :as strip]))

(def y -27)

(defn corridor
  "Air cells of a 3x3 corridor along axis on line from a to b, floor at y-1."
  [axis line from to]
  (set (for [a (range from (inc to)) w [-1 0 1] h [0 1 2]]
         (if (= axis :x) [a (+ y h) (+ line w)] [(+ line w) (+ y h) a]))))

(defn shell
  "The air cells and the stone round them, as seen blocks."
  [air]
  (let [around (for [[x yy z] air d [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]] (mapv + [x yy z] d))
        cells (-> (zipmap air (repeat "cave_air")) (into (map (fn [c] [c "stone"]) (remove air around))))]
    cells))

(defn support-cells [axis line a]
  (let [at (fn [off h] (if (= axis :x) [a (+ y h) (+ line off)] [(+ line off) (+ y h) a]))]
    (merge (into {} (for [off [-1 1] h [0 1]] [(at off h) "oak_fence"]))
           (into {} (for [off [-1 0 1]] [(at off 2) "oak_planks"])))))

(defn rail-cells [axis line from to]
  (into {} (for [a (range from (inc to))] [(if (= axis :x) [a y line] [line y a]) "rail"])))

(defn blocks-of [cells]
  (mapv (fn [[[x yy z] n]] {:name n :pos {:x x :y yy :z z}}) cells))

(defn t-world
  "Stem along z at x=0 (z 0..19) into a corridor along x at z=20 (x -20..20); unseen when (unseen? [x y z])."
  [unseen?]
  (let [air (into (corridor :x 20 -20 20) (corridor :z 0 0 19))
        cells (merge (shell air)
                     (rail-cells :x 20 -20 20) (rail-cells :z 0 0 18)
                     (into {} (mapcat #(support-cells :x 20 %) (remove zero? (range -16 17 4))))
                     (into {} (mapcat #(support-cells :z 0 %) [2 6 10 14]))
                     {[3 (inc y) 20] "cobweb" [0 (inc y) 8] "cobweb"})]
    (blocks-of (remove (fn [[c _]] (unseen? c)) cells))))

(defn summary [segs] (set (map (juxt :id :axis :from :to :open-ends) segs)))

(deftest t-crossing-is-three-segments
  (is (= #{["x-27-20:-20..0" :x -20 0 #{}]
           ["x-27-20:0..20" :x 0 20 #{}]
           ["z-27-0:0..20" :z 0 20 #{}]}
         (summary (strip/segments (t-world (constantly false)))))))

(deftest unseen-far-end-is-open
  (let [segs (strip/segments (t-world (fn [[x]] (> x 12))))]
    (is (= #{["x-27-20:-20..0" :x -20 0 #{}]
             ["x-27-20:0..12" :x 0 12 #{:to}]
             ["z-27-0:0..20" :z 0 20 #{}]}
           (summary segs)))))

(deftest merge-extends-an-open-end-and-never-shrinks
  (let [first-read (strip/segments (t-world (fn [[x]] (> x 12))))
        mem (strip/merge-segments {} first-read)
        read-2 (strip/segments (t-world (constantly false)))
        merged (strip/merge-segments (update-vals mem #(assoc % :state :stripped)) read-2)
        stripped (strip/merge-segments merged (strip/segments []))
        shorter (strip/merge-segments merged [{:id "x" :axis :x :y y :line 20 :from 2 :to 5 :open-ends #{}}])]
    (is (= :open (:state (mem "x-27-20:0..12"))))
    (is (= {:from 0 :to 20 :open-ends #{} :state :stripped} (select-keys (merged "x-27-20:0..12") [:from :to :open-ends :state])))
    (is (= merged stripped))
    (is (= merged shorter))))

(deftest order-puts-the-arms-as-leaves-from-the-stem
  (let [segs (strip/segments (t-world (constantly false)))
        tree (strip/order segs [0 y 3])]
    (is (= "z-27-0:0..20" (:root tree)))
    (is (= ["x-27-20:-20..0" "x-27-20:0..20"] (:leaves tree)))
    (is (= {"x-27-20:-20..0" "z-27-0:0..20" "x-27-20:0..20" "z-27-0:0..20"} (:parent tree)))
    (is (empty? (:unreached tree)))))

(deftest order-from-an-arm-makes-the-stem-and-other-arm-children
  (let [tree (strip/order (strip/segments (t-world (constantly false))) [-18 y 20])]
    (is (= "x-27-20:-20..0" (:root tree)))
    (is (= #{"z-27-0:0..20" "x-27-20:0..20"} (set (:leaves tree))))))

(def seg {:axis :x :y y :line 20 :from 0 :to 12})

(defn takings-world []
  (let [air (corridor :x 20 0 12)
        floor (into {} (for [a (range 0 13) w [-1 0 1]] [[a (dec y) (+ 20 w)] "stone"]))]
    (blocks-of
     (merge (shell air) floor
            (support-cells :x 20 2)
            (rail-cells :x 20 3 4)
            {[3 (inc y) 20] "cobweb"
             [6 y 22] "iron_ore"
             [7 y 22] "coal_ore" [7 y 21] "stone"
             [9 (dec y) 20] "oak_planks" [9 (- y 2) 20] "cave_air"
             [10 (+ y 2) 20] "oak_planks" [10 (+ y 3) 20] "gravel"
             [12 y 19] "oak_planks" [12 y 18] "lava"}))))

(defn cells-of [xs] (mapv (comp (juxt :x :y :z) :pos) xs))

(deftest targets-take-wood-rails-cobweb-and-exposed-ore-in-order
  (let [{:keys [targets]} (strip/targets (takings-world) seg {:entry [0 y 20]})]
    (is (= {:wood 7 :rail 2 :cobweb 1 :ore 1} (frequencies (map :class targets))))
    (is (= (sort (map #(:x (:pos %)) targets)) (map #(:x (:pos %)) targets)))
    (is (= [:cobweb :rail] (map :class (filter #(= 3 (:x (:pos %))) targets)))
        "a cobweb on the walk line comes before the rail beside it")
    (is (= [[6 y 22]] (cells-of (filter #(= :ore (:class %)) targets))) "the buried coal is not taken")))

(deftest targets-start-at-the-end-nearest-the-entry
  (let [{:keys [targets]} (strip/targets (takings-world) seg {:entry [12 y 20]})]
    (is (= (reverse (sort (map #(:x (:pos %)) targets))) (map #(:x (:pos %)) targets)))))

(deftest kept-bridge-falling-and-lava-planks-are-not-targets
  (let [{:keys [targets kept]} (strip/targets (takings-world) seg {:entry [0 y 20]})]
    (is (= #{{:pos {:x 9 :y (dec y) :z 20} :why :bridge}
             {:pos {:x 10 :y (+ y 2) :z 20} :why :falling}
             {:pos {:x 12 :y y :z 19} :why :lava}}
           (set kept)))
    (is (empty? (filter #(contains? (set (map :pos kept)) (:pos %)) targets)))))

(deftest take-set-limits-the-classes
  (let [{:keys [targets]} (strip/targets (takings-world) seg {:entry [0 y 20] :take #{:rail}})]
    (is (= #{:rail} (set (map :class targets))))))

(deftest merge-drops-a-read-that-spans-two-remembered-segments
  (let [seg (fn [from to] {:id (str from ".." to) :axis :x :y y :line 20 :from from :to to :open-ends #{}})
        mem (strip/merge-segments {} [(seg 0 12) (seg 12 18)])
        merged (strip/merge-segments mem [(seg 0 18)])]
    (is (= mem merged))))

(deftest mouth-finds-a-side-opening-not-yet-looked-into
  (let [seg {:axis :x :y y :line 16 :from 0 :to 30}
        side (corridor :z 20 12 15)
        blocks (shell (into (corridor :x 16 0 30) side))
        at (fn [a h w] [a (+ y h) (+ 16 w)])
        wall (fn [cell] (get blocks cell "stone"))]
    (is (= 19 (strip/mouth wall seg [])) "the first wall cell that is air")
    (is (= 28 (strip/mouth (fn [c] (if (= c (at 28 1 2)) nil (wall c))) seg [10 19])) "a never-seen wall cell counts too")
    (is (nil? (strip/mouth wall seg [20])) "a stop within reach of it has looked")))
