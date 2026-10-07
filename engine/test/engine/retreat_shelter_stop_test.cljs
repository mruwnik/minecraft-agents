(ns engine.retreat-shelter-stop-test
  "The retreat's side pocket (and the shell check the pit shares) read the rock through perception (Q98): what a dig lays open stops the refuge (open
  wall, fluid) or, for lava, is sealed with a carried block (:on-lava :seal, the default) or left alone (:on-lava :stop)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-refuge :as refuge]))

(def dead-end-cells (set (for [x (range 0 9) y [64 65]] [x y 0])))

(defn key-of [x y z] (str x "," y "," z))

(defn rock
  "Stone over x -3..9, y bottom..67, z -1..1 less the open cells, then the extra blocks laid over it."
  [bottom open extra]
  (into extra (for [x (range -3 10) y (range bottom 68) z [-1 0 1]
                    :when (not (or (open [x y z]) (contains? extra (key-of x y z))))]
                [(key-of x y z) "stone"])))

(defn ^:async look-at-cells! [p cells]
  (loop [cells cells]
    (when-let [[x y z] (first cells)]
      (await (.look p "t1" #js {:pos #js {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}}))
      (recur (rest cells)))))

(defn ^:async see-tunnel!
  "The body has looked along the tunnel (from its mouth and from further in, as a player walks it): its walls, floor and
  roof, nothing inside the rock."
  [raw p]
  (.setOwner p "t1")
  (let [home (:pos (fake/self raw))
        walls (for [x (range -1 9) y [63 64 65 66] z [-1 0 1] :when (not (dead-end-cells [x y z]))] [x y z])]
    (fake/swap-self! raw assoc :pos [5.5 64 0.5])
    (await (look-at-cells! p walls))
    (fake/swap-self! raw assoc :pos home)
    (await (look-at-cells! p walls)))
  p)

(defn sleep [ms] (js/Promise. (fn [r] (js/setTimeout r ms))))

(defn ^:async round
  "Submit the retreat over the world and run its first rounds; the primitives p."
  [world args]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (assoc world :entities [{:id 9 :name "skeleton" :kind "hostile" :pos {:x 4 :y 64 :z 0} :health 20}]))
        p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (fn [] @clock)}))
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (await (see-tunnel! raw p))
    (core/submit! eng (list 'jobs.survival.retreat args) {})
    (let [r (core/tick! eng)]
      (await (sleep 700))
      (swap! (fake/state raw) assoc :entities [])
      (swap! clock + 300000)
      (await r))
    p))

(defn calls [p kind] (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)])
                           (filter #(= kind (.-name %)) (.-calls (.-world p)))))

(def kit [{:name "stone_pickaxe" :count 1}])

(defn pocket-world
  "A dead end whose back wall hides one more cell, at x -2 (an open cell: air, or the named block)."
  [hidden]
  {:blocks (rock 60 dead-end-cells
                       (into {(key-of -2 64 0) hidden (key-of -2 65 0) hidden}
                             (for [x (range 0 9) y (range 60 64)] [(key-of x y 0) "obsidian"]))) :inventory kit})

(deftest a-pocket-that-lays-open-a-cave-is-not-sealed-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (round (pocket-world "air") {}))]
          (is (= [[-1 64 0] [-1 65 0]] (calls p "dig")) "both cells dug, the cave behind them showed")
          (is (not-any? #{[0 64 0] [0 65 0]} (calls p "place")) "never sealed in at the way in"))))))

(deftest a-pocket-that-lays-open-lava-seals-it-with-a-dug-block-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (round (pocket-world "lava") {}))]
          (is (some #{[-2 64 0]} (calls p "place")) "the lava cell is filled")
          (is (not= "lava" (.-name (.blockAt p #js {:x -2 :y 64 :z 0}))) "and holds a block now"))))))

(deftest a-pocket-that-lays-open-lava-only-stops-when-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (round (pocket-world "lava") {:on-lava :stop}))]
          (is (not-any? #{[-2 64 0]} (calls p "place")) "nothing placed into the lava")
          (is (not-any? #{[0 64 0] [0 65 0]} (calls p "place")) "not sealed in beside it"))))))

(defn cell [x y z] {:x x :y y :z z})

(deftest open-shell-is-the-first-seen-open-neighbour-outside-keep
  (let [p (tu/seeing-all (tu/fake {:blocks (assoc (into {} (for [x (range -2 3) y (range 62 66) z (range -2 3)] [(key-of x y z) "stone"]))
                                                  (key-of 0 63 1) "air")}))]
    (is (= (cell 0 63 1) (refuge/open-shell p (cell 0 63 0) #{})))
    (is (nil? (refuge/open-shell p (cell 0 63 0) #{(cell 0 63 1)})) "its own cells and the way in do not count")
    (is (nil? (refuge/open-shell p (cell -1 63 -1) #{})) "rock all round")))

(defn sensing-stub
  "A body that has seen only the named cells ({[x y z] name}); every other cell is unknown."
  [seen]
  #js {:sensedAt (fn [pos] (let [k [(.-x pos) (.-y pos) (.-z pos)]]
                             (if-let [n (seen k)] #js {:name n :pos pos} #js {:unknown true :pos pos})))})

(deftest rock-name-guesses-stone-behind-a-seen-face-but-never-beside-a-seen-open-cell
  (let [face {[1 0 0] "stone"}]
    (is (= "stone" (dig-cells/rock-name (sensing-stub face) (cell 0 0 0))) "a seen solid face behind it")
    (is (nil? (dig-cells/rock-name (sensing-stub (assoc face [0 1 0] "air")) (cell 0 0 0))) "a seen open neighbour would have shown it")
    (is (nil? (dig-cells/rock-name (sensing-stub {}) (cell 0 0 0))) "nothing seen near")))

;; ------------------------------------------------------------------ the pit's first dig

(defn plain-world
  "Flat stone ground (top y 63) with the named hidden or seen blocks laid over it; a stone beside the start (side) makes
  the pit 2 deep, else it is 3."
  [extra side?]
  {:blocks (merge (into {} (for [x (range -3 10) y (range 58 64) z (range -3 4)] [(key-of x y z) "stone"]))
                  ;; a walled yard, 3x3 inside, a block clear over the ground: too low to leave by, and nothing beside the feet
                  (into {} (for [x (range -2 3) z (range -2 3) y (range 65 69)
                                 :when (or (= 2 (Math/abs x)) (= 2 (Math/abs z)))]
                             [(key-of x y z) "stone"]))
                  (when side? {(key-of 0 64 -1) "stone"})
                  extra)
   :inventory kit})

(defn ^:async see-plain! [raw p]
  (.setOwner p "t1")
  (await (look-at-cells! p (for [x (range -3 4) y [63 64] z (range -3 4)] [x y z])))
  p)

(defn ^:async pit-round
  "The retreat over a plain with a skeleton beside, the body having seen only the surface; {:p primitives :kinds event
  kinds :stood the block names of every cell the body's feet were in :feet-y where it ends}."
  [world args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (assoc world :entities [{:id 9 :name "skeleton" :kind "hostile" :pos {:x 1 :y 64 :z 1} :health 20}]))
        stood (atom #{})
        _ (add-watch (fake/state raw) ::stood
                     (fn [_ _ _ w] (swap! stood conj (fake/block-name w (mapv js/Math.floor (fake/body-pos w))))))
        p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (fn [] @clock)}))
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (await (see-plain! raw p))
    (core/submit! eng (list 'jobs.survival.retreat args) {})
    (let [r (core/tick! eng)]
      (await (sleep 700))
      (swap! (fake/state raw) assoc :entities [])
      (swap! clock + 300000)
      (await r))
    {:p p :kinds (set (map :kind @seen)) :stood @stood :feet-y (js/Math.floor (second (:pos (fake/self raw))))}))

(defn block-at [p x y z] (.-name (.blockAt p #js {:x x :y y :z z})))

(def deep-cells
  "Every cell a 3-deep pit from (-1 64 -1) may dig or drop onto: its own column and the side column (0 _ -1)."
  (for [x [-1 0] y [62 61 60]] [x y -1]))

(deftest a-pit-never-drops-the-body-into-lava-under-a-dug-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[x y z] (remove #{[0 62 -1] [-1 60 -1]} deep-cells)]
          (let [r (await (pit-round (plain-world {(key-of x y z) "lava"} false) {}))]
            (is (not ((:stood r) "lava")) (str "lava at " [x y z] ": the body never stands in it"))
            (is (not ((:kinds r) :shelter_seal_failed)) (str "lava at " [x y z] ": sealed while the body stood above it"))
            (is (not= "lava" (block-at (:p r) x y z)) (str "lava at " [x y z] ": filled before the drop"))
            (is ((:kinds r) :retreat_sealed) (str "lava at " [x y z] ": hidden in the pit"))))))))

(deftest lava-no-dig-lays-open-is-left-and-the-body-hides
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (pit-round (plain-world {(key-of -1 60 -1) "lava"} false) {}))]
          (is (not ((:stood r) "lava")) "the body never stands in it")
          (is (= "lava" (block-at (:p r) -1 60 -1)) "under the start column's last floor, never opened")
          (is ((:kinds r) :retreat_sealed) "hidden in the pit"))))))

(deftest lava-under-the-first-dug-cell-with-no-block-to-fill-it-stops-the-pit-before-the-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (pit-round (plain-world {(key-of 0 62 -1) "lava"} false) {}))]
          (is (not ((:stood r) "lava")) "the body never stands in it")
          (is ((:kinds r) :shelter_seal_failed) "nothing carried and the first drop not taken yet: the failed seal is said")
          (is (= "lava" (block-at (:p r) 0 62 -1)) "the lava is left")
          (is (not-any? #{[0 62 -1] [-1 62 -1]} (calls (:p r) "dig")) "the pit goes no deeper"))))))

(deftest a-2-deep-pit-never-drops-the-body-into-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [cell [[0 62 0] [1 62 0] [0 61 0] [1 61 0]]]
          (let [r (await (pit-round (plain-world {(apply key-of cell) "lava"} true) {}))]
            (is (not ((:stood r) "lava")) (str "lava at " cell ": the body never stands in it"))
            (is (not ((:kinds r) :shelter_seal_failed)) (str "lava at " cell ": sealed while the body stood above it"))))))))

(deftest a-pit-leaves-lava-alone-when-asked-and-never-enters-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[x y z] deep-cells]
          (let [r (await (pit-round (plain-world {(key-of x y z) "lava"} false) {:on-lava :stop}))]
            (is (not ((:stood r) "lava")) (str "lava at " [x y z] ": the body never stands in it"))
            (is (= "lava" (block-at (:p r) x y z)) "the lava is left")
            (is (not ((:kinds r) :shelter_seal_failed)) "no seal tried")))))))

(deftest a-pit-stops-at-water-before-the-body-drops-into-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[x y z] deep-cells]
          (let [r (await (pit-round (plain-world {(key-of x y z) "water"} false) {}))]
            (is (not-any? #(<= (second %) y) (calls (:p r) "dig")) (str "water at " [x y z] ": the pit stops when it shows"))))))))

(deftest a-pit-stops-at-a-seen-open-side-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (pit-round (plain-world {(key-of -1 63 0) "air"} false) {}))]
          (is (not-any? #(< (second %) 63) (calls (:p r) "dig")) "nothing dug below the level the open wall showed at"))))))

(deftest a-pit-in-plain-rock-hides-the-body-three-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (pit-round (plain-world {} false) {}))]
          (is ((:kinds r) :retreat_sealed) "hidden in the pit")
          (is (= 61 (:feet-y r)) "three down"))))))

(deftest a-zigzag-pit-digs-beside-the-body-and-drops-onto-floors-it-can-see
  (let [{:keys [steps plugs roof]} (dig-cells/pit-steps (cell 0 64 0) [1 0] 2)]
    (is (= [[(cell 1 63 0)] [(cell 0 63 0) (cell 0 62 0)]] (mapv :dig steps)) "no dig under the feet")
    (is (= [(cell 1 63 0) (cell 0 62 0)] (mapv :to steps)))
    (is (= [(cell 1 62 0) (cell 0 61 0)] (mapv :floor steps)) "each floor 2 under the feet of the cell dropped from, in the other column")
    (is (= [(cell 1 63 0)] plugs) "the side cell beside the head")
    (is (= (cell 0 64 0) roof)))
  (is (= [[(cell 0 63 0)] [(cell 0 62 0)]] (mapv :dig (:steps (dig-cells/pit-steps (cell 0 64 0) nil 2)))) "straight down"))
