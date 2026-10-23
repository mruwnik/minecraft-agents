(ns engine.walk-world-unknown-test
  "The unseen mask of the walks' pathWorld (jobs.lib.walk.world/path-world with an unknown spec): cells the body never saw
  or felt read as void_air, known ones as they are now; the mask and its options ride on the snapshot through every with-*,
  into the planner's options.unknown and the kept searches' keys. Without the spec: as before. Under ground the cells
  far from all it saw open read as rock (jobs.lib.walk.rock)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.path.blocks :as blocks]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.rock :as rock]
            [jobs.lib.walk.world :as wworld]))

(defn sensed
  "Fake primitives for spec wrapped with a perception that has made one sight pass from the body."
  [spec]
  (let [p (tu/fake spec)
        raw (fake-raw/create p)
        per (perception/create raw {:now (constantly 1000000)})
        w (perception/wrap p per)]
    (aset w "rawWorld" raw)
    (perception/pass! per)
    {:p p :w w}))

(def wall-world
  "Stone ground, a stone wall at x 2 (y 64..65, z -1..1), oak planks behind it at x 3; the body at the origin facing east."
  {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 30
   :blocks (merge (box -4 63 -4 8 63 4 "stone") (box 2 64 -1 2 65 1 "stone") {"3,64,0" "oak_planks"})})

(def void (delay (first (blocks/state-ids (blocks/default-state-table) "void_air"))))
(def stone (delay (first (blocks/state-ids (blocks/default-state-table) "stone"))))

(defn state [pw x y z] (.stateAt ^js (.-snapshot pw) x y z))

(deftest without-a-spec-the-world-reads-as-it-is
  (let [{:keys [w]} (sensed wall-world)
        pw (wworld/path-world w)]
    (is (not= @void (state pw 3 64 0)) "behind the wall: x-ray as before")
    (is (nil? (wworld/unknown-of pw)))
    (is (nil? (wworld/unknown-key pw)))))

(deftest with-a-spec-unseen-cells-read-void-air-and-seen-ones-live
  (let [{:keys [w]} (sensed wall-world)
        pw (wworld/path-world w (wworld/unknown-spec))]
    (is (= @stone (state pw 2 64 0)) "the wall face, seen")
    (is (= @void (state pw 3 64 0)) "behind the wall, never seen")
    (is (= @stone (state pw 1 63 0)) "the floor ahead, seen")))

(deftest an-unseen-cell-is-read-as-water-where-the-walk-reads-water
  (let [world (assoc wall-world :blocks (merge (:blocks wall-world) {"3,64,0" "water"}))
        {:keys [w]} (sensed world)
        wet? (fn [pw] ((wworld/water-read-fn pw) 3 64 0))]
    (is (true? (wet? (wworld/path-world w))) "unmasked: the block the planner swims")
    (is (false? (wet? (wworld/path-world w (wworld/unknown-spec)))) "masked with the head out of water: void_air")))

(deftest the-spec-rides-on-the-snapshot-through-every-decoration
  (let [{:keys [w]} (sensed wall-world)
        spec (wworld/unknown-spec)
        pw (wworld/path-world w spec)
        missed (.-missed (wworld/unknown-of pw))]
    (is (identical? (.-missed spec) missed) "one missed Set per spec: every pathWorld of a go-to attempt shares it")
    (doseq [decorated [(wworld/with-walls pw [{:x 1 :y 64 :z 0}]) (wworld/with-dangers pw #js [])
                       (wworld/with-avoid pw [[1 64 0]]) (wworld/with-tolls pw [{:x 1 :y 64 :z 0 :factor 2}])
                       (wworld/with-dark pw {:at (fn [_ _ _] 0)} nil nil)]]
      (is (identical? missed (.-missed (wworld/unknown-of decorated))))
      (is (= @void (state decorated 3 64 0))))
    (is (identical? (wworld/unknown-of pw) (.-unknown (wplan/plan-options pw 1 nil nil))) "the planner's options.unknown")))

(deftest a-masked-search-ends-at-the-unknown-node-limit-a-plain-one-keeps-the-planners
  (let [{:keys [w]} (sensed wall-world)
        masked (wworld/path-world w (wworld/unknown-spec))
        plain (wworld/path-world w)]
    (is (= 12000 (.-maxNodes (wplan/plan-options masked 1 nil nil))) "a goal no seen route reaches floods unseen cells: stops at the setting")
    (is (= 5000 (.-maxNodes (settings/with-settings {:jobs.lib.walk.world/unknown-max-nodes 5000} #(wplan/plan-options masked 1 nil nil)))))
    (is (undefined? (.-maxNodes (wplan/plan-options plain 1 nil nil))) "an unmasked search keeps the planner's 200000")))

(deftest the-spec-holds-the-prices-the-mask-id-and-sight
  (let [{:keys [w]} (sensed wall-world)
        u (wworld/unknown-of (wworld/path-world w (settings/with-settings {:jobs.lib.cost.planner/unknown-prices {:peek-s 2}} wworld/unknown-spec)))]
    (is (= [@void 2 (:aim-s cost/unknown-prices) (wworld/unseen-share w)]
           [(.-id u) (.-peekS u) (.-aimS u) (.-cell u)]))
    (is (= 1 (.-seeDepth u)) "no torch in hand: near 4")
    (is (fn? (.-seeLit u)))))

(deftest a-torch-in-the-off-hand-sees-deeper
  (let [{:keys [w]} (sensed (assoc wall-world :equipment {"offHand" {:name "torch" :count 1}}))]
    (is (= 4 (.-seeDepth (wworld/unknown-of (wworld/path-world w (wworld/unknown-spec))))))))

(deftest the-key-changes-with-each-missed-floor
  (let [{:keys [w]} (sensed wall-world)
        pw (wworld/path-world w (wworld/unknown-spec))
        before (wworld/unknown-key pw)]
    (.add (.-missed (wworld/unknown-of pw)) "x")
    (is (some? before))
    (is (not= before (wworld/unknown-key pw)))))

(deftest see-lit-reads-the-visible-table-on-live-light
  (let [{:keys [p w]} (sensed wall-world)
        lit? (look/see-lit-fn w)]
    (is (= 1 (lit? 1 64 0)) "daylight")
    (swap! (fake/state p) assoc :light-default [0 0])
    (is (= 0 ((look/see-lit-fn w) 1 64 0)) "dark")))

(deftest a-masked-plan-at-night-prices-unseen-cells-once
  (let [{:keys [p w]} (sensed wall-world)]
    (swap! (fake/state p) assoc :time 18000)
    (is (= 1 ((:at (look/dark-fn w)) 3 64 0)) "unmasked: an unseen cell is dark at night")
    (is (= 0 ((:at (look/dark-fn w {:unseen-dark? false})) 3 64 0)) "masked: the mask prices it")))

(def air-id (delay (first (blocks/state-ids (blocks/default-state-table) "air"))))

(defn memory-of
  "(fn [cx sy cz]) a memory (ids + 1) holding the cells [x y z] as air (or [x y z id] as that state), nil for a section with none of them."
  [cells]
  (let [m (reduce (fn [m [x y z id]]
                    (let [k [(bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)]
                          ids (or (get m k) (js/Uint16Array. 4096))]
                      (aset ids (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15)) (inc (or id @air-id)))
                      (assoc m k ids)))
                  {} cells)]
    (fn [cx sy cz] (get m [cx sy cz]))))

(defn rock? [known seeds [x y z]]
  (== 1 (aget (rock/rock-section known (rock/open-ids) 3 (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4) seeds)
              (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15)))))

(deftest an-unseen-cell-is-rock-past-3-blocks-of-every-seen-open-cell-and-seed
  (let [known (memory-of [[5 70 5] [15 66 5]])]
    (are [cell rock] (= rock (rock? known [] cell))
      [8 70 5] false
      [9 70 5] true
      [8 73 8] false
      [2 67 2] false
      [1 70 5] true
      [18 66 5] false
      [19 66 5] true
      [-14 70 5] true)
    (is (false? (rock? known [[40 60 -9]] [42 58 -11])) "2 from a seed")
    (is (identical? rock/all-rock (rock/rock-section (memory-of []) (rock/open-ids) 3 0 4 0 [])) "nothing seen near: all rock")))

(def ladder-id (delay (first (blocks/state-ids (blocks/default-state-table) "ladder"))))
(def stone-id (delay (first (blocks/state-ids (blocks/default-state-table) "stone"))))

(deftest an-unseen-cell-straight-above-a-seen-ladder-is-not-rock-until-a-seen-block-ends-the-shaft
  (let [ladder (for [y (range 66 70)] [5 y 5 @ladder-id])
        shaft (memory-of ladder)
        capped (memory-of (conj (vec ladder) [5 75 5 @stone-id]))]
    (are [known cell rock] (= rock (rock? known [] cell))
      shaft [5 90 5] false
      shaft [6 90 5] true
      shaft [5 50 5] true
      capped [5 74 5] false
      capped [5 76 5] true)))

(def pocket-world
  "A body in a 1x2 pocket in a stone mass (x, z -8..8, y 62..67)."
  {:self {:pos {:x 0.5 :y 64 :z 0.5}}
   :blocks (dissoc (box -8 62 -8 8 67 8 "stone") "0,64,0" "0,65,0")})

(defn pocket
  "pocket-world's pathWorld with spec, in sky light sky."
  [sky spec]
  (let [{:keys [p w]} (sensed pocket-world)]
    (swap! (fake/state p) assoc :light-default [sky 0])
    (wworld/path-world w spec)))

(deftest under-ground-the-unseen-cells-far-from-all-seen-open-read-stone
  (let [buried (pocket 0 (wworld/unknown-spec))
        sky (pocket 15 (wworld/unknown-spec))
        off (settings/with-settings {:jobs.lib.walk.world/rock-beyond false} #(pocket 0 (wworld/unknown-spec)))
        goal (pocket 0 (wworld/with-seeds (wworld/unknown-spec) [7 64 0] nil))
        own (pocket 0 (wworld/with-seeds (wworld/unknown-spec) nil [[-7 64 0]]))]
    (is (= [@void @stone @void] (map #(state buried % 64 0) [3 4 -3])) "3 from the feet unseen, 4 rock")
    (is (= [@void @void] (map #(state % 4 64 0) [sky off])) "under the sky, or the setting off: unseen")
    (is (= @void (state goal 4 64 0)) "3 from the goal")
    (is (= [@void @stone] [(state own -4 64 0) (state own 4 64 0)]) "3 from a cell of its own ways")
    (is (not= (wworld/unknown-key buried) (wworld/unknown-key sky)) "the guess is in a kept search's key")))
