(ns engine.walk-world-unknown-test
  "The unseen mask of the walks' pathWorld (jobs.lib.walk.world/path-world with an unknown spec): cells the body never saw
  or felt read as void_air, known ones as they are now; the mask and its options ride on the snapshot through every with-*,
  into the planner's options.unknown and the kept searches' keys. Without the spec: as before."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.path.blocks :as blocks]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.walk.plan :as wplan]
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

(deftest the-spec-rides-on-the-snapshot-through-every-decoration
  (let [{:keys [w]} (sensed wall-world)
        spec (wworld/unknown-spec)
        pw (wworld/path-world w spec)
        missed (.-missed (wworld/unknown-of pw))]
    (is (identical? (.-missed spec) missed) "one missed Set per spec: every pathWorld of a go-to attempt shares it")
    (doseq [decorated [(wworld/with-walls pw [{:x 1 :y 64 :z 0}]) (wworld/with-dangers pw #js [])
                       (wworld/with-avoid pw [[1 64 0]]) (wworld/with-tolls pw [{:x 1 :y 64 :z 0 :factor 2}])
                       (wworld/with-dark pw {:at (fn [_ _ _] 0)} nil)]]
      (is (identical? missed (.-missed (wworld/unknown-of decorated))))
      (is (= @void (state decorated 3 64 0))))
    (is (identical? (wworld/unknown-of pw) (.-unknown (wplan/plan-options pw 1 nil nil))) "the planner's options.unknown")))

(deftest a-masked-search-ends-at-the-unknown-node-limit-a-plain-one-keeps-the-planners
  (let [{:keys [w]} (sensed wall-world)
        masked (wworld/path-world w (wworld/unknown-spec))
        plain (wworld/path-world w)]
    (is (= 100000 (.-maxNodes (wplan/plan-options masked 1 nil nil))) "a goal no seen route reaches floods unseen cells: stops at the setting")
    (is (= 5000 (.-maxNodes (settings/with-settings {:jobs.lib.walk.world/unknown-max-nodes 5000} #(wplan/plan-options masked 1 nil nil)))))
    (is (undefined? (.-maxNodes (wplan/plan-options plain 1 nil nil))) "an unmasked search keeps the planner's 200000")))

(deftest the-spec-holds-the-prices-the-mask-id-and-sight
  (let [{:keys [w]} (sensed wall-world)
        u (wworld/unknown-of (wworld/path-world w (settings/with-settings {:jobs.lib.cost.planner/unknown-prices {:peek-s 2}} wworld/unknown-spec)))]
    (is (= [@void 2 (:aim-s cost/unknown-prices) (:cell cost/unknown-prices) (:floor cost/unknown-prices)]
           [(.-id u) (.-peekS u) (.-aimS u) (.-cell u) (.-floor u)]))
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
