(ns engine.seen-work-reads-c-test
  "Mine, explore.search and the build readers read cells from what the body sees or remembers: behind stone a cell is
  unknown (nil), in view it is used. The primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.ctx :as ctx]
            [engine.seen-work-reads-test :refer [wrapped]]
            [jobs.build.from-plan :as from-plan]
            [jobs.explore.search :as search]
            [jobs.gather.mine :as mine]))

(def far {:x 0 :y 64 :z 4})

(defn ground [z] (into {} (for [y [62 63] x (range -2 3) :let [k (str x "," y "," z)]] [k "stone"])))

(deftest mine-snapshot-skips-ground-behind-stone
  (let [blocks (merge (ground 3) (ground 4) (ground 5))
        snap (fn [w] (mine/snapshot {:primitives w} {:x 0 :y 64 :z 4}))]
    (is (= 15 (count (snap (wrapped blocks false)))) "in view: the top layer, the one under it is hidden")
    (is (empty? (snap (wrapped blocks true))) "behind a wall: unknown, not recorded")))

(deftest mine-cut-hazard-needs-sight-of-the-cell
  (let [blocks {"0,64,4" "stone" "0,65,4" "stone"}
        hazard (fn [w] (mine/cut-hazard {:primitives w :args {}} far))]
    (is (= {:reason :not-loaded :at far} (hazard (wrapped blocks true))) "unseen cell is unknown")
    (is (nil? (hazard (wrapped blocks false))) "seen stone: no hazard")))

(deftest mine-owed-ignores-unseen-ground
  (let [blocks {"0,64,4" "air"}
        owed (fn [w] (with-redefs [ctx/mem (constantly {:ground [{:pos far :name "stone"}]})]
                       (mine/owed {:primitives w})))]
    (is (empty? (owed (wrapped blocks true))) "behind stone: not judged owed")
    (is (= 1 (count (owed (wrapped blocks false)))) "seen air: owed")))

(deftest explore-stand-reader-is-nil-for-an-unseen-cell
  (let [blocks {"0,64,4" "stone"}]
    (is (nil? ((search/seen-at (wrapped blocks true)) [0 64 4])))
    (is (= "stone" ((search/seen-at (wrapped blocks false)) [0 64 4])))))

(deftest build-world-block-needs-sight
  (let [blocks {"0,64,4" "stone"}]
    (is (nil? (from-plan/world-block (wrapped blocks true) [0 64 4])))
    (is (= {:name "stone"} (from-plan/world-block (wrapped blocks false) [0 64 4])))))
