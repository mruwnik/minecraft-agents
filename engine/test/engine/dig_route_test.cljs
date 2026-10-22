(ns engine.dig-route-test
  (:require [cljs.test :refer [deftest is testing]]
            [engine.settings :as settings]
            [jobs.lib.dig-route :as dr]))

(defn world
  "block-at over a stone mass below y 10 with the given cells overridden."
  [overrides]
  (fn [[x y z :as c]] (or (overrides c) (if (< y 10) "stone" "air"))))

(deftest a-pit-beside-the-body-needs-no-dig-when-open
  (is (= {:dig []} (dr/route (world {[1 9 0] "air" [1 8 0] "air"}) [0 10 0] [1 8 0] {}))))

(deftest a-sealed-pocket-is-dug-down-in-walking-order
  (let [r (dr/route (world {[3 8 0] "air"}) [0 10 0] [3 8 0] {})]
    (is (seq (:dig r)))
    (is (not (some #{[3 8 0]} (:dig r))) "the drop's own cell is open already")
    (is (<= (count (:dig r)) 12))))

(deftest lava-on-the-way-is-not-dug-through
  (let [lava (into {} (for [x (range -5 10) z (range -5 5) y [8 9]] [[x y z] "lava"]))]
    (is (= {:problem :no-route} (dr/route (world (assoc lava [3 8 0] "air")) [0 10 0] [3 8 0] {})))))

(deftest too-many-digs-is-no-route
  (let [deep (fn [c] (or ({[30 8 0] "air" [0 20 0] "air" [0 21 0] "air"} c) (if (< (c 1) 20) "stone" "air")))]
    (is (= {:problem :no-route} (dr/route deep [0 20 0] [30 8 0] {:max-digs 5 :max-nodes 100000})))))

(deftest a-step-down-does-not-dig-above-the-head
  (let [low (fn [c] (or ({[1 9 0] "air" [1 8 0] "air"} c) (if (or (< (c 1) 10) (>= (c 1) 12)) "stone" "air")))]
    (is (= {:dig []} (dr/route low [0 10 0] [1 8 0] {})))))

(deftest the-fall-limit-is-an-option
  (let [w (world {[1 9 0] "air" [1 8 0] "air" [1 7 0] "air" [1 6 0] "air"})]
    (is (= {:dig []} (dr/route w [0 10 0] [1 6 0] {:max-fall 4})))
    (is (seq (:dig (dr/route w [0 10 0] [1 6 0] {:max-fall 1}))))))

(deftest digs-are-priced-by-the-callers-seconds-per-block
  (let [hard (into {} (for [y (range 10 14) z (range -3 4)] [[1 y z] "obsidian"]))
        w (fn [c] (or ({[3 10 0] "air" [3 11 0] "air" [-1 10 0] "air" [-1 11 0] "air"} c)
                      (hard c)
                      (if (< (c 1) 10) "stone" "dirt")))]
    (is (some (comp #{"obsidian"} w) (:dig (dr/route w [-1 10 0] [3 10 0] {})))
        "by count the way through the obsidian wall is the shortest")
    (is (not-any? (comp #{"obsidian"} w) (:dig (dr/route w [-1 10 0] [3 10 0] {:dig-s #(if (= "obsidian" %) 50 1) :step-s 0.2})))
        "priced, it digs under the wall")))

(deftest the-default-limits-are-settings
  (let [w (world {[3 8 0] "air"})]
    (is (seq (:dig (dr/route w [0 10 0] [3 8 0] {}))))
    (settings/with-settings {:jobs.lib.dig-route/max-digs 0}
      (fn [] (is (= {:problem :no-route} (dr/route w [0 10 0] [3 8 0] {})))))))

(deftest a-cell-under-sand-or-gravel-is-not-dug-and-the-route-goes-round
  (let [at (fn [roof] (fn [[x y z :as c]] (or (get roof c) (if (< y 12) "stone" "air"))))
        roof {[1 11 0] "gravel" [2 11 0] "sand" [3 11 0] "gravel"}
        free {[0 9 0] "air" [0 10 0] "air" [3 9 0] "air" [3 10 0] "air"}
        opened (fn [r] (set (:dig r)))]
    (is (= {:problem :no-route} (dr/route (at (merge roof free)) [0 9 0] [3 9 0] {:margin 0})) "the straight line is all under falling blocks")
    (is (not-any? (comp #{[1 10 0] [2 10 0]} identity) (opened (dr/route (at (merge roof free)) [0 9 0] [3 9 0] {:margin 2 :max-digs 30}))) "the detour opens nothing under them")))

(deftest a-cell-is-priced-once-however-many-steps-reach-it
  (let [calls (atom [])
        dig-s (fn [n cell] (swap! calls conj [n cell]) 1)]
    (dr/route (world {[8 8 8] "air"}) [0 10 0] [8 8 8] {:dig-s dig-s :step-s 0.5})
    (is (= (count @calls) (count (set @calls))))))

(deftest a-walker-passes-a-gate-and-a-plant-but-not-a-fence-or-wall
  (is (every? dr/open? ["air" "oak_fence_gate" "oak_sapling" "poppy"]))
  (is (not-any? dr/open? ["oak_fence" "cobblestone_wall" "stone" nil])))

(deftest a-far-search-stops-at-the-node-budget
  (let [goal [76 52 96]
        reads (atom #{})
        w (fn [c] (swap! reads conj c) (cond (#{goal (update goal 1 inc)} c) "air" (< (c 1) 64) "stone" :else "air"))]
    (is (= {:problem :budget} (dr/route w [0 64 0] goal {:dig-s (constantly 0.5) :step-s 0.2 :max-nodes 500})))
    (is (< (count @reads) 6000) "cells read: about a dozen a node expanded, not the whole box between")))
