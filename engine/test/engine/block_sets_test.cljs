(ns engine.block-sets-test
  "jobs.lib.block-sets: the named sets nest as their callers rely on."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.block-sets :as bs]
            [jobs.survival.retreat-walk :as walk]))

(deftest passable-names-are-the-collision-free-ones
  (are [n] (bs/passable? n)
    "air" "light" "short_grass" "cornflower" "azure_bluet" "lilac" "rail" "powered_rail" "redstone_wire"
    "oak_sign" "white_carpet" "torch" "wall_torch" "oak_sapling" "snow" "sweet_berry_bush")
  (are [n] (not (bs/passable? n))
    nil "cobweb" "tripwire" "stone" "water" "lava" "cactus" "oak_fence" "oak_leaves"))

(deftest a-walker-swims-water-and-treats-an-unloaded-cell-as-open
  (is (bs/passable-or-water? "water"))
  (is (not (bs/passable-or-water? "lava")))
  (is (walk/passable? nil))
  (are [n] (walk/passable? n) "cornflower" "rail" "light" "water"))

(deftest the-harm-sets-nest
  (is (every? bs/bad-floor bs/harms-in-cell))
  (is (every? #(not (bs/passable? %)) (disj bs/harms-in-cell "sweet_berry_bush" "wither_rose")) "a walker refuses what it cannot pass")
  (is (not (some bs/replaceable (disj bs/harms-in-cell "lava")))))
