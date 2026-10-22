(ns engine.gather-strip-ready-test
  "The safe? and fit? exports of the gather, strip and forestry jobs (jobs.lib.ready), as statuses over a hut at night and a
  zombie by day; and that a job no longer holds itself back for night or danger in can-start?."
  (:require [cljs.test :refer [deftest is are]]
            [engine.ready-test :as rt]
            [engine.shelter-support :as st]
            [jobs.animals.shear :as shear]
            [jobs.combat.hunt :as hunt]
            [jobs.gather.strip-tunnel :as strip-tunnel]
            [jobs.items.collect-drops :as collect-drops]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.forestry.harvest-wood :as harvest-wood]
            [jobs.forestry.plant-sapling :as plant-sapling]
            [jobs.gather.get-seeds :as get-seeds]
            [jobs.gather.cast-obsidian :as cast-obsidian]
            [jobs.gather.mine :as mine]
            [jobs.strip.corridor :as corridor]
            [jobs.strip.loot-cart :as loot-cart]
            [jobs.strip.seal :as seal]
            [jobs.strip.site :as site]))

(def inside [5 64 0])
(def outside [15 64 0])

(defn ctx [world args] (assoc (rt/ctx-of world) :args args))

(defn kind [check world args] (first (check (ctx world args))))

(deftest roaming-jobs-wait-at-night-above-ground
  (are [check] (= [:later :ok] [(kind check rt/open-night {}) (kind check rt/tunnel {})])
    get-seeds/safe? cast-obsidian/safe? mine/safe? harvest-wood/safe? collect-drops/safe?))

(deftest roaming-jobs-do-not-leave-a-room-under-a-dark-sky
  (are [check args] (= [:later :ok] [(kind check rt/night-hut args) (kind check rt/tunnel args)])
    harvest-wood/safe? {:radius 16 :ignore-zones? true}
    hunt/safe? {:radius 24}
    shear/safe? {:radius 16}))

(def sheep-outside {:id 8 :name "sheep" :kind "passive" :pos {:x 15 :y 64 :z 0}})

(deftest animal-jobs-judge-the-animal-they-walk-to
  (is (= [:later :ok]
         [(kind shear/safe? (assoc rt/night-hut :entities [sheep-outside]) {:radius 16})
          (kind shear/safe? (assoc rt/night-hut :entities [(assoc sheep-outside :pos {:x 3 :y 64 :z 0})]) {:radius 16})])
      "a sheep outside the hut is a walk into the dark; one inside it is not"))

(deftest a-tunnel-dug-from-a-hut-at-night-is-not-a-walk-out
  (is (= :ok (kind strip-tunnel/safe? rt/night-hut {:heading "east"}))))

(deftest jobs-going-to-a-place-judge-that-place
  (are [check arg] (= [:ok :later] [(kind check rt/night-hut {arg inside}) (kind check rt/night-hut {arg outside})])
    fell-tree/safe? :at
    plant-sapling/safe? :at
    seal/safe? :at
    loot-cart/safe? :near
    site/safe? :at))

(deftest fell-tree-and-loot-cart-without-a-place-roam
  (are [check] (= :later (kind check rt/open-night {}))
    fell-tree/safe? loot-cart/safe?))

(def tree-outside
  (update rt/night-hut :blocks merge {"15,64,0" "oak_log" "15,65,0" "oak_log" "15,66,0" "oak_log" "15,67,0" "oak_leaves"}))

(deftest fell-tree-without-an-at-judges-the-tree-it-would-walk-to
  (is (= [:later :ok] [(kind fell-tree/safe? tree-outside {:radius 16 :ignore-zones? true}) (kind fell-tree/safe? rt/night-hut {:radius 16 :ignore-zones? true})])
      "a sheltered body is not sent out to a tree in the dark; with no tree there is nothing to leave for"))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})
(def segment {:y 64 :axis :x :line 0 :from 0 :to 8 :open-ends #{}})
(def zombie-day (assoc rt/open-night :time st/noon :entities [zombie]))

(deftest a-corridor-with-a-reachable-zombie-is-later-through-safe
  (is (= [:later :ok]
         [(kind corridor/safe? zombie-day {:segment segment})
          (rt/lit 0 15 #(kind corridor/safe? (dissoc zombie-day :entities) {:segment segment}))])))

(deftest can-start-no-longer-holds-for-danger-or-night
  (are [check world args] (= :ok (kind check world args))
    corridor/can-start? zombie-day {:segment segment}))

(deftest fit-is-later-only-below-the-risky-health
  (are [check] (= :ok (kind check rt/night-hut {}))
    get-seeds/fit? mine/fit? collect-drops/fit? seal/fit?))
