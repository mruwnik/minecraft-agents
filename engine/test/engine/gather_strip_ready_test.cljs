(ns engine.gather-strip-ready-test
  "The safe? and fit? exports of the gather, strip and forestry jobs (jobs.lib.ready), as statuses over a hut at night and a
  zombie by day; and that a job no longer holds itself back for night or danger in can-start?."
  (:require [cljs.test :refer [deftest is are]]
            [engine.ready-test :as rt]
            [engine.shelter-support :as st]
            [jobs.forestry.collect-drops :as collect-drops]
            [jobs.forestry.fell-from-pillar :as fell-from-pillar]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.forestry.harvest-wood :as harvest-wood]
            [jobs.forestry.plant-sapling :as plant-sapling]
            [jobs.forestry.tidy :as tidy]
            [jobs.gather.get-seeds :as get-seeds]
            [jobs.gather.make-obsidian :as make-obsidian]
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
  (are [check] (= [:later :ok] [(kind check rt/night-hut {}) (kind check rt/tunnel {})])
    get-seeds/safe? make-obsidian/safe? mine/safe? harvest-wood/safe? tidy/safe? collect-drops/safe?))

(deftest jobs-going-to-a-place-judge-that-place
  (are [check arg] (= [:ok :later] [(kind check rt/night-hut {arg inside}) (kind check rt/night-hut {arg outside})])
    fell-tree/safe? :at
    fell-from-pillar/safe? :base
    plant-sapling/safe? :at
    seal/safe? :at
    loot-cart/safe? :near
    site/safe? :at))

(deftest fell-tree-and-loot-cart-without-a-place-roam
  (are [check] (= :later (kind check rt/night-hut {}))
    fell-tree/safe? loot-cart/safe?))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})
(def segment {:y 64 :axis :x :line 0 :from 0 :to 8 :open-ends #{}})
(def zombie-day (assoc rt/open-night :time st/noon :entities [zombie]))

(deftest a-corridor-with-a-reachable-zombie-is-later-through-safe
  (is (= [:later :ok]
         [(kind corridor/safe? zombie-day {:segment segment})
          (rt/lit 0 15 #(kind corridor/safe? (dissoc zombie-day :entities) {:segment segment}))])))

(deftest can-start-no-longer-holds-for-danger-or-night
  (are [check world args] (= :ok (kind check world args))
    corridor/can-start? zombie-day {:segment segment :danger-check true}))

(deftest fit-is-later-only-below-the-risky-health
  (are [check] (= :ok (kind check rt/night-hut {}))
    get-seeds/fit? mine/fit? collect-drops/fit? seal/fit?))
