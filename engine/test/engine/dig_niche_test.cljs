(ns engine.dig-niche-test
  "jobs.survival.dig-niche's choice of site: a solid face with a shell all round, never flat ground, fluid or sand."
  (:require [cljs.test :refer [deftest is]]
            [engine.test-util :as tu]
            [jobs.survival.dig-niche :as niche]))

(defn blocks [name [x0 x1] [y0 y1] [z0 z1]]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(def ground (blocks "dirt" [-2 8] [60 63] [-3 3]))

(defn site
  ([extra] (site extra [{:name "iron_pickaxe" :count 1}]))
  ([extra inventory] (niche/find-site (tu/fake {:blocks (merge ground extra) :inventory inventory}) 16)))

(deftest a-hill-with-a-shell-gives-a-site-facing-it
  (is (= {:stand {:x 3 :y 64 :z 0} :dir [1 0]} (site (blocks "stone" [4 8] [64 66] [-3 3])))))

(deftest flat-ground-has-no-site
  (is (nil? (site {}))))

(deftest a-face-too-thin-or-low-for-the-shell-has-no-site
  (is (nil? (site (blocks "stone" [4 8] [64 65] [-3 3]))) "no ceiling over the niche")
  (is (nil? (site (blocks "stone" [4 8] [64 66] [0 0]))) "no side walls"))

(defn ok? [extra]
  (niche/niche-ok? (tu/fake {:blocks (merge ground (blocks "stone" [4 8] [64 66] [-3 3]) extra)
                             :inventory [{:name "iron_pickaxe" :count 1}]})
                   {:x 3 :y 64 :z 0} [1 0]))

(deftest sand-gravel-or-water-in-or-round-the-niche-rules-it-out
  (is (true? (ok? {})))
  (is (false? (ok? (blocks "sand" [4 8] [64 66] [-3 3]))) "sand falls")
  (is (false? (ok? {"5,66,0" "gravel"})) "gravel over the niche")
  (is (false? (ok? {"4,64,1" "water"})) "water beside it")
  (is (false? (ok? {"6,64,0" "water"})) "water behind it"))

(deftest stone-needs-a-pickaxe-the-body-carries
  (is (nil? (site (blocks "stone" [4 8] [64 66] [-3 3]) [])))
  (is (some? (site (blocks "dirt" [4 8] [64 66] [-3 3]) []))))
