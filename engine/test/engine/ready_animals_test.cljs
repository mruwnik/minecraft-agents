(ns engine.ready-animals-test
  "The safe? and fit? exports of the animal, apiary and village jobs: the place each judges and the checker it binds."
  (:require [cljs.test :refer [deftest is are]]
            [engine.pen-gate-test :as pg]
            [engine.ready-test :as rt]
            [engine.shelter-support :as st]
            [jobs.animals.breed :as breed]
            [jobs.animals.cull :as cull]
            [jobs.animals.herd :as herd]
            [jobs.animals.lead-to :as lead-to]
            [jobs.animals.leash :as leash]
            [jobs.animals.lure-to :as lure-to]
            [jobs.animals.pen-check :as pen-check]
            [jobs.animals.shear :as shear]
            [jobs.animals.shut-gate :as shut-gate]
            [jobs.animals.tend :as tend]
            [jobs.animals.unleash :as unleash]
            [jobs.apiary.guard :as guard]
            [jobs.apiary.harvest :as harvest]
            [jobs.apiary.maintain :as apiary-maintain]
            [jobs.lib.world-files :as world]
            [jobs.village.breed :as village-breed]
            [jobs.village.feed :as feed]
            [jobs.village.maintain :as village-maintain]
            [jobs.village.roll :as roll]
            [jobs.village.trade :as trade]))

(def hut-cell {:x 5 :y 64 :z 0})
(def far-cell {:x 15 :y 64 :z 0})

(def plans
  {"hut-gate" (pg/pen-plan "hut-gate" [5 64 0])
   "far-gate" (pg/pen-plan "far-gate" [20 64 0])
   "hut-part" {:id "hut-part" :parts [{:id "a" :cells [[5 64 0]] :want "oak_planks"}]}
   "far-part" {:id "far-part" :parts [{:id "a" :cells [[15 64 0]] :want "oak_planks"}]}
   "pens" {:id "pens" :parts [{:id "hut" :box [[5 64 0] [5 64 0]] :herd {:mob "cow" :target 2}}
                              {:id "far" :box [[15 64 0] [15 64 0]] :herd {:mob "cow" :target 2}}]}})

(defn status
  "The status of check over a night hut, the body inside, the args and the plans given."
  [check args & [world]]
  (let [c (-> (rt/ctx-of (or world rt/night-hut))
              (assoc :args args)
              (assoc-in [:engine :world] (world/of-data plans {})))]
    (rt/lit 0 15 #(first (check c)))))

(deftest roaming-jobs-wait-for-dawn-above-ground-and-work-by-day
  (are [check world expected] (= expected (status check {} world))
    breed/safe? rt/open-night :later
    herd/safe? rt/open-night :later
    lead-to/safe? rt/open-night :later
    leash/safe? rt/open-night :later
    lure-to/safe? rt/open-night :later
    shear/safe? rt/open-night :later
    village-breed/safe? rt/open-night :later
    feed/safe? rt/open-night :later
    breed/safe? (assoc rt/open-night :time st/noon) :ok
    feed/safe? (assoc rt/open-night :time st/noon) :ok))

(deftest staying-jobs-judge-where-the-body-stands
  (are [check world expected] (= expected (status check {} world))
    unleash/safe? rt/night-hut :ok
    trade/safe? rt/night-hut :ok
    unleash/safe? rt/open-night :later
    trade/safe? rt/open-night :later))

(def far-box {:min far-cell :max far-cell})
(def hut-box {:min hut-cell :max hut-cell})

(deftest area-jobs-judge-the-middle-of-their-area-at-night
  (are [check args expected] (= expected (status check args))
    cull/safe? {:box hut-box} :ok
    cull/safe? {:box far-box} :later
    cull/safe? {:center {:x 15 :z 0}} :later
    tend/safe? {:box hut-box} :ok
    tend/safe? {:box far-box} :later
    pen-check/safe? {:box hut-box} :ok
    pen-check/safe? {:box far-box} :later
    pen-check/safe? {:at [15 64 0]} :later
    guard/safe? {:box {:from hut-cell :to hut-cell}} :ok
    guard/safe? {:box {:from far-cell :to far-cell}} :later
    harvest/safe? {:center far-cell} :later
    apiary-maintain/safe? {:center hut-cell} :ok
    apiary-maintain/safe? {:center far-cell} :later
    roll/safe? {:pos [5 64 0]} :ok
    roll/safe? {:pos [15 64 0]} :later
    village-maintain/safe? {:plan "hut-part"} :ok
    village-maintain/safe? {:plan "far-part"} :later))

(deftest a-pen-reached-by-plan-is-judged-at-its-box
  (are [part expected] (= expected (status tend/safe? {:plan "pens" :part part}))
    "hut" :ok
    "far" :later))

(deftest shut-gate-judges-the-gates-of-its-plan
  (are [plan expected] (= expected (status shut-gate/safe? {:plan plan}))
    "hut-gate" :ok
    "far-gate" :later))

(deftest fit-jobs-rest-below-the-risky-health-and-ok-jobs-never
  (are [check expected] (= expected (status check {} {:self {:pos {:x 0 :y 64 :z 0} :health 5}}))
    breed/fit? :later
    cull/fit? :later
    tend/fit? :later
    herd/fit? :later
    guard/fit? :later
    roll/fit? :later
    village-maintain/fit? :later
    pen-check/fit? :ok
    shut-gate/fit? :ok
    unleash/fit? :ok
    trade/fit? :ok))
