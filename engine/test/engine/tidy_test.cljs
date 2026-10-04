(ns engine.tidy-test
  "Tidying up after trespassing: a dig or place in another's zone is recorded in body memory, and
  jobs.survival.restore-broken puts the cells back when the body is safe."
  (:require [cljs.test :refer [deftest is async]]
            [engine.zones-survival-test :as zs]
            [engine.fake :as fake]
            [engine.core :as core]
            [engine.jobs.tidy :as tidy]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

(defn tidy-entries [eng] (mapv :data (mem/entries (mem/view (:store eng)) :tidy)))

(defn seed! [eng entries]
  (doseq [e entries] (mem/write! (:store eng) :tidy e tidy/tidy-policy)))

(def dug {:cell [0 65 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0})
(def placed {:cell [0 65 0] :action :place :was "air" :now "cobblestone" :zone "vault" :tries 0})
(def stone (:inventory {:inventory [{:name "stone" :count 2}]}))
(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})

(def enclosed {"0,65,0" "stone" "0,66,0" "stone"})

(deftest breathe-records-what-it-dug-in-a-foreign-zone-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones recorded] [[[(zs/whole-zone "Miles")] [{:cell [0 65 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0}
                                                              {:cell [0 66 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0}]]
                                  [[(zs/whole-zone "fake")] []]
                                  [[] []]
                                  [nil []]]]
          (let [{:keys [eng]} (zs/setup {:blocks enclosed} zones)]
            (core/submit! eng '(jobs.survival.breathe {:min-oxygen 12}) {})
            (await (core/tick! eng))
            (is (= recorded (tidy-entries eng)) (pr-str zones))))))))

(defn restore!
  "Run restore-broken on a world with seeded entries; [eng p seen] after the job ran."
  [world zones entries]
  (let [{:keys [eng] :as s} (zs/setup world zones)]
    (seed! eng entries)
    (core/submit! eng '(jobs.survival.restore-broken) {})
    s))

(deftest restore-places-the-dug-block-when-carried-and-safe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! {:inventory [{:name "stone" :count 2}]} [(zs/whole-zone "Miles")] [dug])]
          (await (zs/run-until-empty eng 6))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (zs/calls p "dig")) "digs nothing")
          (is (= [] (tidy-entries eng)))
          (is (= [[[0 65 0]]] (mapv :cells (zs/trespass seen :tidy.restored))))
          (is (= [[]] (mapv :cells (zs/trespass seen :tidy.not-restored)))))))))

(deftest restore-digs-a-placed-block-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! {:blocks {"0,65,0" "cobblestone"}} [(zs/whole-zone "Miles")] [placed])]
          (await (zs/run-until-empty eng 6))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "dig"))))
          (is (= [] (zs/calls p "place")))
          (is (= [[[0 65 0]]] (mapv :cells (zs/trespass seen :tidy.restored)))))))))

(deftest restore-skips-and-warns-what-it-cannot-put-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world entries why kept]
                [[{:inventory []} [dug] :not-carried 1]
                 [{:inventory [{:name "stone" :count 2}] :self {:health 6}} [dug] :unsafe 1]
                 [{:inventory [{:name "stone" :count 2}] :entities [zombie]} [dug] :unsafe 1]
                 [{:inventory [{:name "stone" :count 2}] :blocks {"0,65,0" "dirt"}} [dug] :changed 0]]]
          (let [{:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] entries)]
            (await (zs/run-until-empty eng 6))
            (is (= [] (zs/calls p "place")) (pr-str why))
            (is (= [] (zs/calls p "dig")) (pr-str why))
            (is (= kept (count (tidy-entries eng))) (pr-str why))
            (is (= [[{:cell [0 65 0] :was "stone" :why why}]] (mapv :cells (zs/trespass seen :tidy.not-restored))) (pr-str why))
            (is (= [[]] (mapv :cells (zs/trespass seen :tidy.restored))) (pr-str why))))))))
