(ns engine.ready-test
  "jobs.lib.ready: the safe? and fit? checkers a job binds, as statuses over the shelter, the light and the mobs."
  (:require [cljs.test :refer [deftest is are]]
            [engine.hut-shelter-test :as hut]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter :as sh]
            [jobs.lib.danger :as danger]))

(defn ctx-of
  "A job's ctx over a fake body built from world (a fresh job id: the check cache is per job)."
  [world]
  (let [p (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world)))]
    {:id (str (random-uuid)) :slots [] :engine {:warned (atom #{})} :primitives p :view (fn [] {:now 0 :data {}})}))

(def inside {:x 5 :y 64 :z 0})
(def outside {:x 15 :y 64 :z 0})
(def barn-roof (tu/box 14 67 -2 16 67 2 "oak_planks"))
(def night-hut (merge (hut/hut-world inside {}) {:time st/night}))
(def open-night {:time st/night :self {:pos {:x 0 :y 64 :z 0}}})
(def tunnel {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks (tu/box 0 66 -1 6 66 1 "stone")})

(defn lit
  "(f) with every cell's light read as block and sky (the fake world has no light data)."
  [block sky f]
  (with-redefs [sh/block-light-at (constantly block) sh/sky-light-at (constantly sky)] (f)))

(defn kind [check world] (first (check (ctx-of world))))
(defn reason [check world] (:reason (nth (check (ctx-of world)) 2)))

(deftest safe-at-open-ground-at-night-waits-for-dawn
  (let [check (ready/safe-at (constantly outside))
        s (lit 0 15 #(check (ctx-of open-night)))]
    (is (= :later (first s)))
    (is (= #{:dawn} (:lowered-by (nth s 2))))
    (is (re-find #"15 64 0" (second s)) "names the place")
    (is (re-find #"dawn" (second s)) "and what helps")))

(deftest safe-at-a-sheltered-place-is-ok-and-a-walk-out-of-the-hut-is-not
  (are [world to expected] (= expected (kind (ready/safe-at (constantly to)) world))
    night-hut {:x 6 :y 64 :z 0} :ok
    night-hut outside :later
    (assoc night-hut :blocks (merge (:blocks night-hut) barn-roof)) outside :later
    tunnel {:x 4 :y 64 :z 0} :ok
    (assoc open-night :time st/noon) {:x 4 :y 64 :z 0} :look))

(deftest safe-at-open-ground-by-day-is-ok-where-the-light-is-seen
  (is (= :ok (lit 0 15 #(kind (ready/safe-at (constantly outside)) (assoc open-night :time st/noon))))))

(deftest safe-here-is-ok-in-the-hut
  (is (= :ok (kind ready/safe-here night-hut))))

(deftest safe-here-outside-at-night-is-later
  (is (= :later (lit 0 15 #(kind ready/safe-here open-night)))))

(deftest safe-if-lit-waits-at-night-above-ground-even-from-a-hut
  (are [world expected] (= expected (kind ready/safe-if-lit world))
    night-hut :later
    tunnel :ok
    (assoc open-night :time st/noon) :ok))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})
(def wall (tu/box 2 64 -2 2 66 2 "stone"))

(deftest threatened-is-later-for-a-reachable-zombie-and-ok-behind-a-wall
  (are [world expected] (= expected (kind ready/threatened world))
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]} :later
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie] :blocks (merge wall (tu/box 2 67 -2 4 67 2 "stone") (tu/box 3 64 -2 4 66 -2 "stone") (tu/box 3 64 2 4 66 2 "stone") (tu/box 4 64 -2 4 66 2 "stone"))} :ok
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}} :ok))

(def skeleton {:id 8 :name "skeleton" :kind "hostile" :pos {:x 12 :y 64 :z 0}})
(def at-origin {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}})

(defn with-entry
  "ctx-of world, its engine holding a hostile-near register entry with args."
  [world args]
  (assoc-in (ctx-of world) [:engine :state] (atom {:register [{:id :hostile-near :trigger :hostile-near :args args}]})))

(deftest threatened-reads-the-hostile-near-entrys-args-and-the-triggers-options
  (are [world args expected] (= expected (first (ready/threatened (with-entry (merge at-origin world) args))))
    {:entities [zombie]} {} :later
    {:entities [zombie]} {:radius 2} :ok
    {:entities [(assoc skeleton :pos {:x 0 :y 64 :z 20})]} {} :ok
    {:entities [skeleton]} {} :later
    {:entities [skeleton]} {:ranged-radius 5} :ok))

(deftest threatened-keeps-the-triggers-visible-only-option
  (let [far (assoc zombie :pos {:x 5 :y 64 :z 0})
        behind (tu/box 2 64 0 3 66 0 "stone")]
    (are [args expected] (= expected (first (ready/threatened (with-entry (merge at-origin {:entities [far] :blocks behind}) args))))
      {} :ok
      {:visible-only false} :later)))

(deftest threatened-is-read-once-per-window
  (let [c (ctx-of (merge at-origin {:entities [zombie]}))
        asked (atom 0)]
    (with-redefs [danger/hostile-near? (fn [& _] (swap! asked inc) true)]
      (ready/threatened c)
      (ready/threatened c))
    (is (= 1 @asked))))

(deftest safe-at-adds-the-threat-to-the-exposure
  (is (= :later (kind (ready/safe-at (constantly {:x 1 :y 64 :z 0}))
                      {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))

(deftest fit-is-later-at-5-hp
  (are [hp expected] (= expected (kind ready/fit? {:self {:pos {:x 0 :y 64 :z 0} :health hp}}))
    5 :later
    20 :ok))

(deftest ok-is-always-ok
  (is (= :ok (kind ready/ok open-night))))

(deftest an-exposure-over-the-limit-is-judged-by-what-lowers-it
  (are [block sky world expected] (= expected (lit block sky #(kind (ready/safe-at (constantly outside)) world)))
    8 15 open-night :ok
    0 15 open-night :later
    0 0 (assoc open-night :time st/noon) :never))

(deftest an-unseen-cell-asks-for-a-look-at-it
  (let [s ((ready/safe-at (constantly outside)) (ctx-of open-night))]
    (is (= :look (first s)))
    (is (= [outside] (:look-at (nth s 2))))))
