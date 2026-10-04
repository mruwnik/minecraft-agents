(ns engine.explore-search-test
  "jobs.explore.search against the fake world, with a notes store over a temp world folder."
  (:require [cljs.test :refer [deftest is are async]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.notes :as notes]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as world]
            [jobs.explore.search :as search]))

(def ground
  "Grass at y 63 under every column within 100 of the origin: every leg the patterns choose from the origin stands
  there, and the planner has a floor to walk over between them."
  (tu/box -100 63 -100 100 63 100 "grass_block"))

(defn setup
  "An engine over the fake world spec (ground added) with a notes store for body Fake in a temp world folder."
  [spec]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        dir (tu/tmp-dir)
        p (tu/fake (update spec :blocks #(merge ground %)))
        store (notes/open {:world-dir dir :body "Fake" :emit (fn [_])})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (assoc (world/of-data {} {}) :notes store)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock :dir dir :store store}))

(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))

(defn ^:async run-until-done
  "Tick 700 ms apart until the list is empty or n ticks have run."
  [{:keys [eng clock] :as s} n]
  (loop [i 0]
    (when (and (< i n) (not (finished? s)))
      (swap! clock + 700)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async search!
  [args spec & [n before]]
  (let [s (setup spec)]
    (when before (before s))
    (core/submit! (:eng s) (list 'jobs.explore.search args) {})
    (await (run-until-done s (or n 400)))))

(defn event-of [{:keys [seen]} kind] (first (filter #(= kind (:kind %)) @seen)))
(defn walks
  "The targets of the walks go-to made, in order (each walk writes a :moved entry)."
  [{:keys [eng]}]
  (mapv (comp :target :data) (mem/entries (mem/view (:store eng)) :moved)))
(defn file-notes [{:keys [dir]}]
  (:value (notes/parse-file (fs/readFileSync (path/join dir "notes" "Fake.edn") "utf8"))))
(defn write-other! [{:keys [dir]} body ns]
  (fs/mkdirSync (path/join dir "notes") #js {:recursive true})
  (fs/writeFileSync (path/join dir "notes" (str body ".edn")) (notes/render body ns)))

(def diamond {"45,64,3" "diamond_block"})

;; ------------------------------------------------------------------ pure

(deftest the-spiral-rings-the-origin-at-spacing-within-the-distance
  (is (= [[-16 -16] [0 -16] [16 -16] [16 0] [16 16] [0 16] [-16 16] [-16 0]]
         (search/spiral-points [0 0] 16 23)))
  (is (= [[0 -16] [16 0] [0 16] [-16 0]] (search/spiral-points [0 0] 16 20)))
  (is (= 24 (count (search/spiral-points [100 100] 16 46)))))

(deftest outward-goes-ahead-first-then-to-the-sides
  (is (= [[0 -16] [8 -12] [-8 -12] [12 -4] [-12 -4] [16 0] [-16 0]]
         (search/outward-points [0.5 0.5] :north 16)))
  (is (= [16 0] (first (search/outward-points [0 0] :east 16)))))

(defn column [m] (fn [[x y z]] (if (contains? m :unloaded) nil (get m [x y z] "air"))))

(deftest a-leg-stands-on-the-ground-not-the-canopy-and-never-in-the-unknown
  (are [m expected] (= expected (search/stand-cell (column m) 5 5 64))
    {[5 63 5] "grass_block"} {:y 64}
    {[5 70 5] "stone" [5 69 5] "stone"} {:y 71}
    {[5 66 5] "oak_leaves" [5 63 5] "dirt"} {:y 64}
    {[5 63 5] "water"} {:fail :wet}
    {[5 63 5] "dirt" [5 64 5] "oak_log" [5 65 5] "oak_log" [5 66 5] "oak_leaves"} {:fail :no-surface}
    {} {:fail :no-surface}
    {:unloaded true} {:fail :not-loaded}))

;; ------------------------------------------------------------------ the job

(deftest a-target-in-sight-is-found-without-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block"} {:blocks {"10,64,0" "diamond_block"}}))]
          (is (finished? s))
          (is (= :found (:reason (event-of s :search.done))))
          (is (= [{:what "diamond_block" :pos [10 64 0]}] (:found (event-of s :search.done))))
          (is (= [] (walks s))))))))

(deftest a-target-out-of-sight-is-walked-to-found-and-noted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block"} {:blocks diamond}))
              ns (file-notes s)]
          (is (finished? s))
          (is (= :found (:reason (event-of s :search.done))))
          (is (= [[45 64 3]] (map :pos (:found (event-of s :search.done)))))
          (is (pos? (count (walks s))))
          (is (some #(= [:seen "diamond_block" [45 64 3] "Fake"] ((juxt :kind :what :pos :by) %)) ns))
          (is (some #(= [:searched ["diamond_block"]] ((juxt :kind :what) %)) ns))
          (is (nil? (event-of s :search.not-found))))))))

(deftest an-entity-target-is-found-and-noted-by-its-uuid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target ["cow" "pig"]}
                                {:entities [{:id 3 :uuid "u3" :name "cow" :kind "passive" :pos {:x 30.5 :y 64 :z 0.5}}]}))]
          (is (= [{:what "cow" :pos [30 64 0] :id "u3"}] (:found (event-of s :search.done))))
          (is (some #(= [:seen "cow" "u3"] ((juxt :kind :what :id) %)) (file-notes s))))))))

(deftest nothing-within-the-distance-ends-not-found-with-the-coverage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:blocks diamond}))
              e (event-of s :search.not-found)]
          (is (finished? s))
          (is (= :not-found (:reason e)))
          (is (= :distance (:why e)))
          (is (= 4 (get-in e [:coverage :legs])))
          (is (= 5 (get-in e [:coverage :scans])))
          (is (= 14 (get-in e [:coverage :farthest])) "the body stops within 2 of the leg 16 out")
          (is (= [] (:found e))))))))

(deftest the-leg-and-time-bounds-end-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [legs (await (search! {:target "diamond_block" :max-legs 2} {}))
              timed (await (search! {:target "diamond_block" :timeout-s 2} {}))]
          (is (= [:legs 2] ((juxt :why #(get-in % [:coverage :legs])) (event-of legs :search.not-found))))
          (is (= :time (:why (event-of timed :search.not-found)))))))))

(deftest ground-another-body-searched-for-these-targets-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] (search/spiral-points [0 0] 16 23)]
                      {:kind :searched :what ["diamond_block"] :pos [x 64 z] :r 24 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :max-distance 23} {} 400 #(write-other! % "Other" marks)))
              e (event-of s :search.not-found)]
          (is (= [] (walks s)))
          (is (= 8 (get-in e [:coverage :skipped])))
          (is (= :distance (:why e))))))))

(deftest ground-searched-for-other-names-is-not-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks [{:kind :searched :what ["oak_log"] :pos [0 64 -16] :r 24 :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block" :max-distance 20} {} 400 #(write-other! % "Other" marks)))]
          (is (= 4 (count (walks s)))))))))

(deftest a-target-another-body-noted-counts-unless-told-not-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen [{:kind :seen :what "diamond_block" :pos [45 64 3] :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block"} {} 400 #(write-other! % "Other" seen)))
              own (await (search! {:target "diamond_block" :use-notes false :max-distance 20} {} 400
                                  #(write-other! % "Other" seen)))]
          (is (= [{:what "diamond_block" :pos [45 64 3] :noted true :by "Other"}] (:found (event-of s :search.done))))
          (is (= [] (walks s)))
          (is (= :not-found (:reason (event-of own :search.not-found)))))))))

(deftest legs-that-cannot-be-stood-on-or-walked-are-recorded-and-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20}
                                {:unloaded ["0,63,-16"] :unreachable ["16,64,0"]}))
              failed (get-in (event-of s :search.not-found) [:coverage :failed])]
          (is (= #{{:pos [0 64 -16] :reason :not-loaded} {:pos [16 64 0] :reason :unreachable}} (set failed)))
          (is (not-any? #(= {:x 0 :z -16} (select-keys % [:x :z])) (walks s)) "an unloaded column is never walked to"))))))

(def ring-1-cells (mapv (fn [[x z]] (str x ",63," z)) (search/spiral-points [0 0] 16 23)))

(deftest a-search-started-before-the-ground-around-loaded-waits-for-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:unloaded ring-1-cells})]
          (core/submit! (:eng s) '(jobs.explore.search {:target "diamond_block" :max-distance 20}) {})
          (dotimes [_ 20] (swap! (:clock s) + 700) (await (core/tick! (:eng s))))
          (is (not (finished? s)) "still waiting for the chunks")
          (is (= [] (walks s)))
          (.clear (.. (:p s) -world -state -unloaded))
          (await (run-until-done s 400))
          (is (= 4 (count (walks s))))
          (is (= :distance (:why (event-of s :search.not-found)))))))))

(deftest ground-that-never-loads-ends-it-not-loaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:unloaded ring-1-cells}))
              e (event-of s :search.not-found)]
          (is (= :not-loaded (:why e)))
          (is (<= (get-in e [:coverage :scans]) 17) "a look every 2 s while waiting, not every tick")
          (is (= 4 (count (get-in e [:coverage :failed]))))
          (is (= [] (walks s))))))))

(deftest three-failed-legs-in-a-row-end-it-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block"} {:unreachable ["-16,64,-16" "0,64,-16" "16,64,-16"]}))
              e (event-of s :search.not-found)]
          (is (= :stuck (:why e)))
          (is (= 3 (count (get-in e [:coverage :failed])))))))))

(deftest outward-walks-along-the-heading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :pattern :outward :heading :east :max-distance 40} {}))]
          (is (= [16 30] (take 2 (map :x (walks s)))) "the second leg is 16 further than where the body stopped, 2 short of the first")
          (is (every? #(<= (js/Math.hypot (:x %) (:z %)) 40) (walks s)) "no leg beyond :max-distance")
          (is (= :distance (:why (event-of s :search.not-found)))))))))

(deftest an-unknown-pattern-or-heading-ends-it-before-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:pattern :zigzag} {:pattern :outward :heading :up}]]
          (let [s (await (search! (assoc args :target "diamond_block") {}))]
            (is (= :bad-args (:why (event-of s :search.not-found))) (str args))
            (is (= [] (walks s)) (str args))))))))

(deftest it-declines-without-a-target
  (let [c {:args {:target nil}}]
    (is (not (search/check c)))
    (is (not (search/check {:args {:target []}})))))

(deftest it-waits-only-until-the-wait-is-over
  (are [m now expected] (= expected (search/waiting? m now))
    {} 5000 false
    {:wait-until 6000} 5000 true
    {:wait-until 6000} 6000 false))
