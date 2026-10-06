(ns engine.watch-test
  "jobs.lib.watch: a job that stares at its work turns its head between acts, when it is in danger, so the view cone
  catches what comes from the side or behind (card 943cac28)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.watch :as watch]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.test-util :as tu]))

;; The body stands at (0.5 64 0.5) facing south (+z); north (-z) is behind it.

(def body {:x 0.5 :y 64 :z 0.5})
(defn mob [id name x z] {:id id :name name :kind "hostile" :pos {:x (+ x 0.5) :y 64 :z (+ z 0.5)}})

(defn rig
  "{:p :c :clock :looks :events :data}: fake primitives under a perception, and a stub ctx over a shared body memory.
  The clock only moves when a test moves it."
  [spec]
  (let [clock (atom 1000000)
        raw-p (tu/fake (merge {:self {:pos body} :floor [-50 -50 50 50]} (dissoc spec :light :dig-ms)))
        _ (swap! (fake/state raw-p) merge (select-keys spec [:dig-ms]) (when-let [l (:light spec)] {:light-default l}))
        per (perception/create (fake-raw/create raw-p) {:now #(deref clock)})
        p (perception/wrap raw-p per)
        data (atom mem/empty-data)
        looks (atom [])
        events (atom [])
        ctx (fn []
              {:primitives p
               :view (fn [] {:data @data :now @clock})
               :remember (fn [kind d policy]
                           (swap! data mem/add-entry kind {:t @clock :data d} policy))
               :act (fn [k a]
                      (when (= :look k) (swap! looks conj (js->clj a :keywordize-keys true)))
                      (js/Promise.resolve ((aget p (name k)) nil a)))
               :emit (fn [kind level fields] (swap! events conj (assoc fields :kind kind :level level)))})]
    {:p p :raw raw-p :c (ctx) :ctx ctx :clock clock :looks looks :events events}))

(defn later! [clock ms] (swap! clock + ms))
(defn dark [] {:light [0 0]})
(defn check [f] (async done (tu/run-async done f)))
(defn known-ids [p] (mapv #(.-id %) (.knownMobs p)))

(deftest risk-levels
  (let [{:keys [c]} (rig {})] (is (= :quiet (watch/risk c {})) "a lit day cell, no mobs")
    (is (= :risky (watch/risk c {:risky? true})) "the job says so"))
  (let [{:keys [c]} (rig (dark))] (is (= :risky (watch/risk c {})) "a dark cell"))
  (let [{:keys [c]} (rig {:entities [(mob 1 "zombie" 0 10)]})]
    (is (= :alert (watch/risk c {})) "a hostile known within 24")))

(deftest a-quiet-place-never-looks
  (check (fn ^:async t [] (let [{:keys [c looks]} (rig {})]
        (is (= :skipped (await (watch/watch! c {}))))
        (is (= [] @looks))))))

(deftest a-risky-scan-waits-its-interval-and-parent-and-child-share-the-clock
  (check (fn ^:async t [] (let [{:keys [c ctx clock looks]} (rig {:light [0 0]})
            child (ctx)]
        (is (= :scanned (await (watch/watch! c {}))))
        (let [n (count @looks)]
          (later! clock 1000)
          (is (= :skipped (await (watch/watch! child {}))) "1 s after the parent's scan")
          (is (= n (count @looks)))
          (later! clock 2000)
          (is (= :scanned (await (watch/watch! child {}))) "3 s after")
          (is (> (count @looks) n)))))))

(deftest an-alert-scans-every-two-seconds
  (check (fn ^:async t [] (let [{:keys [c clock]} (rig {:entities [(mob 1 "zombie" 0 10)]})]
        (is (= :scanned (await (watch/watch! c {}))))
        (later! clock 2100)
        (is (= :scanned (await (watch/watch! c {}))))))))

(def tunnel
  "Stone round the body's cell (0 64 0) on both sides, ahead (south) and above; open behind (north)."
  (merge (tu/box -1 64 -1 -1 65 20 "stone") (tu/box 1 64 -1 1 65 20 "stone")
         (tu/box -1 66 -1 1 66 20 "stone") (tu/box -1 64 1 1 65 1 "stone")))

(deftest a-tunnel-has-one-look-back-and-open-ground-three
  (check (fn ^:async t [] (let [{:keys [c looks]} (rig {:blocks tunnel :light [0 0]})]
        (await (watch/watch! c {}))
        (is (= 1 (count @looks)))
        (let [{:keys [x z y]} (:pos (first @looks))]
          (is (< z 0.5) "toward the north")
          (is (< (js/Math.abs (- x 0.5)) 0.01))
          (is (< (js/Math.abs (- y 65.62)) 0.01) "level with the eye")))
      (let [{:keys [c looks]} (rig {:light [0 0]})]
        (await (watch/watch! c {}))
        (is (= 3 (count @looks)))
        (is (every? #(= 65.62 (:y (:pos %))) @looks))))))

(deftest a-creeper-behind-is-known-after-the-scan
  (check (fn ^:async t [] (let [{:keys [c p events]} (rig {:entities [(mob 1 "creeper" 0 -6)]})]
        (is (= [] (known-ids p)) "before: unseen behind")
        (is (= :saw (await (watch/watch! c {:risky? true}))))
        (is (= [1] (known-ids p)))
        (is (= [:watch.saw] (mapv :kind @events)))
        (is (= "creeper" (:name (first @events)))))
      (let [{:keys [c p]} (rig {:entities [(mob 1 "creeper" 0 -3)] :light [0 0]})]
        (is (= :saw (await (watch/watch! c {}))) "in the dark a mob is seen only within 4")
        (is (= [1] (known-ids p))))
      (let [{:keys [c p]} (rig {:entities [(mob 1 "creeper" 0 -6)]
                                :blocks (tu/box -3 64 -3 3 65 -3 "stone")})]
        (is (= :scanned (await (watch/watch! c {:risky? true}))))
        (is (= [] (known-ids p)) "behind a wall it stays unknown")))))

(deftest a-mob-sample-follows-each-look
  (check (fn ^:async t [] (let [{:keys [c p raw]} (rig {:entities [(mob 1 "creeper" 0 -6)]})]
        (await (watch/watch! c {:risky? true}))
        (is (= [1] (mapv #(.-id %) (array-seq (.knownMobs p)))))
        (is (some? raw))))))

(deftest a-heard-zombie-without-a-line-is-turned-to-once
  (check (fn ^:async t [] (let [{:keys [c looks clock]}
            (rig {:entities [(mob 1 "zombie" 6 -3)] :blocks (tu/box 5 64 -6 5 65 -1 "stone")})]
        (is (= :turned (await (watch/watch! c {}))))
        (is (= 1 (count @looks)))
        (later! clock 1000)
        (is (= :skipped (await (watch/watch! c {}))) "not again within 5 s")
        (is (= 1 (count @looks)))))))

(deftest a-turn-to-a-heard-mob-emits-watch-turned-once
  (check (fn ^:async t [] (let [{:keys [c clock events looks]}
            (rig {:entities [(mob 1 "zombie" 6 -3)] :blocks (tu/box 5 64 -6 5 65 -1 "stone")})]
        (await (watch/watch! c {}))
        (later! clock 1000)
        (await (watch/watch! c {}))
        (let [ev (filterv #(= :watch.turned (:kind %)) @events)]
          (is (= 1 (count ev)))
          (is (= "zombie" (:name (first ev))))
          (is (not (contains? (first ev) :pos)) "heard only: no exact place")
          (is (not (contains? (first ev) :distance)))
          (is (= :north-east (:direction (first ev))))
          (is (= :near (:band (first ev))))
          (is (not= {:x 6.5 :y 64 :z -2.5} (:pos (first @looks))) "the body faces the direction, not the mob"))))))

(deftest a-long-dig-is-preceded-by-a-scan
  (check (fn ^:async t [] (let [{:keys [c looks]} (rig {:dig-ms 3000 :light [0 0]})]
        (is (= :scanned (await (watch/watch! c {:risky? true :before-dig {:x 0 :y 64 :z 2}}))))
        (is (pos? (count @looks))))
      (let [{:keys [c]} (rig {:dig-ms 500 :light [0 0]})]
        (await (watch/watch! c {:risky? true :every-ms 1000000}))
        (is (= :skipped (await (watch/watch! c {:risky? true :every-ms 1000000 :before-dig {:x 0 :y 64 :z 2}})))
            "a short dig does not scan")))))