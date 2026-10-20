(ns engine.sight-bench-test
  "Bench of one full sight pass on four worlds (lit field, dark field with a torch, sealed dark room, open flat field):
  cells visited per pass, visits per distinct cell, ms per pass and ns per visited cell, printed as a table. The fake
  world's own lookups are part of the time, so compare runs of this bench with each other, not with the live body.
  The asserts only check the counts are sane; time is never a pass/fail."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.sight-set-pin-test :as pin]
            [engine.test-util :as tu]))

(def passes 8)

(def worlds
  [{:name "lit-field" :blocks pin/pillars :world {:yaw 0}}
   {:name "dark-field" :blocks pin/pillars :world {:yaw 270 :light-default [0 0] :light {[3 65 3] [0 14]}}}
   {:name "sealed-room" :blocks pin/cave :world {:yaw 45 :light-default [0 0]}}
   {:name "open-field" :blocks (tu/box -60 60 -60 60 63 60 "grass_block") :world {:yaw 0}}])

(defn rig [{:keys [blocks world]} wrap]
  (let [p (tu/blind (tu/fake {:blocks blocks}))]
    (swap! (fake/state p) merge world)
    (let [raw (fake-raw/create p)]
      (perception/create (wrap raw) {}))))

(defn counting
  "raw with stateAt also adding each looked-at cell to the volatile set."
  [seen]
  (fn [^js raw]
    (let [at (.-stateAt raw)
          w (js/Object.assign #js {} raw)]
      (set! (.-stateAt w) (fn [x y z] (vswap! seen conj (+ (* 1000003 (+ x 5000)) (* 1009 (+ y 5000)) (+ z 5000))) (at x y z)))
      w)))

(defn measure [w]
  (let [per (rig w identity)
        _ (perception/pass! per)
        c0 (:cells (perception/stats per))
        t0 (js/performance.now)
        _ (dotimes [_ passes] (perception/pass! per))
        ms (/ (- (js/performance.now) t0) passes)
        cells (/ (- (:cells (perception/stats per)) c0) passes)
        seen (volatile! #{})
        cper (rig w (counting seen))
        _ (perception/pass! cper)
        visits (:cells (perception/stats cper))]
    {:world (:name w) :cells cells :distinct (count @seen) :visits-per-distinct (/ visits (max 1 (count @seen)))
     :ms-per-pass ms :ns-per-cell (/ (* 1e6 ms) (max 1 cells))}))

(defn fmt [x] (if (integer? x) (str x) (.toFixed x 2)))

(deftest sight-pass-bench
  (let [rows (mapv measure worlds)]
    (println "\nsight pass bench (fake world)\nworld cells/pass distinct visits/distinct ms/pass ns/cell")
    (doseq [r rows]
      (println (apply str (interpose " " (map #(if (string? %) % (fmt %))
                                              ((juxt :world :cells :distinct :visits-per-distinct :ms-per-pass :ns-per-cell) r))))))
    (is (every? #(pos? (:cells %)) rows))
    (is (every? #(>= (:visits-per-distinct %) 1) rows))))
