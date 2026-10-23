(ns engine.boat-unseen-test
  "boat-drive, boat-land and cross-water look at a given cell they have never seen, and one they still cannot make out
  is driven nearer (or the job ends with a reason): they never wait on it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

(defn looked-at?
  "A look at cell [x y z] was made."
  [p [x y z]]
  (boolean (some #(when-let [q (and (= "look" (.-name %)) (some-> (.-args %) .-pos))]
                    (and (= x (js/Math.floor (.-x q))) (= y (js/Math.floor (.-y q))) (= z (js/Math.floor (.-z q)))))
                 (.-calls (.-world p)))))

(defn unseen-until-looked!
  "Make cell [x y z] of p read as never seen until a look at it was made."
  [p [x y z :as cell]]
  (let [omniscient (.-sensedAt p)]
    (aset p "sensedAt" (fn [pos] (if (and (= x (.-x pos)) (= y (.-y pos)) (= z (.-z pos)) (not (looked-at? p cell)))
                                   #js {:unknown true}
                                   (omniscient pos))))))

(defn unseen-beyond!
  "Make cell [x y z] of p read as never seen while the body is more than d blocks from it (never seen at all with d 0)."
  [p [x y z] d]
  (let [omniscient (.-sensedAt p)]
    (aset p "sensedAt" (fn [pos]
                         (let [[bx _ bz] (get-in @(fake/state p) [:self :pos])]
                           (if (and (= x (.-x pos)) (= y (.-y pos)) (= z (.-z pos))
                                    (> (js/Math.hypot (- (+ x 0.5) bx) (- (+ z 0.5) bz)) d))
                             #js {:unknown true}
                             (omniscient pos)))))))

(defn waiting-events [seen] (filter #(= :waiting (:kind %)) @seen))

(def lake (into {} (for [x (range 0 21) z (range 0 21)] [(str x ",63," z) "water"])))
(def bank (into {} (for [z (range 0 21)] [(str "21,63," z) "stone"])))

(deftest boat-drive-looks-at-an-unseen-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks lake :entities [{:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 2.5 :y 63.5 :z 2.5} :yaw 0}]
                                         :self {:vehicle 9}})]
          (unseen-until-looked! p [10 63 2])
          (await (tu/child-outcome eng 'jobs.movement.boat-drive {:pos [10 63 2]} 60))
          (is (looked-at? p [10 63 2]))
          (is (some #(= "paddle" (.-name %)) (.-calls (.-world p))) "steered after the look"))))))

(deftest boat-land-looks-at-an-unseen-land-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (merge lake bank)
                                         :entities [{:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 18.5 :y 63.5 :z 10.5} :yaw 0 :health 1
                                                     :dismountAt {:x 21 :y 64 :z 10}}]
                                         :self {:vehicle 9 :pos [18.5 63.5 10.5]}})]
          (unseen-until-looked! p [21 64 12])
          (await (tu/child-outcome eng 'jobs.movement.boat-land {:pos [21 64 12]} 90))
          (is (looked-at? p [21 64 12])))))))

(defn boat-at [x z] {:entities [{:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x x :y 63.5 :z z} :yaw 0 :health 1
                                 :dismountAt {:x 21 :y 64 :z 10}}]
                     :self {:vehicle 9 :pos [x 63.5 z]}})

(deftest boat-drive-steers-to-a-cell-it-never-makes-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup (merge {:blocks lake} (boat-at 2.5 2.5)))]
          (unseen-beyond! p [10 63 2] 0)
          (let [res (await (tu/child-outcome eng 'jobs.movement.boat-drive {:pos [10 63 2]} 60))]
            (is (= :done (:status res)) (pr-str res))
            (is (looked-at? p [10 63 2]) "looked at it first")
            (is (empty? (waiting-events seen)) "never waits on the unseen cell")))))))

(deftest boat-drive-drives-nearer-and-stops-on-a-cell-that-turns-out-no-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (merge {:blocks (assoc lake "14,63,2" "stone")} (boat-at 2.5 2.5)))]
          (unseen-beyond! p [14 63 2] 5)
          (let [res (await (tu/child-outcome eng 'jobs.movement.boat-drive {:pos [14 63 2]} 60))]
            (is (= :not-water (:reason res)) (pr-str res))
            (is (some #(= "paddle" (.-name %)) (.-calls (.-world p))) "drove nearer to make it out")))))))

(deftest boat-land-drives-nearer-to-a-given-land-cell-it-cannot-make-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (merge {:blocks (merge lake bank)} (boat-at 4.5 12.5)))]
          (unseen-beyond! p [21 64 12] 6)
          (let [res (await (tu/child-outcome eng 'jobs.movement.boat-land {:pos [21 64 12]} 120))]
            (is (= :done (:status res)) (pr-str res))
            (is (= {:x 21 :y 64 :z 12} (:land res)))))))))

(deftest boat-land-ends-on-a-given-land-cell-it-never-makes-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup (merge {:blocks (merge lake bank)} (boat-at 4.5 12.5)))]
          (unseen-beyond! p [21 64 12] 0)
          (let [res (await (tu/child-outcome eng 'jobs.movement.boat-land {:pos [21 64 12]} 120))]
            (is (= :no-shore (:reason res)) (pr-str res))
            (is (re-find #"make out" (str (get-in res [:ending :cause :hint]))) (pr-str res))
            (is (empty? (waiting-events seen)))))))))

(def cw-lake
  "cross-water-test's lake: water at y 63 over a stone bed, x 4..6, z -3..3; the far bank x 7."
  (into {} (for [x (range 4 7) z (range -3 4) [y b] [[63 "water"] [62 "stone"]]] [(str x "," y "," z) b])))

(deftest cross-water-looks-at-a-given-far-shore-it-has-never-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks cw-lake :inventory [{:name "oak_boat" :count 1}] :dismountDry true})]
          (unseen-until-looked! p [7 64 0])
          (let [res (await (tu/child-outcome eng 'jobs.movement.cross-water {:pos [7 64 0] :recover false} 200))]
            (is (= :done (:status res)) (pr-str res))
            (is (looked-at? p [7 64 0]))
            (is (empty? (waiting-events seen)))))))))

(deftest cross-water-ends-on-a-given-far-shore-it-never-makes-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks cw-lake :inventory [{:name "oak_boat" :count 1}] :dismountDry true})]
          (unseen-beyond! p [7 64 0] 0)
          (let [res (await (tu/child-outcome eng 'jobs.movement.cross-water {:pos [7 64 0] :recover false} 200))]
            (is (= :stopped (:status res)) (pr-str res))
            (is (empty? (waiting-events seen)))))))))
