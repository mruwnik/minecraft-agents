(ns engine.boat-unseen-test
  "boat-drive and boat-land look at a given cell they have never seen before they wait on it."
  (:require [cljs.test :refer [deftest is async]]
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
