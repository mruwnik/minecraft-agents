(ns engine.cross-water-test
  "jobs.movement.cross-water against the fake world: launch, drive to the far shore, land and take the boat back."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.boat-unseen-test :as unseen]
            [engine.library-test :as lt]
            [engine.settings :as settings]
            [engine.seek-contract-test :as seek]
            [engine.test-util :as tu]
            [jobs.movement.cross-water :as cw]))

(def lake
  "Water at y 63 at x 4..6, z -3..3 in the stone floor: the near bank is x 3, the far bank x 7 (a boat needs room round it: no bank block beside its cell)."
  (into {} (for [x (range 4 7) z (range -3 4)] [(str x ",63," z) "water"])))

(def boat-item {:name "oak_boat" :count 1})
(def far {:x 7 :y 64 :z 0})

(def world {:blocks lake :inventory [boat-item] :dismountDry true})

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.cross-water args 200))]
    {:res res :p p :seen seen :eng eng}))

(defn child-ended? [seen slot]
  (boolean (some #(and (= :child_ended (:kind %)) (= slot (or (:slot %) (get-in % [:data :slot])))) @seen)))

(defn aboard? [p] (some? (:vehicle (js->clj (.self p) :keywordize-keys true))))

(def aboard-world
  "The body in a boat on the lake (a placed boat in the fake has no dismount cell: the recover leg needs one on the bank)."
  {:blocks lake
   :entities [{:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 4.5 :y 63.5 :z 0.5} :yaw 0 :health 1 :drops [{:name "oak_boat" :count 1}]
               :dismountAt [7.5 64 0.5]}]
   :self {:vehicle 9 :pos [4.5 63.5 0.5]}})

(deftest aboard-it-skips-the-launch-lands-and-takes-the-boat-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run aboard-world {:pos [7 64 0]}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= {:x 7 :y 64 :z 0} (:land res)))
          (is (true? (:recovered res)))
          (is (not (aboard? p)))
          (is (= 1 (get (lt/inv p) "oak_boat")) "the boat came along")
          (is (not (child-ended? seen :launch)))
          (is (child-ended? seen :land)))))))

(deftest recover-false-leaves-the-boat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run world {:pos [7 64 0] :recover false}))]
          (is (= :done (:status res)) (pr-str res))
          (is (false? (:recovered res)))
          (is (= far (:land res)))
          (is (child-ended? seen :launch))
          (is (nil? (get (lt/inv p) "oak_boat"))))))))

(deftest picks-a-far-shore-when-none-is-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run world {:settings {::cw/min-cross 4} :recover false}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 7 (:x (:land res))) "a cell on the far side"))))))

(defn ^:async waits-on
  "How a listed cross-water call over world ends: {:event :waiting :reason} for a start fact that does not hold now."
  [world args]
  (await (seek/ending-of #(:p (lt/setup world)) (list 'jobs.movement.cross-water args))))

(deftest start-facts-are-conditions
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w args expected]
                [["a given cell that is no shore" world {:pos [9 64 0]} {:event :waiting :reason :no-far-shore}]
                 ["no shore far enough" world {:settings {::cw/min-cross 60}} {:event :waiting :reason :no-far-shore}]
                 ["no water within :radius of the body" world {:pos [7 64 0] :settings {::cw/radius 1}} {:event :waiting :reason :not-water}]
                 ["an item that is no boat" world {:pos [7 64 0] :item "stick"} {:event :stopped :kind :impossible :reason :bad-args}]]]
          (is (= expected (await (waits-on w args))) label))))))

(deftest a-given-cell-over-seen-water-is-no-shore-though-its-air-is-not-made-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :waiting :reason :no-far-shore}
               (await (seek/ending-of #(let [p (:p (lt/setup world))]
                                         (unseen/unseen-beyond! p [5 64 0] 0)
                                         (unseen/unseen-beyond! p [5 65 0] 0)
                                         p)
                                      (list 'jobs.movement.cross-water {:pos [5 64 0]})))))))))

(deftest no-boat-and-none-to-fetch-is-condition-failed-no-boat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (dissoc world :inventory) {:pos [7 64 0]}))]
          (is (= {:status :stopped :reason :no-boat} (select-keys res [:status :reason])) (pr-str res))
          (is (= :condition-failed (:kind (:ending res)))))))))

(deftest an-unloaded-given-cell-is-crossed-toward-then-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup (assoc world :unloaded ["7,64,0"]))]
          (core/submit! eng '(jobs.movement.cross-water {:pos [7 64 0]}) {})
          (await (tu/run-until-empty eng 60))
          (is (empty? (filterv #(= :waiting (:kind %)) @seen)))
          (is (child-ended? seen :launch))
          (is (= :no-shore (:reason (last (filterv #(and (= :stopped (:kind %)) (= ["j1"] (:chain %))) @seen))))
              (pr-str (mapv #(select-keys % [:kind :reason :chain]) (filterv #(= :stopped (:kind %)) @seen)))))))))

(defn spot
  "cw/far-spot from the world origin with ::min-cross min-cross, and the goal cell when given."
  ([p min-cross] (spot p min-cross nil))
  ([p min-cross goal]
   (settings/with-settings {::cw/min-cross min-cross} #(cw/far-spot p {:x 0 :y 64 :z 0} {:goal goal}))))

(deftest far-shore-prefers-the-far-side
  (let [{:keys [p]} (lt/setup world)]
    (is (= 7 (:x (:land (spot p 4)))))))

(def two-channels
  "Water at x 4 and x 9, z -3..3: land at x 5..8 between them, a far bank at x 10."
  (into {} (for [x [4 9] z (range -3 4)] [(str x ",63," z) "water"])))

(defn land-x [spot] (:x (:land spot)))

(deftest far-spot-takes-the-farthest-across-water-not-the-nearest
  (let [{:keys [p]} (lt/setup {:blocks two-channels})]
    (is (= 10 (land-x (spot p 4))))))

(deftest far-spot-excludes-the-start-bank
  (let [{:keys [p]} (lt/setup {:blocks two-channels})]
    (is (= 5 (land-x (spot p 1 {:x 0 :y 64 :z 0})))
        "nearest the goal, but the start bank (x 3) has no water between")))

(deftest far-spot-with-a-goal-takes-the-spot-nearest-it
  (let [{:keys [p]} (lt/setup {:blocks two-channels})]
    (is (= 8 (land-x (spot p 4 {:x 8 :y 64 :z 0}))))
    (is (= 10 (land-x (spot p 4 {:x 12 :y 64 :z 0}))))))

(defn ^:async run-saved
  "Run cross-water as the child of a parent that first saved a crossing to `land`: a restart after the launch."
  [world land]
  (let [{:keys [eng p seen]} (lt/setup world)
        out (atom :not-done)
        parent {:round (fn ^:async saving-round [c]
                         (when-not (get-in (ctx/mem c) [:children :kid :crossing])
                           (ctx/update-mem! c assoc-in [:children :kid :crossing] {:land land}))
                         (await ((:round (tu/recording-parent 'jobs.movement.cross-water {} out)) c)))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'saving-parent parent))]
    (core/submit! eng '(saving-parent) {})
    (await (tu/run-until-empty eng 200))
    {:res @out :p p :seen seen}))

(deftest on-foot-away-from-the-far-shore-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run-saved world far))]
          (is (= {:status :stopped :reason :not-landed} (select-keys res [:status :reason])) (pr-str res))
          (is (= :not-aboard (contract/cause-key (:because (:ending res)))) "the cause names the landing's stop"))))))

(deftest on-foot-on-the-far-shore-after-a-restart-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run-saved (assoc world :self {:pos [7.5 64 0.5]}) far))]
          (is (= :done (:status res)) (pr-str res))
          (is (= far (:land res)))
          (is (false? (:recovered res))))))))

(deftest launches-from-foot-lands-and-takes-the-boat-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res seen]} (await (run world {:pos [7 64 0]}))]
          (is (= :done (:status res)) (pr-str res))
          (is (true? (:recovered res)))
          (is (child-ended? seen :launch))
          (is (child-ended? seen :land)))))))

(deftest lands-and-reports-a-failed-recover
  ;; the boat drops nothing, so the pick-up fails: the crossing still lands, then says why the boat stayed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res seen]} (await (run (assoc-in aboard-world [:entities 0 :drops] []) {:pos [7 64 0]}))]
          (is (= {:status :stopped :reason :recover-failed} (select-keys res [:status :reason])) (pr-str res))
          (is (= :not-collected (contract/cause-key (:because (:because (:ending res))))) (pr-str res))
          (is (child-ended? seen :land)))))))

(deftest no-boat-carries-the-launch-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (dissoc world :inventory) {:pos [7 64 0]}))]
          (is (= :no-boat (:reason res)) (pr-str res))
          (is (= :need (contract/cause-key (:because (:because (:ending res))))) (pr-str res)))))))

(deftest a-boat-in-a-chest-is-fetched-then-the-crossing-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res seen]} (await (run (-> world
                                                 (dissoc :inventory)
                                                 (assoc-in [:blocks "-2,64,3"] "chest")
                                                 (assoc :containers {"-2,64,3" [boat-item]}
                                                        :zones [{:name "home" :min [-6 60 -6] :max [12 70 6] :owner "Fake"}]))
                                             {:pos [7 64 0] :recover false}))]
          (is (= :done (:status res)) (pr-str res))
          (is (child-ended? seen :launch)))))))

(deftest it-looks-around-before-it-says-no-far-shore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup world)
              _ (tu/seeing-after-look p)
              res (await (tu/child-outcome eng 'jobs.movement.cross-water {:settings {::cw/min-cross 4} :recover false} 200))]
          (is (some #(= "look" (.-name %)) (.-calls (.-world p))))
          (is (= :done (:status res)) (pr-str res)))))))

(deftest standing-on-the-given-far-shore-is-already-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run (assoc world :self {:pos [7.5 64 0.5]}) {:pos [7 64 0]}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= far (:land res)))
          (is (false? (:recovered res)))
          (is (not (aboard? p)))
          (is (not (child-ended? seen :launch))))))))

(deftest a-failed-child-keeps-its-data-and-renamed-cause
  (let [r {:kind :nothing-possible :cause :blocked :data {:spots 2 :drive :blocked}}]
    (is (= {:spots 2 :drive :blocked} (:data (cw/failure {} r {}))))
    (is (= :no-boat (contract/cause-key (cw/failure {} {:kind :condition-failed :cause :need} {:need :no-boat}))))))

(deftest only-a-body-standing-dry-at-the-landing-is-ashore
  (doseq [[label pos ashore] [["on the landing" [7.5 64 0.5] true]
                              ["a swimmer within reach of it" [6.5 63 0.5] false]]]
    (let [{:keys [p]} (lt/setup (assoc world :self {:pos pos}))]
      (is (= ashore (cw/on-shore? {:primitives p} far)) label))))
