(ns engine.cross-water-test
  "jobs.movement.cross-water against the fake world: launch, drive to the far shore, land and take the boat back."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [jobs.movement.cross-water :as cw]))

(def lake
  "A channel of water at y 63 at x 4, z -3..3 in the stone floor: the near bank is x 3, the far bank x 5 (the fake dismount steps one block on
  from the boat, so a wider lake would drop the body into water)."
  (into {} (for [z (range -3 4)] [(str "4,63," z) "water"])))

(def boat-item {:name "oak_boat" :count 1})
(def far {:x 5 :y 64 :z 0})

(def world {:blocks lake :inventory [boat-item]})

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.cross-water args 200))]
    {:res res :p p :seen seen :eng eng}))

(defn aboard? [p] (some? (:vehicle (js->clj (.self p) :keywordize-keys true))))

(def wide "Water at y 63 at x 4..6, z -3..3: the far bank is x 7."
   (into {} (for [x (range 4 7) z (range -3 4)] [(str x ",63," z) "water"])))

(def aboard-world
  "The body in a boat on the wide lake (a placed boat in the fake has no dismount cell: the recover leg needs one on the bank)."
  {:blocks wide
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
          (is (not-any? #(= :boat.launched (:kind %)) @seen))
          (is (some #(= :boat.landed (:kind %)) @seen)))))))

(deftest recover-false-leaves-the-boat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run world {:pos [5 64 0] :recover false}))]
          (is (= :done (:status res)) (pr-str res))
          (is (false? (:recovered res)))
          (is (= far (:land res)))
          (is (some #(= :boat.launched (:kind %)) @seen))
          (is (nil? (get (lt/inv p) "oak_boat"))))))))

(deftest picks-a-far-shore-when-none-is-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run world {:min-cross 4}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 5 (:x (:land res))) "a cell on the far side"))))))

(deftest stops-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w args reason]
                [["a given cell that is no shore" world {:pos [9 64 0]} :no-far-shore]
                 ["no shore far enough" world {:min-cross 60} :no-far-shore]
                 ["no water within :radius of the body" world {:pos [5 64 0] :radius 1} :not-water]
                 ["no boat and none to fetch" (dissoc world :inventory) {:pos [5 64 0]} :no-boat]]]
          (let [{:keys [res]} (await (run w args))]
            (is (= {:status :stopped :reason reason} (select-keys res [:status :reason])) (pr-str [label res]))))))))

(deftest an-unseen-given-cell-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup (assoc world :unloaded ["5,64,0"]))]
          (core/submit! eng '(jobs.movement.cross-water {:pos [5 64 0]}) {})
          (await (tu/run-until-empty eng 3))
          (is (= [:unseen] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest far-shore-prefers-the-far-side
  (let [{:keys [p]} (lt/setup world)]
    (is (= 5 (:x (:land (cw/far-spot p {:x 0 :y 64 :z 0} {:radius 24 :min-cross 4})))))))

(def two-channels
  "Water at x 4 and x 9, z -3..3: land at x 5..8 between them, a far bank at x 10."
  (into {} (for [x [4 9] z (range -3 4)] [(str x ",63," z) "water"])))

(defn land-x [spot] (:x (:land spot)))

(deftest far-spot-takes-the-farthest-across-water-not-the-nearest
  (let [{:keys [p]} (lt/setup {:blocks two-channels})]
    (is (= 10 (land-x (cw/far-spot p {:x 0 :y 64 :z 0} {:radius 24 :min-cross 4}))))))

(deftest far-spot-excludes-the-start-bank
  (let [{:keys [p]} (lt/setup {:blocks two-channels})]
    (is (= 5 (land-x (cw/far-spot p {:x 0 :y 64 :z 0} {:radius 24 :min-cross 1 :goal {:x 0 :y 64 :z 0}})))
        "nearest the goal, but the start bank (x 3) has no water between")))

(deftest far-spot-with-a-goal-takes-the-spot-nearest-it
  (let [{:keys [p]} (lt/setup {:blocks two-channels})]
    (is (= 8 (land-x (cw/far-spot p {:x 0 :y 64 :z 0} {:radius 24 :min-cross 4 :goal {:x 8 :y 64 :z 0}}))))
    (is (= 10 (land-x (cw/far-spot p {:x 0 :y 64 :z 0} {:radius 24 :min-cross 4 :goal {:x 12 :y 64 :z 0}}))))))

(defn ^:async run-saved
  "Run cross-water as the child of a parent that first saved a crossing to `land`: a restart after the launch."
  [world land]
  (let [{:keys [eng p seen]} (lt/setup world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async saving-round [c]
                         (when-not (seq (ctx/entries c cw/crossing-kind))
                           (ctx/remember! c cw/crossing-kind {:job (:root c) :land land} cw/crossing-policy))
                         (let [r (await (ctx/call-child c :kid 'jobs.movement.cross-water {}))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           (when (= :stopped r) (reset! out (ctx/child-result c :kid)))
                           r))}
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
          (is (= :not-aboard (get-in res [:cause :reason])) "the cause names the landing's stop"))))))

(deftest on-foot-on-the-far-shore-after-a-restart-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run-saved (assoc world :self {:pos [5.5 64 0.5]}) far))]
          (is (= :done (:status res)) (pr-str res))
          (is (= far (:land res)))
          (is (false? (:recovered res))))))))

(deftest launches-from-foot-and-reports-a-failed-recover
  ;; the fake's placed boat drops nothing, so the pick-up fails: the crossing still lands, then says why the boat stayed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res seen]} (await (run world {:pos [5 64 0]}))]
          (is (= {:status :stopped :reason :recover-failed} (select-keys res [:status :reason])) (pr-str res))
          (is (= :not-collected (get-in res [:cause :cause :reason])) (pr-str res))
          (is (some #(= :boat.launched (:kind %)) @seen))
          (is (some #(= :boat.landed (:kind %)) @seen)))))))

(deftest no-boat-carries-the-launch-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (dissoc world :inventory) {:pos [5 64 0]}))]
          (is (= :no-boat (:reason res)) (pr-str res))
          (is (= {:child :launch :reason :need} (select-keys (:cause res) [:child :reason])) (pr-str res)))))))

(deftest a-boat-in-a-chest-is-fetched-then-the-crossing-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res seen]} (await (run (-> world
                                                 (dissoc :inventory)
                                                 (assoc-in [:blocks "-2,64,3"] "chest")
                                                 (assoc :containers {"-2,64,3" [boat-item]}
                                                        :zones [{:name "home" :min [-6 60 -6] :max [12 70 6] :owner "Fake"}]))
                                             {:pos [5 64 0] :recover false}))]
          (is (= :done (:status res)) (pr-str res))
          (is (some #(= :fetch.done (:kind %)) @seen))
          (is (some #(= :boat.launched (:kind %)) @seen)))))))

(deftest it-looks-around-before-it-says-no-far-shore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup world)
              _ (tu/seeing-after-look p)
              res (await (tu/child-outcome eng 'jobs.movement.cross-water {:min-cross 4} 200))]
          (is (some #(= "look" (.-name %)) (.-calls (.-world p))))
          (is (= :done (:status res)) (pr-str res)))))))
