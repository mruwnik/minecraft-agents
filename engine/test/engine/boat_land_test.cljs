(ns engine.boat-land-test
  "jobs.movement.boat-land against the fake world: bring the boat to a shore and step off."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.movement.boat-land :as boat-land]
            [engine.test-util :as tu]
            [engine.sense-test :as st]
            [jobs.lib.shore :as shore]))

(defn pond
  "Water at y 63 over x in [x0, x1] and z in [z0, z1]."
  [x0 x1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) z (range z0 (inc z1))] [(str x ",63," z) "water"])))

(def lake (pond 0 20 0 20))

(def east-bank "Stone at y 63 along x 21 (the only land: the fake world has no floor)." (into {} (for [z (range 0 21)] [(str "21,63," z) "stone"])))

(defn world-with
  "The lake with the east bank, with the boat at x z; a dismount lands at the stand cell land."
  [x z land]
  {:blocks (merge lake east-bank)
   :entities [{:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x x :y 63.5 :z z} :yaw 0 :health 1 :drops [{:name "oak_boat" :count 1}] :dismountAt land}]
   :self {:vehicle 9 :pos [x 63.5 z]}})

(def world (world-with 18.5 10.5 {:x 21 :y 64 :z 10}))

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.boat-land args 90))]
    {:res res :p p :seen seen :eng eng}))

(defn aboard? [p] (some? (:vehicle (js->clj (.self p) :keywordize-keys true))))

(deftest lands-on-the-nearest-shore-and-steps-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run world {}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= {:x 21 :y 64 :z 10} (:land res)))
          (is (false? (:recovered res)))
          (is (not (aboard? p)))
          (is (= {:x 21 :y 64 :z 10} (:land res))))))))

(deftest steps-off-from-the-water-cell-beside-the-shore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run (world-with 19.3 10.5 {:x 21 :y 64 :z 10}) {}))
              boat (first (filter #(= 9 (.-id %)) (array-seq (.entities p #js {:radius 64}))))]
          (is (= :done (:status res)) (pr-str res))
          (is (<= (js/Math.abs (- 20.5 (.. boat -pos -x))) 0.6) "the dismount is from the cell touching the land, not a cell short of it"))))))

(deftest a-given-land-cell-is-the-shore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (world-with 18.5 10.5 {:x 21 :y 64 :z 12}) {:pos [21 64 12]}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= {:x 21 :y 64 :z 12} (:land res))))))))

(deftest stops-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w args reason]
                [["no shore in sight, guessed spots looked at" world {:settings {:jobs.movement.boat-launch/radius 1}} :no-shore]
                 ["the given cell is not a shore" world {:pos [10 64 10]} :no-shore]
                 ["the given shore is across land"
                  (update world :blocks merge (pond 0 10 30 34) (into {} (for [x (range 0 11)] [(str x ",63,35") "stone"])))
                  {:pos [5 64 35]} :blocked]]]
          (let [{:keys [res]} (await (run w args))]
            (is (= {:status :stopped :reason reason} (select-keys res [:status :reason])) (pr-str [label res]))))))))

(deftest on-foot-fails-its-condition-not-aboard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (dissoc world :self) {}))]
          (is (= [:condition-failed :not-aboard] [(:kind (:ending res)) (:reason res)])))))))

(deftest shore-needs-seen-cells
  (let [{:keys [p]} (lt/setup (assoc world :unloaded ["21,64,3"]))]
    (is (nil? (shore/spot-at p {:x 21 :y 64 :z 3})) "an unseen stand cell is no shore")
    (is (some? (shore/spot-at p {:x 21 :y 64 :z 4})))))

(deftest an-unloaded-given-cell-is-driven-nearer-then-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup (assoc world :unloaded ["21,64,12"]))]
          (core/submit! eng '(jobs.movement.boat-land {:pos [21 64 12]}) {})
          (await (tu/run-until-empty eng 30))
          (is (empty? (filterv #(= :waiting (:kind %)) @seen)))
          (is (= [:no-shore] (mapv :reason (filterv #(= :stopped (:kind %)) @seen))))
          (is (some #(= "paddle" (:name %)) @seen) "drove nearer first"))))))

(deftest a-failed-approach-drive-is-the-because-of-no-shore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (-> world (assoc :unloaded ["21,64,12"]) (update :blocks assoc "20,63,12" "stone")) {:pos [21 64 12]}))]
          (is (= :no-shore (:reason res)) (pr-str res))
          (is (= :not-water (contract/cause-key (:because (:ending res)))) (pr-str res)))))))

(deftest recover-takes-the-boat-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run world {:recover true}))]
          (is (= :done (:status res)) (pr-str res))
          (is (true? (:recovered res)))
          (is (empty? (filter #(= "oak_boat" (:name %)) (fake/entities p)))))))))




(defn island-pond
  "A pond at x 14..16, z 23..25 ringed by stone at y 63: its ring is a shore the boat cannot reach."
  []
  (merge (pond 14 16 23 25)
         (into {} (for [x (range 13 18) z (range 22 27) :when (not (<= 14 x 16 23 z 25))] [(str x ",63," z) "stone"]))))

(def blocked-world
  "The boat at the lake's south end; the nearest shore spots are the island pond's ring, then the east bank."
  (assoc-in (world-with 15.5 20.5 {:x 21 :y 64 :z 20}) [:blocks] (merge lake east-bank (island-pond))))

(deftest max-spots-limits-the-shores-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run blocked-world {:settings {:jobs.movement.boat-land/max-spots 1}}))
              {res2 :res} (await (run blocked-world {:settings {:jobs.movement.boat-land/max-spots 40}}))]
          (is (= {:status :stopped :reason :blocked} (select-keys res [:status :reason])) (pr-str res))
          (is (= :done (:status res2)) "with room to try on, the reachable east bank lands")
          (is (= 21 (:x (:land res2)))))))))

(deftest every-spot-blocked-is-blocked-with-the-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (assoc blocked-world :blocks (merge lake (island-pond))) {:settings {:jobs.movement.boat-land/max-spots 2}}))]
          (is (= :blocked (:reason res)) (pr-str res))
          (is (= :blocked (contract/cause-key (:because (:ending res)))) (pr-str res)))))))

(deftest a-failed-dismount-is-not-landed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (assoc world :dismountFails true) {}))]
          (is (= :not-landed (:reason res)) (pr-str res))
          (is (= :dismount-failed (contract/cause-key (:because (:ending res)))) (pr-str res)))
        (let [{:keys [res]} (await (run (world-with 18.5 10.5 {:x 10 :y 63 :z 10}) {}))]
          (is (= :not-landed (:reason res)) "the dismount left the body in water"))))))

(deftest a-drive-that-fails-is-drive-failed-with-its-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (assoc-in world [:entities 0 :name] "minecart") {}))]
          (is (= :drive-failed (:reason res)) (pr-str res))
          (is (= :not-a-boat (contract/cause-key (:because (:ending res)))) (pr-str res)))))))

(deftest bad-pos-is-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run world {:pos [5 -9999 5]}))]
          (is (= :bad-args (:reason res)) (pr-str res)))))))

(deftest recover-fails-when-the-boat-is-not-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res seen]} (await (run (assoc-in world [:entities 0 :drops] []) {:recover true}))]
          (is (= :recover-failed (:reason res)) (pr-str res))
          (is (= {:x 21 :y 64 :z 10} (:land res)))
          (is (some? (:because (:ending res)))))))))

(deftest drive-failure-is-blocked-only-when-every-spot-was-blocked
  (let [blocked {:kind :nothing-possible :cause :blocked}
        other {:kind :condition-failed :cause :not-a-boat}]
    (is (= [:blocked blocked] (boat-land/drive-failure [blocked blocked])))
    (is (= [:drive-failed other] (boat-land/drive-failure [blocked other blocked])) "one other failure among blocked ones")))

(deftest a-drive-that-is-not-done-passes-its-cause-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (assoc-in world [:entities 0 :name] "minecart") {:settings {:jobs.movement.boat-land/max-spots 3}}))]
          (is (= :drive-failed (:reason res)) (pr-str res))
          (is (some? (:because (:ending res)))))))))

(deftest recover-lands-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res seen]} (await (run world {:recover true}))]
          (is (= :done (:status res)) (pr-str res))
          (is (true? (:recovered res))))))))

(deftest resumes-from-the-landed-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup (assoc (world-with 18.5 10.5 {:x 21 :y 64 :z 10}) :self {:pos [21.5 64 10.5]}))
              _ (core/submit! eng '(jobs.movement.boat-land {:recover true}) {})
              id "j1"] ; the first job of a fresh engine
          (mem/write! (:store eng) boat-land/landed-kind {:job id :land {:x 21 :y 64 :z 10} :boat 9} boat-land/landed-policy)
          (await (tu/run-until-empty eng 90))
          (is (empty? (:list (core/state eng))))
          (is (empty? (filter #(= "paddle" (:name %)) @seen)) "no second drive"))))))

(deftest conditions-name-the-start-facts
  (let [c (fn [w args] (assoc (st/stub-ctx (:p (lt/setup w))) :args args))]
    (is (= [:never :bad-args] (tu/kind-reason (boat-land/bad-args (c world {:pos [5 -9999 5]})))))
    (is (= [:ok :args] (tu/kind-reason (boat-land/bad-args (c world {})))))
    (is (= [:later :not-aboard] (tu/kind-reason (boat-land/not-aboard (c (dissoc world :self) {})))))
    (is (= [:ok :mounted] (tu/kind-reason (boat-land/not-aboard (c world {})))))
    (is (= [:look :no-shore] (tu/kind-reason (settings/with-settings {:jobs.movement.boat-launch/radius 1} #(boat-land/no-shore (c world {}))))))
    (is (= [:ok :shore] (tu/kind-reason (boat-land/no-shore (c world {})))))
    (is (= [:ok :shore] (tu/kind-reason (boat-land/no-shore (c world {:pos [10 64 10]})))) "a given cell is judged by the round, which may drive nearer")))

(deftest the-landing-record-never-expires
  (is (= :forever (:ttl boat-land/landed-policy)) "a boat left ashore stays owed until seen gone"))

(deftest done-is-ok-only-ashore-at-the-given-cell
  (let [c (fn [w args] (assoc (st/stub-ctx (:p (lt/setup w))) :args args))
        ashore (assoc world :self {:pos [21.5 64 10.5]})
        cell [21 64 10]]
    (is (= [:ok :ashore] (tu/kind-reason (boat-land/done? (c ashore {:pos cell})))))
    (is (= :later (first (tu/kind-reason (boat-land/done? (c world {:pos cell}))))) "still on the boat")
    (is (= :later (first (tu/kind-reason (boat-land/done? (c ashore {}))))) "no cell to be ashore on")))
