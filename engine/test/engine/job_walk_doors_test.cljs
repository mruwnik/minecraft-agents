(ns engine.job-walk-doors-test
  "The jobs that walk to a target (leash, shear, unleash, give, follow, pace) use the engine walker, so a body inside its
  doored hut leaves through the door instead of getting noPath from the raw pathfinder primitive."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.harvest-test :as h]
            [engine.hut-shelter-test :as hut]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.moved-test :as ut]))

(def inside {:x 5 :y 64 :z 0})

(defn animal [name extra] (merge {:id 3 :uuid "u3" :name name :kind "passive" :pos {:x 5 :y 64 :z 7}} extra))

(def steve {:id 5 :name "Steve" :kind "player" :pos {:x 5 :y 64 :z 7}})

(defn ^:async leaves-through-the-door!
  "Run the job in the hut with the target outside; check the body went out by the walker, the door is shut again."
  [world job]
  (let [{:keys [eng p]} (ut/setup (merge (hut/hut-world inside {}) world))]
    (core/submit! eng job {})
    (await (st/tick-n eng 8))
    (is (= [] (ut/calls p "moveTo")) "no raw pathfinder walk")
    (is (<= 3 (.-z (.-pos (.self p)))) "the body is outside, past the door")
    (is (false? (hut/door-open? p)) "the door is shut behind it")))

(deftest leash-from-inside-a-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (leaves-through-the-door! {:inventory [{:name "lead" :count 2}] :entities [(animal "cow" {})]}
                                         '(jobs.animals.leash {:mob "cow"})))))))

(deftest shear-from-inside-a-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (leaves-through-the-door! {:inventory [{:name "shears" :count 1}] :entities [(animal "sheep" {})]}
                                         '(jobs.animals.shear {})))))))

(deftest unleash-from-inside-a-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (leaves-through-the-door! {:entities [(animal "cow" {:leashed true :leashedToMe true})]}
                                         '(jobs.animals.unleash {:mob "cow"})))))))

(deftest give-from-inside-a-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (leaves-through-the-door! {:inventory [{:name "bread" :count 5}] :entities [steve]}
                                         '(jobs.items.give {:player "Steve" :item "bread"})))))))

(deftest follow-from-inside-a-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (leaves-through-the-door! {:entities [steve]} '(jobs.movement.follow {:player "Steve"})))))))

(deftest pace-from-inside-a-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (leaves-through-the-door! {} '(jobs.movement.pace {:a {:x 5 :y 64 :z 7} :b {:x 5 :y 64 :z 9} :laps 1})))))))

(deftest harvest-from-inside-a-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (ut/setup (-> (hut/hut-world inside {})
                                            (update :blocks merge (h/field "wheat" 7 [5] [8]))
                                            (assoc :ages (h/ages 7 [5] [8]) :drops h/wheat-drops)))
              _ (tu/seeing-all p)]
          (core/submit! eng '(jobs.farm.harvest {:center {:x 5 :y 64 :z 8} :radius 3}) {})
          (await (st/tick-n eng 40))
          (is (= [] (ut/calls p "moveTo")) "no raw pathfinder walk")
          (is (seq (ut/calls p "dig")) "the crop outside was cut")
          (is (false? (hut/door-open? p)) "the door is shut behind it"))))))
