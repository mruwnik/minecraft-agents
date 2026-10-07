(ns engine.seen-pen-test
  "The pen readers of jobs.animals.herd read what the body sees or remembers. A pen cell it has not seen is unknown:
  the job goes and looks at it; only a cell seen as open ends :no-pen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.herd-test :as ht]
            [engine.hostile-test :as h]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.animals.herd-run :as hr]))

(defn scan!
  "Turn through every heading, level and down, a sight pass at each."
  [s]
  (doseq [yaw [0 45 90 135 180 225 270 315] pitch [0 20 35 50 70]]
    (swap! (fake/state (:p s)) assoc :yaw yaw :pitch pitch)
    (perception/pass! (aget (:p s) "perception"))))

(defn seeing
  "The body west of the pen after a full sight pass; aged?: then 30 s on and looking at its feet, so the fence and floor
  are remembered and the gate (a mutable block, remembered 10 s) is unknown."
  [w aged?]
  (let [s (ht/clock-on-wait! (h/setup-seeing (ht/world w) nil 700))]
    (scan! s)
    (when aged?
      (swap! (:clock s) + 30000)
      (perception/pass! (aget (:p s) "perception")))
    s))

(defn read [s] (hr/read-pen {:primitives (:p s) :args {:box ht/box}}))

(defn gate-unknown? [s]
  (boolean (some #(and (= :unloaded (:why %)) (= ht/gate (:pos %))) (:leaks (read s)))))

(deftest read-pen-reads-what-the-body-sees
  (is (false? (gate-unknown? (seeing {} false))) "gate in view")
  (is (true? (gate-unknown? (seeing {} true))) "gate remembered past its 10 s: unknown, the fence still remembered"))

(deftest a-pen-with-an-unknown-gate-herds-and-does-not-end-no-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (seeing {:entities [(ht/cow 1 4 3)]} true)
              _ (ht/submit! s {:target 1})
              _ (await (ht/run-ticks s 12))]
          (is (= :brought (:reason (ht/done-event s)))))))))
