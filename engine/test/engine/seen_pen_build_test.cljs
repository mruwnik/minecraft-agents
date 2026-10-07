(ns engine.seen-pen-build-test
  "Build/pen reads its pen from what the body sees or remembers: behind stone a cell is unknown and the pen is not
  closed, in view it is used. The primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.perception :as perception]
            [engine.pen-build-test :as pbt]
            [engine.seen-work-reads-test :refer [wrapped]]
            [jobs.build.pen :as pen]))

(def air (into {} (for [x (range -2 9) y (range 64 68) z (range -2 9)] [(str x "," y "," z) "air"])))

(def blocks (merge air pbt/ground pbt/built-pen))

(def cells (for [[k want] pbt/built-pen :let [[x y z] (map #(js/parseInt % 10) (.split k ","))]]
             {:pos [x y z] :want want}))

(defn scanned
  "wrapped, after a sight pass at every heading and pitch so the whole pen is seen."
  [blocks wall?]
  (let [w (wrapped blocks wall?)]
    (doseq [yaw [0 45 90 135 180 225 270 315] pitch [0 20 35 50 70]]
      (swap! (fake/state w) assoc :yaw yaw :pitch pitch)
      (perception/pass! (aget w "perception")))
    w))

(defn read [w] (pen/read-pen {:primitives w :args {:max-cells 500}} cells))

(deftest read-pen-needs-sight-of-the-cells
  (is (not (:closed? (read (scanned blocks true)))) "behind a wall: unknown, not read")
  (is (true? (:closed? (read (scanned blocks false)))) "in view: read"))
