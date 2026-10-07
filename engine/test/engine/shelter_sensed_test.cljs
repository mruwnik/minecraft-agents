(ns engine.shelter-sensed-test
  "The shelter reads (dig-in cells, shelter helpers) answer from what the body feels, sees or remembers, never from the
  raw blockAt: behind stone a cell is unknown, after a dig opened the line it is seen. The primitives are wrapped; their
  blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]))

(defn sensed-only
  "Wrapped fake primitives for spec, lit, whose raw blockAt throws."
  [spec]
  (let [p (tu/fake spec)
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        w (perception/wrap p per)]
    (aset w "blockAt" (fn [_] (throw (js/Error. "raw blockAt"))))
    {:p p :w w}))

(def wall (tu/box -2 63 3 2 67 3 "stone"))
(def water {:x 0 :y 65 :z 6})
(def hole [0 65 3])

(deftest behind-stone-it-is-unknown-and-after-a-dig-it-is-seen
  (let [{:keys [p w]} (sensed-only {:blocks (merge wall {"0,65,6" "water"})})]
    (is (false? (dig-cells/wet? w water)) "behind the wall nothing is known")
    (is (nil? (u/seen-name w water)))
    (fake/remove-block! p hole)
    (is (true? (dig-cells/wet? w water)) "the dug cell opened the line")
    (is (= "water" (u/seen-name w water)))))

(deftest an-unknown-cell-under-the-floor-is-the-callers-stated-guess
  (let [{:keys [p w]} (sensed-only {:blocks (merge wall {"0,65,6" "water"})})]
    (is (= "stone" (u/block-name-or w water "stone")))
    (is (false? (sh/solid-at? w water)))
    (fake/remove-block! p hole)
    (is (= "water" (u/block-name-or w water "stone")))))
