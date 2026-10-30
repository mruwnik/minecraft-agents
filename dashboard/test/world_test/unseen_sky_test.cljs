(ns world-test.unseen-sky-test
  (:require [cljs.test :refer [deftest is]]
            [engine.path.blocks :as blocks]
            [world-test.unseen-solid :as us]
            [world-test.unseen-sky :as w]))

(defn- id [name] (us/open-ids) (first (blocks/state-ids (blocks/default-state-table) name)))

(def walled
  "A stone floor under open sky, the feet at y 64, a 5-high stone wall at x 6; the open cells behind it at sky light 15
  and the rest of the open air too."
  (let [stone (id "stone") air (id "air")]
    (fn [x y z] (let [solid? (or (< y 64) (and (= x 6) (<= y 68)))]
                  {:id (if solid? stone air) :block 0 :sky (if solid? 0 15)}))))

(deftest the-unseen-ground-behind-a-wall-counts-under-the-wide-open-sky-seen-over-it
  (let [t (w/stand-tally walled 0 64 0)]
    (is (seq t) "the ground behind the wall, under the seen air over its top")
    (is (every? (fn [[[kind n] v]] (and (= :wide kind) (pos? n) (= (:n v) (nth (:sky v) 14)))) t)
        "no side of the seen cell over them is seen solid, and every one is lit by sky 15")))
