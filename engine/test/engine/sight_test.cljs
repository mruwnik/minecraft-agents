(ns engine.sight-test
  "engine.sight: line of sight over a block grid (a port of js/sight.test.mjs)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.sight :as sight]))

(defn solid-set [& cells] (let [s (set cells)] (fn [x y z] (contains? s [x y z]))))

(deftest line-clear-cases
  (are [from to solid clear] (= clear (apply sight/line-clear (concat from to [solid])))
    [0.5 64.5 0.5] [5.5 64.5 0.5] (solid-set) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (solid-set [3 64 0]) false
    [0.5 64.5 0.5] [5.5 64.5 0.5] (solid-set [3 64 1]) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (solid-set [0 64 0]) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (solid-set [5 64 0]) true
    [0.5 64.5 0.5] [4.5 64.5 4.5] (solid-set [2 64 2]) false
    [0.5 66.5 0.5] [6.5 64.5 0.5] (solid-set [2 66 0]) false
    [5.5 64.5 0.5] [0.5 64.5 0.5] (solid-set [2 64 0]) false))

(def cube [0 0 0 1 1 1])
(def post [0.375 0 0.375 0.625 1.5 0.625])
(def pane [0.4375 0 0.4375 0.5625 1 0.5625])

(defn shapes-set [& pairs] (let [m (into {} pairs)] (fn [x y z] (get m [x y z] []))))

(deftest ray-clear-cases
  (are [from to shapes clear] (= clear (apply sight/ray-clear (concat from to [shapes])))
    [0.5 64.5 0.5] [5.5 64.5 0.5] (shapes-set) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (shapes-set [[3 64 0] [cube]]) false
    [0.5 64.5 0.5] [5.5 64.5 0.5] (shapes-set [[3 64 1] [cube]]) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (shapes-set [[0 64 0] [cube]]) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (shapes-set [[5 64 0] [cube]]) true
    [0.5 64.5 0.2] [5.5 64.5 0.2] (shapes-set [[3 64 0] [post]]) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (shapes-set [[3 64 0] [post]]) false
    [0.5 66.0 0.5] [5.5 66.0 0.5] (shapes-set [[3 64 0] [post]]) true
    [0.5 64.5 0.5] [5.5 64.5 0.5] (shapes-set [[3 64 0] [pane]]) false
    [0.5 64.5 0.2] [5.5 64.5 0.2] (shapes-set [[3 64 0] [pane]]) true
    [0.5 64.5 0.5] [4.5 64.5 4.5] (shapes-set [[2 64 2] [cube]]) false
    [5.5 64.5 0.5] [0.5 64.5 0.5] (shapes-set [[2 64 0] [cube]]) false))
