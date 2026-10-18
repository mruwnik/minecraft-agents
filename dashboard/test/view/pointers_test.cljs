(ns view.pointers-test
  (:require [clojure.test :refer [deftest are is]]
            [view.camera :as cam]
            [view.pointers :as p]))

(def W 640)
(def H 360)
(def eye {:x 0 :y 0 :z 0})
(def basis (cam/camera-basis {:yaw 0 :pitch 0 :fov 90}))
(defn at [x y z] {:x x :y y :z z})
(defn near? [a b] (< (abs (- a b)) 1e-6))

(deftest straight-ahead-is-the-picture-centre
  (let [r (p/pointer-for basis (at 0 0 -10) W H 28)]
    (is (:on-screen? r))
    (is (near? 320 (:px r)))
    (is (near? 180 (:py r)))
    (is (near? 10 (:dist r)))))

(deftest off-screen-points-sit-on-the-inset-edge-and-aim-at-the-target
  (are [offset px py angle] (let [r (p/pointer-for basis offset W H 28)]
                              (and (not (:on-screen? r)) (near? px (:px r)) (near? py (:py r)) (near? angle (:angle r))))
    (at 10 0 -2) 612 180 0 ; right: yaw 0 faces -z, +x is to the right
    (at -10 0 -2) 28 180 js/Math.PI
    (at 0 10 -2) 320 28 (- (/ js/Math.PI 2))
    (at 0 -10 -2) 320 332 (/ js/Math.PI 2)))

(deftest behind-the-camera-points-to-the-side-it-is-on
  (let [r (p/pointer-for basis (at 5 0 5) W H 28)]
    (is (not (:on-screen? r)))
    (is (near? 612 (:px r)))
    (is (near? 180 (:py r))))
  (let [r (p/pointer-for basis (at 0 0 10) W H 28)]
    (is (not (:on-screen? r)) "directly behind: still somewhere on the edge")
    (is (near? 332 (:py r)))))

(deftest a-diagonal-hits-the-nearer-edge
  (let [r (p/pointer-for basis (at 10 10 -1) W H 28)]
    (is (not (:on-screen? r)))
    (is (<= 28 (:px r) 612))
    (is (near? 28 (:py r)))))

(def zombie {:min [-0.3 0 -10] :max [0.3 1.95 -9.4] :kind "hostile" :name "zombie_villager"})
(def bob {:min [20 0 -3] :max [20.6 1.8 -2.4] :kind "player" :name "player" :label "Bob"})
(def cow {:min [0 0 -5] :max [1 1 -4] :kind "cattle" :name "cow"})

(deftest only-hostiles-and-players-get-pointers-nearest-first
  (let [r (p/pointers basis eye [zombie cow bob] W H)]
    (is (= ["hostile" "player"] (map :kind r)) "the zombie is 10 away, Bob about 20")
    (is (= [true false] (map :on-screen? r)) "the zombie is ahead, Bob far to the right")))

(deftest a-target-the-body-does-not-see-gets-no-pointer
  (let [r (p/pointers basis eye [(assoc zombie :seen false) (assoc bob :seen true)] W H)]
    (is (= ["player"] (map :kind r)) "a hostile behind a wall is not known to the body; a box with no :seen flag counts as seen")))

(deftest labels-name-the-target-and-round-the-distance
  (let [r (p/pointers basis eye [zombie bob] W H)]
    (is (= #{"Bob 20m" "zombie villager 10m"} (set (map :text r))))))

(deftest at-most-max-pointers
  (let [many (for [i (range 30)] (assoc zombie :min [0 0 (- (+ 5 i))] :max [1 2 (- (+ 4 i))]))]
    (is (= p/MAX-POINTERS (count (p/pointers basis eye many W H))))))

(deftest agent-key-cases
  (are [v expected] (= expected (p/agent-key? v))
    "claude/ProbeDrive" true "w_1/Bob-2" true "ProbeDrive" false "a/b/c" false "/b" false "a/" false "a b/c" false "" false nil false js/undefined false))
