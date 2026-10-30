(ns engine.seen-sections-test
  "jobs.lib.seen-sections over a real perception on the fake world."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.store :as store]
            [engine.test-util :as tu]
            [jobs.lib.seen-sections :as seen]))

(defn seeing [p]
  (let [per (perception/create (fake-raw/create p) {:radius 10 :ray-deg 2 :now (constantly 1000000) :rand (constantly 0.5)})
        w (perception/wrap p per)]
    (perception/pass! per)
    w))

(defn st [w] (:st (aget w "perception")))

(defn remembered? [w [x y z]] (not (neg? (store/remembered-id (st w) x y z))))

(deftest a-cell-s-steps-to-the-memory-are-the-fewest-to-a-remembered-section-and-change-by-one-a-step
  (let [w (seeing (tu/fake-on-floor {}))
        f (seen/steps-fn w 3)
        secs (let [a (atom [])] (.forEach (store/store-of (st w) (.-dim (st w))) (fn [^js s] (swap! a conj [(* 16 (.-cx s)) (* 16 (.-sy s)) (* 16 (.-cz s))]))) @a)
        gap (fn [a lo] (max 0 (- lo a) (- a (+ lo 15))))
        oracle (fn [[x y z]] (reduce min ##Inf (for [[bx by bz] secs] (max (+ (gap x bx) (gap z bz)) (js/Math.ceil (/ (gap y by) 3))))))
        cells (for [x (range -60 61 7) y (range 0 130 9) z (range -60 61 11)] [x y z])]
    (is (seq secs))
    (is (= (map oracle cells) (map #(apply f %) cells)))
    (is (zero? (f 0 64 0)) "the body's own cell")
    (is (every? (fn [[x y z]] (every? #(<= (js/Math.abs (- (f x y z) (apply f %))) 1) [[(inc x) y z] [x y (inc z)] [x (inc y) z]])) cells)
        "within one of each neighbour's")
    (is (nil? (seen/steps-fn (tu/fake {}) 3)) "no memory, no fn")))

(deftest the-openings-since-a-mark-are-the-cells-seen-open-that-were-not
  (let [p (tu/fake-on-floor {})
        w (seeing p)
        lo [-8 60 -8] hi [8 70 8]
        m (seen/mark w lo hi)]
    (is (remembered? w [-6 63 6]) "the floor ahead of the body")
    (is (= [] (seen/opened w m lo hi)) "nothing written since")
    (perception/pass! (aget w "perception"))
    (is (= [] (seen/opened w m lo hi)) "a look that shows the same is no opening")
    (fake/remove-block! p {:x -6 :y 63 :z 6})
    (perception/pass! (aget w "perception"))
    (is (some #{[-6 63 6]} (seen/opened w m lo hi)) "the floor cell taken away")
    (is (not-any? #{[0 64 0] [1 64 0] [1 63 0]} (seen/opened w m lo hi)) "not one open or solid as it was")
    (is (= [] (seen/opened w m [-5 60 -8] hi)) "outside the box")
    (is (= [] (seen/opened w (seen/mark w lo hi) lo hi)) "against a fresh mark")))

(deftest the-frontier-toward-a-goal-is-a-seen-stand-at-sight-s-edge-on-the-goal-s-side
  (let [w (seeing (tu/fake-on-floor {}))
        ;; the body looks west and south
        f (seen/toward w [0 64 0] [-28 64 8] 3 0.3 1 100)]
    (is (vector? f))
    (is (< (f 0) -4) (str f " is out toward the goal"))
    (is (< 0 (f 2)) (str f " is on the goal's side"))
    (is (remembered? w f))
    (is (some #(not (remembered? w %)) [(update f 0 inc) (update f 0 dec) (update f 2 inc) (update f 2 dec) (update f 1 + 2)]) "a face it has not seen")
    (is (nil? (seen/toward w [0 64 0] [-28 64 8] 3 0.3 1 5)) "no way through it under max-s, were the unseen open")
    (is (nil? (seen/toward w [0 64 0] [0 64 0] 3 0.3 1 100)) "none nearer a goal the body stands at")))

