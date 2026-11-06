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

(deftest a-cell-s-steps-to-the-memory-are-the-fewest-to-the-box-of-a-section-s-remembered-cells-and-change-by-one-a-step
  (let [w (seeing (tu/fake-on-floor {}))
        f (seen/steps-fn w 3)
        ;; each section's remembered cells' box, [lo hi]
        secs (let [a (atom [])]
               (.forEach (store/store-of (st w) (.-dim (st w)))
                         (fn [^js s] (let [o [(* 16 (.-cx s)) (* 16 (.-sy s)) (* 16 (.-cz s))]
                                           cells (for [dx (range 16) dy (range 16) dz (range 16) :let [c (mapv + o [dx dy dz])] :when (remembered? w c)] c)]
                                       (when (seq cells) (swap! a conj [(apply mapv min cells) (apply mapv max cells)])))))
               @a)
        gap (fn [a lo hi] (max 0 (- lo a) (- a hi)))
        oracle (fn [[x y z]] (reduce min ##Inf (for [[[lx ly lz] [hx hy hz]] secs]
                                                 (max (+ (gap x lx hx) (gap z lz hz)) (js/Math.ceil (/ (gap y ly hy) 3))))))
        cells (for [x (range -60 61 7) y (range 0 130 9) z (range -60 61 11)] [x y z])]
    (is (seq secs))
    (is (= (map oracle cells) (map #(apply f %) cells)))
    (is (zero? (f 0 64 0)) "the body's own cell")
    (is (every? (fn [[x y z]] (every? #(<= (js/Math.abs (- (f x y z) (apply f %))) 1) [[(inc x) y z] [x y (inc z)] [x (inc y) z]])) cells)
        "within one of each neighbour's")
    (is (nil? (seen/steps-fn (tu/fake {}) 3)) "no memory, no fn")))

(deftest a-cell-remembered-since-counts-in-the-next-fn-s-boxes
  (let [w (seeing (tu/fake-on-floor {}))
        before ((seen/steps-fn w 1) 0 0 0)]
    (is (< 48 before) "the floor's section remembers nothing as low as y 48")
    (store/record! (st w) (store/store-of (st w) (.-dim (st w))) 0 48 0 1 1000000)
    (is (= 48 ((seen/steps-fn w 1) 0 0 0)))))

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


(deftest a-box-s-faces-are-unseen-when-no-remembered-cell-lies-within-r-of-them-inside-or-out
  (let [w (seeing (tu/fake-on-floor {}))
        cells (atom [])]
    (.forEach (store/store-of (st w) (.-dim (st w)))
              (fn [^js s] (let [o [(* 16 (.-cx s)) (* 16 (.-sy s)) (* 16 (.-cz s))]]
                            (doseq [dx (range 16) dy (range 16) dz (range 16) :let [c (mapv + o [dx dy dz])] :when (remembered? w c)]
                              (swap! cells conj c)))))
    (let [[x0 y0 z0] (apply mapv min @cells) [x1 y1 z1] (apply mapv max @cells)
          ;; the memory's sections are whole: a box 3 past each section's remembered cells
          [sx0 sy0 sz0] (mapv #(* 16 (js/Math.floor (/ % 16))) [x0 y0 z0])
          [sx1 sy1 sz1] (mapv #(+ 15 (* 16 (js/Math.floor (/ % 16)))) [x1 y1 z1])]
      (is (true? (seen/faces-unseen? w [(- sx0 3) (- sy0 3) (- sz0 3) (+ sx1 3) (+ sy1 3) (+ sz1 3)] 3 -64)) "round all the memory")
      (is (false? (seen/faces-unseen? w [(- x0 2) (- y0 3) (- z0 3) (+ x1 3) (+ y1 3) (+ z1 3)] 3 -64)) "a cell 2 inside a face")
      (is (true? (seen/faces-unseen? w [(- sx0 3) (- y0 1) (- sz0 3) (+ sx1 3) (+ sy1 3) (+ sz1 3)] 3 (- y0 1))) "the floor face left out")
      (is (true? (seen/faces-unseen? w [(+ sx1 4) y0 z0 (+ sx1 40) y1 z1] 3 -64)) "beside the memory, 4 past it")
      (is (false? (seen/faces-unseen? w [(+ x1 3) y0 z0 (+ x1 40) y1 z1] 3 -64)) "beside the memory, 3 past it"))
    (store/record! (st w) (store/store-of (st w) (.-dim (st w))) 1000 64 0 1 1000000)
    (is (false? (seen/faces-unseen? w [998 60 -10 1010 70 10] 3 -64)) "a place far off, 2 inside a face")
    (is (true? (seen/faces-unseen? w [990 60 -10 1010 70 10] 3 -64)) "a place far off, deep inside")
    (is (nil? (seen/faces-unseen? (tu/fake {}) [0 0 0 1 1 1] 3 -64)) "no memory, no answer")))
