(ns view.camera-test
  "The camera maths and its inverse (ported from test/view-project.test.mjs)."
  (:require [clojure.test :refer [deftest are is]]
            [view.camera :as cam]))

(def W 640)
(def H 360)
(def cameras [{:yaw 0 :pitch 0 :fov 70} {:yaw 0.7 :pitch -0.3 :fov 90} {:yaw -2.1 :pitch 0.4 :fov 50}])
(def pixels [[0 0] [320 180] [639 359] [17 301] [500 42]])
(def eye {:x 8 :y 66 :z 15})
(def level (cam/camera-basis {:yaw 0 :pitch 0 :fov 70}))

(deftest project-point-inverts-ray-dir
  (doseq [camera cameras [px py] pixels]
    (let [basis (cam/camera-basis camera)
          d (cam/ray-dir basis px py W H)
          back (cam/project-point basis (update-vals d #(* % 7.3)) W H)]
      (is (< (abs (- (:px back) px)) 1e-6) (str "px " (:px back) " vs " px))
      (is (< (abs (- (:py back) py)) 1e-6) (str "py " (:py back) " vs " py)))))

(deftest project-point-is-nil-behind-and-at-the-camera-plane
  (are [p] (nil? (cam/project-point level p W H))
    {:x 0 :y 0 :z 1} {:x 3 :y 1 :z 0} {:x 0 :y 0 :z 0}))

(deftest face-region-shrinks-by-the-inset
  (let [face {:axis :z :at 5 :x [4 12] :y [64 68]}
        full (cam/face-region level eye face W H 0)
        inset (cam/face-region level eye face W H 0.25)]
    (is (= [true true true true]
           [(> (:x0 inset) (:x0 full)) (< (:x1 inset) (:x1 full)) (> (:y0 inset) (:y0 full)) (< (:y1 inset) (:y1 full))]))
    (is (<= (abs (- (+ (:x0 full) (:x1 full)) (+ (:x0 inset) (:x1 inset)))) 2))))

(deftest face-region-handles-each-axis-and-a-face-behind-the-camera
  (are [face visible] (= visible (some? (cam/face-region level eye face W H)))
    {:axis :z :at 5 :x [4 12] :y [64 68]} true
    {:axis :y :at 65 :x [6 10] :z [8 11]} true
    {:axis :x :at 12 :y [64 68] :z [5 9]} true
    {:axis :z :at 20 :x [4 12] :y [64 68]} false))

(deftest js-faces-agree-with-the-maps
  (let [basis (cam/js-camera-basis #js {:yaw 0 :pitch 0 :fov 70})
        region (cam/js-face-region basis #js {:x 8 :y 66 :z 15} #js {:axis "z" :at 5 :x #js [4 12] :y #js [64 68]} W H js/undefined)
        want (cam/face-region level eye {:axis :z :at 5 :x [4 12] :y [64 68]} W H)]
    (is (= want {:x0 (.-x0 region) :y0 (.-y0 region) :x1 (.-x1 region) :y1 (.-y1 region)}))
    (is (= 0 (.-x (cam/js-direction-for 0 0))))))
