(ns dashboard.ui.mapmodel-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.ui.mapmodel :as mm]))

(deftest body-position
  (are [body expected] (= expected (mm/body-pos body))
    {:view {:pos {:x 1.5 :z 2.5}} :state {:pos {:x 1 :z 2}}} {:x 1.5 :z 2.5}
    {:state {:pos {:x 1 :z 2}}} {:x 1 :z 2}
    {:engine {:pos {:x 7 :z 8}}} {:x 7 :z 8}
    {:view {:pos {:x nil :z 2}} :state {:pos {:x 1 :z 2}}} {:x 1 :z 2}
    {} nil))

(deftest kind-colors
  (are [kind expected] (= expected (mm/kind-color kind))
    "base" mm/base-color
    "farm" mm/farm-color
    "danger" mm/danger-color
    "enemy" mm/danger-color
    "who-knows" mm/place-color
    nil mm/place-color))

(deftest status-colors
  (are [status expected] (= expected (mm/status-color status))
    :working "#3fb950" :idle "#8b949e" :trouble "#d29922" :offline "#6e7681"))

(deftest zoom-labels
  (are [scale expected] (= expected (mm/show-place-labels? scale))
    0.1 false
    0.49 false
    0.5 true
    4 true))

(deftest min-rect
  (are [rect expected] (= expected (select-keys (mm/min-size rect 6) [:px :py :w :h]))
    {:px 10 :py 10 :w 20 :h 30} {:px 10 :py 10 :w 20 :h 30}
    {:px 10 :py 10 :w 2 :h 30} {:px 8 :py 10 :w 6 :h 30}
    {:px 10 :py 10 :w 0 :h 0} {:px 7 :py 7 :w 6 :h 6}))

(deftest rect-hit
  (are [x y expected] (= expected (mm/in-rect? {:px 10 :py 10 :w 20 :h 20} x y))
    10 10 true
    30 30 true
    9 15 false
    15 31 false))

(deftest pick-plan-smallest-first
  (let [plans [{:name "big" :px 0 :py 0 :w 100 :h 100} {:name "small" :px 10 :py 10 :w 10 :h 10}]]
    (are [x y expected] (= expected (:name (mm/pick-plan plans x y)))
      15 15 "small"
      50 50 "big"
      200 200 nil)))

(deftest bodies-bounds
  (are [bodies expected] (= expected (mm/body-points bodies))
    [{:up true :state {:pos {:x 1 :z 2}}} {:up false :state {:pos {:x 50 :z 50}}} {:up true}]
    [{:x 1 :z 2}]
    [] []))

(deftest plan-rect
  (are [region expected] (= expected (mm/plan-box {:region region}))
    {:min [10 60 -70] :max [20 64 -61]} {:x1 10 :z1 -70 :x2 21 :z2 -60}
    {:min [0 0 0] :max [0 0 0]} {:x1 0 :z1 0 :x2 1 :z2 1}
    nil nil))                                           ; a plan with no cells (its blueprint is missing) has no region

(deftest bounds-rect
  (is (= {:x1 -3 :z1 4 :x2 0 :z2 5} (mm/bounds-box {:min [-3 70 4] :max [-1 71 4]}))))

(deftest elements-show-when-zoomed-in
  (are [scale expected] (= expected (mm/show-plan-elements? scale))
    0.5 false
    2.9 false
    3 true
    8 true))

(deftest edge-markers
  (are [item expected] (= expected (mm/edge-marker {:w 200 :h 100 :inset 10} item))
    {:px 100 :py 50} nil
    {:px 0 :py 0} nil
    {:px 200 :py 100} nil
    {:px 100 :py -1} {:x 100 :y 10 :angle (- (/ js/Math.PI 2))}
    {:px 300 :py 50} {:x 190 :y 50 :angle 0}
    {:px -100 :py 50} {:x 10 :y 50 :angle js/Math.PI}
    {:px 100 :py 400} {:x 100 :y 90 :angle (/ js/Math.PI 2)}
    {:px 400 :py 250} {:x 160 :y 90 :angle (js/Math.atan2 200 300)}))

(deftest compact-distance
  (are [blocks expected] (= expected (mm/distance-text blocks))
    0 "0"
    12 "12"
    999 "999"
    1000 "1k"
    2349 "2.3k"
    12500 "12.5k"
    nil ""))

(def tile-index {[0 0] 10 [1 0] 11 [0 1] 12 [5 5] 13 [-1 -1] 14})

(deftest tile-range-of-a-view
  (are [view canvas expected] (= expected (mm/tile-range view canvas))
    {:origin-x 0 :origin-z 0 :scale 1} {:w 32 :h 32} {:cx1 0 :cx2 1 :cz1 0 :cz2 1}
    {:origin-x -1 :origin-z -17 :scale 1} {:w 16 :h 16} {:cx1 -1 :cx2 0 :cz1 -2 :cz2 -1}
    {:origin-x 0 :origin-z 0 :scale 2} {:w 64 :h 32} {:cx1 0 :cx2 1 :cz1 0 :cz2 0}))

(deftest visible-terrain-tiles
  (are [view canvas expected] (= expected (mapv (juxt :cx :cz :mtime :px :py :size) (:items (mm/visible-terrain view canvas tile-index))))
    ;; only the dumped tiles in the viewport; px/py are the tile's corner in pixels, size its edge
    {:origin-x 0 :origin-z 0 :scale 1} {:w 32 :h 32} [[0 0 10 0 0 16] [1 0 11 16 0 16] [0 1 12 0 16 16]]
    {:origin-x 8 :origin-z 8 :scale 2} {:w 32 :h 32} [[0 0 10 -16 -16 32] [1 0 11 16 -16 32] [0 1 12 -16 16 32]]
    {:origin-x 100 :origin-z 100 :scale 1} {:w 32 :h 32} []))

(deftest terrain-mode-by-tile-count
  (are [canvas scale mode] (= mode (:mode (mm/visible-terrain {:origin-x 0 :origin-z 0 :scale scale} canvas tile-index)))
    {:w 800 :h 600} 1 :tiles
    {:w 1920 :h 1080} 1 :coverage
    {:w 1920 :h 1080} 4 :tiles
    ;; 45 x 45 tiles is 2025 > 2000
    {:w 721 :h 721} 1 :coverage
    {:w 1920 :h 1080} 0.05 :coverage))

(deftest coverage-lists-the-dumped-columns-in-view
  (let [{:keys [items]} (mm/visible-terrain {:origin-x -16 :origin-z -16 :scale 0.01} {:w 1920 :h 1080} tile-index)]
    (is (= #{[0 0] [1 0] [0 1] [5 5] [-1 -1]} (set (map (juxt :cx :cz) items))))))
