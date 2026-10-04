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

;; ---------------------------------------------------------------- villagers
(defn watcher [name up t villagers & {:keys [dimension] :or {dimension "overworld"}}]
  {:name name :up up :view {:poseT t :dimension dimension :villagers villagers}})

(def now 1000000)

(deftest villager-sightings-are-live-only-from-a-body-that-is-up-and-fresh
  (are [body expected] (= expected (map (juxt :id :old?) (mm/villager-sightings [body] now)))
    (watcher "A" true (- now 2000) [{:id 1 :x 0 :z 0}]) [[1 false]]
    (watcher "A" true (- now mm/villager-live-ms) [{:id 1 :x 0 :z 0}]) [[1 false]]
    (watcher "A" true (- now mm/villager-live-ms 1) [{:id 1 :x 0 :z 0}]) [[1 true]]
    (watcher "A" false (- now 2000) [{:id 1 :x 0 :z 0}]) [[1 true]]))

(deftest villager-sightings-go-after-the-drop-time
  (are [age expected] (= expected (count (mm/villager-sightings [(watcher "A" false (- now age) [{:id 1 :x 0 :z 0}])] now)))
    (dec mm/villager-drop-ms) 1
    mm/villager-drop-ms 1
    (inc mm/villager-drop-ms) 0))

(deftest villager-sightings-carry-position-time-and-who-saw
  (is (= [{:id 1 :x 4 :y 5 :z 6 :seen-by "A" :t (- now 3000) :age-ms 3000 :old? false}]
         (mm/villager-sightings [(watcher "A" true (- now 3000) [{:id 1 :x 4 :y 5 :z 6}])] now))))

(deftest villager-sightings-keep-the-freshest-sighting-of-each-villager
  (let [bodies [(watcher "Old" false (- now 600000) [{:id 1 :x 0 :z 0} {:id 2 :x 10 :z 10}])
                (watcher "New" true (- now 1000) [{:id 1 :x 3 :z 3}])]]
    (is (= [["New" 1 3 false] ["Old" 2 10 true]]
           (map (juxt :seen-by :id :x :old?) (sort-by :id (mm/villager-sightings bodies now)))))))

(deftest villager-sightings-ignore-other-dimensions-and-bodies-without-poses
  (is (= [] (mm/villager-sightings [(watcher "A" true (- now 1000) [{:id 1 :x 0 :z 0}] :dimension "the_nether")
                                    {:name "B" :up true}
                                    {:name "C" :up true :view {:villagers [{:id 2 :x 0 :z 0}]}}]
                                   now))))

(deftest villager-tip-says-who-and-how-long-ago
  (are [s expected] (= expected (mm/villager-tip s))
    {:seen-by "A" :age-ms 3000 :old? false} "villager, seen by A 3s ago"
    {:seen-by "A" :age-ms 300000 :old? true} "villager, seen by A 5m ago (old: nobody watching now)"))

;; ---------------------------------------------------------------- name labels
(deftest pick-label-finds-the-body-name-under-the-point
  (let [boxes [{:kind :body :name "A" :px 10 :py 10 :w 30 :h 12}
               {:kind :place :name "p" :px 100 :py 10 :w 30 :h 12}
               {:kind :body :name "B" :px 10 :py 40 :w 30 :h 12}]]
    (are [x y expected] (= expected (:name (mm/pick-label boxes x y)))
      11 11 "A"
      40 22 "A"
      20 45 "B"
      41 11 nil
      110 15 nil
      0 0 nil)))

;; ---------------------------------------------------------------- players list
(def status-of {"A" :working "B" :idle "C" :offline "A0" :offline})

(deftest player-rows-list-every-body-then-the-humans
  (let [bodies [{:name "C" :up false :state {:pos {:x 1 :y 2 :z 3}}}
                {:name "A0" :up false}
                {:name "B" :up true :state {:pos {:x 10.4 :y 64.6 :z -5.6}}}
                {:name "A" :up true}]
        humans [{:name "Zed" :x 5 :y 6 :z 7 :seenBy "B"}]
        rows (mm/player-rows bodies humans #(status-of (:name %)))]
    (is (= [["A" :body :working "no position"]
            ["B" :body :idle "10, 65, -6"]
            ["A0" :body :offline "no position"]
            ["C" :body :offline "1, 2, 3"]
            ["Zed" :human nil "5, 6, 7"]]
           (map (juxt :name :kind :status :where) rows)))
    (is (= [nil {:x 10.4 :z -5.6} nil {:x 1 :z 3} {:x 5 :z 7}] (map :pos rows)))))

(deftest villager-labels-need-a-closer-zoom
  (are [scale expected] (= expected (mm/show-villager-labels? scale))
    0.5 false
    1.49 false
    1.5 true
    8 true))

(deftest player-click-selects-centres-and-opens-the-popup
  (are [row expected] (= expected (mm/player-click-events row))
    {:kind :body :name "A" :pos {:x 1 :z 2}} [[:select {:kind :body :name "A"}] [:center-on 1 2] [:open-detail "A"]]
    {:kind :body :name "A" :pos nil} [[:select {:kind :body :name "A"}] [:open-detail "A"]]
    {:kind :human :name "Zed" :pos {:x 5 :z 7}} [[:select {:kind :human :name "Zed"}] [:center-on 5 7]]))

;; ---------------------------------------------------------------- plan conflicts on the map
(def pair {:plans ["a" "b"] :count 4 :shown 2 :box {:min [2 64 2] :max [3 64 3]} :cells [[2 64 2] [3 64 3]]})

(deftest a-conflict-is-marked-over-its-box-and-points-at-its-centre
  (is (= [{:kind :conflict :name "a x b: 4 cells" :box {:x1 2 :z1 2 :x2 4 :z2 4} :wx 3 :wz 3 :cells [[2 64 2] [3 64 3]]}]
         (mm/conflict-marks [pair])))
  (is (= [] (mm/conflict-marks nil))))

(deftest a-conflict-label-can-be-clicked-like-a-body-name
  (let [boxes [{:kind :conflict :name "a x b: 4 cells" :px 10 :py 10 :w 90 :h 12 :wx 3 :wz 3}]]
    (is (= "a x b: 4 cells" (:name (mm/pick-label boxes 20 15))))
    (is (nil? (mm/pick-label boxes 200 15)))))
