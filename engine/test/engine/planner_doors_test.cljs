(ns engine.planner-doors-test
  "engine/js/path/planner-doors.test.mjs against the ClojureScript planner: doors, gates and trapdoors opened by hand
  (wood, copper) or by redstone (iron: a button, lever or plate on the approach side)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.planner-fixture :as pf :refer [world near plan]]))

(defn plan-course
  ([name] (pf/course-plan name {}))
  ([name options] (pf/course-plan name options)))
(defn run [snapshot from goal options] (plan snapshot {:from from :goal goal} options))
(defn opened-by [r] (vec (mapcat #(:opens %) (get-in r [:path :steps]))))
(defn cost [r] (get-in r [:path :cost]))
(defn summary [r] (get-in r [:path :summary]))
(defn close? [a b] (< (js/Math.abs (- a b)) 1e-6))

(defn state-of [name props]
  (.stateAt ^js (pf/snapshot {:blocks [[0 64 0 name (clj->js props)]]}) 0 64 0))

(deftest state-table-openables
  (are [name props openable]
       (let [id (state-of name props)]
         (and (= openable (aget (.-openable ^js @pf/table) id))
              (= (get {0 0} openable (state-of name (assoc props :open true)))
                 (aget (.-openState ^js @pf/table) id))))
    "oak_door" {:half "lower" :open false} 1
    "bamboo_door" {:half "upper" :open false} 1
    "crimson_door" {:half "lower" :open false} 1
    "copper_door" {:half "lower" :open false} 1
    "oak_fence_gate" {:open false} 1
    "warped_fence_gate" {:open false} 1
    "oak_trapdoor" {:open false} 1
    "copper_trapdoor" {:open false} 1
    "iron_door" {:half "lower" :open false} 2
    "iron_trapdoor" {:open false} 2
    "oak_door" {:half "lower" :open true} 0
    "oak_fence_gate" {:open true} 0
    "stone" {} 0))

(deftest courses-opened-by-hand
  (are [name pattern block]
       (let [r (plan-course name)]
         (and (= "found" (:status r))
              (re-find pattern (summary r))
              (= [block] (opened-by r))
              (= 1 (:opens (cost r)))))
    "door-closed" #"opens 1 door\b" {:x 2880 :y 161 :z 3216}
    "wood-door" #"opens 1 door\b" {:x 2880 :y 161 :z 3216}
    "fence-gate" #"opens 1 gate\b" {:x 2880 :y 161 :z 3216}))

(deftest course-door-open-nothing-opened
  (let [r (plan-course "door-open")]
    (is (= "found" (:status r)))
    (is (= [] (opened-by r)))
    (is (not (re-find #"opens" (summary r))))))

(deftest course-gate-airlock-two-gates
  (let [r (plan-course "gate-airlock")]
    (is (= "found" (:status r)))
    (is (re-find #"opens 2 gates" (summary r)))
    (is (= 2 (count (opened-by r))))))

(deftest course-plate-door
  (let [r (plan-course "plate-door")]
    (is (= "found" (:status r)))
    (is (re-find #"steps on 1 plate" (summary r)))
    (is (= [{:x 2880 :y 161 :z 3216 :via "plate" :at {:x 2879 :y 161 :z 3216}}] (opened-by r)))))

(deftest course-iron-button
  (let [r (plan-course "iron-button")]
    (is (= "found" (:status r)))
    (is (re-find #"presses 1 button" (summary r)))
    (is (= [{:x 2880 :y 161 :z 3216 :via "button" :at {:x 2879 :y 162 :z 3215}}] (opened-by r)))))

(deftest course-iron-door-is-a-wall
  (is (not= "found" (:status (plan-course "iron-door")))))

(deftest opening-costs-seconds
  (let [hand (:seconds (cost (plan-course "door-closed")))
        dear-hand (:seconds (cost (plan-course "door-closed" {:costs {:open 6}})))
        button (:seconds (cost (plan-course "iron-button")))
        dear-button (:seconds (cost (plan-course "iron-button" {:costs {:openRedstone 6.5}})))]
    (is (close? dear-hand (+ hand 5)))
    (is (close? dear-button (+ button 5)))))

;; hand-built worlds: stone to y 63 (feet cells y 64), a wall x 8 across everything
(defn wall [& extra] (world {:fill (into [[8 64 -2 8 67 40 "stone"]] extra)}))
(def start {:x 2 :y 64 :z 5})
(def goal (near 14 64 5))
(defn door [name z & [props]]
  [[8 64 z 8 64 z name (merge {:half "lower" :facing "east"} props)]
   [8 65 z 8 65 z name (merge {:half "upper" :facing "east"} props)]])
(defn wall-with [& groups] (apply wall (apply concat groups)))

(deftest doors-in-a-wall
  (are [snapshot status opens]
       (let [r (run snapshot start goal {:goalFlood 0})]
         (and (= status (:status r)) (= opens (count (opened-by r)))))
    (wall-with (door "copper_door" 5)) "found" 1
    (wall-with (door "bamboo_door" 5)) "found" 1
    (wall-with (door "oak_door" 5 {:hinge "left"}) (door "oak_door" 6 {:hinge "right"})) "found" 1
    (wall-with (door "iron_door" 5)) "partial" 0
    (wall [8 64 5 8 64 5 "air"]
          [8 65 5 8 65 5 "oak_trapdoor" {:half "bottom" :open false :facing "north"}]) "found" 1))

(defn button [x] [x 65 4 x 65 4 "stone_button" {:face "wall" :facing (get {7 "west" 9 "east"} x)}])
(defn iron-door [x] (wall-with (door "iron_door" 5) [(button x)]))

(deftest iron-door-with-button-on-one-face
  (are [x from to status via]
       (let [r (run (iron-door x) from to {:goalFlood 0})]
         (and (= status (:status r)) (= via (mapv :via (opened-by r)))))
    7 {:x 2 :y 64 :z 5} (near 14 64 5) "found" ["button"]
    9 {:x 2 :y 64 :z 5} (near 14 64 5) "partial" []
    9 {:x 14 :y 64 :z 5} (near 2 64 5) "found" ["button"]
    7 {:x 14 :y 64 :z 5} (near 2 64 5) "partial" []))

(deftest iron-door-with-lever-on-this-side
  (let [lever [7 65 4 7 65 4 "lever" {:face "wall" :facing "west"}]
        r (run (wall-with (door "iron_door" 5) [lever]) start goal {:goalFlood 0})]
    (is (= "found" (:status r)))
    (is (= ["lever"] (mapv :via (opened-by r))))
    (is (re-find #"pulls 1 lever" (summary r)))))

(deftest lever-door-costs-more-than-button-door
  (let [lever [7 65 4 7 65 4 "lever" {:face "wall" :facing "west"}]
        secs (fn [act] (:seconds (cost (run (wall-with (door "iron_door" 5) [act]) start goal {:goalFlood 0}))))
        dear (:seconds (cost (run (wall-with (door "iron_door" 5) [lever]) start goal {:goalFlood 0 :costs {:openLever 20}})))]
    (is (> (secs lever) (secs (button 7))))
    (is (close? (+ (secs lever) (- 20 (:openLever pf/default-costs))) dear))))

(deftest lever-door-loses-to-a-short-walk-around
  (let [lever [7 65 4 7 65 4 "lever" {:face "wall" :facing "west"}]
        r (run (world {:fill [[8 64 0 8 67 40 "stone"] lever
                              [8 64 5 8 64 5 "iron_door" {:half "lower" :facing "east"}]
                              [8 65 5 8 65 5 "iron_door" {:half "upper" :facing "east"}]]})
               start goal {:goalFlood 0})]
    (is (= "found" (:status r)))
    (is (= [] (opened-by r)) "round the end of the wall, no lever")))

(deftest iron-door-prefers-a-button-to-a-nearer-lever
  (let [lever [7 65 4 7 65 4 "lever" {:face "wall" :facing "west"}]
        r (run (wall-with (door "iron_door" 5) [lever [7 65 6 7 65 6 "stone_button" {:face "wall" :facing "west"}]]) start goal {:goalFlood 0})]
    (is (= ["button"] (mapv :via (opened-by r))))))

(deftest button-more-than-4-blocks-away-out-of-reach
  (let [far [3 65 4 3 65 4 "stone_button" {:face "wall" :facing "west"}]
        r (run (wall-with (door "iron_door" 5) [[2 64 3 2 66 3 "stone"] far]) start goal {:goalFlood 0})]
    (is (false? (and (= "found" (:status r)) (pos? (count (opened-by r))))))))

(deftest plate-in-front-of-iron-door-opens-from-this-side-only
  (let [plate (fn [x] [x 64 5 x 64 5 "stone_pressure_plate"])
        west (run (wall-with (door "iron_door" 5) [(plate 7)]) start goal {:goalFlood 0})
        east (run (wall-with (door "iron_door" 5) [(plate 9)]) start goal {:goalFlood 0})]
    (is (= ["found" "partial"] [(:status west) (:status east)]))
    (is (= ["plate"] (mapv :via (opened-by west))))))

(defn hatch
  "a closed wooden floor hatch over a ladder shaft: stone to y 63 with the shaft cut down to y 56, the hatch at y 63"
  [facing]
  (pf/snapshot {:fill [[-2 50 -2 60 63 40 "stone"]
                       [8 56 5 8 62 5 "air"] [3 56 5 7 57 5 "air"]
                       [8 56 5 8 62 5 "ladder" {:facing "west"}]
                       [8 63 5 8 63 5 "oak_trapdoor" {:half "bottom" :open false :facing facing}]]}))

(deftest closed-floor-hatch-over-ladder-going-down
  (are [facing status opens summary-ok]
       (let [r (run (hatch facing) {:x 2 :y 64 :z 5} (near 4 56 5) {:goalFlood 0 :maxDrop 1})]
         (and (= status (:status r))
              (= opens (count (opened-by r)))
              (= summary-ok (boolean (and (re-find #"opens 1 trapdoor" (summary r))
                                          (re-find #"ladder down" (summary r)))))
              ;; going down nothing is pressed to the ladder's wall (the body would stand on its top edge)
              (not-any? :hatch (get-in r [:path :steps]))))
    "east" "found" 1 true
    "north" "found" 1 true
    "west" "found" 1 true))

(defn sill-room
  "a room a step up: its floor (and the doorway's sill) at y 64, so its feet cells are y 65, behind a wall x 8 with an
  oak door in it at z 5 standing on the sill; outside, west of the wall, feet cells are y 64"
  [facing open]
  (wall [9 64 -2 20 64 40 "stone"] [8 64 5 8 64 5 "stone"]
        [8 65 5 8 65 5 "oak_door" {:half "lower" :facing facing :open open}]
        [8 66 5 8 66 5 "oak_door" {:half "upper" :facing facing :open open}]))

(deftest out-of-a-room-through-a-door-on-a-sill-one-step-down
  (are [facing open from]
       (= "found" (:status (run (sill-room facing open) from (near 2 64 5) {:goalFlood 0})))
    "east" false {:x 12 :y 65 :z 5}
    "west" false {:x 12 :y 65 :z 5}
    "east" true {:x 12 :y 65 :z 5}
    "west" true {:x 12 :y 65 :z 5}
    "east" false {:x 8 :y 65 :z 5}
    "west" false {:x 8 :y 65 :z 5}
    "east" true {:x 8 :y 65 :z 5}
    "west" true {:x 8 :y 65 :z 5}))

(deftest into-a-room-through-a-door-on-a-sill-one-step-up
  (are [facing open]
       (= "found" (:status (run (sill-room facing open) {:x 2 :y 64 :z 5} (near 12 65 5) {:goalFlood 0})))
    "east" false
    "west" false
    "east" true
    "west" true))

(defn bed-room
  "A wall at x 8 with a door at z 5 opening onto a bed at x 9, room beyond with its roof at y 70; a lintel over the door
  unless lintel is false."
  [lintel]
  (wall-with (door "oak_door" 5)
             (when-not lintel [[8 66 5 8 67 5 "air"]])
             [[9 64 5 9 64 5 "red_bed" {:part "foot" :facing "east"}] [9 64 4 9 64 4 "stone"] [9 64 6 9 64 6 "stone"]
              [10 64 5 10 64 5 "red_bed" {:part "head" :facing "east"}]
              [9 70 -2 14 70 40 "stone"]]))

(deftest a-door-onto-a-bed-is-passed-only-without-a-two-high-lintel
  (are [lintel flood status] (= status (:status (run (bed-room lintel) start (near 12 64 5) {:goalFlood flood})))
    false 0 "found"
    false 1 "found"
    true 0 "partial"))

(deftest a-gate-over-a-hole-is-opened-and-jumped
  (are [props opens]
       (let [w (world {:fill [[8 60 -2 9 63 40 "air"] [8 64 -2 8 64 40 "oak_fence"]]
                       :blocks [[8 64 5 "oak_fence_gate" (merge {:facing "east"} props)]]})
             r (run w start goal {:goalFlood 0})]
         (and (= "found" (:status r)) (= opens (count (opened-by r)))
              (some #{(:gap pf/MOVE)} (map :move (get-in r [:path :steps])))))
    {:open false} 1
    {:open true} 0))

(deftest a-fence-over-a-hole-is-not-jumped
  (let [w (world {:fill [[8 60 -2 9 63 40 "air"] [8 64 -2 8 64 40 "oak_fence"]]})]
    (is (= "partial" (:status (run w start goal {:goalFlood 0}))))))
