(ns engine.planner-edge-stop-test
  "engine.path.planner-tuned's options.stopAtEdge (card 7a031d15): a search toward an unloaded goal ends at the first node
  it expands at the loaded edge and names it as its frontier, instead of searching all loaded land first."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu :refer [floor]]))

(defn result-over
  "Plan from [x y z] to the near goal [x y z] (range 0) over a fake world spec; the planner's result as cljs data."
  [spec [fx fy fz] [x y z] options]
  (let [pw (.pathWorld (tu/fake spec))]
    (-> (planner/plan (.-snapshot pw)
                      #js {:from #js {:x fx :y fy :z fz} :goal #js {:kind "near" :x x :y y :z z :range 0}}
                      (js/Object.assign #js {:table (.-table pw) :space (.-space pw)} (clj->js options)))
        (js->clj :keywordize-keys true))))

;; a stone floor x 0..63, z 0..63 (16 chunks loaded, the rest not); the goal far east of it, unloaded
(def field {:blocks (floor 0 0 63 63)})
(def start [10 64 30])
(def far-goal [200 64 30])

(defn cell [m] (mapv m [:x :y :z]))

(deftest by-default-a-search-toward-an-unloaded-goal-searches-all-loaded-land
  (let [r (result-over field start far-goal {})]
    (is (= ["partial" "goal-unloaded"] [(:status r) (:reason r)]))
    (is (> (:expanded r) 4000) "every floor cell")
    (is (= 62 (first (cell (:frontier r)))) "the frontier at the east edge")))

;; with weight 1 nodes come out in order of cost plus heuristic, so the first edge node is the one the full search names
(deftest stop-at-edge-names-the-same-frontier-after-far-fewer-nodes
  (let [full (result-over field start far-goal {:weight 1})
        early (result-over field start far-goal {:weight 1 :stopAtEdge true})]
    (is (= ["partial" "goal-unloaded"] [(:status early) (:reason early)]))
    (is (= (cell (:frontier full)) (cell (:frontier early))))
    (is (= (cell (:frontier early)) (cell (last (get-in early [:frontier :path :steps])))) "its path ends there")
    (is (< (:expanded early) 200) (str "expanded " (:expanded early) " of " (:expanded full)))))

;; go-to's weight 1.2: the frontier is still a node at the loaded edge toward the goal
(deftest stop-at-edge-with-weight-ends-at-the-edge-toward-the-goal
  (let [r (result-over field start far-goal {:weight 1.2 :stopAtEdge true})]
    (is (= 62 (first (cell (:frontier r)))))
    (is (< (:expanded r) 200))))

(defn known-key
  "The planner's knownKey of cell [x y z] for goal [gx _ gz]."
  [[x y z] [gx _ gz]]
  (+ (* (+ (* (+ (- x gx) 1024) 2048) (+ (- z gz) 1024)) 1024) (+ y 512)))

;; an edge node earlier searches knew to its end does not end the search: it ends at another edge node
(deftest stop-at-edge-passes-known-land
  (let [first-stop (cell (:frontier (result-over field start far-goal {:weight 1 :stopAtEdge true})))
        known (js/Set. #js [(known-key first-stop far-goal)])
        r (result-over field start far-goal {:weight 1 :stopAtEdge true :knownCells known})]
    (is (some? (:frontier r)))
    (is (not= first-stop (cell (:frontier r))))
    (is (not (get-in r [:frontier :known])))))

;; a loaded goal: the option changes nothing
(deftest stop-at-edge-leaves-a-loaded-goal-alone
  (let [r (result-over field start [60 64 30] {:stopAtEdge true})]
    (is (= "found" (:status r)))))

;; the cells a search that stopped at the edge did not expand are not known land
(deftest stop-at-edge-knows-only-what-it-expanded
  (let [r (result-over field start far-goal {:weight 1 :stopAtEdge true :knownCells (js/Set.)})
        full (result-over field start far-goal {:weight 1 :knownCells (js/Set.)})]
    (is (< 0 (.-size (:known r)) (.-size (:known full))))))
