(ns engine.planner-edge-stop-test
  "engine.path.planner-tuned's options.stopAtEdge (card 7a031d15): a search toward an unloaded goal ends at the first node
  it expands at the loaded edge and names it as its frontier, instead of searching all loaded land first."
  (:require [cljs.test :refer [deftest is async]]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu :refer [floor box]]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]))

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

;; the start itself at the loaded edge, not known (a teleport, a chunk gap): the search does not end on the start (a
;; one-step path, go-to walks nowhere); it ends at another edge node, nearer the goal
(deftest stop-at-edge-does-not-end-on-the-start
  (let [edge-start [62 64 30]
        r (result-over field edge-start far-goal {:weight 1.2 :stopAtEdge true :knownCells (js/Set.)})]
    (is (some? (:frontier r)))
    (is (not= edge-start (cell (:frontier r))))
    (is (pos? (count (get-in r [:frontier :path :steps]))))))

;; a dead end: the land beyond every edge stays unloaded. Each round names an edge not named before (go-to remembers the
;; one it walked to as known), then none is left: a clear end, not the same spot for ever
(deftest stop-at-edge-rounds-name-new-edges-until-none-is-left
  (let [small {:blocks (floor 0 0 15 15)}
        s [8 64 8]
        g [200 64 8]]
    (loop [known (js/Set.) seen [] n 0]
      (let [r (result-over small s g {:weight 1.2 :stopAtEdge true :knownCells known})
            f (some-> (:frontier r) cell)]
        (is (< n 400) "ends")
        (when (and f (< n 400) (not (get-in r [:frontier :known])))
          (is (not (some #{f} seen)) (str "new edge " f))
          (.add known (known-key f g))
          (recur known (conj seen f) (inc n)))))))

;; a body sealed in a 3x3 pen (walls and roof) at a chunk side, the goal far and unloaded (live: 9 nodes). The search
;; ends :start-enclosed (the start flood ran out with no node at the loaded edge); an open start keeps :goal-unloaded. The edge stop
;; changes nothing in a pen: both searches expand the 9 cells and name no frontier (no way leads out), so go-to is told
;; there is no edge to walk to; the edge stop did not end the search on the start
(def pen
  (merge (box 46 63 28 50 63 32 "stone")
         (apply dissoc (box 46 64 28 50 66 32 "stone") (keys (box 47 64 29 49 65 31 "stone")))
         (box 47 66 29 49 66 31 "stone")))

(deftest a-sealed-pen-with-an-unloaded-goal-ends-the-same-with-the-edge-stop
  (doseq [opts [{} {:stopAtEdge true}]]
    (let [r (result-over {:blocks pen} [48 64 30] far-goal (merge {:weight 1.2 :knownCells (js/Set.)} opts))]
      (is (= ["partial" "start-enclosed" 9] [(:status r) (:reason r) (:expanded r)]) (str opts))
      (is (nil? (:frontier r)) (str opts)))))

;; ---- the sweep after the edge stop ----

;; a 1-wide floor (feet 64) along x 0..63 at z 24 (off the chunk-side band along z), the goal far east: the edge stop meets
;; the east end, then the sweep runs west from the start along the corridor
(def corridor-floor (floor 0 24 63 24))
(def corridor-goal [200 64 24])

(defn corridor-result [blocks sx options]
  (result-over {:blocks (merge corridor-floor blocks)} [sx 64 24] corridor-goal (merge {:weight 1.2 :knownCells (js/Set.) :knownEdges (js/Set.)} options)))

;; The sweep leaves the way on west recorded: west of each corridor cell it knows lies an edge it noted, or every cell
;; there in the chunk-side band (where an edge can be) is known. Else, once the land past a chunk side unloads, the known
;; cell reads as a spent edge and nothing says the corridor goes on
(deftest a-swept-cell-known-leaves-its-way-on-recorded
  (doseq [sx (range 24 52)]
    (let [r (corridor-result {} sx {:stopAtEdge true})
          k #(known-key [% 64 24] corridor-goal)
          known? #(.has (:known r) (k %))
          edge-xs (filter #(contains? (set (:edges r)) (k %)) (range 0 64))
          band? #(#{0 1 14 15} (mod % 16))]
      (doseq [x (filter known? (range 0 sx))]
        (is (or (some #(< % x) edge-xs) (every? known? (filter band? (range 0 x))))
            (str "start " sx ": " x " known, nothing west of it recorded; edges " (vec edge-xs)))))))

;; water two cells west of the start (the walker's limits refuse it) and a pit there (a drop over the damage budget): only
;; the sweep meets them. The result is the edge stop's: not limited (no second search without the limits), no damage
;; refused, its reason and frontier unchanged
(def corridor-water (box 38 64 24 38 64 24 "water"))
(def corridor-pit (merge (box 38 57 24 38 57 24 "stone") {"38,63,24" "air"}))

(deftest a-refusal-only-the-sweep-meets-leaves-the-result-alone
  (let [limits {:limits {:kinds planner/AVOID-WATER}}
        damage {:maxDrop 16 :damageBudget 0}]
    (is (true? (:limited (corridor-result corridor-water 40 limits))) "the whole search meets the water")
    (is (true? (:damageRefused (corridor-result corridor-pit 40 damage))) "the whole search meets the pit")
    (doseq [[blocks options] [[corridor-water limits] [corridor-pit damage]]]
      (let [r (corridor-result blocks 40 (assoc options :stopAtEdge true))
            plain (corridor-result {} 40 {:stopAtEdge true})]
        (is (not (:limited r)) (str options))
        (is (not (:damageRefused r)) (str options))
        (is (= (:reason plain) (:reason r)) (str options))
        (is (= (cell (:frontier plain)) (cell (:frontier r))) (str options))))))

;; a search whose edge stop comes after more than half its round's budget: the sweep ends at the budget, and the round
;; ends the search with its frontier (else go-to walks a partial and drops the search, frontier and known land)
(deftest a-sweep-that-meets-the-round-budget-ends-the-search-with-its-frontier
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pw (.pathWorld (tu/fake {:blocks corridor-floor}))
              [gx gy gz] corridor-goal
              query #js {:from #js {:x 40 :y 64 :z 24} :goal #js {:kind "near" :x gx :y gy :z gz :range 0}}
              options (fn [] #js {:table (.-table pw) :space (.-space pw) :weight 1.2 :stopAtEdge true
                                  :knownCells (js/Set.) :knownEdges (js/Set.)})
              whole (planner/plan (.-snapshot pw) query (options))]
          (let [[_ within] (await (tu/with-expansion-budget 2
                                    #(wsearch/run-search! {} {:walled pw :limited (planner/create-plan (.-snapshot pw) query (options))}
                                                          (- (.-expanded whole) 6) nil corridor-goal 0 1.2)))]
            (is (some? within) "the search ends in its round")
            (is (= (cell (js->clj (.-frontier whole) :keywordize-keys true))
                   (some-> within :r .-frontier (js->clj :keywordize-keys true) cell)))))))))

;; ---- nearestToEdge: the walk that brings an open edge back into view ----

;; all loaded land known, and an open edge (options.knownEdges) at (-5 64 8), out of the loaded land: a walkway (feet 76)
;; x 0..20 at z 8, dropping at its east end onto ground (feet 64) along x 21 and back west along z 15 to x 5. The
;; walkway's west end lies 5 columns from the edge, 12 blocks above it; the ground's nearest edge node 10 columns off, on
;; its level. Chunks load along x and z, so the walkway's end is the walk that loads the edge again
(def walkway-over-ground
  (merge (box 0 75 8 20 75 8 "stone") (box 21 63 8 21 63 15 "stone") (box 5 63 15 21 63 15 "stone")))

(deftest the-walk-to-an-open-edge-takes-the-node-nearest-along-x-and-z
  (let [g [200 64 8]
        known (js/Set. (clj->js (for [k (keys walkway-over-ground)
                                      :let [[x y z] (map js/parseInt (.split k ","))]]
                                  (known-key [x (inc y) z] g))))
        r (result-over {:blocks walkway-over-ground} [10 76 8] g
                       {:weight 1.2 :maxDrop 16 :knownCells known :knownEdges (js/Set. #js [(known-key [-5 64 8] g)])})]
    (is (= [0 76 8] (cell (:frontier r))) (str "frontier " (:frontier r)))))
