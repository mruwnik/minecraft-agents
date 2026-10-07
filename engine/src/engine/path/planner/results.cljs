(ns engine.path.planner.results
  "Search methods: a finished search's result: path, steps, summary, frontier and progress."
  (:require [engine.path.planner.base :refer [CLIMB-NAMES KIND-DOOR KIND-GATE KIND-TRAPDOOR LADDER MIN-CLOSER MOVE-CLIMB-UP MOVE-CORNER MOVE-DROP MOVE-GAP MOVE-JUMP MOVE-JUMP-CLIMB MOVE-OPEN MOVE-SWIM MOVE-SWIM-DOWN MOVE-SWIM-UP OPEN-REACH UNLOADED wall-dx wall-dz]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

;; ---- results (cold: once per plan) ----

(defn- stand16 [^js step] (+ (* (.-y step) 16) (.-h step)))

(defn- count-steps [^js steps from pred]
  (loop [k from n 0]
    (if (< k (.-length steps))
      (recur (inc k) (if ^boolean (pred (aget steps k) (when (pos? k) (aget steps (dec k)))) (inc n) n))
      n)))

(defn- some-of [n one many] (when (pos? n) (if (== n 1) one (str n many))))

(defn- phrase [verb noun n] (when (pos? n) (str verb " " n " " noun (if (> n 1) "s" ""))))

(defn- move-of [^js step] (.-move step))

(defn- climbing-move? [m] (and (>= m MOVE-CLIMB-UP) (<= m MOVE-OPEN)))

(defn- swimming-move? [m] (>= m MOVE-SWIM))

(defn- distance [^js a ^js b] (js/Math.hypot (- (.-x a) (.-x b)) (- (.-z a) (.-z b))))

;; consecutive legs with one key are one run: "ladder up 9"; `key-of` gives a leg's key or nil for a leg that ends the run
(defn- run-strings [^js legs key-of]
  (let [keys #js []
        counts #js []]
    (loop [k 0 prev nil]
      (when (< k (.-length legs))
        (let [key (key-of (aget legs k))]
          (cond
            (nil? key) (recur (inc k) nil)
            (and (some? prev) (identical? prev key))
            (do (aset counts (dec (.-length counts)) (inc (aget counts (dec (.-length counts)))))
                (recur (inc k) key))
            :else (do (.push keys key)
                      (.push counts 1)
                      (recur (inc k) key))))))
    (.map keys (fn [key k] (str key " " (aget counts k))))))

(defn- legs-of [^js steps]
  (let [legs #js []]
    (loop [k 1]
      (when (< k (.-length steps))
        (.push legs #js [(aget steps k) (aget steps (dec k))])
        (recur (inc k))))
    legs))

(defn- depth [^js leg]
  (let [^js s (aget leg 0) ^js p (aget leg 1)]
    (js/Math.round (/ (- (stand16 p) (stand16 s)) 16))))

(defn- sum-hypot [^js legs]
  (.reduce legs (fn [sum ^js leg] (+ sum (distance (aget leg 0) (aget leg 1)))) 0))

(defn- max-of [^js xs] (.apply js/Math.max nil xs))

(extend-type Search
  Object

  ;; ---- results ----
  (stepsTo [s node]
    (let [out #js []]
      (loop [i node]
        (when-not (== i -1)
          (let [shape (aget (.-shapes s) i)
                tight (== (bit-and (bit-shift-right shape 4) 1) 1)
                x (aget (.-xs s) i)
                z (aget (.-zs s) i)
                step #js {:x x :y (aget (.-ys s) i) :z z :h (aget (.-hs s) i) :move (aget (.-moves s) i) :corner (== (aget (.-corners s) i) 1)
                          :px (+ x (if tight (/ (bit-and (bit-shift-right shape 5) 31) 16) 0.5))
                          :pz (+ z (if tight (/ (bit-and (bit-shift-right shape 10) 31) 16) 0.5))}]
            (when ^boolean (.isWater s x (aget (.-ys s) i) z) (unchecked-set step "swim" true))
            (let [dmg (- (aget (.-dmgs s) i) (if (neg? (aget (.-parents s) i)) 0 (aget (.-dmgs s) (aget (.-parents s) i))))]
              (when (pos? dmg) (unchecked-set step "damage" dmg)))
            (when (pos? (aget (.-opens s) i)) (unchecked-set step "opens" (aget ^js (.-open-lists s) (dec (aget (.-opens s) i)))))
            (let [p (aget (.-parents s) i)
                  wall (.hatchWall s x (aget (.-ys s) i) z)]
              (when (and (pos? wall) (>= p 0) (< (aget (.-ys s) p) (aget (.-ys s) i)))
                (unchecked-set step "hatch" true)
                (unchecked-set step "px" (+ x 0.5 (* 0.3 (aget wall-dx wall))))
                (unchecked-set step "pz" (+ z 0.5 (* 0.3 (aget wall-dz wall))))))
            (when (== (bit-and (bit-shift-right shape 15) 1) 1)
              (unchecked-set step "cx" (+ x (/ (bit-and (bit-shift-right shape 16) 31) 16)))
              (unchecked-set step "cz" (+ z (/ (bit-and (bit-shift-right shape 21) 31) 16))))
            (.push out step))
          (recur (aget (.-parents s) i))))
      (.withBends s (.reverse out))))

  ;; the facing (1 east, 2 west, 3 south, 4 north) of the ladder under a trapdoor at x,y,z, or 0: a climb up into that cell
  ;; is aimed at the ladder's wall, where the body stands on the ladder's top edge when the trapdoor's panel leaves it free
  (hatchWall [s x y z]
    (let [id (.stateAt ^js (.-snapshot s) x y z)
          below (.stateAt ^js (.-snapshot s) x (dec y) z)]
      (if (or (== id UNLOADED) (== below UNLOADED) (not (== (aget (.-tbl-open-kind s) id) KIND-TRAPDOOR))
              (not (== (aget (.-tbl-climb-name s) below) LADDER)))
        0
        (aget (.-tbl-facing s) below))))

  ;; "ladder up 9": consecutive climbing legs on one kind of climbable in one direction are one run
  (climbRuns [s ^js legs]
    (run-strings legs
                 (fn [^js leg]
                   (let [^js st (aget leg 0) ^js p (aget leg 1)]
                     (when (climbing-move? (.-move st))
                       (let [named (aget (.-tbl-climb-name s) (.stateAt ^js (.-snapshot s) (.-x st) (.-y st) (.-z st)))
                             named (if (zero? named) (aget (.-tbl-climb-name s) (.stateAt ^js (.-snapshot s) (.-x p) (.-y p) (.-z p))) named)]
                         (str (js/String (aget CLIMB-NAMES named)) " " (if (> (.-y st) (.-y p)) "up" "down"))))))))

  ;; "water column up 20", "bubble lift up 18", "magma column down 15": consecutive vertical swim legs of one kind and way
  (swimRuns [s ^js legs]
    (run-strings legs
                 (fn [^js leg]
                   (let [^js st (aget leg 0) ^js p (aget leg 1)
                         m (.-move st)]
                     (when (or (== m MOVE-SWIM-UP) (== m MOVE-SWIM-DOWN))
                       (let [bubbles (js/Math.max (aget (.-tbl-bubble s) (.stateAt ^js (.-snapshot s) (.-x st) (.-y st) (.-z st)))
                                                  (aget (.-tbl-bubble s) (.stateAt ^js (.-snapshot s) (.-x p) (.-y p) (.-z p))))]
                         (if (== m MOVE-SWIM-UP)
                           (if (== bubbles 1) "bubble lift up" "water column up")
                           (if (== bubbles 2) "magma column down" "water column down"))))))))

  ;; "opens 2 doors", "opens 1 gate", "presses 1 button", "steps on 1 plate": what the path opens, by what opens it
  (openRuns [s ^js steps]
    (let [all #js []]
      (loop [k 0]
        (when (< k (.-length steps))
          (let [opens-here (unchecked-get (aget steps k) "opens")]
            (when (some? opens-here) (.apply (.-push all) all opens-here)))
          (recur (inc k))))
      (let [by-hand (fn [kind-code noun]
                      (phrase "opens" noun
                              (.-length (.filter all (fn [^js o]
                                                       (and (undefined? (unchecked-get o "via"))
                                                            (== (aget (.-tbl-open-kind s) (.stateAt ^js (.-snapshot s) (.-x o) (.-y o) (.-z o))) kind-code)))))))
            by-via (fn [via verb noun]
                     (phrase verb noun (.-length (.filter all (fn [^js o] (identical? (unchecked-get o "via") via))))))]
        (.filter #js [(by-hand KIND-DOOR "door") (by-hand KIND-GATE "gate") (by-hand KIND-TRAPDOOR "trapdoor")
                      (by-via "button" "presses" "button") (by-via "lever" "pulls" "lever") (by-via "plate" "steps on" "plate")]
                 some?))))

  (summarize [s ^js steps node]
    (let [legs (legs-of steps)
          blocks (js/Math.round (sum-hypot legs))
          rise? (fn [^js leg]
                  (let [^js st (aget leg 0) ^js p (aget leg 1)
                        m (.-move st)]
                    (and (not (climbing-move? m)) (not (swimming-move? m)) (not (== m MOVE-DROP)) (not (== m MOVE-GAP))
                         (> (stand16 st) (stand16 p)))))
          ups (.-length (.filter legs rise?))
          deep? (fn [d] (>= d 2))
          falls (.filter (.map (.filter legs (fn [^js leg] (let [^js st (aget leg 0)] (and (== (.-move st) MOVE-DROP) (not ^boolean (.isWater s (.-x st) (.-y st) (.-z st))))))) depth) deep?)
          splashes (.filter (.map (.filter legs (fn [^js leg] (let [^js st (aget leg 0)] (and (== (.-move st) MOVE-DROP) ^boolean (.isWater s (.-x st) (.-y st) (.-z st)))))) depth) deep?)
          swum (js/Math.round (sum-hypot (.filter legs (fn [^js leg] (let [^js st (aget leg 0) m (.-move st)]
                                                                       (or (== m MOVE-SWIM) (and (== m MOVE-CORNER) (true? (unchecked-get st "swim")))))))))
          lowest (js/Math.max 0 (- (.-c-air-supply s) (aget (.-peaks s) node))) ; a grace lets the peak pass the supply
          fall-hp (.reduce steps (fn [sum ^js st] (if (and (== (.-move st) MOVE-DROP) (some? (unchecked-get st "damage"))) (+ sum (unchecked-get st "damage")) sum)) 0)
          gaps (.-length (.filter legs (fn [^js leg] (== (.-move ^js (aget leg 0)) MOVE-GAP))))
          slides (count-steps steps 0 (fn [^js st _] (true? (.-corner st))))
          gap-ups (.-length (.filter legs (fn [^js leg] (let [^js st (aget leg 0) ^js p (aget leg 1)] (and (== (.-move st) MOVE-GAP) (> (stand16 st) (stand16 p)))))))
          lava (loop [k 0] (cond (== k (.-length steps)) false
                                 (let [^js st (aget steps k)] ^boolean (.lavaNear s (.-x st) (.-y st) (.-z st))) true
                                 :else (recur (inc k))))
          parts #js [(str blocks " blocks")
                     (when (pos? swum) (str "swims " swum))]]
      (.apply (.-push parts) parts (.swimRuns s legs))
      (.apply (.-push parts) parts (.climbRuns s legs))
      (.apply (.-push parts) parts (.openRuns s steps))
      (.push parts (some-of ups "1 step up" " steps up"))
      (.push parts (when (== (.-length falls) 1) (str "1 drop of " (aget falls 0) (if (pos? fall-hp) (str " (" (/ (js/Math.round (* 10 fall-hp)) 10) " hp)") ""))))
      (.push parts (when (> (.-length falls) 1) (str (.-length falls) " drops, deepest " (max-of falls))))
      (.push parts (when (== (.-length splashes) 1) (str "drops " (aget splashes 0) " into water")))
      (.push parts (when (> (.-length splashes) 1) (str (.-length splashes) " drops into water, deepest " (max-of splashes))))
      (.push parts (some-of gaps "1 gap jump" " gap jumps"))
      (.push parts (some-of slides "1 corner slide" " corner slides"))
      (.push parts (some-of gap-ups "1 jump up over a gap" " jumps up over gaps"))
      (.push parts (when lava "passes 1 cell from lava"))
      (.push parts (when (pos? (aget (.-peaks s) node)) (str "lowest air " (js/Math.round lowest) " s")))
      (.join (.filter parts some?) ", ")))

  (pathTo [s node]
    (let [steps (.stepsTo s node)
          n (.-length steps)
          cost #js {:seconds (aget (.-secs s) node)
                    :risk (aget (.-risks s) node)
                    :damage (aget (.-dmgs s) node)
                    :drown (aget (.-drowns s) node)
                    :darkSeconds (aget (.-darks s) node)
                    :maxDrop (loop [k 1 best 0]
                               (if (< k n)
                                 (let [^js st (aget steps k) ^js p (aget steps (dec k))]
                                   (recur (inc k) (js/Math.max best (if (and (== (.-move st) MOVE-DROP) (not (true? (unchecked-get st "swim"))))
                                                                      (/ (- (stand16 p) (stand16 st)) 16) 0))))
                                 best))
                    :jumps (count-steps steps 0 (fn [^js st _] (let [m (.-move st)] (or (== m MOVE-JUMP) (== m MOVE-GAP) (== m MOVE-JUMP-CLIMB)))))
                    :climbed (count-steps steps 0 (fn [^js st _] (climbing-move? (.-move st))))
                    :opens (loop [k 0 total 0]
                             (if (< k n)
                               (let [opened (unchecked-get (aget steps k) "opens")]
                                 (recur (inc k) (+ total (if (some? opened) (.-length opened) 0))))
                               total))
                    :waterSeconds (aget (.-wsecs s) node)
                    :airMin (js/Math.max 0 (- (.-c-air-supply s) (aget (.-peaks s) node)))
                    :waterDrop (loop [k 1 best 0]
                                 (if (< k n)
                                   (let [^js st (aget steps k) ^js p (aget steps (dec k))]
                                     (recur (inc k) (js/Math.max best (if (and (== (.-move st) MOVE-DROP) (true? (unchecked-get st "swim")))
                                                                        (/ (- (stand16 p) (stand16 st)) 16) 0))))
                                   best))}]
      #js {:steps steps :cost cost :summary (.summarize s steps node)}))

  ;; Did the search run out of land with no node at the loaded edge: the start's region is closed in the loaded world?
  (startEnclosed [s]
    (and (identical? (.-reason s) "exhausted") (== (.-edge-node s) -1)
         (loop [i 0]
           (cond
             (>= i (.-n-nodes s)) true
             ^boolean (.atLoadedEdge s i) false
             :else (recur (inc i))))))

  (outcome [s status why path one-way]
    (let [frontier (when-not (identical? status "found") (.frontierOf s))
          why (if (and (identical? why "goal-unloaded") ^boolean (.startEnclosed s)) "start-enclosed" why)]
      #js {:status status
           :reason why
           :ms (.-elapsed s)
           :expanded (.-expanded s)
           :stats #js {:masks (.-masks s) :tightMasks (.-tight-masks s) :tightCells (.-size ^js (.-tight-seen s)) :regions (.-regions-seen s) :maskMs (.-mask-ms s) :flooded (.-flooded s) :preFlooded (.-pre-flooded s)}
           :path path
           :oneWay one-way
           :frontier frontier
           :known ^js (.-known-new s)
           :edges ^js (.-edges-new s)
           :searchedOut ^boolean (.-searched-out s)
           :limited ^boolean (.-limit-refused s)
           :damageRefused (and ^boolean (.-damage-refused s) (not (identical? status "found")))}))

  ;; ends the search if it is not over; an exhausted search that turned a ladder away at a gap, or a swim move for lack of air,
  ;; says so
  (settle [s]
    (when-not ^boolean (.-finished s) (.finish s "budget"))
    (cond
      (and (identical? (.-reason s) "exhausted") ^boolean (.-gap-seen s)) (set! (.-reason s) "ladder-gap")
      (and (identical? (.-reason s) "exhausted") ^boolean (.-lethal-seen s)) (set! (.-reason s) "air-lethal")
      (and (identical? (.-reason s) "exhausted") ^boolean (.-air-seen s)) (set! (.-reason s) "air")))

  ;; the first one-way step on the way to the node nearest the goal, -1 when there is none or the result has no partial end
  (oneWayNode [s]
    (if (or (nil? (.-reason s)) (identical? (.-reason s) "start-not-standable") (identical? (.-reason s) "goal-not-standable") ^boolean (.-returnable s) (== (.-best-node s) -1))
      -1
      (.firstOneWay s (.-best-node s))))

  ;; Does the node stand within OPEN-REACH columns of unloaded land (at its feet height)? Then the land it is on runs on past what
  ;; is loaded; a node with loaded land all round it is a dead end as far as the world is known.
  (atLoadedEdge [s node]
    (let [x (aget (.-xs s) node) y (aget (.-ys s) node) z (aget (.-zs s) node)]
      (loop [dx (- OPEN-REACH) dz (- OPEN-REACH)]
        (cond
          (> dx OPEN-REACH) false
          (> dz OPEN-REACH) (recur (inc dx) (- OPEN-REACH))
          (== (.stateAt ^js (.-snapshot s) (+ x dx) y (+ z dz)) UNLOADED) true
          :else (recur dx (inc dz))))))

  (nearest [s]
    #js {:path (if (== (.-best-node s) -1) nil (.pathTo s (.-best-node s))) :distance (.-best-distance s)})

  ;; Where an unfinished search has got to: {path distance startDistance oneWay}.
  ;; - path: the way to the expanded node nearest the goal, cut before the first step the body cannot undo, so the
  ;;   body can always come back. nil (distance the start's) when the cut leaves no step or its end stands on magma.
  ;; - distance and startDistance: that end's and the start's distance to the goal.
  ;; - oneWay: set when the way to the nearest node holds such a step: {move x y z distance path open}, with the
  ;;   uncut path. open is true when the nearest node stands at the loaded edge (atLoadedEdge, not on magma).
  ;; nil when nothing was expanded but the start, or there is neither.
  (progress [s]
    (if (or (not ^boolean (.-started s)) (<= (.-best-node s) 0) ^boolean (.-verifying s))
      nil
      (let [ow (.firstOneWay s (.-best-node s))
            end (if (neg? ow) (.-best-node s) (aget (.-parents s) ow))
            cut (when-not (or (<= end 0) ^boolean (.endsOnMagma s (aget (.-xs s) end) (aget (.-ys s) end) (aget (.-zs s) end) (aget (.-hs s) end)))
                  end)
            one-way (when-not (neg? ow)
                      #js {:move (aget (.-moves s) ow) :x (aget (.-xs s) ow) :y (aget (.-ys s) ow) :z (aget (.-zs s) ow)
                           :distance (.-best-distance s) :path (.pathTo s (.-best-node s))
                           :open (and ^boolean (.atLoadedEdge s (.-best-node s))
                                      (not ^boolean (.endsOnMagma s (aget (.-xs s) (.-best-node s)) (aget (.-ys s) (.-best-node s)) (aget (.-zs s) (.-best-node s)) (aget (.-hs s) (.-best-node s)))))})]
        (when (or (some? cut) (some? one-way))
          #js {:path (when (some? cut) (.pathTo s cut))
               :distance (if (some? cut) (.distanceTo s (aget (.-xs s) cut) (aget (.-zs s) cut)) (.-start-distance s))
               :startDistance (.-start-distance s)
               :oneWay one-way}))))

  ;; The key of cell x y z in options.knownCells and the result's known: relative to the goal along x and z (a frontier
  ;; node lies within frontier-reach of it), so the memory of one goal's searches holds small numbers.
  ;; -1 for a cell the packing cannot hold (1024 or more from the goal along x or z, y outside -512..511).
  (knownKey [s x y z]
    (let [dx (- x (.-goal-x s)) dz (- z (.-goal-z s))]
      (if (or (>= (js/Math.abs dx) 1024) (>= (js/Math.abs dz) 1024) (< y -512) (>= y 512))
        -1
        (+ (* (+ (* (+ dx 1024) 2048) (+ dz 1024)) 1024) (+ y 512)))))

  ;; The frontier node: where the searched land runs on into land not loaded, so a way may go on there. Of the nodes
  ;; at the loaded edge (atLoadedEdge) within frontier-reach blocks of the goal (along x and along z), the one with
  ;; the least cost plus heuristic. -1 when none. Only columns within OPEN-REACH of a chunk's side can be at the edge.
  ;; With options.knownCells (land earlier searches toward this goal searched to the end, while the land round it was
  ;; loaded: the loaded land follows the body, so land searched before reads as a loaded edge again once it unloads), a
  ;; node in it is taken only when no other node is at the edge, and options.knownEdges, when given, still holds an edge
  ;; no search has known since (one an earlier search saw and did not take, which a way through known land may lead
  ;; back to); else there is no frontier. While known-new is a Set, the scan adds there the key of each such node
  ;; not at the edge that the search expanded (land this search knows to its end; a search that ended at its first edge
  ;; node, edgeStop, leaves the nodes it did not expand out), and to edges-new the key of each unknown node at the edge.
  (frontierNode [s]
    (let [lo OPEN-REACH
          hi (- 16 OPEN-REACH)
          track (some? ^js (.-known-new s))]
      (loop [i 0
             best -1
             best-f js/Infinity
             kbest -1
             kbest-f js/Infinity]
        (if (< i (.-n-nodes s))
          (let [x (aget (.-xs s) i) y (aget (.-ys s) i) z (aget (.-zs s) i)
                mx (bit-and x 15) mz (bit-and z 15)
                expanded (== (aget (.-heap-pos s) i) -2)]
            (if (and (or (< mx lo) (>= mx hi) (< mz lo) (>= mz hi))
                     (<= (js/Math.max (js/Math.abs (- x (.-goal-x s))) (js/Math.abs (- z (.-goal-z s)))) (.-frontier-reach s))
                     (not ^boolean (.endsOnMagma s x y z (aget (.-hs s) i))))
              (let [k (when (some? ^js (.-known-cells s)) (.knownKey s x y z))
                    known (and (some? k) ^boolean (.has ^js (.-known-cells s) k))
                    edge ^boolean (.atLoadedEdge s i)
                    f (+ (aget (.-gs s) i) (.heuristic s x z))]
                (cond
                  (not edge) (do (when (and track (not known) expanded) (.add ^js (.-known-new s) k))
                                 (recur (inc i) best best-f kbest kbest-f))
                  (and known (< f kbest-f)) (recur (inc i) best best-f i f)
                  known (recur (inc i) best best-f kbest kbest-f)
                  :else (do (when (some? ^js (.-edges-new s)) (.push ^js (.-edges-new s) k))
                            (if (< f best-f)
                              (recur (inc i) i f kbest kbest-f)
                              (recur (inc i) best best-f kbest kbest-f)))))
              (do (when (and track (some? ^js (.-known-edges s)))
                    (.noteOffBand s i x y z expanded))
                  (recur (inc i) best best-f kbest kbest-f))))
          (cond
            (not (neg? best)) best
            (neg? kbest) kbest
            :else (let [open (.openEdges s)]
                    (cond
                      (nil? open) kbest
                      (zero? (.-length open)) (do (set! (.-searched-out s) true) -1)
                      :else (.nearestToEdge s open))))))))

  ;; An off-band node of a search that ended at its first edge (edgeStop) and was never expanded lies in land this search
  ;; did not cover: it is left in edges-new (a handful: the fringe of the search) as an edge a way may still lead from. An
  ;; expanded node that options.knownEdges holds is covered now: it goes in known-new.
  (noteOffBand [s i x y z ^boolean expanded]
    (let [k (.knownKey s x y z)]
      (cond
        (neg? k) nil
        (and expanded ^boolean (.has ^js (.-known-edges s) k)) (.add ^js (.-known-new s) k)
        (and (not expanded) (>= (.-edge-node s) 0) (< (.-length ^js (.-edges-new s)) 64)
             (<= (js/Math.max (js/Math.abs (- x (.-goal-x s))) (js/Math.abs (- z (.-goal-z s)))) (.-frontier-reach s))
             (not (and (some? ^js (.-known-cells s)) ^boolean (.has ^js (.-known-cells s) k))))
        (.push ^js (.-edges-new s) k))))

  ;; the cells of options.knownEdges this search has not known to its end (known-new) as keys, an array; nil when it holds none
  ;; (absent options.knownEdges: taken as open).
  (openEdges [s]
    (when (some? ^js (.-known-edges s))
      (let [out #js []
            it (.values ^js (.-known-edges s))]
        (loop []
          (let [^js n (.next it)]
            (cond
              ^boolean (.-done n) out
              (and (some? ^js (.-known-new s)) ^boolean (.has ^js (.-known-new s) (.-value n))) (recur)
              :else (do (.push out (.-value n)) (recur))))))))

  ;; the cell [x z] of a knownKey
  (keyX [s k] (+ (- (js/Math.floor (/ (js/Math.floor (/ k 1024)) 2048)) 1024) (.-goal-x s)))

  (keyZ [s k] (+ (- (mod (js/Math.floor (/ k 1024)) 2048) 1024) (.-goal-z s)))

  ;; With no unknown edge left in the loaded land and an open edge out of sight (options.knownEdges, one an earlier search
  ;; saw and did not cover; the loaded land follows the body), the known frontier node that lies nearest to such an edge
  ;; (ties: the least cost plus heuristic): the walk to it brings that edge back into view. -1 when none.
  (nearestToEdge [s open]
    (let [lo OPEN-REACH
          hi (- 16 OPEN-REACH)]
      (loop [i 0 best -1 best-d js/Infinity best-f js/Infinity]
        (if (< i (.-n-nodes s))
          (let [x (aget (.-xs s) i) y (aget (.-ys s) i) z (aget (.-zs s) i)
                mx (bit-and x 15) mz (bit-and z 15)]
            (if (and (or (< mx lo) (>= mx hi) (< mz lo) (>= mz hi))
                     (<= (js/Math.max (js/Math.abs (- x (.-goal-x s))) (js/Math.abs (- z (.-goal-z s)))) (.-frontier-reach s))
                     (not ^boolean (.endsOnMagma s x y z (aget (.-hs s) i)))
                     ^boolean (.atLoadedEdge s i))
              (let [d (loop [j 0 d js/Infinity]
                        (if (< j (.-length open))
                          (let [k (aget open j)]
                            (recur (inc j) (js/Math.min d (js/Math.max (js/Math.abs (- x (.keyX s k))) (js/Math.abs (- z (.keyZ s k)))))))
                          d))
                    f (+ (aget (.-gs s) i) (.heuristic s x z))]
                (if (or (< d best-d) (and (== d best-d) (< f best-f)))
                  (recur (inc i) i d f)
                  (recur (inc i) best best-d best-f)))
              (recur (inc i) best best-d best-f)))
          best))))

  ;; the result's frontier: {x y z path known} of frontierNode for a search that ran out of land to search (exhausted,
  ;; its box, a ladder at a gap, air), nil otherwise or when no node stands at the loaded edge. known: the node is in
  ;; options.knownCells. A search that ran out of land to its end (exhausted, ladder-gap) with options.knownCells also
  ;; collects the result's known (frontierNode). A search that ended at its first edge node (edgeStop) names that node.
  (frontierOf [s]
    (when (or (identical? (.-reason s) "exhausted") (identical? (.-reason s) "box") (identical? (.-reason s) "ladder-gap") (identical? (.-reason s) "air"))
      (when (and (some? ^js (.-known-cells s)) (or (identical? (.-reason s) "exhausted") (identical? (.-reason s) "ladder-gap")))
        (set! (.-known-new s) (js/Set.))
        (set! (.-edges-new s) #js []))
      (let [scanned (.frontierNode s)
            node (if (neg? (.-edge-node s)) scanned (.-edge-node s))]
        (when-not (neg? node)
          (let [x (aget (.-xs s) node) y (aget (.-ys s) node) z (aget (.-zs s) node)]
            #js {:x x :y y :z z :path (.pathTo s node)
                 :known (and (some? ^js (.-known-cells s)) ^boolean (.has ^js (.-known-cells s) (.knownKey s x y z)))
                 :target (when (some? ^js (.-known-cells s)) (.targetOf s x z))})))))

  ;; [x z] of the cell of options.knownEdges, not covered by this search, nearest to x z; nil when none
  (targetOf [s x z]
    (let [open (.openEdges s)]
      (when (and (some? open) (pos? (.-length open)))
        (loop [j 0 best nil best-d js/Infinity]
          (if (< j (.-length open))
            (let [k (aget open j)
                  kx (.keyX s k) kz (.keyZ s k)
                  d (js/Math.max (js/Math.abs (- x kx)) (js/Math.abs (- z kz)))]
              (if (< d best-d) (recur (inc j) #js [kx kz] d) (recur (inc j) best best-d)))
            best)))))

  ;; the result; one-way-node is the first one-way step on the way to the nearest node (-1: none), clean-end the nearest node of
  ;; the returnable search then run ({path distance})
  (resultFrom [s one-way-node ^js clean-end]
    (cond
      (or (identical? (.-reason s) "start-not-standable") (identical? (.-reason s) "goal-not-standable")) (.outcome s "none" (.-reason s) nil nil)
      (nil? (.-reason s)) (let [^js r (.outcome s "found" (.-reason s) (.pathTo s (.-goal-node s)) nil)]
                      ;; a goal set's result names the goal reached (the first whose area holds the end)
                      (when (pos? (.-n-goals s))
                        (set! (.-goal r) (.goalAt s (aget (.-xs s) (.-goal-node s)) (aget (.-ys s) (.-goal-node s)) (aget (.-zs s) (.-goal-node s)))))
                      r)
      :else
      (let [clean (not (neg? one-way-node))
            end-path (cond clean (.-path clean-end) (== (.-best-node s) -1) nil :else (.pathTo s (.-best-node s)))
            end-distance (cond clean (.-distance clean-end) (== (.-best-node s) -1) js/Infinity :else (.-best-distance s))
            one-way (when (and clean (< (.-best-distance s) end-distance))
                      #js {:move (aget (.-moves s) one-way-node) :x (aget (.-xs s) one-way-node) :y (aget (.-ys s) one-way-node) :z (aget (.-zs s) one-way-node)
                           :distance (.-best-distance s) :path (.pathTo s (.-best-node s)) :open (.atLoadedEdge s (.-best-node s))})]
        (cond
          (identical? (.-reason s) "budget") (.outcome s "partial" "budget" end-path one-way)
          ^boolean (.-goal-unloaded s) (.outcome s "partial" "goal-unloaded" end-path one-way)
          (and (some? end-path) (>= (- (.-start-distance s) end-distance) MIN-CLOSER)) (.outcome s "partial" (.-reason s) end-path one-way)
          :else (.outcome s "none" (.-reason s) nil one-way))))))
