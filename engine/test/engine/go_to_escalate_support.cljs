(ns engine.go-to-escalate-support
  "Shared helpers of the engine.go-to-escalate test namespaces."
  (:require [engine.contract :as contract]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-support :as gts :refer [hiding]]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [plan.shape :as shape]
            [jobs.movement.go-to.escalation :as esc]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [triggers.access.scaffold-left :as scaffold-left]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(defn setup
  ([world] (setup world nil))
  ([world zones]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p (cond-> (tu/fake (dissoc world :plans :sight :seeing-all)) (:sight world) (tu/seeing-within (:sight world)) (:seeing-all world) tu/seeing-all)
         eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                                   :backoff false
                                   :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                            (sequential? zones) (assoc :world (ew/of-data {} {} zones))
                            (map? zones) (assoc :world (ew/of-data (:plans zones) {} []))))]
     {:eng eng :p p :seen seen :clock clock})))

(defn recording-parent
  "A parent that runs job with args as its child and keeps the child's result in out."
  [out job args]
  {:round (fn ^:async recording-round [c]
            (let [e (await (ctx/call c :kid job args))]
              (if (= :continue e) :continue (do (reset! out (gts/result-of e)) (ctx/end c :done nil)))))})

(defn ^:async tick-out! [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async tick-clocked!
  "tick-out! with the engine's clock moving 500 ms a tick, for waits that end on time."
  [eng clock n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (swap! clock + 500)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async run-job!
  "Run job (default go-to) with args as a child over world (zones: a zone list); {:eng :p :seen :out}."
  ([world args] (run-job! world 'jobs.movement.go-to args))
  ([world job args] (run-job! world job args nil))
  ([world job args zones] (run-job! world job args zones false))
  ([world job args zones clocked?]
   (let [{:keys [eng clock] :as s} (setup world zones)
         out (atom :not-done)
         eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out job args)))]
     (core/submit! eng '(recording-parent) {})
     (let [n (await (if clocked? (tick-clocked! eng clock 1000) (tick-out! eng 1000)))]
       (assoc s :eng eng :out out :ticks n)))))

(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))

(defn block [p cell] (get-in @(fake/state p) [:blocks cell] "air"))

(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(def pit-cells #{"0,61,0" "0,62,0" "0,63,0"})

(def pit
  "A 3-deep 1x1 pit (feet at y 61) in a dirt block x -2..2, y 60..63, z -2..2; stone ground 4 deep east of it (x 3..14)
  whose top is at the pit's rim (feet y 64)."
  (apply dissoc (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "dirt")) pit-cells))

(def stone-pit
  "The pit with stone walls (a pickaxe block)."
  (apply dissoc (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "stone")) pit-cells))

(def in-pit {:self {:pos {:x 0 :y 61 :z 0}}})

(def room
  "A sealed 5x5 dirt room (inside x -2..2, z -2..2, feet y 64; walls one thick at x/z +-3, y 64..65; roof y 66) on a
  stone floor x -6..12, z -6..6."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 66 3 "dirt") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(def in-room {:self {:pos {:x 0 :y 64 :z 0}}})

(def hollow
  "A 2-deep 20x20 hollow in solid dirt (open x 3..22, z -10..9, y 65..66; floor y 64; top of the dirt at y 66, feet
  on top at y 67)."
  (apply dissoc
         (box 0 60 -13 25 66 12 "dirt")
         (for [x (range 3 23) z (range -10 10) y [65 66]] (str x "," y "," z))))

;; ------------------------------------------------------------------ escape (pure reads)
(defn ba [world] (fn [[x y z]] (get (:blocks world) (str x "," y "," z) "air")))

(def ledge-room
  "A sealed dirt room 5 high (inside x -2..2, z -2..2, feet y 64..68) on stone, with a dirt post 2 high in its middle:
  a body on the post (feet y 66) has no wall beside it and no level cell to step to."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 69 3 "dirt") (for [x (range -2 3) z (range -2 3) y (range 64 69)] (str x "," y "," z)))
         (box 0 64 0 0 65 0 "dirt")))

(def on-ledge {:self {:pos {:x 0 :y 66 :z 0}} :blocks ledge-room})

(def wide-pit
  "A 3-deep 5x5 pit (inside x -2..2, z -2..2, feet y 61) in a dirt block x -3..3, y 60..63, z -3..3; stone ground east
  of it (x 4..14) whose top is at the pit's rim (feet y 64)."
  (apply dissoc (merge (box 4 60 -3 14 63 3 "stone") (box -3 60 -3 3 63 3 "dirt"))
         (for [x (range -2 3) z (range -2 3) y [61 62 63]] (str x "," y "," z))))

(def plank-yard
  "An unroofed yard on stone (inside x -2..2, z -2..2, feet y 64) walled 2 high in dirt, but for oak planks in the
  east wall at z 0."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 65 3 "dirt") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))
         (box 3 64 0 3 65 0 "oak_planks")))

(declare holes dug-cells)

(def foreign
  "Another owner's zone over the whole test area: go-to digs there only with :ignore-zones?, and puts back what it dug."
  [{:name "vault" :owner "Miles" :min [-60 0 -60] :max [60 120 60]}])

(def trespass {:ignore-zones? true})

(def deep-pit
  "A 5-deep 1x1 pit (feet at y 59) in dirt x -2..2; stone ground east of it (x 3..14) topped at the rim (feet y 64)."
  (apply dissoc (merge (box 3 56 -3 14 63 3 "stone") (box -2 56 -2 2 63 2 "dirt")) (for [y (range 59 64)] (str "0," y ",0"))))

(def stone-room
  "The sealed room with stone walls and roof (a pickaxe block)."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 66 3 "stone") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(defn run-in-shelter!
  "go-to goal (default [8 64 0]) from inside a dirt shelter at the origin (a :shelter entry), the world's time as given."
  ([time] (run-in-shelter! time [8 64 0] 1))
  ([time goal range]
  (let [cell (merge (box -6 63 -6 12 63 6 "stone") (dissoc (box -1 64 -1 1 66 1 "dirt") "0,64,0" "0,65,0"))
        {:keys [eng p seen]} (setup {:self {:pos {:x 0 :y 64 :z 0}} :blocks cell :time time :inventory [{:name "dirt" :count 5}]})
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent
                                    (recording-parent out 'jobs.movement.go-to {:pos goal :range range})))]
    (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built}
                {:cap 10 :ttl 86400000})
    (core/submit! eng '(recording-parent) {})
    (-> (tick-out! eng 300) (.then (fn [_] {:out out :p p :seen seen}))))))

(defn ledger [eng] (mapv :data (mem/entries (mem/view (:store eng)) :tidy)))

(defn holes [eng] (set (map :cell (filter :escalation (ledger eng)))))

(defn dug-cells [p] (set (map #(mapv (js->clj (.-pos (.-args %)) :keywordize-keys true) [:x :y :z]) (calls p "dig"))))

(defn restores-after-the-last-stair?
  "Whether every put-back event (go-to.restored, go-to.restore-skipped) comes after the last go-to.escalated: a stair
  that leaves the body shut in is followed by the next at once, never refilled under it."
  [seen]
  (let [last-esc (apply max (map :seq (events-of seen :go-to.escalated)))]
    (every? #(> (:seq %) last-esc) (concat (events-of seen :go-to.restored) (events-of seen :go-to.restore-skipped)))))

(def deep-shaft
  "A 1x1 shaft 18 deep (feet at y 46) in solid dirt x -8..38, y 42..63, z -6..6: room for 3 stairs east and the goals at x 10 and 30."
  (apply dissoc (box -8 42 -6 38 63 6 "dirt") (for [y (range 46 64)] (str "0," y ",0"))))

(def own-pit-wall
  "The pit's dirt walls as a plan this body made (a hut's walls)."
  (shape/with-author {:id "hut" :parts [{:id "w" :cells (vec (for [x (range -2 3) y (range 60 64) z (range -2 3)
                                                                  :when (not (contains? pit-cells (str x "," y "," z)))]
                                                              [x y z])) :want "dirt"}]} "Fake"))

;; ------------------------------------------------------------------ review follow-ups (card 732fce4c)
(def cobble-room
  "room with cobblestone walls (the walls are not natural)."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 66 3 "cobblestone")
                (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(def room-and-deck
  "The sealed room with a stone deck (x 7..9, z -1..1, y 70) floating high over the floor beside it: the deck is cut off by a drop."
  (merge room (box 7 70 -1 9 70 1 "stone")))

(def pocket
  "A 1x2 air pocket at [8 64 0] inside a stone block (x 6..10, y 64..66, z -2..2) on a stone floor wide enough not to read as
  enclosed (reach-room/room-cells): open land round it."
  (apply dissoc (merge (box -20 63 -20 20 63 20 "stone") (box 6 64 -2 10 66 2 "stone")) ["8,64,0" "8,65,0"]))

(def free-by-pocket {:self {:pos {:x 0 :y 64 :z 0}} :blocks pocket :inventory [{:name "stone_pickaxe" :count 1}]})

(def high-pocket
  "The pocket of `pocket` raised to [8 67 0] in a stone block x 6..10, y 64..69, z -2..2."
  (apply dissoc (merge (box -20 63 -20 20 63 20 "stone") (box 6 64 -2 10 69 2 "stone")) ["8,67,0" "8,68,0"]))

(defn moat
  "The goal's 1x1 stone island at [8 63 0] in an 11x11 moat of liquid one deep on the stone floor."
  [liquid]
  (merge (box -20 62 -20 20 62 20 "stone") (box -20 63 -20 20 63 20 "stone") (box 3 63 -5 13 63 5 liquid) {"8,63,0" "stone"}))

;; ---- door-back reads what the body senses
(def door-cell
  {:self {:pos {:x 0.5 :y 64 :z 0.2}}
   :floor tu/walk-floor
   :blocks {"0,64,0" "oak_door" "0,65,0" "oak_door"}
   :states {"0,64,0" {:open false :half "lower" :facing "north"} "0,65,0" {:open false :half "upper" :facing "north"}}})

(defn dark-at [p cells]
  (let [sensed (.-sensedAt p) dark (set cells)]
    (aset p "sensedAt" (fn [q] (if (dark [(.-x q) (.-y q) (.-z q)]) #js {:unknown true} (sensed q))))))

;; ------------------------------------------------------------------ a 1-cell gap under a low ceiling
(def gap-tunnel
  "A 2-high tunnel (x 0..10, y 64..65, z 0) in stone, its floor at y 63 broken at x 5 down to y 62: a 1-cell gap, too
  deep to climb out of, under a ceiling too low for the gap jump."
  (apply dissoc (box -2 60 -2 12 67 2 "stone")
         "5,63,0" "5,62,0"
         (for [x (range 0 11) y [64 65]] (str x "," y ",0"))))

;; a mine reached by drops: the only way back on foot is a long loop (20 east, 10 up a staircase over 20, 30 back) that costs
;; more than three stairs of the 10 the goal is above
(def long-loop
  (let [open (fn [x y z] [(str x "," y "," z) (str x "," (inc y) "," z)])]
    (apply dissoc (box -2 45 -3 45 66 3 "stone")
           (concat (mapcat #(open % 50 0) (range 0 21))
                   (mapcat #(concat (open (+ 20 %) (+ 50 (quot % 2)) 0) [(str (+ 20 %) "," (+ 52 (quot % 2)) ",0")]) (range 21))
                   (mapcat #(open % 60 2) (range 10 41)) (open 40 60 1)))))

(def loop-tolls (vec (for [x (range 0 21)] {:x x :y 50 :z 0 :factor 60})))

(def stair-below-surface
  "Stone x -45..5, y 0..40, z -3..3 with a tunnel west: a stair up from (0,10) to its dead end at (-9,19) under 21 blocks
  of rock, and a mine floor (y 9) that runs on west and drops 3 at x -12 to a lower floor going on to x -38."
  (let [carve (concat (for [k (range 10) y [(+ 10 k) (+ 11 k) (+ 12 k)]] [(- k) y 0])
                      (for [x (range 0 -12 -1) y [10 11 12]] [x y 0])
                      (for [x (range -12 -39 -1) y [7 8 9 10 11]] [x y 0]))]
    (apply dissoc (box -45 0 -3 5 40 3 "stone") (map (fn [[x y z]] (str x "," y "," z)) carve))))

(def cave
  "A sealed 2-high 12x12 cave (x 0..11, z -6..5, y 64..65) inside a stone hill (x -6..20, y 60..70) on a stone plain."
  (apply dissoc (merge (box -30 63 -20 40 63 20 "stone") (box -6 64 -8 20 70 8 "stone"))
         (for [x (range 0 12) z (range -6 6) y [64 65]] (str x "," y "," z))))

(def below-pocket
  "Stone x -20..20, y 55..63 under a body on top at [0 64 0], with a 1x2 pocket at [3 60 0]: the goal no door or stair leads
  down to, reached by a :dig-route of [2 63 0] [3 63 0] [3 62 0]."
  (apply dissoc (box -20 55 -20 20 63 20 "stone") ["3,60,0" "3,61,0"]))

(defn ^:async dig-route-run!
  "go-to into below-pocket's goal for a body carrying inventory over blocks; {:out :p :seen :digs}, digs the args of each
  jobs.blocks.dig call. sense (fn [p]) replaces the fake's omniscient sensing when given."
  [blocks inventory sense]
  (let [{:keys [eng p seen]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks :inventory inventory})
        digs (atom [])
        out (atom :not-done)
        dig-round (get-in eng [:jobs 'jobs.blocks.dig :round])
        eng (-> eng
                (assoc-in [:jobs 'recording-parent] (recording-parent out 'jobs.movement.go-to {:pos [3 60 0] :range 0 :unknown false}))
                (assoc-in [:jobs 'jobs.blocks.dig :round] (fn [c] (swap! digs conj (:args c)) (dig-round c))))]
    (some-> sense (apply [p]))
    (core/submit! eng '(recording-parent) {})
    (await (tick-out! eng 1000))
    {:out out :p p :seen seen :digs digs}))
