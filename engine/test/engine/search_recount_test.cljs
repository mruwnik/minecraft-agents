(ns engine.search-recount-test
  "The recount of the route search's measured prices (jobs.lib.cost/search-cell-s, go-to's flood cell): times a search to its
  end over fixed scenes and prints the pooled seconds and cells to put in the prices' docs. Never fails on a time."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-route :as dr]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.lib.util :as u]
            [engine.test-util :as tu :refer [box]]))

(defn now [] (js/performance.now))

(defn world
  "block-at over a stone mass below y 10 with the given cells overridden."
  [overrides]
  (fn [[x y z :as c]] (or (overrides c) (if (< y 10) "stone" "air"))))

(defn shell
  "Obsidian one cell thick round [0 5 6] (a dear wall round the goal, the cell itself air), as a map of cell to name."
  []
  (into {[0 5 6] "air"} (for [x (range -1 2) y (range 4 7) z (range 5 8) :when (not= [x y z] [0 5 6])] [[x y z] "obsidian"])))

(defn to-end
  "Run a search to its end in slices of 500 cells, or until limit-s seconds pass: {:cells :reads :s :ended?}."
  [run limit-s]
  (let [t0 (now)]
    (loop [cells 0 reads 0]
      (let [[a e _ rd] (run 500) cells (+ cells e) reads (+ reads rd) s (/ (- (now) t0) 1000)]
        (cond a {:cells cells :reads reads :s s :ended? true :answer a}
              (> s limit-s) {:cells cells :reads reads :s s :ended? false}
              :else (recur cells reads))))))

(def dear (fn [n _] (if (= "obsidian" n) 50 1)))

(defn body-search
  "A priced search over a fake body's seen world (stone below y 10 and the extra cells), priced as a cut prices its digs."
  [extra from goal]
  (let [p (tu/fake {:blocks (merge (box -30 0 -30 30 9 30 "stone") extra)})
        prices (dr/body-prices p (constantly false))]
    (dr/priced-search (fn [[x y z]] (u/seen-name p {:x x :y y :z z})) from goal (assoc prices :max-s 1000))))

(def scenes
  "name -> fn [] of a search run on a scene. A cut reads the world through the body's primitives, so the price is of searches
  that do (the bare dr/search is 2-3 times quicker a cell: tools of the same scenes run there give 0.03 to 0.07 ms)."
  {:shell #(body-search (shell) [0 10 0] [0 5 6])
   :far-dig #(body-search {[20 4 6] "air" [20 5 6] "air"} [0 10 0] [20 4 6])
   :deep-shell #(body-search (assoc (shell) [0 2 6] "air") [0 10 0] [0 2 6])
   :diagonal #(body-search {[12 10 12] "air"} [0 10 0] [12 10 12])})

(defn flood-scene
  "The flood go-to's nearest-stand makes, timed over reps floods of a flat slab: {:cells :s}."
  [reps]
  (let [b (tu/fake {:blocks (box -30 63 -30 30 63 30 "stone")})
        t0 (now)
        cells (reduce + (for [_ (range reps)] (do (choose/nearest-stand b [20 64 20]) 1024)))]
    {:cells cells :s (/ (- (now) t0) 1000)}))

(defn line [label {:keys [cells s]}]
  (println (str "RECOUNT " label ": " cells " cells in " (.toFixed s 3) " s = " (.toFixed (/ (* 1000 s) cells) 4) " ms/cell")))

(deftest recount-prints-the-search-and-flood-prices
  "A bench: runs only with RECOUNT=1 in the environment, never in the normal suite."
  (when (.. js/process -env -RECOUNT)
   (let [rows (doall (for [[k mk] scenes] [k (to-end (mk) 20)]))
        pooled (reduce (fn [a [_ r]] (-> a (update :cells + (:cells r)) (update :reads + (:reads r)) (update :s + (:s r)))) {:cells 0 :reads 0 :s 0} rows)
        flood (flood-scene 30)]
    (doseq [[k r] rows] (line k r) (line (str k " reads") (assoc r :cells (:reads r))))
    (line :pooled-search pooled)
    (line :pooled-reads (assoc pooled :cells (:reads pooled)))
    (line :flood flood)
    (println (str "RECOUNT search-cell-s " (/ (:s pooled) (:cells pooled)) " search-read-s " (/ (:s pooled) (:reads pooled)) " flood-cell-s " (/ (:s flood) (:cells flood))))
    (is (every? (comp :ended? second) rows)))))

(defn median [xs] (nth (sort xs) (quot (count xs) 2)))

(defn pillared
  "A stone slab at y 63 with stone pillars (two high) on a grid, as a map of cell to name for tu/fake's :blocks overrides."
  []
  (into {} (for [x (range -30 31 3) z (range -30 31 3) y [64 65]] [[x y z] "stone"])))

(defn timed-reps
  "reps runs of (run), each returning its cells: {:cells :s} summed, :per-cell the median ms a cell and :low/:high its spread."
  [reps run]
  (let [rows (vec (for [_ (range reps)] (let [t0 (now) cells (run)] [cells (- (now) t0)])))
        per (map (fn [[c ms]] (/ ms c)) rows)]
    {:cells (reduce + (map first rows)) :s (/ (reduce + (map second rows)) 1000)
     :per-cell (median per) :low (apply min per) :high (apply max per)}))

(defn walk-scene
  "reach-room/walk-steps to a goal in the air (no way): the cells it expands (reach/node-budget)."
  [blocks]
  (let [p (tu/fake {:blocks (merge (box -30 63 -30 30 63 30 "stone") blocks)})]
    (fn [] (let [n (volatile! 0)]
             (reach-room/walk-steps p {:x 0 :y 64 :z 0} {:x 0 :y 80 :z 0} #(vswap! n + %))
             @n))))

(defn flood-room-scene
  "reach-room/room-flood from [0 64 0]: the cells it charges a meter (room-cells on a slab that floods open, the room when walled)."
  [blocks]
  (let [p (tu/fake {:blocks (merge (box -30 63 -30 30 63 30 "stone") blocks)})]
    (fn [] (binding [reach/*search* (reach/meter 1e6 0 1 1 1)]
             (reach-room/room-flood p (reach-room/room-kind-at p (fn [x y z] (reach-room/body-kind-of (u/seen-block p {:x x :y y :z z})))) 0 64 0)
             (js/Math.round (- 1e6 @(:left reach/*search*)))))))

(defn walled
  "A closed room: stone walls (two high) round x, z in -10..10 over the slab, and a roof."
  []
  (into {} (for [x (range -11 12) z (range -11 12) y [64 65 66]
                 :when (or (= y 66) (#{-11 11} x) (#{-11 11} z))] [[x y z] "stone"])))

(deftest recount-prints-the-walk-and-room-flood-prices
  "A bench: runs only with RECOUNT=1 in the environment, never in the normal suite."
  (when (.. js/process -env -RECOUNT)
   (let [scenes {:walk-slab (walk-scene {}) :walk-pillars (walk-scene (pillared))
                :room-slab (flood-room-scene {}) :room-pillars (flood-room-scene (pillared)) :room-closed (flood-room-scene (walled))}
        rows (into {} (for [[k run] scenes] [k (timed-reps 7 run)]))
        pooled (fn [ks] (reduce (fn [a k] (-> a (update :cells + (:cells (rows k))) (update :s + (:s (rows k))))) {:cells 0 :s 0} ks))]
    (doseq [[k {:keys [per-cell low high]}] rows]
      (println (str "RECOUNT " k ": median " (.toFixed per-cell 4) " ms/cell, low " (.toFixed low 4) ", high " (.toFixed high 4))))
    (line :pooled-walk (pooled [:walk-slab :walk-pillars]))
    (line :pooled-room (pooled [:room-slab :room-pillars :room-closed]))
    (is (= 5 (count rows))))))

(defn bedrock-shell
  "Bedrock one cell thick round c (c itself air): a goal no cost reaches, so a search expands the region round it."
  [[cx cy cz :as c]]
  (into {c "air"} (for [x (range -1 2) y (range -1 2) z (range -1 2) :when (not= [x y z] [0 0 0])] [[(+ cx x) (+ cy y) (+ cz z)] "bedrock"])))

(defn scene-search
  "A priced search from from to an unreachable goal over a fake body's world: stone below y 10, the extra cells, unseen? the cells it has not seen."
  [extra unseen? from goal]
  (let [p (tu/fake {:blocks (merge (box -30 0 -30 30 9 30 "stone") extra)})
        prices (dr/body-prices p unseen?)]
    (dr/priced-search (fn [[x y z]] (u/seen-name p {:x x :y y :z z})) from goal (assoc prices :max-s 1000))))

(defn to-cells
  "Run a search in slices of 500 cells until it has expanded at least n: {:cells :reads :s}."
  [run n]
  (let [t0 (now)]
    (loop [cells 0 reads 0]
      (if (>= cells n)
        {:cells cells :reads reads :s (/ (- (now) t0) 1000)}
        (let [[a e _ rd] (run 500) cells (+ cells e) reads (+ reads rd)]
          (if a {:cells cells :reads reads :s (/ (- (now) t0) 1000)} (recur cells reads)))))))

(def big-scenes
  "Scenes of several thousand cells: an open layer, a stone mass to dig, an unseen layer, shallow water."
  {:open #(scene-search (bedrock-shell [0 12 6]) (constantly false) [0 10 0] [0 12 6])
   :stone-mass #(scene-search (bedrock-shell [20 5 20]) (constantly false) [0 5 0] [20 5 20])
   :unseen #(scene-search (bedrock-shell [0 12 6]) (constantly true) [0 10 0] [0 12 6])
   :water #(scene-search (merge (into {} (for [x (range -30 31) y [10 11] z (range -30 31)] [[x y z] "water"])) (bedrock-shell [0 11 6])) (constantly false) [0 10 0] [0 11 6])})

(defn percentile [xs q] (let [v (vec (sort xs))] (nth v (min (dec (count v)) (int (js/Math.ceil (* q (dec (count v)))))))))

(deftest recount-prints-the-walk-and-room-spread
  "RECOUNT-WALK / RECOUNT-ROOM: the p90 over runs (ms/cell) of the walk search and the room flood, per scene and pooled over all
  runs, with the machine's load average: the lines the walk and room ceilings are set from. A bench: runs only with RECOUNT=1 in
  the environment, never in the normal suite."
  (when (.. js/process -env -RECOUNT)
   (let [load1 (aget (.loadavg (js/require "os")) 0)
        runs 60
        spread (fn [label scenes]
                 (let [rows (into {} (for [[k run] scenes] [k (vec (for [_ (range runs)] (let [t0 (now) cells (run)] (/ (- (now) t0) cells))))]))
                       all (mapcat val rows)]
                   (doseq [[k xs] rows]
                     (println (str label " " k ": ms/cell median " (.toFixed (median xs) 4) " p90 " (.toFixed (percentile xs 0.9) 4) " max " (.toFixed (apply max xs) 4))))
                   (println (str label " raw " (pr-str (mapv #(/ (js/Math.round (* 1e4 %)) 1e4) (sort all)))))
                   (println (str label " all: n " (count all) " load " (.toFixed load1 1) " ms/cell median " (.toFixed (median all) 4)
                                 " p90 " (.toFixed (percentile all 0.9) 4) " max " (.toFixed (apply max all) 4)))
                   (count rows)))]
    (is (= 2 (spread "RECOUNT-WALK" {:slab (walk-scene {}) :pillars (walk-scene (pillared))})))
    (is (= 3 (spread "RECOUNT-ROOM" {:slab (flood-room-scene {}) :pillars (flood-room-scene (pillared)) :closed (flood-room-scene (walled))}))))))

(deftest recount-prints-the-ceiling-spread-over-big-scenes
  "A bench: runs only with RECOUNT=1 in the environment, never in the normal suite."
  (when (.. js/process -env -RECOUNT)
   (let [rows (into {} (for [[k mk] (merge big-scenes scenes)] [k (vec (for [_ (range 5)] (to-cells (mk) 4000)))]))
        per (fn [f] (fn [rs] (map #(/ (* 1000 (:s %)) (f %)) rs)))
        cells-of (per :cells) reads-of (per :reads)]
    (doseq [[k rs] rows]
      (println (str "RECOUNT-SPREAD " k ": ms/cell median " (.toFixed (median (cells-of rs)) 4) " p90 " (.toFixed (percentile (cells-of rs) 0.9) 4)
                    ", ms/read median " (.toFixed (median (reads-of rs)) 5) " p90 " (.toFixed (percentile (reads-of rs) 0.9) 5))))
    (let [all (mapcat val rows)]
      (println (str "RECOUNT-SPREAD all: ms/cell mean " (.toFixed (/ (* 1000 (reduce + (map :s all))) (reduce + (map :cells all))) 4)
                    " p90 " (.toFixed (percentile (cells-of all) 0.9) 4) " max " (.toFixed (apply max (cells-of all)) 4)
                    "; ms/read mean " (.toFixed (/ (* 1000 (reduce + (map :s all))) (reduce + (map :reads all))) 5)
                    " p90 " (.toFixed (percentile (reads-of all) 0.9) 5) " max " (.toFixed (apply max (reads-of all)) 5))))
    (is (= 8 (count rows))))))

(defn pool-scene
  "A search through a stone mass x -15..15, y 60..63, z -8..8 (bedrock round it) from an air cell at its west end to a goal in the bedrock,
  with water pools at x in xs on the rows z -4, 0, 4 (y 62): digs beside a pool let its water in to y 62. Run to its end,
  the back search's bound in the heuristic as every caller's: {:cells :reads :s}."
  [xs]
  (let [pools (set (for [x xs z [-4 0 4]] [x 62 z]))
        beside? (fn [[x y z]] (some pools [[(inc x) y z] [(dec x) y z] [x y (inc z)] [x y (dec z)] [x (inc y) z] [x (dec y) z]]))
        in-box? (fn [[x y z]] (and (<= -15 x 15) (<= 60 y 63) (<= -8 z 8)))
        w (fn [c] (cond (#{[-15 62 0] [15 62 0]} c) "air" (pools c) "water" (in-box? c) "stone" :else "bedrock"))
        flood {:level (fn [c] (when (beside? c) 62)) :open (delay [2 0]) :dig (fn [under _] [1 (inc under)]) :step (fn [under] [0 under])}
        run (dr/search w [-15 62 0] [15 62 0] {:step-s 1 :flood flood :back-h? true})]
    (to-end run 60)))

(deftest recount-prints-the-pool-origin-join-cost
  "A bench: runs only with RECOUNT=1 in the environment, never in the normal suite."
  (when (.. js/process -env -RECOUNT)
   (let [join cost/flood-origin-join
        scenes {:none [] :one [0] :two [0 6] :several [-12 -6 0 6 12]}
        run (fn [] (into {} (for [[k xs] scenes] [k (pool-scene xs)])))
        after (run)
        before (try (set! cost/flood-origin-join (fn [a _] a)) (run) (finally (set! cost/flood-origin-join join)))]
    (doseq [k (keys scenes)]
      (println (str "RECOUNT-JOIN " k ": before " (:cells (before k)) " cells " (.toFixed (* 1000 (:s (before k))) 0) " ms; after " (:cells (after k)) " cells " (.toFixed (* 1000 (:s (after k))) 0) " ms")))
    (is (= 4 (count after))))))
