(ns engine.search-recount-test
  "The recount of the route search's measured prices (jobs.lib.cost/search-cell-s, go-to's flood cell): times a search to its
  end over fixed scenes and prints the pooled seconds and cells to put in the prices' docs. Never fails on a time."
  (:require [cljs.test :refer [deftest is]]
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
  (let [rows (doall (for [[k mk] scenes] [k (to-end (mk) 20)]))
        pooled (reduce (fn [a [_ r]] (-> a (update :cells + (:cells r)) (update :reads + (:reads r)) (update :s + (:s r)))) {:cells 0 :reads 0 :s 0} rows)
        flood (flood-scene 30)]
    (doseq [[k r] rows] (line k r))
    (line :pooled-search pooled)
    (line :pooled-reads (assoc pooled :cells (:reads pooled)))
    (line :flood flood)
    (println (str "RECOUNT search-cell-s " (/ (:s pooled) (:cells pooled)) " search-read-s " (/ (:s pooled) (:reads pooled)) " flood-cell-s " (/ (:s flood) (:cells flood))))
    (is (every? (comp :ended? second) rows))))

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
  "reach-room/room-flood from [0 64 0]: the cells it reads (room-cells on a slab that floods open, the room's floor when walled)."
  [blocks cells]
  (let [p (tu/fake {:blocks (merge (box -30 63 -30 30 63 30 "stone") blocks)})]
    (fn [] (reach-room/room-flood p (reach-room/room-kind-at p (fn [x y z] (reach-room/body-kind-of (u/seen-block p {:x x :y y :z z})))) 0 64 0) cells)))

(defn walled
  "A closed room: stone walls (two high) round x, z in -10..10 over the slab, and a roof."
  []
  (into {} (for [x (range -11 12) z (range -11 12) y [64 65 66]
                 :when (or (= y 66) (#{-11 11} x) (#{-11 11} z))] [[x y z] "stone"])))

(deftest recount-prints-the-walk-and-room-flood-prices
  (let [scenes {:walk-slab (walk-scene {}) :walk-pillars (walk-scene (pillared))
                :room-slab (flood-room-scene {} 1024) :room-pillars (flood-room-scene (pillared) 1024) :room-closed (flood-room-scene (walled) 441)}
        rows (into {} (for [[k run] scenes] [k (timed-reps 7 run)]))
        pooled (fn [ks] (reduce (fn [a k] (-> a (update :cells + (:cells (rows k))) (update :s + (:s (rows k))))) {:cells 0 :s 0} ks))]
    (doseq [[k {:keys [per-cell low high]}] rows]
      (println (str "RECOUNT " k ": median " (.toFixed per-cell 4) " ms/cell, low " (.toFixed low 4) ", high " (.toFixed high 4))))
    (line :pooled-walk (pooled [:walk-slab :walk-pillars]))
    (line :pooled-room (pooled [:room-slab :room-pillars :room-closed]))
    (is (= 5 (count rows)))))
