(ns engine.search-recount-test
  "The recount of the route search's measured prices (jobs.lib.cost/search-cell-s, go-to's flood cell): times a search to its
  end over fixed scenes and prints the pooled seconds and cells to put in the prices' docs. Never fails on a time."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.dig-route :as dr]
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
  "Run a search to its end in slices of 500 cells, or until limit-s seconds pass: {:cells :s :ended?}."
  [run limit-s]
  (let [t0 (now)]
    (loop [cells 0]
      (let [[a e] (run 500) cells (+ cells e) s (/ (- (now) t0) 1000)]
        (cond a {:cells cells :s s :ended? true :answer a}
              (> s limit-s) {:cells cells :s s :ended? false}
              :else (recur cells))))))

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
        pooled (reduce (fn [a [_ r]] (-> a (update :cells + (:cells r)) (update :s + (:s r)))) {:cells 0 :s 0} rows)
        flood (flood-scene 30)]
    (doseq [[k r] rows] (line k r))
    (line :pooled-search pooled)
    (line :flood flood)
    (println (str "RECOUNT search-cell-s " (/ (:s pooled) (:cells pooled)) " flood-cell-s " (/ (:s flood) (:cells flood))))
    (is (every? (comp :ended? second) rows))))
