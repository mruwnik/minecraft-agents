(ns jobs.lib.cost.door
  "The price of a door dug through a wall whose far side the body cannot see: the unseen rows are a run of wall of
  unknown length, each row wall with wall-on-chance. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::wall-rows {:default {:k 0 :n 0} :spec (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)})
                :doc "Counts of the unseen rows of doors' walls: k found wall of n looked at (the wall rows, and each far side found open). Recount: `node tools/event-counts.mjs far-side-rows` (dashboard/src/world_test/far_side_rows.cljs) over clear-path.far-side events: a door given up counts its rows as wall only (censored), none yet."}})

(defn wall-on-chance
  "The chance an unseen row of a wall seen rows thick is wall too: the worse of the counted rows' ((k + 1) / (n + 2),
  ::wall-rows) and this wall's own, its seen rows all wall ((rows + 1) / (rows + 2))."
  [rows]
  (let [{:keys [k n]} (settings/get settings ::wall-rows)]
    (max (/ (inc k) (+ n 2)) (/ (inc rows) (+ rows 2)))))

(defn door-s
  "Expected seconds of a door whose far side is unseen: sure-s (the rows dug and seen) and, the unseen rows wall each with
  chance q, a row-s for each dug of the most the budget most leaves (k); past those the door is a dead end, priced
  as all it dug again (it bought nothing) plus the walk back, back-s of k (fn)."
  [{:keys [sure-s row-s most q back-s]}]
  (let [k (max 0 (js/Math.floor (/ (- most sure-s) row-s)))
        qk (if (== q 1) 1 (js/Math.pow q k))
        dug (if (== q 1) k (/ (* q (- 1 qk)) (- 1 q)))
        dead (* q qk)]
    (+ sure-s (* row-s dug) (if (zero? dead) 0 (* dead (+ sure-s (* k row-s) (back-s k)))))))
