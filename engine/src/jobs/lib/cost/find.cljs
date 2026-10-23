(ns jobs.lib.cost.find
  "The chance a look round finds the animals a search is for: a mixture prior (base-rates/animal-ground-share of the
  land spawns them, at base-rates/animal-pack-density, the rest none) lowered by the ground within ::region that looks for
  them found none on (live :searched notes, engine.notes), so a search over empty ground soon stops paying. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.base-rates :as base-rates]))

(a/defargs settings
  {::region {:default 128 :spec (a/num-in 1 nil)
             :doc "Blocks round a spot whose searched ground tells the chance of a find there: the game sets animals by biome, and a biome spans a few hundred blocks."}
   ::leg-spacing {:default 16 :doc "Blocks between a search's legs (jobs.explore.search)." :spec (a/int-in 4 64)}})

(defn leg-spacing [] (settings/get settings ::leg-spacing))

(def cell "Blocks on a side of the squares searched ground is counted in." 8)

(defn density
  "Packs per block of the kinds of names where they spawn, nil when one has no base rate (a block: no chance is priced)."
  [names]
  (when (and (seq names) (every? base-rates/animal-pack-density names))
    (reduce + (map base-rates/animal-pack-density names))))

(defn searched-for? [names {:keys [kind what]}]
  (and (= :searched kind) (every? (set what) names)))

(defn in-vain?
  "Whether note is a :searched note for every one of names whose look saw none of them (its :saw: kept, barred or
  baby ones seen still show the land has them)."
  [names n]
  (and (searched-for? names n) (not-any? (set (:saw n)) names)))

(defn looked-area
  "Blocks of ground within ::region (XZ) of [x z] that the :searched notes of ns for every one of names looked over in vain (in-vain?),
  each square of `cell` counted once."
  [ns names [x z]]
  (let [region (settings/get settings ::region)
        squares (fn [{[nx _ nz] :pos r :r}]
                  (for [i (range (js/Math.floor (/ (- nx r) cell)) (inc (js/Math.floor (/ (+ nx r) cell))))
                        j (range (js/Math.floor (/ (- nz r) cell)) (inc (js/Math.floor (/ (+ nz r) cell))))
                        :let [cx (* (+ i 0.5) cell) cz (* (+ j 0.5) cell)]
                        :when (and (<= (js/Math.hypot (- cx nx) (- cz nz)) r)
                                   (<= (js/Math.hypot (- cx x) (- cz z)) region))]
                    (+ (* i 1000003) j)))]
    (->> ns
         (filter #(in-vain? names %))
         (filter (fn [{[nx _ nz] :pos r :r}] (<= (js/Math.hypot (- nx x) (- nz z)) (+ region r))))
         (into #{} (mapcat squares))
         count
         (* cell cell))))

(defn find-chance
  "Chance a look over `a` blocks finds one of names after `looked` blocks round it were looked over for them in vain: the
  chance this land spawns them (animal-ground-share, lowered by e^(-density x looked) for the ground that showed none)
  times the chance a look that wide meets a pack there (1 - e^(-density x a)). 1 for names with no base rate."
  [names looked a]
  (if-let [d (density names)]
    (let [q base-rates/animal-ground-share
          w (* q (js/Math.exp (- (* d looked))))]
      (* (/ w (+ w (- 1 q))) (- 1 (js/Math.exp (- (* d a))))))
    1))

(defn look-chance
  "find-chance of a look round of radius r at [x z], over the ground the notes ns searched for names within ::region."
  [ns names at r]
  (find-chance names (looked-area ns names at) (* js/Math.PI r r)))

(defn search-s
  "Seconds a search for names spends per find: legs of leg-s seconds, each looking over `a` blocks of new ground after
  `looked` searched in vain, walked while a leg costs less than the worth left times its find-chance (the search's
  :price stop), the seconds expected until a find or that stop over the chance of a find by then: a search tried
  before a sure way of cost C (taken when it finds none) is cheaper exactly when this is under C. Infinity when no leg
  pays or names have no base rate (no chance is priced). Each leg spends leg-s (positive) of the worth, so it ends."
  [names looked a leg-s worth]
  {:pre [(pos? leg-s)]}
  (if-not (density names)
    js/Infinity
    (loop [looked looked spent 0 miss 1 expect 0]
      (let [chance (find-chance names looked a)]
        (if (> leg-s (* chance (- worth spent)))
          (if (< miss 1) (/ expect (- 1 miss)) js/Infinity)
          (recur (+ looked a) (+ spent leg-s) (* miss (- 1 chance)) (+ expect (* miss leg-s))))))))
