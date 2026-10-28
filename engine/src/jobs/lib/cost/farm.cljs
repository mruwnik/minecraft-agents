(ns jobs.lib.cost.farm
  "What a farm field's cells are worth to a pass and what its steps take: the measured share of field cells ripe or
  sowable at a pass (::field-prior), the chance a cell seen growing is ripe by now (ripe-chance), the harvests a crop is worth over the horizon (harvests), what a sown cell is worth (sown-worth), and the seconds each step works (step-s)."
  (:require [engine.args :as a]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.cost.food :as food]
            [jobs.lib.cost.table :as table]
            [jobs.lib.cost.value :as value]
            [jobs.lib.crops :as crops]))

(def beta? (a/pred "[alpha beta], both over 0" #(and (vector? %) (= 2 (count %)) (every? (fn [x] (and (number? x) (pos? x))) %))))
(def line? (a/map-with {:n (a/int-in 0 nil) :fixed-s (a/num-in 0 nil) :per-unit-s (a/num-in 0 nil)}))

(a/defargs settings
  {::field-prior {:default {:fields 122 :cells 1859 :ripe [0.22 1.919] :sowable [1.083 2.019]}
                  :doc "The Beta priors of the share of a field's cells holding a ripe crop (:ripe) and bare farmland or untilled ground (:sowable) when a pass starts, the :prior of node tools/event-counts.mjs field-passes: 122 fields of the jobs.farm.tend endings in the trial bodies' logs to 2026-10-25 (1859 cells; ripe = harvest :cut, sowable = :bare + :untilled at the end + :planted), each share fitted by moments over the fields: wide, as the fields differ (ripe mean 0.10 from 2.1 pseudo-cells, sowable 0.35 from 3.1)."
                  :spec (a/map-with {:fields (a/int-in 0 nil) :cells (a/int-in 0 nil) :ripe beta? :sowable beta?})}
   ::field-work {:default {:harvest {:n 39 :fixed-s 23.589 :per-unit-s 1.564}
                           :till {:n 49 :fixed-s 1.517 :per-unit-s 0.932}
                           :plant {:n 12 :fixed-s 0 :per-unit-s 0.737}
                           :irrigate {:n 6 :fixed-s 3.188 :per-unit-s 0}
                           :fertilize {:n 1 :fixed-s 3.343 :per-unit-s 0}}
                 :doc "Seconds each jobs.farm.tend step's child works, fixed + per-unit x its units (harvest :cut, till :tilled, plant :planted, irrigate :poured, fertilize :used), the :work of node tools/event-counts.mjs field-passes: the step children's child_started to child_ended in the trial bodies' logs to 2026-10-25 against the pass endings' step summaries, a least-squares line per step (:n the children; irrigate and fertilize too few to tell a unit's time)."
                 :spec (a/map-of keyword? line?)}})

(def day-s "Seconds of a Minecraft day." 1200)

(defn horizon-s
  "Seconds over which a crop still to grow is needed: the food reserve's days (cost.food/days). A crop ripe later than that
  is worth nothing now, one ripe at once all its worth, as cost/wait-price prices a wait for a find."
  []
  (* food/days day-s))

(defn share
  "The posterior mean share of a field's never-seen cells (no age to grow from) that are k-like (:ripe or :sowable), given its seen cells: k (the
  expected count of such among them) of n, over the ::field-prior Beta."
  [kind k n]
  (let [[al be] (get (settings/get settings ::field-prior) kind)]
    (/ (+ al k) (+ al be n))))

(def default-simulation-distance "Chunks around a player that tick when the server has not announced its own: vanilla's." 10)

(defn growing-s
  "The seconds of secs a field `dist` blocks (horizontal) from the body grew: all of them within the server's simulation
  distance, none beyond (no position history is kept, so a field out of range now is taken as out of range since it was seen)."
  [secs dist]
  (if (<= dist (* 16 (or (game/simulation-distance) default-simulation-distance))) secs 0))

(defn at-least-chance
  "The chance at least m growth stages grew in secs at rate stages a second (Poisson)."
  [m secs rate]
  (let [mu (* rate (max 0 secs))]
    (if (<= m 0)
      1
      (- 1 (reduce + (map (fn [k] (* (js/Math.exp (- mu)) (/ (js/Math.pow mu k) (reduce * 1 (range 1 (inc k)))))) (range m)))))))

(defn ripe-chance
  "The chance a field cell seen holding crop at stage `age` `secs` seconds ago is ripe now: at least the stages left to
  its ripe-age grew in that time (Poisson at rate stages a second, crops/stages-per-s). A cell never seen has no age: share."
  [crop age secs rate]
  (at-least-chance (- (get crops/ripe-age crop 7) age) secs rate))

(def harvests
  "The harvests a crop at stage age growing at rate stages a second (crops/stages-per-s) and replanted at each cut is worth now: the mean
  over horizon-s of the harvests ripe by then (the k-th at its stages left plus k-1 whole growths), so a harvest ripe at once counts 1
  and one past the horizon nothing, as cost/wait-price prices a wait for a find. Memoised: patch scans price every patch with it."
  (memoize
   (fn [crop age rate]
     (let [h (horizon-s)
           n 32
           ripe (get crops/ripe-age crop 7)
           left (max 0 (- ripe age))
           by (fn [t] (reduce + (take-while #(> % 1e-6) (map #(at-least-chance (+ left (* % ripe)) t rate) (range)))))]
       (/ (reduce + (map #(by (* h (/ (+ % 0.5) n))) (range n))) n)))))

(defn sown-worth
  "Seconds sowing a cell with crop is worth, at rate (crops/stages-per-s): the hunger points each harvest gives less the seed
  replanted (value/crop-points, value/food-points), at value/food-point-worth, times its harvests from stage 0. Never below 0: a cell not worth its seed is left bare."
  [crop rate]
  (* (value/food-point-worth) (harvests crop 0 rate)
     (max 0 (- (value/crop-points crop) (value/food-points (crops/seed-of crop))))))

(defn step-s
  "Seconds step works over units units: its ::field-work line, 0 for no units."
  [step units]
  (let [{:keys [fixed-s per-unit-s]} (get (settings/get settings ::field-work) step)]
    (if (pos? units) (+ fixed-s (* per-unit-s units)) 0)))

(defn earthwork-s
  "Seconds levelling a patch whose cells are `blocks` off its commonest height in all: each block dug or filled, dirt's dig time
  and a step to reach it."
  [blocks]
  (* blocks (+ (table/dig-cost "dirt") (value/per-block))))
