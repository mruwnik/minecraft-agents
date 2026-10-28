(ns jobs.lib.cost.caught
  "The prior of a walk away being caught whatever its route (jobs.lib.cost.answer/flee-answer-on), and of a fight going
  bad (jobs.lib.cost.answer/fight-answer), from the trial bodies' flights and fights. Pure.

  A flight from mobs of a group (group-of) is caught at a chance logistic(a + b (health - 10)) of the health it starts
  at, smooth in it; caught, its mobs land a Poisson count of blows of mean lambda, else of mean mu (the walk's own). A
  flight counted as lived took the blows it was hurt by; one that died, at least the blows that kill at its health (at the
  trial difficulty): its health weighs on its death through those blows, and the slope b is what the flights add beyond
  them. posterior: the grid posterior of a, b, lambda and mu over a group's counts (::flights); priors: the chance at
  10 hp uniform, b uniform over [-0.4, 0.4], lambda 1/lambda, mu uniform. A fight is counted alike (::fights): gone bad
  at the chance, its mobs land lambda blows, else mu (the fight's own); one followed by a flight counts what the flight
  took too, the price of choosing it."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.threat :as threat]))

(def counts? (a/coll-of (a/coll-of (a/num-in 0 nil))))

(a/defargs settings
  {::flights {:default {:zombie {:lived [[1 0 1] [3 0 2] [4 0 2] [4 1 1] [5 1 1] [6 0 1] [7 0 1] [8 1 1] [9 2 1] [10 0 1] [10 3 1] [11 0 2] [11 4 1]
                                         [12 0 1] [12 6 1] [13 1 1] [14 7 1] [19 0 1] [20 0 18] [20 1 2] [20 2 2] [20 3 1] [20 4 1]]
                                 :died [[1 1 2] [2 1 2] [3 2 3] [4 2 2] [5 2 1] [6 3 3] [7 3 1] [8 4 3] [9 4 1] [11 5 1] [15 6 1] [17 7 1]
                                        [19 8 1] [20 8 3]]}
                        :bow {:lived [[1 0 2] [2 0 3] [4 0 1] [5 2 1] [6 1 1] [6 3 1] [7 0 2] [8 0 2] [8 1 3] [9 0 1] [11 0 1] [11 1 1] [12 2 1]
                                      [13 0 2] [16 1 1] [17 2 1] [17 4 1] [17 7 1] [18 1 1] [19 0 2] [19 1 1] [20 0 11] [20 1 2]]
                              :died [[1 1 1] [6 3 1]]}}
              :spec (a/map-of keyword? (a/map-with {:lived counts? :died counts?}))
              :doc "Per group of mob kinds (group-of), the trial flights from it: :lived [health blows n], n flights begun at health that lived through it hurt by blows (a shooter's arrows) of it; :died [health blows n], n that died of it, blows the least that kill at health. A flight is the run of flee decisions about the group, each within 30 s of the one before, and the 30 s after it. Recount: node tools/event-counts.mjs caught-flights (trial bodies, easy), 2026-10-25."}
   ::fights {:default {:zombie {:lived [[11 2 1] [12 2 1] [13 0 2] [13 2 1] [13 3 1] [13 4 1] [14 1 2] [14 3 1] [15 1 1] [15 3 1] [16 0 1] [16 1 1] [17 0 1]
                                        [17 4 1] [18 0 4] [18 1 1] [18 2 3] [19 0 1] [20 0 17] [20 1 8] [20 2 12] [20 3 5] [20 4 3] [20 7 1] [20 8 1]]
                                :died [[9 4 1] [20 8 4]]}
                       :bow {:lived [[10 1 1] [11 0 1] [11 1 2] [11 2 1] [12 2 1] [13 0 2] [13 1 1] [15 0 1] [15 1 2] [15 2 1] [15 3 1] [17 1 2] [17 2 1]
                                     [17 5 1] [18 0 1] [18 1 2] [19 1 1] [20 0 3] [20 1 5] [20 2 7] [20 3 7] [20 4 1] [20 5 1] [20 6 1]]
                             :died [[12 5 1]]}
                       :spider {:lived [[10 0 2] [11 0 2] [12 0 2] [15 1 1] [16 5 1] [18 0 1] [18 1 2] [18 4 1] [20 0 10] [20 1 4] [20 2 1] [20 3 1]]
                                :died []}}
             :spec (a/map-of keyword? (a/map-with {:lived counts? :died counts?}))
             :doc "As ::flights, the trial fights with a sword or axe: a fight the run of fight decisions about the group, each within 30 s of the one before, and the 30 s after it; none against a drowned (its swum fights are priced by cost.fight/wet-rate). Recount: node tools/event-counts.mjs caught-flights :fight-counts, 2026-10-27."}})

(def groups
  "The mob kinds a flight counts as from one group: what group-of reads."
  {:zombie #{"zombie" "husk" "drowned" "zombie_villager"} :creeper #{"creeper"} :bow threat/bow-mobs :spider #{"spider" "cave_spider"}})

(defn group-of
  "The group whose counts (which: ::flights or ::fights) price mob-name's: its own when counted, else the zombie family's."
  [which mob-name]
  (let [counted (settings/get settings which)]
    (or (some (fn [[g kinds]] (when (and (contains? kinds mob-name) (contains? counted g)) g)) groups) :zombie)))

(defn sigmoid [x] (/ 1 (+ 1 (js/Math.exp (- x)))))

(defn poisson-pmfs
  "Float64Array of the Poisson(lam) chances of 0..n-1."
  [lam n]
  (let [ps (js/Float64Array. n)]
    (aset ps 0 (js/Math.exp (- lam)))
    (doseq [i (range 1 n)] (aset ps i (/ (* (aget ps (dec i)) lam) i)))
    ps))

(defn grid [lo step n] (mapv #(+ lo (* step %)) (range n)))

(def posterior
  "{:points #js [a b lambda w] (weight over 1e-7, mu summed out) :blows E[lambda] :shape E[lambda]^2 / Var[lambda]} of a
  group's counts {:lived :died} (::flights'). Memoized: a few groups."
  (memoize
    (fn [{:keys [lived died]}]
      (let [rows (into (mapv (fn [[h k n]] [h k n false]) lived) (map (fn [[h m n]] [h m n true])) died)
            nr (count rows)
            top-k (inc (reduce max 1 (map second rows)))
            as (grid -3 0.25 25) bs (grid -0.4 0.05 17) lams (grid 0.5 0.5 30) mus (grid 0 0.15 9)
            terms (fn [x] (let [ps (poisson-pmfs x (inc top-k))]
                            (js/Float64Array.from (map (fn [[_ k _ died?]] (if died? (- 1 (reduce + (take k ps))) (aget ps k))) rows))))
            hs (js/Float64Array.from (map #(- (first %) 10) rows))
            ns (js/Float64Array.from (map #(nth % 2) rows))
            mu-terms (mapv terms mus)
            ab (for [a as b bs] [a b])
            ps (mapv (fn [[a b]] (js/Float64Array.from (map #(sigmoid (+ a (* b %))) hs))) ab)
            logs (js/Float64Array. (* (count ab) (count lams)))
            sums (js/Float64Array. (count mus))]
        (doseq [[li lam] (map-indexed vector lams)
                :let [v (terms lam)]
                [j [a]] (map-indexed vector ab)
                :let [p (ps j) pa (sigmoid a)]]
          (dotimes [mi (count mus)]
            (let [u (mu-terms mi)]
              (aset sums mi (loop [i 0 acc 0]
                              (if (< i nr)
                                (let [pi (aget p i)]
                                  (recur (inc i) (+ acc (* (aget ns i) (js/Math.log (max 1e-300 (+ (* (- 1 pi) (aget u i)) (* pi (aget v i)))))))))
                                acc)))))
          (let [m (reduce max (array-seq sums))]
            (aset logs (+ (* j (count lams)) li)
                  (+ m (js/Math.log (areduce sums i acc 0 (+ acc (js/Math.exp (- (aget sums i) m)))))
                     (js/Math.log (* pa (- 1 pa))) (- (js/Math.log lam))))))
        (let [top (reduce max (array-seq logs))
              ws (.map logs #(js/Math.exp (- % top)))
              total (areduce ws i acc 0 (+ acc (aget ws i)))
              points (into-array (for [[j [a b]] (map-indexed vector ab) [li lam] (map-indexed vector lams)
                                       :let [w (/ (aget ws (+ (* j (count lams)) li)) total)]
                                       :when (< 1e-7 w)]
                                   #js [a b lam w]))
              moment (fn [f] (areduce points i acc 0 (let [p (aget points i)] (+ acc (* (aget p 3) (f (aget p 2)))))))
              blows (moment identity)
              spread (- (moment #(* % %)) (* blows blows))]
          {:points points :blows blows :shape (when (pos? spread) (/ (* blows blows) spread))})))))

(def chance
  "The posterior mean chance a flight from a group of counts begun at health is caught. Memoized."
  (memoize
    (fn [counts health]
      (let [{:keys [points]} (posterior counts) x (- health 10)]
        (areduce points i acc 0 (let [p (aget points i)] (+ acc (* (aget p 3) (sigmoid (+ (aget p 0) (* (aget p 1) x)))))))))))

(defn prior
  "{:chance :blows :shape} of a flight (which ::flights, the default) or a fight (::fights) with mob-name begun at
  health (default 20, read in whole hp: chance's memo keys on it): the posterior mean chance it is caught or goes bad (chance) and of the blows it then takes, the Gamma shape of their mean's uncertainty (blow-chance's width)."
  ([mob-name health] (prior ::flights mob-name health))
  ([which mob-name health]
   (let [counts (get (settings/get settings which) (group-of which mob-name))
         {:keys [blows shape]} (posterior counts)]
     {:chance (chance counts (js/Math.round (or health 20))) :blows blows :shape shape})))
