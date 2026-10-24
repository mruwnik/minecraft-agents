(ns world-test.caught-flights
  "tools/event-counts.mjs caught-flights: counts the measured prior jobs.lib.cost.answer ::caught from the trial bodies' event logs.
  An episode is a run of flee decisions about the zombie family, each within gap-ms of the one before; its health the first's.
  Within gap-ms after its last decision it is caught when the zombie family killed the body or landed two blows or more (one is the
  flight's own start, answer/start-reach); its blows are those hits. Per health band (answer ::caught's :from), k caught of n,
  the deaths, and :blows, the Poisson mean of a caught flight's blows: the grid posterior mean (prior 1/lambda) of the deaths of
  the caught episodes, each dying when at least ceil(health / hit) blows land, hit a zombie's blow on easy.
  Pure part: episodes, report."
  (:require [jobs.lib.cost.threat :as threat]))

(def gap-ms 30000)
(def family #{"zombie" "husk" "drowned" "zombie_villager"})

(defn episodes
  "Rows [{:t :health :died? :hits}] of the episodes in records (world-test.event-counts/scan-line's, sorted by :t)."
  [records]
  (let [eps (reduce (fn [eps {:keys [k t mob decision health]}]
                      (if (and (= :decided k) (family mob) (= :flee decision))
                        (let [e (peek eps)]
                          (if (and e (<= (- t (:end e)) gap-ms)) (conj (pop eps) (assoc e :end t)) (conj eps {:t t :end t :health health})))
                        eps))
                    [] records)
        within (fn [{:keys [t end]} r] (<= t (:t r) (+ end gap-ms)))]
    (for [e eps :when (:health e)]
      {:t (:t e) :health (:health e)
       :died? (boolean (some #(and (= :died (:k %)) (family (:cause %)) (within e %)) records))
       :hits (reduce + 0 (keep #(when (and (= :hurt (:k %)) (some family (:causes %)) (within e %)) (or (:hits %) 1)) records))})))

(defn caught? [{:keys [died? hits]}] (or died? (<= 2 hits)))

(defn tail [lam k] (- 1 (reduce + (take k (reductions (fn [p i] (/ (* p lam) i)) (js/Math.exp (- lam)) (iterate inc 1))))))

(defn blows
  "The grid posterior mean of lambda over caught rows: each died? with chance tail(lambda, ceil(health / hit))."
  [rows hit]
  (let [lams (map #(* 0.1 %) (range 1 300))
        logs (map (fn [lam] (- (reduce + (map (fn [{:keys [health died?]}]
                                                 (let [p (min (- 1 1e-12) (max 1e-12 (tail lam (js/Math.ceil (/ health hit)))))]
                                                   (js/Math.log (if died? p (- 1 p)))))
                                               rows))
                               (js/Math.log lam)))
                  lams)
        top (apply max logs)
        ws (map #(js/Math.exp (- % top)) logs)]
    (/ (reduce + (map * lams ws)) (reduce + ws))))

(defn r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn report
  "{:bands [{:from :k :n :deaths}] :blows} of rows, banded at froms (health at least each, below the next)."
  [rows froms]
  (let [band (fn [[lo hi]] (filter #(and (<= lo (:health %)) (< (:health %) hi)) rows))]
    {:bands (mapv (fn [[lo :as b]] (let [rs (band b)] {:from lo :k (count (filter caught? rs)) :n (count rs) :deaths (count (filter :died? rs))}))
                  (map vector froms (concat (rest froms) [##Inf])))
     :blows (r3 (blows (filter caught? rows) (threat/difficulty-scale (get threat/mob-hit "zombie"))))}))
