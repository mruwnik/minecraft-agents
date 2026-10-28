(ns world-test.pen-escapes
  "tools/event-counts.mjs pen-escapes: the measured prior behind jobs.lib.cost.pen ::escape-prior, the rate an animal walks
  out through one fence gap. Rows {run-id {:escapes :animal-s}} of the counts file's :pen-escapes: first exits seen over animal-seconds
  of herd watched, per gap, from fixture pen-gap-escapes (cows in a 5x5 ring with the gap next to every cow: not a pen's
  perimeter rate, so a real pen's per-gap rate is lower).
  report: :setting {:escapes :animal-s} is the Gamma prior ::escape-prior takes: the weak guess `prior` plus the counted rows as pseudo-counts. Pure.")

(def prior
  "The weak educated guess ::escape-prior starts from (one escape per 300 animal-seconds, Q40) at half an escape's weight, so counted rows decide."
  {:escapes 0.5 :animal-s 150})

(defn row
  "The row of a run polled every step-s seconds: escaped animals out (each counted at its first exit) and inside-s animal-seconds
  seen inside the ring at the polls (step-s per animal per poll), times the gaps. An animal that left between two polls was
  inside half a step more."
  [escaped inside-s step-s gaps]
  {:escapes escaped :animal-s (* gaps (+ inside-s (* escaped step-s 0.5)))})

(defn fold
  "counts with rows {run-id {:escapes :animal-s}} merged into :pen-escapes; a row already there stays."
  [counts rows]
  (update counts :pen-escapes #(merge rows %)))

(defn report
  "{:runs :escapes :animal-s :setting {:escapes :animal-s}} of the :pen-escapes rows."
  [rows]
  (let [rs (vals rows)
        e (reduce + (map :escapes rs))
        s (reduce + (map :animal-s rs))]
    {:runs (count rs) :escapes e :animal-s s
     :setting {:escapes (+ (:escapes prior) e) :animal-s (+ (:animal-s prior) s)}}))
