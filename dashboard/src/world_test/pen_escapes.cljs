(ns world-test.pen-escapes
  "tools/event-counts.mjs pen-escapes: the measured prior behind jobs.lib.cost.pen ::escape-prior, the rate an animal walks
  out through one fence gap. Rows {run-id {:escapes :animal-s}} of the counts file's :pen-escapes: escapes seen over animal-seconds
  of herd watched, per gap (a run with g gaps and n animals for t seconds counts n g t animal-s).
  report: :setting {:escapes :animal-s} is the Gamma prior ::escape-prior takes: the educated guess of prior (one escape per
  300 animal-seconds, Q40) plus the counted rows as pseudo-counts. Pure.")

(def prior "The educated guess ::escape-prior starts from, which counted rows add to." {:escapes 1 :animal-s 300})

(defn row
  "The row of a run of animals animals watched secs seconds at gaps gaps with escaped of them out at the end: an animal out counts
  half the watch (it left at a uniform time), one inside all of it, times the gaps."
  [animals secs escaped gaps]
  {:escapes escaped :animal-s (* gaps (+ (* (- animals escaped) secs) (* escaped secs 0.5)))})

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
