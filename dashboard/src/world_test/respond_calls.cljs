(ns world-test.respond-calls
  "tools/event-counts.mjs respond-calls: counts the measured spread behind jobs.lib.hostile-loss ::call-s from the trial
  bodies' logs alone (no fold): each hostile-near reflex call, from its :fired to its :ended (a generation's job id).
  Pure parts: scan-line, report."
  (:require [clojure.string :as str]))

(defn- field [line re] (second (re-find re line)))

(defn scan-line
  "One log line -> {:key [generation job-id] :t ms :edge :fired|:ended} of a hostile-near reflex call, else nil."
  [line]
  (when (str/includes? line ":reflex-id :hostile-near")
    (when-let [edge (#{"fired" "ended"} (field line #":kind :(\w+)"))]
      {:key [(field line #":generation-id \"([^\"]*)\"") (field line #":job-id \"([^\"]*)\"")]
       :t (js/parseFloat (field line #":time-ms (\d+)")) :edge (keyword edge)})))

(defn- r2 [x] (/ (js/Math.round (* 100 x)) 100))

(defn report
  "{:n calls :deciles [seconds at (i + 0.5) / 10] :setting} of the scanned rows: each call fired and ended; :setting the
  value of hostile-loss/::call-s."
  [rows]
  (let [ss (vec (sort (for [[_ rs] (group-by :key rows)
                            :let [fired (some #(when (= :fired (:edge %)) (:t %)) rs)
                                  ended (some #(when (= :ended (:edge %)) (:t %)) rs)]
                            :when (and fired ended (<= fired ended))]
                        (/ (- ended fired) 1000))))
        n (count ss)
        deciles (when (pos? n) (mapv #(r2 (nth ss (min (dec n) (js/Math.floor (* n (/ (+ % 0.5) 10)))))) (range 10)))]
    {:n n :deciles deciles :setting (when deciles {:n n :deciles deciles})}))
