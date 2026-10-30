(ns world-test.pillar-knocks
  "tools/event-counts.mjs pillar-knocks: counts the measured prior behind jobs.lib.cost.ways ::pillar-knock-chance from the
  bodies' logs alone: the blocks a pillar placed (:built of each pillar.done and pillar.gave-up) and the knock-offs
  (pillar.knockback, and a pillar.gave-up for :off-column, which older logs hold alone). Both are counted over the same
  lines: from the first pillar.knockback logged on, or over every log by the :off-column endings while none is.
  Pure parts: scan-line, report.
  The Beta prior is fitted by moments over the bodies' shares (field-passes/beta-fit)."
  (:require [world-test.field-passes :as passes]))

(defn scan-line
  "{:t ms :blocks n :knocks k :ended-off e} one log line adds (at least one of the three), else nil."
  [line]
  (let [t (some-> (re-find #":time-ms (\d+)" line) second js/parseInt)]
    (cond
      (.includes line ":kind :pillar.knockback") {:t t :knocks 1}
      (or (.includes line ":kind :pillar.done") (.includes line ":kind :pillar.gave-up"))
      (cond-> {:t t :blocks (or (some-> (re-find #":built (\d+)" line) second js/parseInt) 0)}
        (.includes line ":reason :off-column") (assoc :ended-off 1)))))

(defn- sums [rows] (apply merge-with + (map #(dissoc % :t :body) rows)))

(defn report
  "{:blocks :knocks :ended-off :bodies :prior :chance} of rows (scan-line's, with a :body): :knocks the knockbacks logged
  and :blocks the blocks placed from the first knockback on, or the :off-column endings and every block while none is
  logged (a lower bound); :chance k/n (nil with no blocks); :prior [knocked kept] the Beta pseudo-counts, fitted by moments
  over the bodies' shares, their pooled counts when too few differ."
  [rows]
  (let [t0 (some->> rows (filter :knocks) (keep :t) seq (apply min))
        logged? (some? t0)
        kept (if logged? (filter #(or (nil? (:t %)) (<= t0 (:t %))) rows) rows)
        k-of #(if logged? (:knocks % 0) (:ended-off % 0))
        {:keys [blocks knocks ended-off] :or {blocks 0 knocks 0 ended-off 0}} (sums kept)
        k (if logged? knocks ended-off)
        by-body (for [[_ rs] (group-by :body kept)
                      :let [b (:blocks (sums rs) 0)]
                      :when (pos? b)]
                  (/ (transduce (map k-of) + 0 rs) b))]
    {:blocks blocks :knocks knocks :ended-off ended-off :bodies (count by-body)
     :chance (when (pos? blocks) (/ (js/Math.round (* 10000 (/ k blocks))) 10000))
     :prior (when (pos? blocks) (if (seq by-body) (passes/beta-fit by-body k blocks) [k (- blocks k)]))}))
