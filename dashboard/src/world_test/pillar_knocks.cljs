(ns world-test.pillar-knocks
  "tools/event-counts.mjs pillar-knocks: counts the measured prior behind jobs.lib.cost.ways ::pillar-knock-chance from the
  bodies' logs alone: the blocks a pillar placed (:built of each pillar.done and pillar.gave-up) and the knock-offs
  (pillar.knockback, and a pillar.gave-up for :off-column, which older logs hold alone).
  Pure parts: scan-line, report.")

(defn scan-line
  "{:blocks n :knocks k} one log line adds, else nil. A give-up :off-column adds a knock only when no pillar.knockback
  is logged for it (older logs): report counts the two kinds apart."
  [line]
  (cond
    (.includes line ":kind :pillar.knockback") {:knocks 1}
    (or (.includes line ":kind :pillar.done") (.includes line ":kind :pillar.gave-up"))
    (cond-> {:blocks (or (some-> (re-find #":built (\d+)" line) second js/parseInt) 0)}
      (.includes line ":reason :off-column") (assoc :ended-off 1))))

(defn report
  "{:blocks :knocks :ended-off :chance} of rows: the knockbacks logged per block placed (nil with no blocks); a log with
  no pillar.knockback line is read from the :off-column endings, a lower bound."
  [rows]
  (let [{:keys [blocks knocks ended-off] :or {blocks 0 knocks 0 ended-off 0}} (apply merge-with + rows)
        k (if (pos? knocks) knocks ended-off)]
    {:blocks blocks :knocks knocks :ended-off ended-off
     :chance (when (pos? blocks) (/ (js/Math.round (* 10000 (/ k blocks))) 10000))}))
