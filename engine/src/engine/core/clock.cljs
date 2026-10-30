(ns engine.core.clock
  "The engine's clocks: wall time, and the TTL clock (wall time less host suspends).")

(defn now [eng] ((:now eng)))

(def suspend-gap-ms
  "Wall-clock time beyond the monotonic clock's that counts as a host suspend, not jitter."
  5000)

(defn ttl-now
  "The clock of TTLs (mutes, moves, entry :ttl-s): wall time minus the host suspends seen so far, so a suspend
  or clock jump cannot end a TTL early. The suspended total is state :clock-lag, saved with the state, so
  :until values stay comparable after a restart; the monotonic clock (:mono) is read at each call and the
  excess of wall time over it since the last call is added to the lag."
  [eng]
  (let [wall (now eng)]
    (when-let [mono-fn (:mono eng)]
      (let [mono (mono-fn)
            [wall0 mono0] (first (reset-vals! (:clock-mark eng) [wall mono]))
            excess (- (- wall wall0) (- mono mono0))]
        (when (> excess suspend-gap-ms)
          (swap! (:state eng) update :clock-lag (fnil + 0) excess))))
    (- wall (:clock-lag @(:state eng) 0))))
