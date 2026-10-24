(ns jobs.lib.shelter.sleepers
  "Who sleeps on the server, and the log-out the night takes when someone does."
  (:require [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]))

(a/defargs settings
  {::default-player-radius {:default 128 :spec (a/int-in 1 nil)
                            :doc "Blocks within which a sleeping player counts when the night is skipped."}})

(defn default-player-radius [] (settings/get settings ::default-player-radius))

(defn sleeping-players
  "Other players within radius the body sees asleep (the primitives report a sleeping pose only in sight)."
  [p radius]
  (let [me (.-username (.self p))]
    (filterv #(and (.-sleeping %) (not= me (.-username %)))
             (array-seq (.entities p #js {:radius radius :kind "player" :max 32})))))

(defn log-out-unsupported?
  "Whether the latest :log-out entry says the server does not support logging out."
  [view]
  (= "unsupported" (:status (:data (mem/latest view :log-out)))))

(defn counts-self?
  "Whether status (tonight's latest :sleep-status entry) counts the body: it is asleep, or the count was sent before it
  woke from its own sleep tonight."
  [p view status]
  (let [woke (:t (mem/latest view :woke))
        slept (:t (sh-clock/latest-since view :slept (sh-clock/tonight p view)))]
    (boolean (and status (or (sh-sense/sleeping? p) (and woke slept (<= slept woke) (<= (:t status) woke)))))))

(defn asleep-known?
  "Whether someone on the server is known to sleep tonight: the latest :sleep-status entry tonight (the action bar's
  count, sent when it changes) counts a sleeper or says the night is skipped (a count that counts-self? needs another
  sleeper), or a sleeping player is in sight."
  [p view]
  (let [status (sh-clock/latest-since view :sleep-status (sh-clock/tonight p view))]
    (boolean (or (and status (if (counts-self? p view status)
                               (> (:sleeping (:data status) 0) 1)
                               (or (:skipping (:data status)) (pos? (:sleeping (:data status) 0)))))
                 (seq (sleeping-players p (default-player-radius)))))))

(defn others-asleep?
  "Whether someone on the server sleeps tonight, as a player can tell: asleep-known?; or the body is back from a log-out
  tonight that a known sleeper called for (not :assumed) and no count has come since its return (:online): unknown is
  taken as still asleep for that one more stint, a stint taken on that assumption alone does not call for another."
  [p view]
  (let [since (sh-clock/tonight p view)
        status (sh-clock/latest-since view :sleep-status since)
        out (sh-clock/latest-since view :log-out since)
        back (:t (mem/latest view :online))]
    (boolean (or (and out back (#{"ok" "cut"} (:status (:data out))) (not (:assumed (:data out)))
                      (or (nil? status) (< (:t status) back)))
                 (asleep-known? p view)))))

(defn log-out-open?
  "Night, someone else asleep (others-asleep?), and no log-out tonight that failed (unsupported, closed)."
  [p view]
  (let [out (sh-clock/latest-since view :log-out (sh-clock/tonight p view))]
    (and (sh-sense/night? p)
         (not (log-out-unsupported? view))
         (or (nil? out) (contains? #{"ok" "cut"} (:status (:data out))))
         (others-asleep? p view))))

(defn log-out-for-sleepers?
  "log-out-open? for a body awake."
  [p view]
  (and (not (sh-sense/sleeping? p)) (log-out-open? p view)))

(defn sleepers-short
  "How many players besides the body must still lie down for the night to skip (0: its own sleep skips it): tonight's
  latest :sleep-status (sleeping of needed, the body's own place taken out when counts-self?), else every player in
  the tab list (the game's default players-sleeping percentage is 100) less those seen asleep."
  [p view]
  (let [status (sh-clock/latest-since view :sleep-status (sh-clock/tonight p view))
        {:keys [skipping sleeping needed]} (:data status)]
    (cond
      skipping 0
      needed (max 0 (- needed (- (or sleeping 0) (if (counts-self? p view status) 1 0)) 1))
      :else (max 0 (- (count (.-players (.self p))) (count (sleeping-players p (default-player-radius))))))))
