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

(defn others-asleep?
  "Whether someone on the server sleeps tonight, as a player can tell: the latest :sleep-status entry tonight (the
  action bar's count, sent when it changes) counts a sleeper or says the night is skipped; or a sleeping player is
  in sight; or the body is back from a log-out tonight and no count has come since its return (:online): unknown is
  taken as still asleep. A count sent before the body woke from its own sleep tonight counts the body: it then needs
  another sleeper, and a skipped night tells nothing."
  [p view]
  (let [since (sh-clock/tonight p view)
        status (sh-clock/latest-since view :sleep-status since)
        out (sh-clock/latest-since view :log-out since)
        back (:t (mem/latest view :online))
        woke (:t (mem/latest view :woke))
        slept (:t (sh-clock/latest-since view :slept since))
        own? (and status woke slept (<= slept woke) (<= (:t status) woke))]
    (boolean (or (and out back (#{"ok" "cut"} (:status (:data out))) (or (nil? status) (< (:t status) back)))
                 (and status (if own?
                               (> (:sleeping (:data status) 0) 1)
                               (or (:skipping (:data status)) (pos? (:sleeping (:data status) 0)))))
                 (seq (sleeping-players p (default-player-radius)))))))

(defn log-out-for-sleepers?
  "Night, awake, someone else asleep (others-asleep?), and no log-out tonight that failed (unsupported, closed)."
  [p view]
  (let [out (sh-clock/latest-since view :log-out (sh-clock/tonight p view))]
    (and (sh-sense/night? p) (not (sh-sense/sleeping? p))
         (not (log-out-unsupported? view))
         (or (nil? out) (contains? #{"ok" "cut"} (:status (:data out))))
         (others-asleep? p view))))
