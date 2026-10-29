(ns jobs.lib.shelter.sleepers
  "Who sleeps or is online on the server, and the log-out the night takes for them."
  (:require [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.danger :as danger]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.util :as u]
            [jobs.lib.shelter.sense :as sh-sense]))

(a/defargs settings
  {::default-player-radius {:default 128 :spec (a/int-in 1 nil)
                            :doc "Blocks within which a sleeping player counts when the night is skipped."}})

(defn default-player-radius [] (settings/get settings ::default-player-radius))

(def sleep-start-s "Seconds in a bed before the night skips (vanilla: 100 ticks of every player's sleep)." 5)

(def rest-box-xz "Blocks either side of a bed a monster keeps its sleeper from resting (vanilla Player.startSleepInBed: an AABB of 8 either way across, 5 either way up and down, round the bed block's bottom centre)." 8)
(def rest-box-y "See rest-box-xz." 5)

(defn monsters-near-bed?
  "Whether a hostile the body knows of stands in the rest box (rest-box-xz, rest-box-y) round the bed cell `at`: vanilla
  refuses the sleep then (\"you may not rest now\"). Known hostiles are taken from the box's far corner as seen from the
  body, since their distance is the body's."
  [p at]
  (let [corner (Math/sqrt (+ (* 2 rest-box-xz rest-box-xz) (* rest-box-y rest-box-y)))
        reach (+ corner (u/dist (u/self-pos {:primitives p}) at))]
    (boolean (some (fn [e] (let [m (danger/mob-pos p e)]
                             (and (<= (Math/abs (- (:x m) (+ (:x at) 0.5))) rest-box-xz) (<= (Math/abs (- (:z m) (+ (:z at) 0.5))) rest-box-xz)
                                  (<= (Math/abs (- (:y m) (:y at))) rest-box-y))))
                   (danger/known-hostiles p reach {})))))

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

(defn others-online "How many other players the tab list shows." [p] (count (.-players (.self p))))

(defn skip-wait-s
  "Seconds of skipped night the other players online lose while the body stays awake, left seconds of night still to run:
  each waits it out. None when tonight's latest count says the night skips anyway, or needs more players than there are
  (the server's sleeping percentage over 100)."
  [p view left]
  (let [{:keys [skipping needed]} (:data (sh-clock/latest-since view :sleep-status (sh-clock/tonight p view)))
        others (others-online p)]
    (if (or skipping (and needed (> needed (inc others)))) 0 (* others left))))

(defn log-out-open?
  "Night, someone else asleep (others-asleep?) or online with a skip to wait for (skip-wait-s), and no log-out tonight
  that failed (unsupported, closed)."
  [p view]
  (let [out (sh-clock/latest-since view :log-out (sh-clock/tonight p view))]
    (and (sh-sense/night? p)
         (not (log-out-unsupported? view))
         (or (nil? out) (contains? #{"ok" "cut"} (:status (:data out))))
         (or (others-asleep? p view) (pos? (skip-wait-s p view 1))))))

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
