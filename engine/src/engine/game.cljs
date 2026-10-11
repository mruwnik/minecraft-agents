(ns engine.game
  "Facts of the game the body plays: the minecraft-data version it is connected with, and the item despawn window."
  (:require [engine.memory :as mem]
            [engine.settings :as settings]))

(def default-version
  "The minecraft-data version used when no body is connected (the connect default, engine/js/connect.mjs)."
  "26.1")

(def eye-height "Blocks from a standing body's feet to its eyes." 1.62)
(def hitbox-half "Half the width of the body's box (0.6 wide)." 0.3)
(def body-height "The height of a standing body's box." 1.8)

(defn version-of
  "The version the body of primitives p is connected with (the raw world's version, from the bot), else the default
  (no bot yet, or primitives without a raw world, as in tests)."
  [p]
  (let [raw (some-> p .-rawWorld)]
    (or (when (and raw (fn? (.-version raw))) (.version raw)) default-version)))

(def version
  "The version game data (foods, drops) is read for; set once per engine (select!)."
  (atom default-version))

(defn select!
  "Read game data for the version the body of primitives p is connected with."
  [p]
  (reset! version (version-of p)))

(def despawn-ticks "How long dropped items lie before they despawn, in game ticks (5 minutes at 20 TPS)." 6000)

(defn despawn-ms "The despawn window in ms at the live game rate." [] (settings/ticks->ms despawn-ticks))

(defn ticks-since
  "Game ticks since entry (a memory entry) at view {:now :age}: world age difference when both are known (it stands
  still while the tick is frozen), else the wall-clock ms since it at the live rate."
  [entry {:keys [now age]}]
  (if (and (number? (:age entry)) (number? age))
    (- age (:age entry))
    (settings/ms->ticks (- now (:t entry)))))

(defn despawn-left-ticks "Ticks left before the drops of the death entry despawn (<= 0: gone)." [entry view]
  (- despawn-ticks (ticks-since entry view)))

(defn death-summary
  "What status reports of a :died memory entry at view {:now :age}: {:pos :cause? :ago-ms :despawns-in-ms :recovered?}
  while the drops can still be there (under the despawn window, 6000 game ticks counted in world age), else nil. :recovered is the decision
  of a :recovered entry newer than the death (:collected, :partial, :skip, :abandoned), when there is one."
  ([entry view] (death-summary entry view nil))
  ([entry view recovered]
   (when entry
     (let [since (ticks-since entry view)
           {:keys [pos cause]} (:data entry)
           decision (when (and recovered (> (:t recovered) (:t entry))) (:decision (:data recovered)))]
       (when (< since despawn-ticks)
         (cond-> {:pos pos :ago-ms (settings/ticks->ms since) :despawns-in-ms (settings/ticks->ms (- despawn-ticks since))}
           cause (assoc :cause cause)
           decision (assoc :recovered decision)))))))

(defn death-status
  "The status :died extra for a memory view."
  [view]
  (death-summary (mem/latest view :died) view (mem/latest view :recovered)))
