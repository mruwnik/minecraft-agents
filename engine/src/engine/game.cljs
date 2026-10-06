(ns engine.game
  "Facts of the game the body plays: the minecraft-data version it is connected with, and the item despawn window.")

(def default-version
  "The minecraft-data version used when no body is connected (the connect default, engine/js/connect.mjs)."
  "26.1")

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

(def despawn-ms "How long dropped items lie before they despawn." (* 5 60 1000))
