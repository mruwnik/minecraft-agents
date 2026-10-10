(ns jobs.lib.game-facts
  "Facts of the game that jobs rely on and that are not tuning: stack sizes, ripe levels, the time dusk comes."
  (:require [clojure.string :as str]))

(def stack-size "Items in a full stack of a stacking item." 64)
(def slot-max "Items one furnace or chest slot holds at most." 64)
(def ripe-level "The honey level at which a hive can be harvested." 5)
(def dusk-tick "The time of day (game ticks) at which dusk comes." 12000)

(defn gatherable?
  "Items with no recipe that the body gets by mining, felling or from sheep: logs, coal, wool."
  [item]
  (boolean (or (= "coal" item) (str/ends-with? item "_log") (str/ends-with? item "_wool"))))
