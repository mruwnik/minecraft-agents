(ns jobs.lib.game-facts
  "Facts of the game that jobs rely on and that are not tuning: stack sizes, ripe levels, the time dusk comes."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]))

(def stack-size "Items in a full stack of a stacking item." 64)
(def slot-max "Items one furnace or chest slot holds at most." 64)
(def ripe-level "The honey level at which a hive can be harvested." 5)
(def dusk-tick "The time of day (game ticks) at which dusk comes." 12000)

(defn gatherable?
  "Items the body gets by mining, felling or from sheep: logs, coal, wool (white wool is also crafted from string)."
  [item]
  (boolean (or (= "coal" item) (str/ends-with? item "_log") (str/ends-with? item "_wool"))))

(def crafted-from
  "How many of `input` one craft of `item` takes in a recipe of `input` alone (4 string for a white_wool), or nil: the
  count items.obtain's recipe plans craft by, for a check that counts what carried items make."
  (memoize
   (fn [version item input]
     (let [md (minecraft-data version)
           id #(some-> (aget (.-itemsByName md) %) .-id)
           ids (fn [r] (keep #(if (number? %) % (some-> % .-id))
                             (if (.-inShape r) (mapcat array-seq (array-seq (.-inShape r))) (array-seq (.-ingredients r)))))]
       (some (fn [r] (let [xs (ids r)] (when (and (seq xs) (every? #{(id input)} xs)) (count xs))))
             (some-> (aget (.-recipes md) (id item)) array-seq))))))
