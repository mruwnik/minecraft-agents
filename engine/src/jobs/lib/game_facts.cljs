(ns jobs.lib.game-facts
  "Facts of the game that jobs rely on and that are not tuning: stack sizes, ripe levels, the time dusk comes."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [engine.game :as game]))

(def stack-size "Items in a full stack of a stacking item." 64)
(def slot-max "Items one furnace or chest slot holds at most." 64)
(def ripe-level "The honey level at which a hive can be harvested." 5)
(def dusk-tick "The time of day (game ticks) at which dusk comes." 12000)

(def smelted-from
  "{output input} of what the furnaces cook, for naming the input of a product. The vanilla recipes are in no
  minecraft-data file."
  (into
   {"iron_ingot" "raw_iron" "gold_ingot" "raw_gold" "copper_ingot" "raw_copper" "netherite_scrap" "ancient_debris"
   "glass" "sand" "smooth_stone" "stone" "brick" "clay_ball" "terracotta" "clay"
   "nether_brick" "netherrack" "smooth_sandstone" "sandstone" "smooth_red_sandstone" "red_sandstone"
   "cracked_stone_bricks" "stone_bricks" "cracked_deepslate_bricks" "deepslate_bricks"
   "cracked_deepslate_tiles" "deepslate_tiles" "cracked_nether_bricks" "nether_bricks" "smooth_quartz" "quartz_block" "lime_dye" "sea_pickle" "green_dye" "cactus"
   "popped_chorus_fruit" "chorus_fruit" "stone" "cobblestone"}
   (map (fn [[raw cooked]] [cooked raw])) game/cooking))

(def log-names
  "The overworld log blocks (the stem woods are not logs)."
  (mapv #(str % "_log") ["oak" "spruce" "birch" "jungle" "acacia" "dark_oak" "mangrove" "cherry" "pale_oak"]))

(def charcoal-inputs
  "Every block the furnace turns to charcoal (the vanilla logs_that_burn tag): logs, stripped logs, woods, stripped woods."
  (vec (for [tree (map #(subs % 0 (- (count %) 4)) log-names)
             form [[nil "_log"] [nil "_wood"] ["stripped_" "_log"] ["stripped_" "_wood"]]]
         (str (first form) tree (second form)))))

(def smelt-inputs
  "{output [input names]} of every furnace product: smelted-from, and charcoal from any burning log or wood."
  (assoc (update-vals smelted-from vector) "charcoal" charcoal-inputs))

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
