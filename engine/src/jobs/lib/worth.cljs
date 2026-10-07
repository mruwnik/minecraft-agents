(ns jobs.lib.worth
  "make-room's own prices for jobs.lib.cost/item-value: coarse keep tiers that decide what may be tossed (item-worth).

  Item value, per stack (an inventory entry), first matching rule wins:

    enchanted  25  the item has a non-empty :enchants or :nbt key
    high       25  diamond and netherite anything, ancient debris, elytra,
                   totem of undying, nether star, beacon, shulker boxes
    medium      5  iron tools and armour, bow, crossbow, shield, redstone,
                   ender pearls, golden apples, emeralds (trade currency)
    metal       2  each raw iron, iron ingot, gold ingot, coal block and iron
                   block: it takes mining, smelting and time to replace, so a
                   stack is worth its count times 2
    low         1  food, logs, planks, wood, stone/wooden/golden tools,
                   leather gear
    junk        0  everything else (seeds, dirt, cobblestone, saplings,
                   sticks, flowers, unknown names)"
  (:require [jobs.lib.cost :as cost]))

(def prices
  "make-room's prices, [[regex price] ...] in priority order (first match wins; unmatched items cost 0)."
  [[#"^(diamond|netherite)(_|$)|^(ancient_debris|elytra|totem_of_undying|nether_star|beacon)$|shulker_box$" 25]
   [#"^iron_(pickaxe|axe|shovel|hoe|sword|helmet|chestplate|leggings|boots)$" 5]
   [#"^(bow|crossbow|shield|redstone|ender_pearl|golden_apple|emerald|emerald_block)$" 5]
   [#"^(coal_block|iron_block|iron_ingot|gold_ingot|raw_iron)$" 2]
   [#"^(cooked_.*|bread|apple|carrot|baked_potato|golden_carrot|melon_slice|sweet_berries)$" 1]
   [#"_(log|planks|wood)$" 1]
   [#"^(stone|wooden|golden)_(pickaxe|axe|shovel|hoe|sword)$" 1]
   [#"^leather" 1]])

(def enchanted-worth 25)

(def per-count "Metals: a stack is worth its count times the price." #"^(coal_block|iron_block|iron_ingot|gold_ingot|raw_iron)$")

(defn enchanted?
  "Whether the item carries a non-empty :enchants or :nbt."
  [item]
  (boolean (some #(seq (get item %)) [:enchants :nbt])))

(defn item-worth
  "The worth of one inventory entry ({:name :count? ...}); a missing :count counts as 1."
  [{:keys [name count] :as item}]
  (if (enchanted? item)
    enchanted-worth
    (let [each (:each (first (:items (cost/item-value [(assoc item :count 1)] :prices prices :else 0))))]
      (if (re-find per-count name) (* each (or count 1)) each))))
