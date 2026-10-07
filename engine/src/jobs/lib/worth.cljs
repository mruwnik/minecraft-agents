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
    ore       1-2  each raw gold (2), raw copper and lapis (1); a stack of coal or
                   charcoal is 1 (settings ::raw-gold-worth, ::ore-worth, ::fuel-worth)
    low         1  food, logs, planks, wood, stone/wooden/golden tools,
                   leather gear
    junk        0  everything else (seeds, dirt, cobblestone, saplings,
                   sticks, flowers, unknown names)"
  (:require [engine.settings :as settings]
            [jobs.lib.cost :as cost]))

(def settings
  {::enchanted-worth {:default 25 :doc "What an enchanted item is worth on top of its base." :type :int :min 0}
   ::raw-gold-worth {:default 2 :doc "Worth of one raw gold." :type :int :min 0}
   ::ore-worth {:default 1 :doc "Worth of one raw copper or lapis lazuli." :type :int :min 0}
   ::fuel-worth {:default 1 :doc "Worth of a stack of coal or charcoal." :type :int :min 0}})

(defn prices
  "make-room's prices, [[regex price] ...] in priority order (first match wins; unmatched items cost 0). A price is per
  stack; the metals' and ores' rows are per item."
  []
  [[#"^(diamond|netherite)(_|$)|^(ancient_debris|elytra|totem_of_undying|nether_star|beacon)$|shulker_box$" 25 :per-stack]
   [#"^iron_(pickaxe|axe|shovel|hoe|sword|helmet|chestplate|leggings|boots)$" 5 :per-stack]
   [#"^(bow|crossbow|shield|redstone|ender_pearl|golden_apple|emerald|emerald_block)$" 5 :per-stack]
   [#"^(coal_block|iron_block|iron_ingot|gold_ingot|raw_iron)$" 2]
   [#"^raw_gold$" (settings/get settings ::raw-gold-worth)]
   [#"^(raw_copper|lapis_lazuli)$" (settings/get settings ::ore-worth)]
   [#"^(cooked_.*|bread|apple|carrot|baked_potato|golden_carrot|melon_slice|sweet_berries)$" 1 :per-stack]
   [#"_(log|planks|wood)$" 1 :per-stack]
   [#"^(stone|wooden|golden)_(pickaxe|axe|shovel|hoe|sword)$" 1 :per-stack]
   [#"^(coal|charcoal)$" (settings/get settings ::fuel-worth) :per-stack]
   [#"^leather" 1 :per-stack]])

(defn enchanted-worth [] (settings/get settings ::enchanted-worth))

(defn item-worth
  "The worth of one inventory entry ({:name :count? ...}); a missing :count counts as 1."
  [item]
  (:value (cost/item-value [item] :prices (prices) :else 0 :enchanted (enchanted-worth))))
