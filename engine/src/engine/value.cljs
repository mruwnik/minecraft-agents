(ns engine.value
  "The coarse item worth tiers make-room reads to decide what may be tossed (item-worth), and the drops' despawn
  window. What a pile is worth and what fetching it costs, as recover-drops decides it, is engine.jobs.value
  (built from minecraft-data).

  Item value, per stack (an inventory entry), first matching rule wins:

    enchanted  25  the item has a non-empty :enchants or :nbt key
    high       25  diamond and netherite anything, ancient debris, elytra,
                   totem of undying, nether star, beacon, shulker boxes
    medium      5  iron tools and armour, bow, crossbow, shield, redstone,
                   ender pearls, golden apples
    metal       2  each raw iron, iron ingot, gold ingot, coal block and iron
                   block: it takes mining, smelting and time to replace, so a
                   stack is worth its count times 2
    low         1  food, logs, planks, wood, stone/wooden/golden tools,
                   leather gear
    junk        0  everything else (seeds, dirt, cobblestone, saplings,
                   sticks, flowers, unknown names)")

(def tier-values {:junk 0 :low 1 :medium 5 :high 25})

(def metal-each 2)

(def rules
  "Name rules in priority order: {:match regex} plus :tier (the stack's worth is the tier value) or
  :each (the stack's worth is :each per item)."
  [{:tier :high :match #"^(diamond|netherite)(_|$)|^(ancient_debris|elytra|totem_of_undying|nether_star|beacon)$|shulker_box$"}
   {:tier :medium :match #"^iron_(pickaxe|axe|shovel|hoe|sword|helmet|chestplate|leggings|boots)$"}
   {:tier :medium :match #"^(bow|crossbow|shield|redstone|ender_pearl|golden_apple)$"}
   {:each metal-each :match #"^(coal_block|iron_block|iron_ingot|gold_ingot|raw_iron)$"}
   {:tier :low :match #"^(cooked_.*|bread|apple|carrot|baked_potato|golden_carrot|melon_slice|sweet_berries)$"}
   {:tier :low :match #"_(log|planks|wood)$"}
   {:tier :low :match #"^(stone|wooden|golden)_(pickaxe|axe|shovel|hoe|sword)$"}
   {:tier :low :match #"^leather"}])

(defn enchanted?
  "Whether the item carries a non-empty :enchants or :nbt."
  [item]
  (boolean (some #(seq (get item %)) [:enchants :nbt])))

(defn rule-for
  "The first rule matching the entry's name, or nil."
  [{:keys [name]}]
  (some #(when (re-find (:match %) name) %) rules))

(defn item-worth
  "The worth of one inventory entry ({:name :count? ...}); a missing :count counts as 1."
  [item]
  (cond
    (enchanted? item) (tier-values :high)
    :else (let [r (rule-for item)]
            (cond (nil? r) 0
                  (:each r) (* (:each r) (or (:count item) 1))
                  :else (tier-values (:tier r))))))

;; ---------------------------------------------------------------- the window

(def despawn-ms "How long dropped items lie before they despawn." (* 5 60 1000))
