(ns jobs.lib.build.kept
  "Blocks a clearing never digs: beds, containers and the like.")

(def kept-pattern
  #"(_bed|chest|barrel|furnace|smoker|crafting_table|shulker_box|hopper|dispenser|dropper|brewing_stand|anvil|enchanting_table)$")

(defn kept? [keep n]
  (or (some? (re-find kept-pattern n)) (boolean (some #{n} keep))))
