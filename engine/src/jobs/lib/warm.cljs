(ns jobs.lib.warm
  "The tables the first danger would build while a mob is beside the body, built once the body is up (the :body/warm hook)."
  (:require [engine.game :as game]
            [engine.path.blocks :as blocks]
            [jobs.lib.cost.table :as table]))

(defn warm!
  "Build the item worth table of the body's game version (as item-value asks it) and the planner's block table."
  []
  (table/table-for @game/version {} nil)
  (blocks/default-state-table)
  nil)
