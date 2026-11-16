(ns jobs.lib.warm
  "The tables the first danger would build while a mob is beside the body, built once the body is up (the :body/warm hook)."
  (:require [engine.game :as game]
            [engine.path.blocks :as blocks]
            [engine.settings :as settings]
            [jobs.lib.cost.caught :as caught]
            [jobs.lib.cost.table :as table]
            [jobs.lib.cost.value :as value]))

(defn steps
  "The warm-up, one fn per table or posterior."
  []
  (concat
   ;; a worth table is three passes (table/table-for): a pass per step, so none holds the loop long
   (for [find (value/tree-finds) passes (range 1 4)]
     #(binding [table/*passes* passes] (value/tree-each @game/version "stone" find)))
   [#(value/measured-worth @game/version)]
   (for [passes (range 1 4)] #(binding [table/*passes* passes] (value/item-value [{:name "stone"}])))
   (for [passes (range 1 4)]
     #(binding [table/*passes* passes]
        (value/item-value [{:name "stone"}] :overrides (value/measured-worths @game/version) :basis (:goal-basis (value/measured-worth @game/version)))))
   [blocks/default-state-table]
   (for [which [::caught/flights ::caught/fights]
         [group counts] (settings/get caught/settings which)
         :when (not= [::caught/flights :creeper] [which group])]
     #(caught/posterior counts))))

(defn warm!
  "Build the item worth tables of the body's game version (as item-value and find-worth ask them), the planner's block table and the
  posterior of every counted flight and fight group (caught/prior: the first masked plan's unseen-share prices spawns with them).
  One step per event-loop turn, so the body's timers run between them; a promise of nil, rejected with a step's error."
  []
  (reduce (fn [p step]
            (.then p #(js/Promise. (fn [ok fail] (js/setImmediate (fn [] (try (step) (ok nil) (catch :default e (fail e)))))))))
          (js/Promise.resolve nil)
          (steps)))
