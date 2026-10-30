(ns jobs.lib.crafting-table
  "Finding a crafting table the body has seen; shared by craft and obtain's plan."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.look :as look]))

(a/defargs settings
  {::radius {:default 32 :doc "Blocks within which a crafting table counts as at hand when a craft plan is made." :spec (a/int-in 0 nil)}})

(defn cell-vec [pos] (mapv #(js/Math.floor %) [(:x pos) (:y pos) (:z pos)]))

(defn nearest
  "The position of the nearest crafting table the body has seen within radius (not one in skip, a set of positions), or nil."
  ([p radius] (nearest p radius nil))
  ([p radius skip]
   (->> (look/seen-blocks p {:names ["crafting_table"] :radius radius :max 8 :live? true})
        (map :pos)
        (remove #(contains? skip (cell-vec %)))
        first)))

(defn at-hand?
  "Whether a craft plan can count on a table: the one named in mem :table, else one seen within ::radius."
  [p mem]
  (boolean (or (:table mem) (nearest p (settings/get settings ::radius)))))
