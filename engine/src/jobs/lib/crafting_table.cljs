(ns jobs.lib.crafting-table
  "Finding a crafting table the body has seen; shared by craft and obtain's plan."
  (:require [jobs.lib.look :as look]))

(defn cell-vec [pos] (mapv #(js/Math.floor %) [(:x pos) (:y pos) (:z pos)]))

(defn nearest
  "The position of the nearest crafting table the body has seen within radius (not one in skip, a set of positions), or nil."
  ([p radius] (nearest p radius nil))
  ([p radius skip]
   (->> (look/seen-blocks p {:names ["crafting_table"] :radius radius :max 8 :live? true})
        (map :pos)
        (remove #(contains? skip (cell-vec %)))
        first)))
