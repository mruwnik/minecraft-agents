(ns jobs.survival.dig-in.walls
  "dig-in's walls and plug modes: the body walled in by jobs.survival.plug-in, or one block in the roof of a closed room."
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.blocks :as lb]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-in.mode :as mode]
            [jobs.survival.dig-in.site :as site]))

(defn note-placed!
  "Job memory: the blocks a plug-in ending (its data's :placed) placed, as the :placed cells, their items and the spent counts."
  [c data]
  (doseq [{:keys [cell item]} (:placed data)
          :let [cell (u/cell-pos cell)]]
    (ctx/update-mem! c #(-> % (update :placed (fnil conj #{}) cell) (assoc-in [:placed-items cell] item) (update-in [:spent item] (fnil inc 0))))))

(defn ^:async walls-round
  "One attempt of walls mode: jobs.survival.plug-in with the walls way and the shelter's blocks. Out of blocks (the carried ones
  were used up or lost, or the body moved to a cell with more open cells than blocks) it forgets the mode so the next round
  chooses again: a pit dug with what it can harvest."
  [c]
  (let [{:keys [max-places roof-height]} (:args c)
        e (await (ctx/call c :plug 'jobs.survival.plug-in
                           {:ways #{:walls} :purpose :shelter :roof-height roof-height :max-places max-places :use {:blocks (mode/blocks c)}}))]
    (if (= :continue e)
      :yield
      (do (note-placed! c (:data e))
          (cond
            (= :done (:kind e)) :done
            (nil? (lb/pick c (mode/blocks c))) (do (ctx/update-mem! c dissoc :mode :start) :continue)
            :else (do (site/remember-failed-site! c (if (= "failed" (get-in e [:data :statuses :walls])) :walls-server-refused :walls-failed))
                      (ctx/emit! c :dig_in_failed :warn {:text (str "cannot wall the body in: " (:text (:data e)))
                                                         :open (dig-cells/open-cells (:primitives c) (sh-sense/feet (:primitives c)))})
                      :done))))))

(defn ^:async plug-round
  "In a closed room: place one block in the hole of the roof over the body (:plug)."
  [c]
  (let [blocks (mode/blocks c)
        item (lb/pick c blocks)
        plug (:plug (ctx/mem c))]
    (if (nil? item)
      (site/fail-site! c :plug-failed "cannot mend the roof: no-item")
      (let [r (await (tidy/place! c plug item true))]
        (cond
          (#{"placed" "occupied"} (.-status r))
          (do (when (= "placed" (.-status r)) (dig-cells/note-placed! c plug item))
              :done)
          (dig-cells/keep-waiting! c :mob-since r) :yield
          :else (site/fail-site! c :plug-failed (str "cannot mend the roof: " (.-status r) (dig-cells/mob-text r))))))))
