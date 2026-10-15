(ns jobs.survival.dig-in.walls
  "dig-in's walls and plug modes: placements round the body, or one block in the roof of a closed room."
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter :as sh]
            [jobs.lib.seal :as seal]
            [jobs.lib.blocks :as lb]
            [jobs.lib.tidy :as tidy]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-in.mode :as mode]
            [jobs.survival.dig-in.site :as site]))

(defn cells-to-try
  "The open cells still worth a placement: not occupied, not refused site/max-refusals times, the least refused first (a cell
  the server refused is retried after the others, which may give it a support)."
  [c open]
  (let [{:keys [occupied refused]} (ctx/mem c)
        tries #(get refused % 0)]
    (->> open
         (remove (or occupied #{}))
         (remove #(>= (tries %) site/max-refusals))
         (sort-by tries))))

(defn open-text [cells]
  (str "open cells " (pr-str (mapv (juxt :x :y :z) cells))))

(defn ^:async walls-round
  "One round of walls mode. Out of blocks (the carried ones were used up or lost, or the body moved to a cell with more
  open cells than blocks) it forgets the mode so the next round chooses again: a pit dug with what it can harvest."
  [c]
  (let [{:keys [max-places roof-height]} (:args c)
        blocks (mode/blocks c)
        p (:primitives c)
        open (dig-cells/open-cells p (sh/feet p))
        cells (cells-to-try c open)
        _ (when (and (seq cells) (seal/off-centre? p)) (await (seal/centre! c)))
        status (await (dig-cells/place-all! c blocks (take max-places (dig-cells/bottom-up p cells (sh/feet p)))))]
    (cond
      (empty? cells) (do (when-not (sh/roofed? p roof-height) (site/remember-failed-site! c :walls-refused))
                         :done)
      (= :wait status) :yield
      (= "no-item" status) (do (ctx/update-mem! c dissoc :mode :start) :continue)
      (not= :ok status) (site/fail-site! c (if (= "failed" status) :walls-server-refused :walls-failed) (str "cannot place a block: " status (:mob-text (ctx/mem c)) "; " (site/open-text open)))
      (dig-cells/sealed-in? p :walls roof-height) :done
      :else :continue)))

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
