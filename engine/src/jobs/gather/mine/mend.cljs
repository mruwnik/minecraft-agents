(ns jobs.gather.mine.mend
  "The mend of jobs.gather.mine: the ground cells under the start that the dig left open, filled by a blocks.fill-cells child."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.choose :as choose]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]))

(defn owed
  "The ground entries whose cell is now air, cave_air or water (unloaded cells are not owed). After a descent the
  cells below the start level are not owed: they are the stair the body walks home by."
  [c]
  (let [{:keys [descent start]} (ctx/mem c)
        way-y (when (pos? (:steps descent 0)) (js/Math.floor (:y start)))]
    (filterv #(let [name (u/seen-name (:primitives c) (:pos %))]
                (and name (or (bs/air name) (= "water" name))
                     (or (nil? way-y) (>= (:y (:pos %)) way-y))))
             (:ground (ctx/mem c)))))

(defn fillers
  "The blocks to fill with, in order: dig-in's building blocks other than the mined item, then the item itself
  when it is a block (the mend must not eat the count)."
  [c]
  (let [{:keys [block] :as a} (:args c)
        item (blocks/item-name a)
        builds (choose/filler-names c :build)
        own (when (or (some #{item} builds) (= item block)) [item])]
    (vec (concat (remove #{item} builds) own))))

(defn mend-permitted
  "The owed cells ({:pos ..}) the job may place on (zones and claims)."
  [c cells]
  (let [ok (set (:allowed (gate/look c :place (map :pos cells) {})))]
    (filterv #(ok (:pos %)) cells)))

(defn cells-text [poss] (str/join " " (map #(str (:x %) "," (:y %) "," (:z %)) poss)))

(defn ^:async mend-round!
  "The permitted owed cells are filled by a blocks.fill-cells child. :continue while it works, :again after a fill (the cells are
  looked at again), nil when nothing is owed or the mend gave up (warn mine.mend-short / mine.mend-failed)."
  [c]
  (let [cells (mend-permitted c (owed c))]
    (if (empty? cells)
      nil
      (let [e (await (ctx/call c :fill 'jobs.blocks.fill-cells {:cells (mapv :pos cells) :use {:blocks (fillers c)} :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
            {:keys [mended left]} (when (map? e) (:data e))
            left (or left (map :pos cells))]
        (when (map? e)
          (ctx/update-mem! c update :mended (fnil + 0) (or mended 0)))
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) :again
          (and (= :condition-failed (:kind e)) (= :need (get-in e [:cause :reason])))
          (do (ctx/emit! c :mine.mend-short :warn {:owed (vec left) :text (str "mine has nothing to mend with: " (cells-text left))})
              nil)
          :else
          (do (ctx/emit! c :mine.mend-failed :warn {:owed (vec left) :text (str "mine could not mend: " (cells-text left))})
              nil))))))
