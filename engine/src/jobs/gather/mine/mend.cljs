(ns jobs.gather.mine.mend
  "The mend of jobs.gather.mine: the ground cells under the start that the run left open, filled by a blocks.fill-cells
  child, and a dig phase resumed when the mend spent the count."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine.end :refer [wrap-up!]]
            [jobs.gather.mine.targets :refer [air carried scan off-ground?]]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.choose :as choose]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :refer [cell-of look-around!]]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::mend-reach {:default 4 :doc "Reach while mending a cut, in blocks." :spec (a/int-in 0 nil)}
   ::max-mend-failures {:default 6 :doc "Failed mends before the job gives up." :spec (a/int-in 1 nil)}
   ::max-resumes {:default 2 :doc "Resumes of a cut before the job gives up." :spec (a/int-in 0 nil)}})

(defn mend-reach [] (settings/get settings ::mend-reach))

(defn max-mend-failures [] (settings/get settings ::max-mend-failures))

(defn owed
  "The ground entries whose cell is now air, cave_air or water (unloaded cells are not owed). After a descent the
  cells below the start level are not owed: they are the stair the body walks home by."
  [c]
  (let [{:keys [descent start]} (ctx/mem c)
        way-y (when (pos? (:steps descent 0)) (js/Math.floor (:y start)))]
    (filterv #(let [name (u/seen-name (:primitives c) (:pos %))]
                (and name (or (air name) (= "water" name))
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
  "The owed cells ({:pos ..}) the job may place on (zones and claims; one mine.declined warn per job when refused)."
  [c cells]
  (let [ok (set (gate/allowed c :mine.declined "mine" :place (map :pos cells)))]
    (filterv #(ok (:pos %)) cells)))

(defn cells-text [poss] (str/join " " (map #(str (:x %) "," (:y %) "," (:z %)) poss)))

(defn spent-on-mend?
  "Whether the mend ate into what the dig brought, leaving fewer than the goal carried."
  [c]
  (let [{:keys [goal at-mend]} (ctx/mem c)
        now (carried c)
        start (- goal (:count (:args c)))]
    (and (< now goal) (< now at-mend) (> at-mend start))))

(defn max-resumes [] (settings/get settings ::max-resumes))

(defn ^:async mended!
  "Nothing is owed. The mend spent the count: look around once from here, then dig on while targets off the ground
  remain (at most max-resumes times). Else finish."
  [c]
  (let [{:keys [resumes reason looked]} (ctx/mem c)
        more? (some #(off-ground? c %) (:targets (scan c)))]
    (cond
      (not (spent-on-mend? c)) (wrap-up! c)
      (not= looked (cell-of (u/self-pos c))) (do (await (look-around! c)) :again)
      (and more? (< (or resumes 0) (max-resumes)))
      (do (ctx/update-mem! c #(-> % (assoc :phase :dig :failures 0 :dry 0 :last-carried (carried c))
                                  (dissoc :collecting)
                                  (update :resumes (fnil inc 0))))
          :again)
      :else (do (ctx/update-mem! c assoc :reason :spent-on-mend :dig-reason reason)
                (wrap-up! c)))))

(defn ^:async mend-round!
  "The permitted owed cells are filled by a blocks.fill-cells child; its end says whether the mend is done, short of
  blocks or failed."
  [c]
  (let [cells (mend-permitted c (owed c))]
    (if (empty? cells)
      (await (mended! c))
      (let [e (await (ctx/call c :fill 'jobs.blocks.fill-cells {:cells (mapv :pos cells) :use {:blocks (fillers c)}
                                                              :reach (mend-reach) :max-failures (max-mend-failures)}))
            {:keys [mended left]} (when (map? e) (:data e))
            left (or left (map :pos cells))]
        (when (map? e)
          (ctx/update-mem! c update :mended (fnil + 0) (or mended 0)))
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) :again
          (and (= :condition-failed (:kind e)) (= :need (get-in e [:cause :reason])))
          (do (ctx/emit! c :mine.mend-short :warn {:owed (vec left) :text (str "mine has nothing to mend with: " (cells-text left))})
              (wrap-up! c))
          :else
          (do (ctx/emit! c :mine.mend-failed :warn {:owed (vec left) :text (str "mine could not mend: " (cells-text left))})
              (wrap-up! c)))))))
