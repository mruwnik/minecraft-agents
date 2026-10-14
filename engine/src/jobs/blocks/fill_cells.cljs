(ns jobs.blocks.fill-cells
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.look :refer [cell-of]]
            [jobs.lib.near :as near]
            [jobs.lib.util :as u]))

(def doc
  "Fill cells that are air, cave_air or water with the first carried block of :any-of (in that order). The caller
  has judged the cells (zones, claims); the job does not ask the rules again. Per step: the lowest cell that is not the
  body's feet or head (then the nearest) is walked to within :reach (one go-near child) and placed; when only the feet
  or head cell is left, the body jump-places. Cells that no longer read air or water are done. A run of more than
  :max-steps steps yields :continue and resumes.

  Needs nothing: a step with no block of :any-of carried ends :short. Six failed steps in a row end :failed (:max-failures);
  a refused place, a walk that is blocked and a refused jump-place each count one, and only a place clears the count.

  Ends :done with {:mended n :left [cells still unfilled] :why nil|:short|:failed}: :why nil when every cell is filled.")

(a/defargs args
  {:cells {:doc "the cells to fill, each [x y z] or {:x :y :z}" :spec (a/coll-of a/position?) :default nil}
   :any-of {:doc "block items to fill with; the first carried one is used" :spec (a/coll-of a/item?) :default nil}
   :reach {:doc "walk within this many blocks of a cell before placing" :spec (a/int-in 0 nil) :default 4}
   :max-failures {:doc "failed steps in a row before the job ends :failed" :spec (a/int-in 1 nil) :default 6}})

(a/defargs settings
  {::max-steps {:default 40 :doc "Steps of one call before it gives the round back with :continue." :spec (a/int-in 1 nil)}})
(defn max-steps [] (settings/get settings ::max-steps))

(defn check [_c] true)

(defn cells-of [c]
  (mapv #(cell-of (a/cell %)) (:cells (:args c))))

(defn owed
  "The cells that still read air, cave_air or water (an unloaded cell is not owed)."
  [c]
  (filterv #(let [n (u/seen-name (:primitives c) %)]
              (contains? #{"air" "cave_air" "water"} n))
           (cells-of c)))

(defn target
  "The cell to fill next: not the body's feet or head, lowest y first, then nearest."
  [c cells]
  (let [feet (cell-of (u/self-pos c))
        head (update feet :y inc)
        here (u/self-pos c)]
    (->> cells
         (remove #(or (= % feet) (= % head)))
         (sort-by (juxt :y #(u/dist here %)))
         first)))

(defn bump! [c k] (ctx/update-mem! c update k (fnil inc 0)))

(defn ^:async place!
  "One cell: resolves to :placed, :failed, or :partial (the walk waits on the world)."
  [c pos item]
  (let [walked (if (> (u/dist (u/self-pos c) pos) (:reach (:args c)))
                 (await (near/go-near! c pos (:reach (:args c)) {:zone-tolls true}))
                 :there)]
    (cond
      (= :partial walked) :partial
      (not= :there walked) :failed
      :else (if (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos pos :item item}))))) :placed :failed))))

(defn ^:async raise!
  [c item]
  (if (contains? #{"done" "partial"} (.-status (await (ctx/act c :jumpPlace (clj->js {:item item :count 1})))))
    :placed
    :failed))

(defn finish! [c why]
  (let [{:keys [mended]} (ctx/mem c)
        left (owed c)]
    (ctx/result! c {:mended (or mended 0) :left left :why why})
    :done))

(defn ^:async round [c]
  (loop [steps 0]
    (let [{:keys [failures]} (ctx/mem c)
          cells (owed c)
          item (when (seq cells) (blocks/pick c (:any-of (:args c))))
          pos (target c cells)]
      (cond
        (empty? cells) (finish! c nil)
        (>= (or failures 0) (:max-failures (:args c))) (finish! c :failed)
        (nil? item) (finish! c :short)
        (or (>= steps (max-steps)) (not (ctx/alive? c))) :continue
        :else
        (let [r (if pos (await (place! c pos item)) (await (raise! c item)))]
          (case r
            :partial :continue
            :placed (do (ctx/update-mem! c #(cond-> (update % :mended (fnil inc 0)) pos (assoc :failures 0))) (recur (inc steps)))
            (do (bump! c :failures) (recur (inc steps)))))))))
