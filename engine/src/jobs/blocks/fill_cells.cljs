(ns jobs.blocks.fill-cells
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.ready :as ready]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.choose :as choose]
            [jobs.lib.look :refer [cell-of]]
            [jobs.lib.pace :as pace]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]))

(def doc
  "Fill cells that are air, cave_air or water with the first carried block of the call's :use {:blocks} (default: the
  carried building blocks, in that order). The caller has judged the cells (zones, claims); the job does not ask the rules
  again. Per step: the lowest cell that is not the body's feet or head (then the nearest) is placed by a jobs.blocks.place
  child (:fetch false, :ignore-zones? true; it walks into reach itself); when only the feet or head cell is left, the body
  jump-places (a bare act, not access.pillar: the block is meant to stay, so no scaffold ledger entry, and the pillar's ceiling and zone checks do not apply). Cells that no longer read air or water are done. It yields :continue only
  when a child waits on the world.

  Needs the blocks carried; it fetches none. Condition, at the start only: {:reason :need :any-of [names]} when cells are
  owed and none of the blocks is carried.

  Ends with data {:mended n :left [cells still unfilled]}. Done: every cell filled (also at once, with 0 mended, when none
  was owed), or :mended >= the call's :least (default :all: a partial fill is not done). Short of that:
  condition-failed :need when no block is carried any more; nothing-possible :failed after the max-failures setting of failed steps in a
  row (a refused place, a walk that is blocked and a refused jump-place each count one, and only a place clears the
  count).")

(a/defargs args
  {:cells {:doc "the cells to fill, each [x y z] or {:x :y :z}" :spec (a/coll-of a/position?) :default nil}})

(a/defargs settings
  {::max-failures {:default 6 :doc "Failed steps in a row before the job ends nothing-possible." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def fit? ready/ok)

(def target [:cells])

(def defaults {:use {:blocks :build} :least :all :spend {:job-time ::job-time-s}})

(defn wanted
  "The block names the call fills with: its :use {:blocks}, else (before the call starts) the carried building blocks."
  [c]
  (vec (or (ctx/use c :blocks)
           (get-in (:args c) [:use :blocks])
           (choose/default-use c :blocks (get-in defaults [:use :blocks])))))

(defn cells-of [c]
  (mapv #(cell-of (a/cell %)) (:cells (:args c))))

(defn owed
  "The cells that still read air, cave_air or water (an unloaded cell is not owed)."
  [c]
  (filterv #(let [n (u/seen-name (:primitives c) %)]
              (contains? #{"air" "cave_air" "water"} n))
           (cells-of c)))

(defn next-cell
  "The cell to fill next: not the body's feet or head, lowest y first, then nearest."
  [c cells]
  (let [feet (cell-of (u/self-pos c))
        head (update feet :y inc)
        here (u/self-pos c)]
    (->> cells
         (remove #(or (= % feet) (= % head)))
         (sort-by (juxt :y #(u/dist here %)))
         first)))

(def safe? (ready/safe-at-by-night (ready/near-unseen-here (fn [c] (or (next-cell c (owed c)) (a/cell (first (:cells (:args c)))))))))

(defn bump! [c k] (ctx/update-mem! c update k (fnil inc 0)))

(defn ^:async place!
  "One cell: resolves to :placed, :failed, or :partial (the place child waits on the world)."
  [c pos item]
  (let [e (await (ctx/call c :place 'jobs.blocks.place {:pos pos :item item :fetch false :ignore-zones? true}))]
    (cond (= :continue e) :partial
          (= :done (:kind e)) :placed
          :else :failed)))

(defn ^:async raise!
  [c item]
  (if (contains? #{"done" "partial"} (.-status (await (ctx/act c :jumpPlace (clj->js {:item item :count 1})))))
    :placed
    :failed))


(def need
  "Later :need while cells are owed and none of the blocks is carried."
  (checks/refusing
   :carried "a block is carried"
   (fn [c]
     (let [names (wanted c)]
       (when (and (seq (owed c)) (nil? (blocks/pick c names)))
         {:reason :need :any-of names})))))

(def can-start? need)

(defn enough?
  "Whether no cell is left or the mended cells reach the call's :least."
  [c]
  (let [least (:least (ctx/frozen-call c) :all)]
    (or (empty? (owed c)) (and (number? least) (>= (:mended (ctx/mem c) 0) least)))))

(defn done?
  "Ok when no cell is owed; else later, with the cells mended and the cells left."
  [c]
  (let [left (owed c)
        mended (:mended (ctx/mem c) 0)]
    (checks/goal (empty? left) [:filled "no cell is owed"] [:owed (str (count left) " cells are owed")]
                 {:units :cells :done mended :left (count left) :result {:mended mended :left left}})))

(defn quote-of
  [c]
  (let [n (count (owed c))
        t (* 3 n)]
    {:uses {:job-time [0 t (* 3 t)]}
     :done [0 n n]}))

(defn finish!
  "The ending: done when enough is mended, else kind and cause."
  [c kind cause]
  (let [{:keys [mended]} (ctx/mem c)
        data {:mended (or mended 0) :left (owed c)}]
    (if (enough? c)
      (ctx/end c :done nil data)
      (ctx/end c kind cause data))))

(defn ^:async step!
  "One cell: :again after it, :continue when the place child waits, or the ending."
  [c]
  (let [{:keys [failures]} (ctx/mem c)
        names (wanted c)
        cells (owed c)
        item (when (seq cells) (blocks/pick c names))
        pos (next-cell c cells)]
    (cond
      (empty? cells) (finish! c nil nil)
      (>= (or failures 0) (settings/get settings ::max-failures)) (finish! c :nothing-possible {:reason :failed})
      (nil? item) (finish! c :condition-failed {:reason :need :any-of names})
      :else
      (let [r (if pos (await (place! c pos item)) (await (raise! c item)))]
        (case r
          :partial :continue
          :placed (do (ctx/update-mem! c #(cond-> (update % :mended (fnil inc 0)) pos (assoc :failures 0))) :again)
          (do (bump! c :failures) :again))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))
