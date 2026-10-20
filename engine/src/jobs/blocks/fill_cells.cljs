(ns jobs.blocks.fill-cells
  (:require [engine.args :as a]
            [jobs.lib.block-sets :as bs]
            [engine.settings :as settings]
            [jobs.lib.ready :as ready]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.choose :as choose]
            [jobs.lib.child :as child]
            [jobs.lib.look :as sight :refer [cell-of]]
            [jobs.lib.pillar :as pl]
            [jobs.lib.pace :as pace]
            [jobs.lib.checks :as checks]
            [jobs.lib.found :as found]
            [jobs.lib.need :as need]
            [engine.contract :as contract]
            [jobs.lib.util :as u]))

(def doc
  "The one job that makes cells solid. Fill cells that are air, cave_air, water or replaceable with the first carried block
  of the call's :use {:blocks} (default: the carried building blocks, in that order). Per step the next cell is the first of :order
  (:lowest, default: the lowest cell that is not the body's feet or head, then the nearest; :given: the order of :cells,
  still not the feet or head while another is left). It is placed by a jobs.blocks.place child (:ways #{:carried}; zones, claims
  and plans are its rules unless :ignore-zones? is set; it walks into reach itself). When only the feet or head cell is left,
  the body jump-places (a bare act, not access.pillar: no scaffold ledger entry). Cells that no longer read air or water are done.
  With :clear-loose, loose things in the cells (rails, cobwebs, torches, fences, ladders ...) are dug first
  (jobs.blocks.dig, no fetch) and the drops picked up; a container in a cell is never dug (ends :blocked). It yields :continue
  only when a child waits on the world.

  A seen mob standing in the next cell is waited out a second at a time (::mob-waits), not counted a failed step.

  Needs the blocks carried. With none carried, the call's :ways minus :carried (default #{:carried}: none; jobs.lib.need) fetch them
  before the next cell; a failed fetch is the ending's :because. Condition, at the start only: {:reason :need :any-of [names]}
  when cells are owed, none of the blocks is carried and the ways fetch none.

  Ends with data {:mended n :left [cells still unfilled]}. Done: every cell filled and the drops picked up (also at once, with
  0 mended, when none was owed), or :mended >= the call's :least (it stops there) (default :all: a partial fill is not done). Short of that:
  condition-failed :need when no block is carried any more, :refused (a zone, claim or plan; data :cell) or :blocked (a
  container, :clear-loose only); nothing-possible :failed after the max-failures setting of failed steps in a
  row (a refused place, a walk that is blocked and a refused jump-place each count one, and only a place clears the
  count).")

(a/defargs args
  {:cells {:doc "the cells to fill, each [x y z] or {:x :y :z}" :spec (a/coll-of a/position?) :default nil}
   :order {:doc "which cell goes next: :lowest (lowest first, then nearest) or :given (the order of :cells)" :spec #{:lowest :given} :default :lowest}
   :clear-loose {:doc "dig loose things (rails, torches, cobwebs ...) out of the cells first and pick up the drops" :spec boolean? :default false}
   :ignore-zones? :shared})

(a/defargs settings
  {::max-failures {:default 6 :doc "Failed steps in a row before the job ends nothing-possible." :spec (a/int-in 1 nil)}
   ::mob-waits {:default 10 :doc "Seconds a mob standing in the next cell is waited out before the cell counts as a failed step." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def fit? ready/ok)

(def target [:cells])

(def ways need/fetch-ways)

(def defaults {:use {:blocks :build} :least :all :ways #{:carried} :spend {:job-time ::job-time-s}})

(def loose #"(^|_)(rail|torch|fence|fence_gate|ladder|lantern|button|lever|carpet|sign|banner|pressure_plate)$|^cobweb$")

(defn kind
  "What a block name means for a fill: :open (air, water or replaceable), :container (never touched), :loose (dug first with
  :clear-loose) or :solid (done)."
  [n]
  (cond
    (or (bs/air n) (= "water" n) (bs/replaceable n)) :open
    (found/containers n) :container
    (and n (re-find loose n)) :loose
    :else :solid))

(defn wanted
  "The block names the call fills with: its :use {:blocks}, else (before the call starts) the carried building blocks."
  [c]
  (vec (or (ctx/use c :blocks)
           (get-in (:args c) [:use :blocks])
           (choose/default-use c :blocks (get-in defaults [:use :blocks])))))

(defn cells-of [c]
  (mapv #(cell-of (a/cell %)) (:cells (:args c))))

(defn kinds-of
  "[cell kind] for the cells that are not solid, in :cells order (an unloaded cell is not owed). Loose things and containers
  count only with :clear-loose."
  [c]
  (let [clear? (:clear-loose (:args c))]
    (into []
          (comp (keep (fn [cell]
                        (when-let [n (u/seen-name (:primitives c) cell)]
                          [cell (kind n)])))
                (filter (fn [[_ k]] (or (= :open k) (and clear? (#{:loose :container} k))))))
          (cells-of c))))

(defn owed
  "The cells still to fill (and, with :clear-loose, to clear)."
  [c]
  (mapv first (kinds-of c)))

(defn next-cell
  "The cell to fill next: not the body's feet or head; by :order, lowest y first then nearest, or as given."
  [c cells]
  (let [feet (cell-of (u/self-pos c))
        head (update feet :y inc)
        here (u/self-pos c)
        open (remove #(or (= % feet) (= % head)) cells)]
    (first (if (= :given (:order (:args c)))
             open
             (sort-by (juxt :y #(u/dist here %)) open)))))

(def safe? (ready/safe-at-by-night (ready/near-unseen-here (fn [c] (or (next-cell c (owed c)) (a/cell (first (:cells (:args c)))))))))

(defn bump! [c k] (ctx/update-mem! c update k (fnil inc 0)))

(defn ^:async place!
  "One cell: resolves to :placed, :failed, :refused, :need, or :partial (the place child waits on the world)."
  [c pos item]
  (let [o (await (blocks/place-cell! c pos item (select-keys (:args c) [:ignore-zones?])))]
    (case o
      :continue :partial
      (:placed :already) :placed
      (:refused :need) o
      :failed)))

(defn ^:async dig!
  "One loose cell: :dug (drops to collect), :partial, :refused or :failed."
  [c pos]
  (let [o (await (blocks/dig-cell! c pos (select-keys (:args c) [:ignore-zones?])))]
    (case o
      :continue :partial
      (:dug :missing) (do (ctx/update-mem! c assoc :collect true) :dug)
      :refused :refused
      :failed)))

(defn ^:async collect!
  [c]
  (let [r (await (ctx/call c :collect 'jobs.items.collect-drops {:radius 3}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :collect) :again))))

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
       (when (and (seq (owed c)) (empty? (need/fetch-how c)) (nil? (blocks/pick c names)))
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
    (checks/goal (and (empty? left) (not (:collect (ctx/mem c)))) [:filled "no cell is owed"] [:owed (str (count left) " cells are owed")]
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

(defn ^:async fetch!
  "The blocks fetched from the call's ways: :continue while it works, :again once got, else the :need ending (the fetch's as :because)."
  [c names n]
  (let [e (await (need/blocks! c names n {:how (need/fetch-how c)}))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) :again
          :else (contract/with-because (finish! c :condition-failed {:reason :need :any-of names}) e))))

(defn mob-in
  "A seen mob whose box overlaps cell (the server refuses a block there), or nil."
  [c cell]
  (pl/mob-in (sight/seen-entities (:primitives c) {:radius 4}) [(:x cell) (:y cell) (:z cell)]))

(defn ^:async wait-mob!
  "A mob stands in the cell: wait a second for it to move on (:continue when the pause is cut, else :again)."
  [c]
  (ctx/update-mem! c update :mob-waits (fnil inc 0))
  (let [r (await (child/pause! c 1 :mob-in-cell))]
    (if (= :continue r) :continue :again)))

(defn ^:async step!
  "One cell: :again after it, :continue when a child waits, or the ending."
  [c]
  (let [{:keys [failures collect mended]} (ctx/mem c)
        least (:least (ctx/frozen-call c) :all)
        names (wanted c)
        kinds (kinds-of c)
        cells (mapv first kinds)
        container (first (filter #(= :container (second %)) kinds))
        item (when (seq cells) (blocks/pick c names))
        pos (next-cell c cells)
        loose? (and pos (= :loose (some (fn [[cell k]] (when (= cell pos) k)) kinds)))]
    (cond
      collect (await (collect! c))
      (and (number? least) (pos? (or mended 0)) (>= mended least)) (finish! c nil nil)
      (empty? cells) (finish! c nil nil)
      container (finish! c :condition-failed {:reason :blocked :cell (first container)})
      (>= (or failures 0) (settings/get settings ::max-failures)) (finish! c :nothing-possible {:reason :failed})
      (and (nil? item) (seq (need/fetch-how c))) (await (fetch! c names (count cells)))
      (nil? item) (finish! c :condition-failed {:reason :need :any-of names})
      (and pos (not loose?) (< (:mob-waits (ctx/mem c) 0) (settings/get settings ::mob-waits)) (mob-in c pos)) (await (wait-mob! c))
      :else
      (let [r (cond loose? (await (dig! c pos))
                    pos (await (place! c pos item))
                    :else (await (raise! c item)))]
        (case r
          :partial :continue
          :refused (finish! c :condition-failed {:reason :refused :cell pos})
          :need (finish! c :condition-failed {:reason :need :any-of names})
          :dug :again
          :placed (do (ctx/update-mem! c #(cond-> (update % :mended (fnil inc 0)) pos (assoc :failures 0 :mob-waits 0))) :again)
          (do (bump! c :failures) :again))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))
