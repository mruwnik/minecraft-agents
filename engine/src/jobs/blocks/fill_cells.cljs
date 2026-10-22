(ns jobs.blocks.fill-cells
  (:require [clojure.string :as string]
            [engine.args :as a]
            [jobs.lib.waits :as waits]
            [jobs.lib.block-sets :as bs]
            [engine.settings :as settings]
            [jobs.lib.ready :as ready]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.choose :as choose]
            [jobs.lib.child :as child]
            [jobs.lib.dig-look :as look]
            [jobs.lib.look :as sight :refer [cell-of]]
            [jobs.lib.pillar :as pl]
            [jobs.lib.pace :as pace]
            [jobs.lib.checks :as checks]
            [jobs.lib.found :as found]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [engine.contract :as contract]
            [jobs.lib.seal :as seal]
            [jobs.lib.util :as u]))

(def doc
  "The one job that makes cells solid. Fill cells that are air, cave_air, water or replaceable with the first carried block
  of the call's :use {:blocks} (default: the carried building blocks, in that order). Per step the next cell is the first of :order
  (:lowest, default: the lowest cell that is not the body's feet or head, then the nearest; :given: the order of :cells,
  still not the feet or head while another is left). It is placed by a jobs.blocks.place child (:ways #{:carried}; zones, claims
  and plans are its rules unless :ignore-zones? is set; it walks into reach itself). When only the feet or head cell is left,
  the body jump-places (a bare act, not access.pillar: no scaffold ledger entry). Cells that no longer read air or water are done.
  With :clear-loose, loose things in the cells (rails, cobwebs, torches, fences, ladders ...) are dug first
  (jobs.blocks.dig, no fetch) and the drops picked up; a container in a cell is never dug (ends :blocked). With :shut-open, a door,
  gate or trapdoor standing open in a cell is shut with a click instead (one that stays open, an iron door, ends :blocked).
  With :in-place the body works from where it stands, without aiming or picking up drops (a shelter: it never moves). With :danger? the blocks may spend soft reserves (never hard ones), only at cells where breaking the block out later releases
  no fluid and drops no sand or gravel column; a block so borrowed is marked :borrowed in its ledger entry. With :purpose each placed block
  goes to the scaffold ledger under it (jobs.access.cleanup takes the plug purposes back). A cell the place refused or that had
  nothing to place against goes behind the others, and is given up after ::max-refusals failed steps. A mob standing in the next
  cell is waited out a second at a time (::mob-waits) when seen (a hostile is not waited on: the call ends :hostile-in-cell), and for jobs.lib.seal/mob-wait-ticks when only a refused place names it, not counted a failed step. A cell a
  place answers occupied for (a block that stays) is left alone, not owed. It yields :continue only when a child waits
  on the world.

  Needs the blocks carried. With none carried, the call's :ways minus :carried (default #{:carried}: none; jobs.lib.need) fetch them
  before the next cell; a failed fetch is the ending's :because. Condition, at the start only: {:reason :need :any-of [names]}
  when cells are owed, none of the blocks is carried and the ways fetch none.

  Ends with data {:mended n :left [cells still unfilled] :placed [{:cell [x y z] :item name}]}, and the last refused place's
  :status and :text after a failure. Done: every cell filled and the drops picked up (also at once, with
  0 mended, when none was owed), or :mended >= the call's :least (it stops there) (default :all: a partial fill is not done). Short of that:
  condition-failed :need when no block is carried any more, :refused (a zone, claim or plan; data :cell) or :blocked (a
  container, :clear-loose only); nothing-possible :failed-steps after the max-failures setting of failed steps in a
  row (a refused place, a walk that is blocked and a refused jump-place each count one, and only a place clears the
  count), :hostile-in-cell {:cell} (a hostile stands in the next cell) or :mob-in-cell {:cell :mobs} (a mob stayed past the wait).")

(a/defargs args
  {:cells {:doc "the cells to fill, each [x y z] or {:x :y :z}" :spec (a/coll-of a/position?) :default nil}
   :order {:doc "which cell goes next: :lowest (lowest first, then nearest) or :given (the order of :cells)" :spec #{:lowest :given} :default :lowest}
   :clear-loose {:doc "dig loose things (rails, torches, cobwebs ...) out of the cells first and pick up the drops" :spec boolean? :default false}
   :shut-open {:doc "shut a door, gate or trapdoor standing open in a cell with a click (it counts as unfilled until shut)" :spec boolean? :default false}
   :in-place {:doc "work from where the body stands: place and dig without aiming at the cell, pick no drops up (a shelter does not move)" :spec boolean? :default false}
   :danger? {:doc "the body places in danger: it may spend soft-reserved blocks (not hard ones) it can dig back out, at cells where taking the block out later is safe (jobs.lib.blocks/safe-to-break?)" :spec boolean? :default false}
   :purpose {:doc "scaffold ledger purpose of each placed block (jobs.access.cleanup); nil: not ledgered" :spec keyword? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::max-refusals {:default 2 :doc "Failed steps at one cell before it is given up; the call fails once only such cells are left." :spec (a/int-in 1 nil)}
   ::max-failures {:default 6 :doc "Failed steps in a row before the job ends nothing-possible." :spec (a/int-in 1 nil)}
   ::mob-waits {:default 10 :doc "Seconds a mob standing in the next cell is waited out before the cell counts as a failed step." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def fit? ready/ok)

(def target [:cells])

(def ways need/fetch-ways)

(def defaults {:use {:blocks :build} :least :all :ways #{:carried} :spend {:job-time ::job-time-s}})

(defn wanted
  "The block names the call fills with: its :use {:blocks}, else (before the call starts) the carried building blocks."
  [c]
  (vec (or (ctx/use c :blocks)
           (get-in (:args c) [:use :blocks])
           (choose/default-use c :blocks (get-in defaults [:use :blocks])))))

(defn cells-of [c]
  (mapv #(cell-of (a/cell %)) (:cells (:args c))))

(defn kinds-of
  "[cell kind] for the cells that are not solid, in :cells order (an unloaded cell is not owed (with :in-place it is, as open), nor one a place answered
  occupied for). Loose things and containers count only with :clear-loose."
  [c]
  (let [clear? (:clear-loose (:args c))
        shut? (:shut-open (:args c))
        in-place? (:in-place (:args c))
        p (:primitives c)]
    (into []
          (comp (keep (fn [cell]
                        (cond
                          (and shut? (seal/open-openable p cell)) [cell :shut]
                          (and in-place? (seal/sealed? p cell)) nil
                          :else (if-let [n (u/seen-name p cell)]
                                  [cell (seal/kind n)]
                                  (when in-place? [cell :open])))))
                (filter (fn [[_ k]] (or (#{:open :shut} k) (and clear? (#{:loose :container} k)))))
                (remove (comp (:occupied (ctx/mem c) #{}) first)))
          (cells-of c))))

(defn owed
  "The cells still to fill (and, with :clear-loose, to clear)."
  [c]
  (mapv first (kinds-of c)))

(defn next-cell
  "The cell to fill next: not the body's feet or head; by :order, lowest y first then nearest, or as given; the cells a place
  failed at come after those it did not."
  [c cells]
  (let [feet (cell-of (u/self-pos c))
        head (update feet :y inc)
        here (u/self-pos c)
        tries (fn [cell] (get-in (ctx/mem c) [:tries cell] 0))
        open (remove #(or (= % feet) (= % head)) cells)]
    (first (sort-by tries (if (= :given (:order (:args c)))
                            open
                            (sort-by (juxt :y #(u/dist here %)) open))))))

(def safe? (ready/safe-at-build (ready/near-unseen-here (fn [c] (or (next-cell c (owed c)) (a/cell (first (:cells (:args c)))))))))

(defn soft-ok?
  "Whether soft-reserved blocks may go in cell pos: a call in danger, at a cell where the block can be taken out safely later."
  [c pos]
  (boolean (and (:danger? (:args c)) (or (nil? pos) (blocks/safe-to-break? (:primitives c) pos)))))

(defn bump! [c k] (ctx/update-mem! c update k (fnil inc 0)))

(defn refusal-text [pos status more]
  (str "a place at " (pr-str (u/cell-vec pos)) " was refused: " status more))

(defn note-placed!
  "Job memory: item was placed at pos (the result's :placed), and in the scaffold ledger under the call's :purpose."
  [c pos item before borrowed?]
  (let [at (u/cell-vec pos)]
    (ctx/update-mem! c update :placed (fnil conj []) {:cell at :item item})
    (when-let [purpose (:purpose (:args c))]
      (ledger/remember! c (-> (ledger/intend (ledger/open-entries (ctx/view c)) (cond-> {:cell at :item item :before before :job (:id c) :purpose purpose} borrowed? (assoc :borrowed true)))
                              (ledger/confirm at))))))

(defn ^:async place!
  "One cell: resolves to :placed, :failed, :refused, :need, :mob (a mob stands in the cell: its names in job memory :mobs) or :partial (the place child waits on the world)."
  [c pos item]
  (let [before (u/seen-name (:primitives c) pos)
        borrowed? (blocks/borrowed? c item)
        e (await (blocks/place-call! c pos item (cond-> (select-keys (:args c) [:ignore-zones?])
                                                  (:in-place (:args c)) (assoc :see false))))
        o (blocks/place-outcome e)]
    (case o
      :continue :partial
      :placed (do (note-placed! c pos item before borrowed?) :placed)
      :already :placed
      :mob (do (ctx/update-mem! c assoc :mobs (get-in e [:data :mobs])) :mob)
      (:refused :need) o
      :occupied (do (ctx/update-mem! c update :occupied (fnil conj #{}) pos) :again)
      (do (ctx/update-mem! c assoc :refusal {:status (name o) :text (refusal-text pos (name o) "")}) :failed))))

(defn ^:async dig!
  "One loose cell: :dug (drops to collect), :partial, :refused or :failed."
  [c pos]
  (let [o (await (blocks/dig-cell! c pos (cond-> (select-keys (:args c) [:ignore-zones?])
                                                (:in-place (:args c)) (assoc :see false))))]
    (case o
      :continue :partial
      (:dug :missing) (do (when-not (:in-place (:args c)) (ctx/update-mem! c assoc :collect true))
                          (await (look/look-at! c (u/cell-vec pos)))
                          :dug)
      :refused :refused
      :failed)))

(defn ^:async shut!
  "One open door, gate or trapdoor: :shut, :partial never, or :stuck (an iron one, or a click that did nothing)."
  [c pos]
  (if (= :shut (await (seal/shut-open! c pos))) :shut :stuck))

(defn ^:async collect!
  [c]
  (let [r (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops {:radius 3})))]
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
       (when (and (seq (owed c)) (empty? (need/fetch-how c)) (nil? (blocks/pick c names (boolean (:danger? (:args c))))))
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
    (checks/goal (and (empty? left) (not (:collect (ctx/mem c)))) [:filled (if (pos? mended) (str "filled " mended " cells") "no cell is owed")] [:owed (str (count left) " cells are owed")]
                 {:units :cells :done mended :left (count left) :result {:mended mended :left left :placed (:placed (ctx/mem c) [])}})))

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
        data (merge {:mended (or mended 0) :left (owed c) :placed (:placed (ctx/mem c) [])} (:refusal (ctx/mem c)))]
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

(defn hostile-in?
  "Whether the mob in cell is a hostile: it hits while waited on, so the call fails at once for its caller to deal with it."
  [c cell]
  (= "hostile" (:kind (mob-in c cell))))

(defn ^:async wait-mob!
  "A mob stands in the cell: wait a second for it to move on (:continue when the pause is cut, else :again)."
  [c]
  (ctx/update-mem! c update :mob-waits (fnil inc 0))
  (let [r (await (child/pause! c 1 :mob-in-cell))]
    (if (= :continue r) :continue :again)))

(defn waiting!
  "A place was refused for a mob the body does not see in the cell: :continue while the wait (jobs.lib.seal/mob-wait-ms) from the
  first such refusal lasts, then the call fails."
  [c pos]
  (let [now (ctx/now c)
        since (or (:mob-since (ctx/mem c)) now)]
    (if (<= (- now since) (seal/mob-wait-ms))
      (do (ctx/update-mem! c assoc :mob-since since) :continue)
      (do (ctx/update-mem! c assoc :refusal {:status "failed" :text (refusal-text pos "failed" (str ", " (string/join ", " (:mobs (ctx/mem c))) " stays in the cell"))})
          (finish! c :nothing-possible {:reason :mob-in-cell :cell (u/cell-vec pos) :mobs (:mobs (ctx/mem c))})))))

(defn ^:async step!
  "One cell: :again after it, :continue when a child waits, or the ending."
  [c]
  (let [{:keys [failures collect mended]} (ctx/mem c)
        least (:least (ctx/frozen-call c) :all)
        names (wanted c)
        kinds (kinds-of c)
        cells (mapv first kinds)
        container (first (filter #(= :container (second %)) kinds))
        given-up? (fn [cell] (>= (get-in (ctx/mem c) [:tries cell] 0) (settings/get settings ::max-refusals)))
        pos (next-cell c (remove given-up? cells))
        item (when (seq cells) (blocks/pick c names (soft-ok? c pos)))
        kind-at (some (fn [[cell k]] (when (= cell pos) k)) kinds)
        loose? (and pos (= :loose kind-at))
        shut? (and pos (= :shut kind-at))]
    (cond
      collect (await (collect! c))
      (and (number? least) (pos? (or mended 0)) (>= mended least)) (finish! c nil nil)
      (empty? cells) (finish! c nil nil)
      container (finish! c :condition-failed {:reason :blocked :cell (first container)})
      (or (>= (or failures 0) (settings/get settings ::max-failures)) (and (nil? pos) (some given-up? cells))) (finish! c :nothing-possible {:reason :failed-steps})
      (and (nil? item) (seq (need/fetch-how c))) (await (fetch! c names (count cells)))
      (nil? item) (finish! c :condition-failed {:reason :need :any-of names})
      (and pos (not loose?) (hostile-in? c pos)) (do (ctx/update-mem! c assoc :refusal {:status "failed" :text (refusal-text pos "failed" ", a hostile stands in the cell")})
                                                      (finish! c :nothing-possible {:reason :hostile-in-cell :cell (u/cell-vec pos)}))
      (and pos (not loose?) (< (:mob-waits (ctx/mem c) 0) (settings/get settings ::mob-waits)) (mob-in c pos)) (await (wait-mob! c))
      :else
      (let [r (cond shut? (await (shut! c pos))
                    loose? (await (dig! c pos))
                    pos (await (place! c pos item))
                    :else (await (raise! c item)))]
        (case r
          :partial :continue
          :refused (finish! c :condition-failed {:reason :refused :cell pos})
          :need (finish! c :condition-failed {:reason :need :any-of names})
          :stuck (finish! c :condition-failed {:reason :blocked :cell pos})
          :mob (waiting! c pos)
          :dug :again
          :again :again
          (:placed :shut) (do (ctx/update-mem! c #(-> % (update :mended (fnil inc 0)) (dissoc :tries :refusal :mob-since) (cond-> pos (assoc :failures 0 :mob-waits 0)))) :again)
          (do (bump! c :failures)
              (ctx/update-mem! c dissoc :mob-since)
              (when pos (ctx/update-mem! c update-in [:tries pos] (fnil inc 0)))
              :again))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))
