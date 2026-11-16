(ns jobs.village.pen
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as b]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.build.wood :as wood]
            [jobs.lib.child :as child]
            [jobs.lib.choose :as choose]
            [jobs.lib.cost.ways :as cost-ways]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.placement :as placement]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.villager :as villager]
            [jobs.lib.villager-pen :as vp]))

(def doc
  "Shut the villager :villager in alone, in a lit one-wide cell beside the workstation of :profession at :pos
  (jobs.lib.villager-pen): the cell floored, walled three high, roofed over it and the workstation, a top slab over the
  workstation (a broken workstation leaves a gap a villager cannot pass and a sneaking body reaches through), its near
  end plugged once the villager stands in it. No other villager may path to a cell next to the workstation (the game's
  rule for claiming a job site): its sides across the pen are walled and the body's stand behind it is boxed in with
  the body inside. The side is the one the villager stands in, else the one with the fewest blocks to place or dig
  among those whose cell, stand and near end are seen open (a pen already standing costs least).
  Each step re-reads the world and does the first owed: get
  building blocks (jobs.items.obtain) and fill the floors, walls and roofs seen open (jobs.blocks.fill-cells); cells
  never seen are looked at first (a :look at the start while it pays, then a walk to see each, digging the box's entry
  when boxed in; still unseen: nothing-possible :unseen), place a wooden slab
  in the upper half over the workstation (a block of another kind there is dug first), light the cell while it reads dark and
  the body can reach it (jobs.build.light-area, once a call), dig a plug of an empty pen open; with the villager in: wait while another
  villager shares the cell, fill the near end (from the stand, digging the box's entry first to walk out); then step
  into the stand (digging the entry when the box is shut), shut the box round the body from inside, place the
  workstation, if missing, from the stand (it is the lure: with the box shut the pen's cell is the only one next to it a
  villager can path to, the game's rule for claiming it), wait for the villager to walk in (a jobless one to the free
  workstation, one employed at it to work), and light a shut pen still dark from the stand (its light reaches the cell
  under the top slab). A cell still dark after the light child ended done is looked at when unknown, then read each
  poll: the same light twice ends nothing-possible :dark.
  The wait counts game ticks of the villagers' walking hours (jobs.lib.villager/take-job-ticks, 1240); reason
  :villager-not-in-pen by day, :night-villager-not-in-pen at night (villagers walk to no job site then).
  Needs: a villager id, a profession with a workstation, the cell (else impossible :bad-args); building blocks, a wooden
  slab, torches and the workstation carried or fetchable.
  A building child that ends done with its piece still unbuilt ends the call nothing-possible :not-built (:piece).
  With the body boxed in its stand of a built pen whose near end is open (jobs.lib.villager-pen/luring) it sees out only
  through the pen, so a villager out of view is waited for to walk in like one in view.
  Ends done when the pen holds the villager (jobs.lib.villager-pen/penned; result {:side [dx dz] :cell [x y z]}), else condition-failed
  :villager-out-of-view (never in view this call and not luring), :occupied (another block at :pos), :need (blocks not carried and no fetch allowed); nothing-possible
  :no-room (no side has its cell, stand and near end open), :unreachable (the stand), :unseen (:cells), :dark, :not-walked-in (the villager
  stayed out for the whole wait; :sites names the other job sites seen), or a child's ending. What it built stays: the
  next call finds the pen standing, but a call that ends other than done with the body boxed in its stand digs the box's
  entry open first (cleanup).")

(a/defargs args
  {:villager {:doc "the villager's entity uuid or numeric entity id" :spec villager/id? :default nil}
   :pos {:doc "the workstation block, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :profession {:doc "villager profession whose workstation stands at :pos, e.g. librarian" :spec string? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds of work one call may take (as jobs.blocks.fill-cells)." :spec (a/int-in 1 nil)}
   ::wall-time-s {:default 1200 :doc "Seconds one call may last, the wait for the villager included: one game day, so a night's wait still leaves a day's walking hours." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(def ways need/fetch-ways)

(def target [:villager :pos])

(def safe? (price/staying :work (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c)))))))
(def fit? ready/fed-fit?)

(defn item-of [args] (villager/workstations (:profession args)))

(defn ws-cell [c] (some-> (:pos (b/parse (:args c))) b/cell))

(defn bad-args
  "Never :bad-args without a villager, a profession with a workstation and a cell."
  [c]
  (let [{:keys [villager]} (:args c)
        {:keys [error]} (b/parse (:args c))]
    (cond
      (not (and (villager/id? villager) (string? (item-of (:args c)))))
      (contract/never :bad-args "needs a villager and a known profession" :why "needs a villager and a known profession")
      error (contract/never :bad-args error :why error)
      :else (contract/ok :args "a villager, a profession and a cell"))))

(defn the-villager [c] (villager/find-by-uuid (:primitives c) (:villager (:args c))))

(defn lay-of
  "The pen's layout: the side chosen for the villager in view, else the built pen the body is boxed in and lures from,
  else the side chosen on an earlier step (the villager leaving the view cone does not unchoose it)."
  [c]
  (let [p (:primitives c)
        ws (ws-cell c)
        e (the-villager c)]
    ;; boxed in, the body sees out only through the pen: the walk-in is what it waits for, not the villager in view
    (if e
      (some->> (vp/choose p ws e) (vp/layout ws))
      (or (vp/luring p ws) (some->> (:side (ctx/mem c)) (vp/layout ws))))))

(defn unseen [p cells] (filterv #(nil? (vp/name-at p %)) cells))

(defn shut-unseen
  "Look :unseen at the pen's shut cells never seen that a look from here pays for (sense/paid-look-at); ok when none."
  [c]
  (let [paid (some->> (lay-of c) :shut (unseen (:primitives c)) seq (sense/paid-look-at c))]
    (if (seq paid)
      (contract/look :unseen "cells of the pen were never seen" :look-at (vec paid))
      (contract/ok :seen "the pen's cells are seen or no look from here shows them"))))

(defn can-start? [c] (let [s (bad-args c)] (if (= :ok (first s)) (shut-unseen c) s)))

(defn done?
  "Ok once the villager stands in a shut, lit pen alone (jobs.lib.villager-pen/penned) with the workstation placed."
  [c]
  (let [e (the-villager c)
        lay (when e (vp/penned (:primitives c) (ws-cell c) e))
        placed? (= (item-of (:args c)) (vp/name-at (:primitives c) (ws-cell c)))]
    (if (and lay placed?)
      (contract/ok :penned "the villager is shut in its pen" :result {:side (:side lay) :cell (:cell lay)})
      (contract/later :penning "the villager is not shut in a pen yet"))))

(defn quote-of
  "The blocks still to place or dig at cost.ways/place-s each; while the villager is not in, the walk-in wait, at most
  take-job-ticks (wall time)."
  [c]
  (let [e (the-villager c)
        ws (ws-cell c)
        side (when ws (vp/choose (:primitives c) ws e))
        in? (boolean (and e side (vp/inside? (vp/layout ws side) e)))
        n (if side (vp/work (:primitives c) (vp/layout ws side) in?) 0)
        t (* n (cost-ways/place-s))
        wait (if in? 0 (/ (settings/ticks->ms (villager/take-job-ticks)) 1000))]
    {:uses {:job-time [0 t ##Inf] :wall-time [0 wait wait]}}))

(defn stop!
  [c kind reason & [data]]
  (ctx/end c kind reason (or data {})))

(defn ^:async child!
  "A child call: :again once it ended done, :continue while it goes on, else this call ends with its ending."
  [c slot job cargs]
  (let [r (await (ctx/call c slot job (cond-> cargs (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) :again
      :else (ctx/end c (:kind r) (:cause r) (:data r)))))

(defn ^:async built!
  "A building child's r: a done child whose piece is still owed (owed? reads the world again) ends the call
  nothing-possible :not-built naming the piece, so a step is never repeated with nothing changed."
  [c piece r owed?]
  (if (and (= :again r) (owed?))
    (stop! c :nothing-possible :not-built {:piece piece :text (str "the " (name piece) " of the pen still reads unbuilt after its child ended done")})
    r))

(defn place-ways [c] (when-some [w (:ways (:args c))] {:ways w}))

(defn ^:async workstation!
  "Place the workstation at the cell."
  [c ws]
  (let [item (item-of (:args c))]
    (await (built! c :workstation (await (child! c :workstation 'jobs.blocks.place (merge {:pos ws :item item} (place-ways c))))
                   #(not= item (vp/name-at (:primitives c) ws))))))

(defn ^:async fill!
  "Fill cells with building blocks, fetched first when too few are carried (the call's ways); extra goes to the
  fill-cells child."
  [c slot cells & [extra]]
  (let [{:keys [short]} (choose/fillers-for c :build (count cells))
        opts (some-> (need/fetch-opts c) (assoc :avoid (some-> (lay-of c) vp/cells)))]
    (cond
      (zero? short) (await (built! c slot (await (child! c slot 'jobs.blocks.fill-cells (merge {:cells cells :clear-loose true} extra)))
                                   #(= cells (vp/owed (:primitives c) cells))))
      (nil? opts) (stop! c :condition-failed :need {:any-of (choose/filler-names c :build) :count short})
      :else (let [e (await (need/blocks! c (choose/filler-names c :build) short opts))]
              (cond (= :continue e) :continue
                    (= :done (:kind e)) :again
                    :else (ctx/end c (:kind e) (:cause e) (:data e)))))))

(defn slab-item
  "The wooden slab to place: one carried, else one of the species jobs.lib.build.wood chooses (fetched by the place)."
  [c]
  (let [carried (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))]
    (or (some #(when (and (wood/species-of %) (re-find #"_slab$" %)) %) (keys carried))
        (str (wood/choose carried nil) "_slab"))))

(defn ^:async slab!
  "A top slab over the workstation: dig what else stands there, then place the slab, clicked so it takes the upper half."
  [c cell]
  (let [p (:primitives c)
        n (vp/name-at p cell)]
    (if (and n (not (bs/air n)))
      (await (built! c :slab (await (child! c :slab-dig 'jobs.blocks.dig {:pos cell :collect true})) #(= n (vp/name-at p cell))))
      (let [item (slab-item c)
            click (placement/click {:block item :type "top"} cell (placement/eye (u/self-pos c)) #(some->> (vp/name-at p %) (hash-map :name)))]
        (if (:refused click)
          (stop! c :nothing-possible :slab-refused {:pos cell :refused (:refused click)})
          (await (built! c :slab (await (child! c :slab 'jobs.blocks.place (merge {:pos cell :item item :click (:click click)} (place-ways c))))
                         #(not (vp/top-slab? p cell)))))))))

(defn ^:async light!
  "Light cell (jobs.build.light-area) once a call: the pen's own while the body can reach it, else the stand, whose
  light reaches the pen's cell under the top slab."
  [c cell]
  (let [r (await (child! c :light 'jobs.build.light-area {:cells [cell]}))]
    (when (= :again r) (ctx/update-mem! c assoc :lit true))
    r))

(defn ^:async dig-open!
  "Dig the first solid cell of cells (a plug of an empty pen, the box's entry) so a villager or the body walks through;
  the box's own filler is dug by hand when no tool harvests it."
  [c slot lay k]
  (let [cell (first (vp/solids (:primitives c) (k lay)))]
    (await (built! c slot (await (child! c slot 'jobs.blocks.dig (cond-> {:pos cell :collect true} (= :entry k) (assoc :need-drop false))))
                   #(vp/solid? (vp/name-at (:primitives c) cell))))))

(defn ^:async step-in!
  "Walk into the stand behind the workstation, where the roll is made from and the box is shut round the body."
  [c lay]
  (let [r (await (child/call-walk* c :stand {:pos (u/cell-pos (:stand lay)) :range 0 :ways #{:walk}}))]
    (cond
      (= :continue r) :continue
      (and (:arrived (:data r)) (vp/at-stand? (:primitives c) lay)) :again
      :else (stop! c :nothing-possible :unreachable {:text "could not step into the stand behind the workstation"}))))

(defn ^:async dark!
  "The cell reads dark after the light child ended done. Unknown to the body: look at it (still unknown after a look:
  :dark). Known: wait a poll while its light changes from the last read (it follows the torch); the same light read
  twice ends nothing-possible :dark."
  [c lay]
  (let [cell (zipmap [:x :y :z] (:cell lay))
        light (sh-sense/cell-light (:primitives c) cell)
        m (ctx/mem c)
        dark #(stop! c :nothing-possible :dark (merge {:cell (:cell lay) :text "the pen's cell still reads dark after the light child ended done"} %))]
    (cond
      (and (nil? light) (:dark-looked m)) (dark {:unseen true})
      (nil? light) (do (ctx/update-mem! c assoc :dark-looked true)
                       (await (look/look-at! c (:cell lay)))
                       :again)
      (= light (:light-read m)) (dark {:light light})
      :else (do (ctx/update-mem! c assoc :light-read light)
                (await (child/pause! c (villager/poll-s) :pen-light-settling))
                :continue))))

(defn ^:async wait-walk-in!
  "Wait a poll for the villager to walk in (or another to walk out), counting walking-hour ticks; past
  take-job-ticks end :not-walked-in."
  [c why]
  (if (< (villager/count-ticks! c) (villager/take-job-ticks))
    (do (await (child/pause! c (villager/poll-s) (if (sh-sense/night? (:primitives c)) (keyword (str "night-" (name why))) why)))
        :continue)
    (stop! c :nothing-possible :not-walked-in {:sites (villager/job-sites c (:pos (b/parse (:args c))))})))

(defn ^:async see!
  "Walk to where the eye sees the nearest of the shut cells never seen (blocks/walk-into-reach!) and look at it; a cell
  looked at once this call and still unseen ends nothing-possible :unseen."
  [c cells]
  (let [cell (first (sort-by #(u/dist (u/self-pos c) (u/cell-pos %)) cells))]
    (if (contains? (:looked (ctx/mem c) #{}) cell)
      (stop! c :nothing-possible :unseen {:cells cells :text "cells of the pen stay unseen after a walk to see them"})
      (let [w (await (b/walk-into-reach! c (u/cell-pos cell) true))]
        (cond
          (= :continue w) :continue
          (:unreachable w) (stop! c :nothing-possible :unseen {:cells cells :why (:unreachable w) :text (str "could not walk to see the pen's cells: " (:text w))})
          (= :again w) (do (ctx/update-mem! c update :looked (fnil conj #{}) cell)
                           (await (look/look-at! c cell))
                           :again)
          :else (ctx/end c (:kind w) (:cause w) (:data w)))))))

(defn ^:async shut!
  "The shut cells seen open are filled (blocks fetched for those only); with none, the unseen ones are looked at, the
  box's entry dug first when the body stands boxed in."
  [c lay]
  (let [p (:primitives c)
        owed (vp/owed p (:shut lay))
        open (filterv #(vp/name-at p %) owed)]
    (cond
      (seq open) (await (fill! c :shut open {:collect false}))
      (and (vp/at-stand? p lay) (not (vp/way-in? p lay))) (await (dig-open! c :entry lay :entry))
      :else (await (see! c owed)))))

(defn ^:async step!
  "The first owed piece of the pen, from the world as it reads now. :again, :continue or the stop."
  [c]
  (let [p (:primitives c)
        ws (ws-cell c)
        e (the-villager c)
        lay (lay-of c)
        block (vp/name-at p ws)
        in? (boolean (and e lay (vp/inside? lay e)))]
    (when (and e lay) (ctx/update-mem! c assoc :side (:side lay)))
    (cond
      (and (nil? e) (nil? lay)) (stop! c :condition-failed :villager-out-of-view)
      (nil? lay) (stop! c :nothing-possible :no-room {:text (str "no side of " (pr-str ws) " has a cell, a stand and a near end open for a villager")})
      (and block (not (bs/air block)) (not= (item-of (:args c)) block)) (stop! c :condition-failed :occupied {:block block})
      (seq (vp/owed p (:shut lay))) (await (shut! c lay))
      (not (vp/top-slab? p (:slab lay))) (await (slab! c (:slab lay)))
      (and (vp/dark? p lay) (not (:lit (ctx/mem c))) (not (and in? (vp/plug-shut? p lay)))) (await (light! c (:cell lay)))
      (and (not in?) (seq (vp/solids p (:plug lay)))) (await (dig-open! c :reopen lay :plug))
      (and in? (seq (vp/others-in p lay (villager/ident e)))) (await (wait-walk-in! c :other-villager-in-pen))
      (and in? (not (vp/plug-shut? p lay)))
      (if (and (vp/at-stand? p lay) (not (vp/way-in? p lay)))
        (await (dig-open! c :exit lay :entry))
        (await (fill! c :plug (vp/with-floors p lay (:plug lay)))))
      (not (vp/at-stand? p lay)) (if (vp/way-in? p lay) (await (step-in! c lay)) (await (dig-open! c :entry lay :entry)))
      (seq (vp/owed p (:box lay))) (await (fill! c :box (vp/with-floors p lay (:box lay)) {:in-place true}))
      (or (nil? block) (bs/air block)) (await (workstation! c ws))
      (not in?) (await (wait-walk-in! c :villager-not-in-pen))
      (and (vp/dark? p lay) (not (:lit (ctx/mem c)))) (await (light! c (:stand lay)))
      (vp/dark? p lay) (await (dark! c lay))
      :else :again)))

(defn ^:async cleanup
  "A call that ends other than done with the body boxed in its stand opens the box's entry (its two cells, each dug once,
  by hand when no tool harvests it), so the body's next walk is not shut in by blocks no ledger holds. Done, or a
  dig child's ending."
  [c ending]
  (let [p (:primitives c)
        dug (:exit-dug (ctx/mem c) #{})
        cell (when-not (= :done (:kind ending))
               (some->> (ws-cell c) (vp/boxed p) :entry (filter #(and (vp/solid? (vp/name-at p %)) (not (dug %)))) first))]
    (if-not cell
      (ctx/end c :done nil)
      (do (ctx/update-mem! c update :exit-dug (fnil conj #{}) cell)
          (let [r (await (child! c :exit 'jobs.blocks.dig {:pos (u/cell-pos cell) :collect true :need-drop false}))]
            (if (= :again r) (await (cleanup c ending)) r))))))

(defn ^:async round
  "Steps until the villager is penned or the call stops."
  [c]
  (await (pace/steps! c #(step! c))))
