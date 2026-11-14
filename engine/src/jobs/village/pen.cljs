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
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.placement :as placement]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.villager :as villager]
            [jobs.lib.villager-pen :as vp]))

(def doc
  "Shut the villager :villager in alone, in a lit one-wide cell beside the workstation of :profession at :pos
  (jobs.lib.villager-pen): the cell floored, walled three high, roofed over it and the workstation, a top slab over the
  workstation (a broken workstation leaves a gap a villager cannot pass and a sneaking body reaches through), its near
  end plugged once the villager stands in it. The side is the one the villager stands in, else the one with the fewest
  blocks to place or dig among those whose cell and near end are seen open (a pen already standing costs least).
  Each step re-reads the world and does the first owed: place the workstation (jobs.blocks.place, the call's :ways), get
  building blocks (jobs.items.obtain) and fill the floor, walls and roof (jobs.blocks.fill-cells), place a wooden slab
  in the upper half over the workstation (a block of another kind there is dug first), light the cell while it reads dark
  (jobs.build.light-area, once a call: its done is the verdict), dig a plug of an empty pen open, step out of the pen behind the workstation, wait for the villager
  to walk in (a jobless one to the free workstation, one employed at it to work), then fill the near end.
  The wait counts game ticks of the villagers' walking hours (jobs.lib.villager/take-job-ticks, 1240); reason
  :villager-not-in-pen by day, :night-villager-not-in-pen at night (villagers walk to no job site then).
  Needs: a villager id, a profession with a workstation, the cell (else impossible :bad-args); building blocks, a wooden
  slab, torches and the workstation carried or fetchable.
  A building child that ends done with its piece still unbuilt ends the call nothing-possible :not-built (:piece).
  Ends done when the pen holds the villager (jobs.lib.villager-pen/penned; result {:side [dx dz] :cell [x y z]}), else condition-failed
  :villager-out-of-view, :occupied (another block at :pos), :need (blocks not carried and no fetch allowed); nothing-possible
  :no-room (no side has its cell and near end open), :not-walked-in (the villager stayed out for the whole wait; :sites
  names the other job sites seen), or a child's ending. What it built stays: the next call finds the pen standing.")

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

(def can-start? bad-args)

(defn the-villager [c] (villager/find-by-uuid (:primitives c) (:villager (:args c))))

(defn done?
  "Ok once the villager stands in a shut, lit pen alone (jobs.lib.villager-pen/penned)."
  [c]
  (let [e (the-villager c)
        lay (when e (vp/penned (:primitives c) (ws-cell c) e))]
    (if lay
      (contract/ok :penned "the villager is shut in its pen" :result {:side (:side lay) :cell (:cell lay)})
      (contract/later :penning "the villager is not shut in a pen yet"))))

(defn quote-of
  "The blocks still to place or dig at cost.ways/place-s each, the wait for the villager not priced."
  [c]
  (let [e (the-villager c)
        ws (ws-cell c)
        side (when ws (vp/choose (:primitives c) ws e))
        n (if side (vp/work (:primitives c) (vp/layout ws side) (boolean (and e (vp/inside? (vp/layout ws side) e)))) 0)
        t (* n (cost-ways/place-s))]
    {:uses {:job-time [0 t ##Inf]}}))

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
  "Fill cells with building blocks, fetched first when too few are carried (the call's ways)."
  [c slot cells]
  (let [{:keys [short]} (choose/fillers-for c :build (count cells))
        opts (need/fetch-opts c)]
    (cond
      (zero? short) (await (built! c slot (await (child! c slot 'jobs.blocks.fill-cells {:cells cells :clear-loose true}))
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
  "Light the cell (jobs.build.light-area) once a call: its done is the verdict, whatever the cell reads after."
  [c lay]
  (let [r (await (child! c :light 'jobs.build.light-area {:cells [(:cell lay)]}))]
    (when (= :again r) (ctx/update-mem! c assoc :lit true))
    r))

(defn ^:async reopen! [c lay]
  (let [cell (first (vp/to-reopen (:primitives c) lay))]
    (await (built! c :reopening (await (child! c :reopen 'jobs.blocks.dig {:pos cell :collect true}))
                   #(vp/solid? (vp/name-at (:primitives c) cell))))))

(defn in-pen? [c lay] (contains? (set (cons (:cell lay) (:plug lay))) (b/feet-cell c)))

(defn ^:async step-out!
  "Walk out of the pen to the cell behind the workstation, where the roll is made from, so the villager can walk in and
  the plug shuts the body out."
  [c ws lay]
  (let [[dx dz] (:side lay)
        [x y z] ws
        r (await (child/call-walk* c :out {:pos {:x (- x dx) :y y :z (- z dz)} :range 1 :ways #{:walk}}))]
    (cond
      (= :continue r) :continue
      (and (:arrived (:data r)) (not (in-pen? c lay))) :again
      :else (stop! c :nothing-possible :unreachable {:text "could not step out of the pen"}))))

(defn ^:async wait-walk-in!
  "Wait a poll for the villager to walk in (or another to walk out), counting walking-hour ticks; past
  take-job-ticks end :not-walked-in."
  [c why]
  (if (< (villager/count-ticks! c) (villager/take-job-ticks))
    (do (await (child/pause! c (villager/poll-s) (if (sh-sense/night? (:primitives c)) (keyword (str "night-" (name why))) why)))
        :continue)
    (stop! c :nothing-possible :not-walked-in {:sites (villager/job-sites c (:pos (b/parse (:args c))))})))

(defn ^:async step!
  "The first owed piece of the pen, from the world as it reads now. :again, :continue or the stop."
  [c]
  (let [p (:primitives c)
        ws (ws-cell c)
        e (the-villager c)
        side (when e (vp/choose p ws e))
        lay (when side (vp/layout ws side))
        block (vp/name-at p ws)
        in? (boolean (and lay (vp/inside? lay e)))]
    (cond
      (nil? e) (stop! c :condition-failed :villager-out-of-view)
      (nil? lay) (stop! c :nothing-possible :no-room {:text (str "no side of " (pr-str ws) " has a cell and a near end open for a villager")})
      (or (nil? block) (bs/air block)) (await (workstation! c ws))
      (not= (item-of (:args c)) block) (stop! c :condition-failed :occupied {:block block})
      (seq (vp/owed p (:shut lay))) (await (fill! c :shut (vp/owed p (:shut lay))))
      (not (vp/top-slab? p (:slab lay))) (await (slab! c (:slab lay)))
      (and (vp/dark? p lay) (not (:lit (ctx/mem c)))) (await (light! c lay))
      (and (not in?) (seq (vp/to-reopen p lay))) (await (reopen! c lay))
      (in-pen? c lay) (await (step-out! c ws lay))
      (not in?) (await (wait-walk-in! c :villager-not-in-pen))
      (seq (vp/others-in p lay (villager/ident e))) (await (wait-walk-in! c :other-villager-in-pen))
      (not (vp/plug-shut? p lay)) (await (fill! c :plug (vp/owed p (:plug lay))))
      :else :again)))

(defn ^:async round
  "Steps until the villager is penned or the call stops."
  [c]
  (await (pace/steps! c #(step! c))))
