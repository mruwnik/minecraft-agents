(ns jobs.build.from-plan
  (:require [jobs.lib.build.wood :as wood]
            [jobs.lib.reserve :as reserve]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.waits :as waits]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.watch :as watch]
            [jobs.lib.pace :as pace]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.build.plan :as plan :refer [retry-distance max-retries facing-ok? body-cells count-fail plan-trouble owed need-of shortage hazards decide rules-input refuse permitted judged no-zones planned how placeable misplaced missing covered digging unseen buildable problem]]
            [jobs.lib.build.scaffold :as scaffold]
            [jobs.build.from-plan.cells :refer [diggable in-reach in-dig-reach seal-price shuts-out? off-low-floor support-looks retreat-dists fill-order]]
            [jobs.build.from-plan.book :refer [walk-fail retry-walks warn-way-in! place-one! settle-placing! dig-one! result-data finish! book-failed!]]
            [jobs.build.from-plan.light :as light]
            [jobs.build.from-plan.walk :refer [give-up trace! leave! exit-of walk-to! reach-cell! look-for-support! shut-in? place-here?]]
            [jobs.lib.build.text :refer [summary stopped-text assoc-fetch-failed]]))

(def meta {:tags [:goal]})

(def doc
  "Build what a plan wants (:plan, optionally only its :part) from what the body carries.
  A planned cell that stands empty gets its block placed. So does a cell holding a replaceable block (leaf_litter,
  fern, vine, water ...), which a place takes over.
  A cell holding any other wrong block (dirt, leaves, stone ...) is dug first and then placed, when its item is
  carried and the rules allow the dig. The best carried tool is used, or the hand for blocks that need none. A
  pickaxe block without a pickaxe is fetched a tool for, else given up as :no-tool. A block that could not be dug, or may not be, is listed
  under :wrong. So is a block that came out of a place in the wrong state (:placed true); it is never dug or placed
  again.
  A door's upper half, a bed's head and a tall plant's upper half are never targets: the lower or foot cell places
  the whole item. A :clear cell holding a block that is not replaceable and no container is dug (nothing carried
  needed); so is a chest, table or the like the body placed itself (body memory :build-placed-kept; its contents drop as items). Crops and trees are not this job's.
  Its walks never dig through the build, except from a hole the body stands in (2 or more levels walled): they may climb out.
  One call works until the plan is built or nothing more applies (it yields only while a walk or fetch waits). Every
  step re-reads the plan and the world and takes the first that applies: walk out of a footprint cell the body stands in
  (once per cell; a place that would shut the body in is held back the same way), place what is buildable within the
  setting ::reach (jobs.lib.build.plan) of the eye, lowest layer first (buildable: its block is carried, the cell below is
  not still owed, it is not the body's feet or head cell; jobs.lib.placement picks the click), dig wrong blocks in reach,
  walk to a stand beside the nearest buildable or diggable cell, raise a temporary pillar and bridge row for a cell no
  ground stand reaches (jobs.lib.build.scaffold; jobs.access.cleanup takes it back however the call ends; a
  zone or footprint that refuses every base refuses the cell :zone (bases that are only floors left standing: the cell is unreachable); too few pillar blocks are fetched, else condition-failed
  {:reason :needs :needs :scaffold-item}), walk toward the nearest unloaded cell (unseen cells are never taken as
  built), else finish.
  A cell whose state no neighbour gives, or with no block to click beside it yet, waits for one. If still missing at
  the end it is given up with jobs.lib.placement's reason (:no-support, after a look at any floor or neighbour never seen; :no-room, :opened, :double-slab). A cell whose place is refused, or whose stand cell
  cannot be reached, :give-up times is given up (:refused, :occupied, :no-item, :unreachable or :unseen).
  A cell shut in by rock (and other plan cells) on every side is tunnelled to (jobs.access.tunnel); faces never seen count as rock only for a cell within reach that the body has looked at, else it walks (go-to digs out of a pocket) first.
  Zones: every place is checked against zones and the footprints of the other active plans, when the cell is
  chosen and again before the place. A cell in a zone that does not allow :place, or in another plan's footprint,
  is refused for good (not counted as given up). It is listed in :refused [{:pos :reason :zone|:claim|:footprint|:hazard}].
  Water beside a cell is no obstacle by default. :accept names the fluid hazards taken (:fluid-adjacent,
  :lava-adjacent). A cell with an untaken one is refused as :hazard.
  Plan properties: :surface true puts each column of the plan on the ground the body has seen there (the column's lowest
  cell stands on the ground's top, the rest keep their height above it; an unseen column stays as planned), for a pen on a
  slope (parts placed from a blueprint stay flat at their :at); :sturdy-ground true accepts a sturdy block on the ground of a rail line (plan.rail/ground) where the plan wants
  fill, not listed :wrong.
  done? holds when no cell is owed, none came out wrong; the engine then ends it :done.
  Ends with data {:placed n :missing [[x y z] ...] :short {item n} :given-up {[x y z] reason} :wrong [{:pos :found
  :want}] :refused [...] :fetch-failed {item {:failed :why}}}: done when nothing is missing (and no block came out wrong: nothing-possible :wrong-blocks) or the cells placed meet
  the call's :least (default :all); else with :text naming the missing count and what it left: nothing-possible
  :incomplete when cells were given up, condition-failed {:reason :no-items :needs} when items are short, condition-failed
  :refused when the rules refuse the rest, nothing-possible :shut-in; each with :next (jobs.lib.next-call: an obtain per short item, then the build again). For a block placed in the wrong state, :found names the state it came out in.
  Events: build.gap (a fence or wall placed over air; warns) and build.way-in (cells the body stood in, above the plan's floor, filled with no door there; warns). Event texts name
  the count and the first few cells. The whole list is in :cells of the event and in the result.
  Start facts: no zone list read (unless :ignore-zones?) is a
  failed condition {:reason :no-zones}; a plan that is missing, unreadable or has no cells to
  build (in :part) is impossible {:reason :plan-trouble :why text} (a plan missing or unreadable waits), at the start and later in the call; at the start, with nothing to place, dig, fetch or
  look at, a failed condition {:reason :no-items :needs {item n}}.
  Wood: a generic wood want (jobs.lib.build.wood: planks, logs, wooden_stairs, wooden_slabs, wooden_doors, wooden_fences,
  fence_gates, wooden_trapdoors ...) is built in one species chosen for the plan (carried most, else the nearest log seen,
  else oak), booked, fetched by that species, and named in the result (:wood \"birch\"); a species name (oak_planks) stays exact.
  A :part whose cells only want air ends done placed 0 saying so.
  When cells are missing but none of their blocks is carried and none can be fetched, the call ends condition-failed :no-items with :needs.
  Material: while cells wait for an item not carried (and not given up or refused), each round fetches it, once no cell can be placed from what is carried (a missing bed never holds up the cells above it, and a bed the body put down itself within ::own-bed-radius is broken and taken up (jobs.access.cleanup, :purpose :bed) before one is fetched; when the ways allow it,
  jobs.lib.need with :explore on: a jobs.items.obtain child for the missing count, walking off to look for what is unseen; an item whose fetch failed is not asked again in the call); a pickaxe block to dig without a pickaxe fetches the tool the same way
  (jobs.items.get-tool). With ways #{:carried}, or once a fetch failed, the build goes on with what is carried and ends
  short (the text and :fetch-failed name each failed fetch and why), a pickaxe block given up :no-tool.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part, and the cells of other parts it stands on (down the column), built first" :spec a/name? :default nil}
   :accept {:doc "fluid hazards of a cell taken: :fluid-adjacent (water beside; placing beside or into water seals and bridges), :lava-adjacent (lava beside; not taken by default: the body stands beside the cell), :falling-block (a dig under sand or gravel that falls in)" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :explore {:doc "walk off to look for blocks and tools that are not in sight when the plan lacks them" :spec boolean? :default true}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(def target [:plan :part])
(def defaults {:least :all :spend {:job-time ::job-time-s}})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one build may take." :spec (a/int-in 1 nil)}
   ::own-bed-radius {:default 64 :doc "Blocks within which a bed the body put down itself (scaffold ledger purpose :bed) is taken up for a plan bed before a new one is fetched." :spec (a/int-in 0 nil)}
   ::off-bed-tries {:default 3 :doc "Walks off a bed or bottom slab the body stands on, before the roof cells over it are given up." :spec (a/int-in 1 nil)}
   ::stall-steps {:default 3 :doc "Steps in a row that return :again with the job memory unchanged (nothing placed, dug, walked or booked) after which the build stops :stalled." :spec (a/int-in 1 nil)}
   ::budget-streak {:default 4 :doc "Places or digs in a row (no cell placed or dug between) whose walk ended :unreachable :budget (a search that flooded the loaded land and found no stand) after which the build stops :unreachable: the next cells would flood the same land." :spec (a/int-in 1 nil)}
   ::look-range {:default 16 :doc "Cells farther than this from the eye are walked to, not looked at: they may be unloaded, which a look cannot tell." :spec (a/num-in 0 nil)}
   ::cell-s {:default 3 :doc "Seconds one cell of the plan is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(defn judged-plan
  "planned, judged once per check-cache window."
  [c]
  (check-cache/cached c :planned #(planned c)))

(def safe? (light/lights-itself (ready/safe-at-build work-place/spot) #(:cells (judged-plan %))))
(def fit? ready/fit?)

(defn plan-now
  "The plan as the check or the round reads it: a check (no token) judged once per check-cache window, a round afresh."
  [c]
  (if (:token c) (planned c) (judged-plan c)))

(defn plan-problem
  "Later :no-zones while no zone list is read, later :plan-trouble for a plan that is missing or unreadable (it may be
  saved or fixed later). A readable plan with no cells to build (in :part) is never: the cause is
  {:reason :plan-trouble :why text}."
  [c]
  (let [{:keys [trouble later?]} (plan-now c)]
    (cond
      (nil? trouble) (contract/ok :plan "the plan can be built")
      (= no-zones trouble) (contract/later :no-zones "no zone list has been read")
      later? (contract/later :plan-trouble trouble :why trouble)
      :else (contract/never :plan-trouble trouble :why trouble))))

(defn look-range [] (settings/get settings ::look-range))

(defn no-items-cause
  "Why there is nothing to build, dig, fetch or look at: the cells' items are not carried and no fetch is due. While the
  cells are unseen but all unknown and within ::look-range, the cause carries :look-at them: the verdict waits for a look."
  [c]
  (let [closed (merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        cached (judged-plan c)
        ;; a look just made the cached cells stale, so unseen ones are judged afresh
        {:keys [cells trouble]} (if (seq (unseen (:cells cached) closed)) (planned c) cached)
        carried (storage/carried-totals (u/inventory (:primitives c)))
        hidden (unseen cells closed)
        cells-of (mapv :pos hidden)]
    (when-not (or trouble
                  (empty? (owed cells))
                  (empty? (shortage (owed cells) carried))
                  (seq (buildable cells carried {}))
                  (seq (digging cells)))
      (let [here (u/self-pos c)
            near (filter #(and (<= (u/eye-dist here %) (look-range)) (sense/unknown? (:primitives c) %)) (sense/targets (:primitives c) here cells-of #{}))]
        (cond
          (and (seq hidden) (not= (count near) (count cells-of))) nil
          (and (empty? hidden) (some? (need-of c cells))) nil
          :else (cond-> {:reason :no-items :plan (:plan (:args c)) :part (:part (:args c))
                         :needs (into {} (shortage (owed cells) carried))}
                  (seq hidden) (assoc :look-at (vec near))))))))

(def no-items
  "Later :no-items {:plan :part :needs} when there is nothing to build, dig, fetch or look at: the cells' items are
  not carried and no fetch is due; a :look first while unseen cells are all within ::look-range."
  (fn [c]
    (if-let [cause (no-items-cause c)]
      (checks/cause-status (if (:look-at cause) :look :later) cause)
      (contract/ok :items "there is something to build"))))

(def can-start? (cond/and plan-problem no-items))
(def can-run? plan-problem)

(defn done?
  "Done when the plan has nothing owed, no block came out wrong (the round ends it :wrong-blocks); the cells placed this call, the cells still owed and the result so far. A check (no token)
  reads the plan judged once per check-cache window, a round afresh."
  [c]
  (let [{:keys [cells trouble]} (plan-now c)
        placed (:placed (ctx/mem c) 0)
        left (count (owed cells))
        plan (:plan (:args c))
        result (when-not trouble (result-data c cells))
        data [:units :cells :done placed :left left :result result]]
    (cond
      trouble (apply contract/later :plan-trouble (str "plan " plan ": " trouble) data)
      (pos? left) (apply contract/later :owed (str "build of " plan ": " left " cells owed") data)
      (seq (:wrong result)) (apply contract/later :wrong (str "build of " plan ": " (count (:wrong result)) " blocks wrong") data)
      :else (apply contract/ok :built (str "build of " plan " done: placed " placed (some->> (when (zero? placed) (plan/nothing-to-place-text c cells)) (str ", "))) data))))

(defn quote-of [c]
  (let [{:keys [cells trouble]} (judged-plan c)
        open (if trouble [] (owed cells))
        n (count open)
        s (* n (settings/get settings ::cell-s))
        items (frequencies (keep :item (vals (into {} (map (juxt :pos identity)) open))))
        ;; a door, gate or trapdoor out of sight for 10 s reads unknown (owed), so the done check may find a built one owed
        ;; where this quote, read a moment later, sees it: the quote allows for them, else a :least call ends at once
        flaky (if trouble 0 (count (filter #(and (= :match (:answer %)) (not (:companion %)) (some->> (:found %) (re-find plan/opening-pattern))) cells)))]
    {:uses (cond-> {:job-time [(* 0.5 s) s (* 4 s)]}
             (seq items) (assoc :items (update-vals items (fn [k] [0 k k]))))
     :done [0 n (+ n flaky)]}))

;; ------------------------------------------------------------------ steps

(defn adopt-scaffold!
  "A ledger block standing where the plan wants it is the plan's wall now: out of the ledger, so no cleanup digs it."
  [c cells]
  (when-let [l (ledger/without-cells (ctx/view c) (set (map :pos (filter #(= :match (:answer %)) cells))))]
    (ledger/remember! c l)))

(defn ^:async build-step!
  "One step of the build (:again, or :continue while a walk waits): place or dig what is in reach, else walk, else finish."
  [c cells]
  (let [_ (adopt-scaffold! c cells)
        _ (ctx/update-mem! c retry-walks (u/self-pos c) false)
        _ (ctx/update-mem! c #(reduce (fn [m cell] (cond-> m (not (contains? (:given-up m) (:pos cell))) (assoc-in [:given-up (:pos cell)] :unreachable)))
                                      % (covered (:primitives c) cells (js/Math.floor (:y (u/self-pos c))) (set (:bad-stands (ctx/mem c))))))
        closed #(merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        wanted (buildable cells (storage/carried-totals (u/inventory (:primitives c))) (closed))
        digs (diggable c (digging cells) (closed))
        given-up (closed)
        lowest-of #(when (seq %) (apply min (map (fn [x] (get (:pos x) 1)) %)))
        ;; a wrong block still to dig keeps the layers above it open: its cell has no stand left once they close
        dig-y (lowest-of digs)
        placeable-now (cond->> (placeable c (permitted c wanted))
                        dig-y (filter #(<= (get (:pos %) 1) dig-y)))
        layer-of (fn [cs] (let [y (lowest-of cs)] (filter #(= y (get (:pos %) 1)) cs)))
        held (set (filter #(shuts-out? c % wanted) (in-reach c placeable-now)))
        todo (let [open (remove held placeable-now)] (if (seq open) open placeable-now))
        layer (layer-of todo)
        mouth (:mouth (ctx/mem c))
        fcell (let [b (u/self-pos c)] (mapv js/Math.floor [(:x b) (:y b) (:z b)]))
        dists (when mouth (retreat-dists (:primitives c) mouth cells))
        near (cond->> (filter #(place-here? c cells %) (remove #(shut-in? c %) (in-reach c layer)))
               (not= :up (scaffold/phase c)) (remove #(contains? (:needs-scaffold (ctx/mem c)) (:pos %)))
               ;; filling a block cut into rock backs out toward the mouth: the deepest cell goes first
               ;; and no cell nearer the mouth than the one the body stands in, which it leaves by
               mouth (fill-order dists fcell (some #(= fcell (:pos %)) wanted)))
        mine (body-cells (u/self-pos c))
        _ (ctx/update-mem! c update :stood (fnil into #{}) mine)
        feet (first (sort mine))
        on-ground (not-any? #(= (update (:pos %) 1 inc) feet) cells)
        underfoot (when on-ground (first (filter #(mine (:pos %)) wanted)))
        ;; standing on a cell of the plan itself, the body has no ground to leave from until nothing else is left to place
        standing (let [cell (first (filter #(mine (:pos %)) wanted))]
                   (when (and cell (not= (:pos cell) (:stepped-off (ctx/mem c)))) cell))
        dig-near (remove #(shut-in? c %) (in-dig-reach c digs))
        nearest #(first (sort-by (fn [cell] (u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos cell)))) %))]
    (cond
      (<= (settings/get settings ::budget-streak) (:budget-streak (ctx/mem c) 0))
      (ctx/end c :nothing-possible :unreachable
               {:placed (:placed (ctx/mem c) 0)
                :text (str "build of " (:plan (:args c)) " stopped: " (:budget-streak (ctx/mem c)) " cells in a row had no stand the body could walk to (the search ran out of its node budget each time)")})
      (scaffold/working? c) (await (scaffold/step! c (give-up)))
      (scaffold/stale? c) (do (scaffold/begin-clean! c) :again)
      (:leave (ctx/mem c)) (do (trace! c :leave! (:leave (ctx/mem c))) (await (leave! c)))
      (and (< (:off-bed (ctx/mem c) 0) (settings/get settings ::off-bed-tries)) (off-low-floor c))
      (do (ctx/update-mem! c #(-> % (assoc :leave (off-low-floor c) :leave-range 0) (update :off-bed (fnil inc 0)))) :again)
      (and underfoot (not= (:pos underfoot) (:stepped-off (ctx/mem c))) (or (not mouth) (and (empty? near) (empty? dig-near))))
      (do (ctx/update-mem! c assoc :leave (exit-of c cells) :stepped-off (:pos underfoot))
          :again)
      (seq near) (do (await (watch/watch! c {}))
                     (warn-way-in! c cells near)
                     (loop [left near]
                       (if (seq left)
                         (let [_ (trace! c :place-one! (:pos (first left)))
                               seal (seal-price c cells (:pos (first left)))
                               placed-before (:placed (ctx/mem c) 0)]
                           (cond
                             (and seal (nil? (:cost seal)))
                             (do (trace! c :seals-in (:pos (first left)))
                                 (ctx/update-mem! c #(-> (count-fail % (:pos (first left)) :unreachable (give-up))
                                                         (assoc :leave (exit-of c cells))))
                                 :again)
                             (= :continue (await (place-one! c (first left) (give-up)))) :continue
                             (and seal (> (:placed (ctx/mem c) 0) placed-before))
                             (do (ctx/update-mem! c assoc :leave (or (:exit seal) (exit-of c cells)) :sealed true) :again)
                             :else (recur (rest left))))
                         :again)))
      (seq dig-near) (loop [left dig-near]
                       (if (seq left)
                         (if (= :continue (do (trace! c :dig-one! (:pos (first left))) (await (dig-one! c (first left) (give-up)))))
                           :continue
                           (recur (rest left)))
                         :again))
      (= :up (scaffold/phase c)) (do (scaffold/finish-up! c cells (into todo (unseen cells given-up)) (nearest layer) (give-up)) :again)
      (seq todo) (await (reach-cell! c cells (nearest layer)))
      (seq digs) (await (walk-to! c cells (nearest digs)))
      (seq (support-looks c wanted)) (await (look-for-support! c cells (support-looks c wanted)))
      (seq (unseen cells given-up)) (await (reach-cell! c cells (nearest (unseen cells given-up))))
      standing (do (ctx/update-mem! c assoc :leave (exit-of c cells) :stepped-off (:pos standing))
                   :again)
      :else (let [before (ctx/mem c)]
              (ctx/update-mem! c retry-walks (u/self-pos c) true)
              (if (= before (ctx/mem c)) (finish! c cells) :again)))))

(defn own-bed-near?
  "Whether a bed this body put down (scaffold ledger purpose :bed) still stands within ::own-bed-radius and no cleanup holds it."
  [c]
  (let [view (ctx/view c)
        held (ledger/held-cells view)
        me (u/self-pos c)]
    (boolean (some (fn [{:keys [cell item]}]
                     (and (not (held cell)) (<= (u/dist me (u/cell-pos cell)) (settings/get settings ::own-bed-radius))
                          (ledger/same-block? item (u/seen-name (:primitives c) (u/cell-pos cell)))))
                   (filter #(= :bed (:purpose %)) (ledger/open-entries view))))))

(defn ^:async take-own-bed!
  "Break the bed this body put down and pick it up (jobs.access.cleanup): :continue while that waits, else :again."
  [c]
  (let [r (waits/waiting-out c (await (ctx/call c :take-bed 'jobs.access.cleanup
                                                {:of :all :purpose #{:bed} :near {:pos (u/self-pos c) :radius (settings/get settings ::own-bed-radius)} :ignore-zones? true
                                                 :checks {:fit? '(always) :safe? '(not (threatened))}})))]
    (if (= :continue r)
      :continue
      (do (sh-spawn/retract-bed! c (:removed (:data r))) :again))))

(defn site-of
  "The cell at the middle of the plan's cells, on their lowest layer."
  [cells]
  (let [ps (map :pos cells)
        mid (fn [i] (js/Math.round (/ (+ (apply min (map #(% i) ps)) (apply max (map #(% i) ps))) 2)))]
    (when (seq ps) [(mid 0) (apply min (map #(% 1) ps)) (mid 2)])))

(defn ^:async fetch-step!
  "Fetch what the plan lacks (jobs.lib.need, a search worth all the plan's items) when a fetch is due: :continue while the child waits on the world, :again
  once it ended (a failed one, a limit too, is booked), nil when nothing is due. A bed the plan lacks is first taken up
  from where the body put its own down, when one stands near."
  [c]
  (when-let [w (plan/problem c)]
    (let [bed? (and (= :need (:reason w)) (wood/bed? (:item w)))]
      (if (and bed? (need/fetch-opts c) (own-bed-near? c))
        (await (take-own-bed! c))
        (let [cells (:cells (planned c))
              o (assoc (need/fetch-opts c) :explore (:explore (:args c)) :for-plan (:plan (:args c)) :center (site-of cells)
                       :search-worth (cost/items-find-worth (frequencies (keep :item cells))))
              e (await (cond
                         bed? (need/blocks! c wood/bed-names (:count w) o)
                         (= :need (:reason w)) (need/item! c (:item w) (:count w) o)
                         :else (need/tool! c {:block (:block w)} o)))]
          (cond
            (= :continue e) :continue
            (= :done (:kind e)) :again
            :else (do (book-failed! c w e) :again)))))))

(defn hold-bed!
  "A bed the plan still has to place, and one carried: reserved for the call's life, so the night does not make it a camp bed."
  [c cells]
  (when (and (some #(wood/bed? (:item %)) (missing cells)) (some #(wood/bed? (:name %)) (u/inventory (:primitives c))))
    (reserve/hold! c (str (:id c) "-bed") {:tag :bed :why "bed for the plan"})))

(defn ^:async step [c]
  (let [{:keys [trouble wood cells]} (planned c)]
    (if trouble
      :continue
      (do (when (and wood (nil? (:wood (ctx/mem c)))) (ctx/update-mem! c assoc :wood wood))
          (hold-bed! c cells)
          (or (await (fetch-step! c))
              (await (light/light! c cells))
              (let [cells (:cells (planned c))]
                (settle-placing! c cells)
                (await (build-step! c cells))))))))

(defn ^:async round
  "The whole attempt: loop the steps until the plan is built or nothing more applies (finish); :continue only while a
  walk or a fetch waits on the world. Steps that go round again with nothing booked end it :stalled, so the body is
  never held with no act."
  [c0]
  (let [same (volatile! 0)
        c (assoc c0 :trace (volatile! nil))
        e (await (pace/steps! c (fn ^:async s []
                            (let [before (ctx/mem c)
                                  r (await (step c))]
                              (cond
                                (or (not= :again r) (not= before (ctx/mem c))) (do (vreset! same 0) r)
                                (< (vswap! same inc) (settings/get settings ::stall-steps)) r
                                :else (let [[branch cell] @(:trace c)
                                            text (str "build of " (:plan (:args c)) " stopped: " (settings/get settings ::stall-steps)
                                                      " steps in a row did nothing and booked nothing (last step " branch " at " (pr-str cell) ")")]
                                        (ctx/emit! c :build.stalled :warn {:plan (:plan (:args c)) :branch branch :cell cell :text text})
                                        (ctx/end c :nothing-possible :stalled
                                                 (assoc (result-data c (:cells (planned c))) :branch branch :cell cell :text text))))))))]
    (apply next-call/on-ending c e 'jobs.build.from-plan args
           (for [[item n] (get-in e [:data :short])] (next-call/call 'jobs.items.obtain {:item item :count n})))))


(defn ^:async cleanup
  "However the call ends: take the scaffold back."
  [c _]
  (adopt-scaffold! c (:cells (planned c)))
  (or (await (scaffold/take-back! c)) (ctx/end c :done nil {})))
