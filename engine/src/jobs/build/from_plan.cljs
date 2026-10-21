(ns jobs.build.from-plan
  (:require [jobs.lib.child :as child]
            [jobs.lib.build.wood :as wood]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.need :as need]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.build.plan :as plan :refer [retry-distance max-retries facing-of facing-ok? body-cells count-fail stands-for plan-trouble owed need-of shortage hazards decide rules-input refuse permitted judged no-zones planned how placeable misplaced missing covered digging unseen buildable problem]]
            [jobs.lib.build.scaffold :as scaffold]
            [jobs.build.from-plan.cells :refer [diggable in-reach in-dig-reach seals? shuts-out? exit-point off-low-floor buried? reaching support-looks]]
            [jobs.build.from-plan.book :refer [walk-fail retry-walks warn-way-in! place-one! settle-placing! dig-one! result-data finish! book-failed!]]
            [jobs.lib.build.text :refer [summary stopped-text assoc-fetch-failed]]))

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
  needed). Crops and trees are not this job's.
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
  cannot be reached, :give-up times is given up (:refused, :occupied, :no-item, :unreachable or :unloaded).
  Zones: every place is checked against zones and the footprints of the other active plans, when the cell is
  chosen and again before the place. A cell in a zone that does not allow :place, or in another plan's footprint,
  is refused for good (not counted as given up). It is listed in :refused [{:pos :reason :zone|:claim|:footprint|:hazard}].
  Water beside a cell is no obstacle by default. :accept names the fluid hazards taken (:fluid-adjacent,
  :lava-adjacent). A cell with an untaken one is refused as :hazard.
  Plan properties: :surface true puts each column of the plan on the ground the body has seen there (the column's lowest
  cell stands on the ground's top, the rest keep their height above it; an unseen column stays as planned), for a pen on a
  slope; :sturdy-ground true accepts a sturdy block on the ground of a rail line (plan.rail/ground) where the plan wants
  fill, not listed :wrong.
  done? holds when no cell is owed, none came out wrong; the engine then ends it :done.
  Ends with data {:placed n :missing [[x y z] ...] :short {item n} :given-up {[x y z] reason} :wrong [{:pos :found
  :want}] :refused [...] :fetch-failed {item {:failed :why}}}: done when nothing is missing (and no block came out wrong: nothing-possible :wrong-blocks) or the cells placed meet
  the call's :least (default :all); else with :text naming the missing count and what it left: nothing-possible
  :incomplete when cells were given up, condition-failed {:reason :no-items :needs} when items are short, condition-failed
  :refused when the rules refuse the rest, nothing-possible :shut-in. For a block placed in the wrong state, :found names the state it came out in.
  Events: build.gap (a fence or wall placed over air; warns) and build.way-in (cells the body stood in, above the plan's floor, filled with no door there; warns). Event texts name
  the count and the first few cells. The whole list is in :cells of the event and in the result.
  Start facts: no zone list read (unless :ignore-zones?) is a
  failed condition {:reason :no-zones}; a plan that is missing, unreadable or has no cells to
  build (in :part) is impossible {:reason :plan-trouble :why text} (a plan missing or unreadable waits), at the start and later in the call; at the start, with nothing to place, dig, fetch or
  look at, a failed condition {:reason :no-items :needs {item n}}.
  Wood: a generic wood want (jobs.lib.build.wood: planks, logs, wooden_stairs, wooden_slabs, wooden_doors, wooden_fences,
  fence_gates, wooden_trapdoors ...) is built in one species chosen for the plan (carried most, else the nearest log seen,
  else oak), booked, fetched by that species, and named in the result (:wood \"birch\"); a species name (oak_planks) stays exact.
  When cells are missing but none of their blocks is carried and none can be fetched, the call ends condition-failed :no-items with :needs.
  Material: while cells wait for an item not carried (and not given up or refused), each round fetches it, once no cell can be placed from what is carried (when the ways allow it,
  jobs.lib.need with :explore on: a jobs.items.obtain child for the missing count, walking off to look for what is unseen; an item whose fetch failed is not asked again in the call); a pickaxe block to dig without a pickaxe fetches the tool the same way
  (jobs.items.get-tool). With ways #{:carried}, or once a fetch failed, the build goes on with what is carried and ends
  short (the text and :fetch-failed name each failed fetch and why), a pickaxe block given up :no-tool.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :accept {:doc "fluid hazards of a cell taken: :fluid-adjacent (water beside; placing beside or into water seals and bridges), :lava-adjacent (lava beside; not taken by default: the body stands beside the cell), :falling-block (a dig under sand or gravel that falls in)" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :explore {:doc "walk off to look for blocks and tools that are not in sight when the plan lacks them" :spec boolean? :default true}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(def target [:plan :part])
(def defaults {:least :all :spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one build may take." :spec (a/int-in 1 nil)}
   ::give-up {:default 3 :doc "Refused places or failed walks after which a cell is given up." :spec (a/int-in 1 nil)}
   ::off-bed-tries {:default 3 :doc "Walks off a bed or bottom slab the body stands on, before the roof cells over it are given up." :spec (a/int-in 1 nil)}
   ::hole-depth {:default 2 :doc "Levels of walls round the body from its feet up that make it a body in a hole, whose walks may climb out." :spec (a/int-in 1 nil)}
   ::hole-walks {:default 2 :doc "Walks in a row that fail from a hole after which the build stops :shut-in." :spec (a/int-in 1 nil)}
   ::budget-streak {:default 4 :doc "Places or digs in a row (no cell placed or dug between) whose walk ended :unreachable :budget (a search that flooded the loaded land and found no stand) after which the build stops :unreachable: the next cells would flood the same land." :spec (a/int-in 1 nil)}
   ::look-range {:default 16 :doc "Cells farther than this from the eye are walked to, not looked at: they may be unloaded, which a look cannot tell." :spec (a/num-in 0 nil)}
   ::cell-s {:default 3 :doc "Seconds one cell of the plan is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(defn give-up [] (settings/get settings ::give-up))

(defn judged-plan
  "planned, judged once per check-cache window."
  [c]
  (check-cache/cached c :planned #(planned c)))

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
      :else (apply contract/ok :built (str "build of " plan " done: placed " placed) data))))

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

;; ------------------------------------------------------------------ keeping a way out

(defn ^:async leave!
  "One go-to call (child :leave) out of the plan's footprint to the :leave cell (within :leave-range, default 1); dropped once the walk ends (arrived
  or not: the next step judges where the body stands). :continue while the walk waits, else :again."
  [c]
  (let [[x y z] (:leave (ctx/mem c))
        r (await (child/call-walk* c :leave {:pos {:x x :y y :z z} :range (:leave-range (ctx/mem c) 1) :ways #{:walk :clear-seal} :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (when-not (= :continue r)
      (ctx/update-mem! c dissoc :leave :leave-range))
    (if (= :continue r) :continue :again)))

(defn in-hole?
  "Whether the body stands in a hole at least ::hole-depth deep (escape/pit-depth)."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    (<= (settings/get settings ::hole-depth) (escape/pit-depth (escape/block-at-of (:primitives c)) [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))))

(defn hole-blocked!
  "Count a failed walk from a hole; at ::hole-walks of them in a row end the build :shut-in, else :again."
  [c]
  (ctx/update-mem! c update :hole-blocked (fnil inc 0))
  (if (< (:hole-blocked (ctx/mem c)) (settings/get settings ::hole-walks))
    :again
    (ctx/end c :nothing-possible :shut-in
             {:placed (:placed (ctx/mem c) 0)
              :text (str "build of " (:plan (:args c)) " stopped: the body is shut in a hole and cannot climb out")})))

(defn ^:async walk-stand!
  "Walk to stand beside cell with go-near options opts; a walk that is blocked (the stand is then bad), or an arrival
  with the cell unseen (:unloaded, :buried) or out of reach, counts a failure. Resolves to go-near's answer."
  [c cell stand opts]
  (let [limit (give-up)
        w (await (near/go-near! c (zipmap [:x :y :z] stand) 0 opts))]
    (when (= :blocked w)
      (ctx/update-mem! c #(cond-> (-> (walk-fail % (:pos cell) :unreachable limit (u/self-pos c))
                                      (update :bad-stands (fnil conj []) stand))
                            (= :budget (:why (:data (near/last-walk c)))) (update :budget-streak (fnil inc 0)))))
    (when (#{:there :partial} w) (ctx/update-mem! c dissoc :budget-streak))
    (when (and (= :there w) (nil? (:found cell)))
      (let [pos (:pos cell)]
        ;; a cell outside the view cone is seen only by turning to it
        (when (dig-look/unknown? (:primitives c) pos) (await (dig-look/look-at! c pos)))
        (when (nil? (u/seen-block (:primitives c) (zipmap [:x :y :z] pos)))
          (ctx/update-mem! c walk-fail pos (if (buried? (:primitives c) pos) :buried :unloaded) limit (u/self-pos c)))))
    (when (and (= :there w) (:found cell) (empty? (if (:dig? cell) (in-dig-reach c [cell]) (in-reach c [cell]))))
      (ctx/update-mem! c walk-fail (:pos cell) :unreachable limit (u/self-pos c)))
    w))

(defn shut-in-walk?
  "Whether the last walk ended because the body is shut in (go-to refuses to dig or build out of a restricted walk)."
  [c]
  (boolean (:shut-in (:data (near/last-walk c)))))

(defn ^:async walk-to!
  "Walk to a stand cell beside cell (:continue while the walk waits, else :again); a cell that cannot be walked to, or is still out of reach (or unseen, as
  :unloaded) on arrival, counts a failure."
  [c cells cell]
  (let [body (u/self-pos c)
        planned (set (map :pos cells))
        bad (set (:bad-stands (ctx/mem c)))
        facing (when-not (or (:click cell) (:dig? cell)) (facing-of (:want cell)))
        all (stands-for (:primitives c) (:pos cell) (js/Math.floor (:y body)) facing planned bad)
        stands (or (seq (reaching c all (:pos cell))) all)
        stand (first (sort-by #(u/dist body (zipmap [:x :y :z] %)) stands))]
    (if-not stand
      (do (ctx/update-mem! c walk-fail (:pos cell) :unreachable (give-up) (u/self-pos c))
          :again)
      (let [shut? (or (in-hole? c) (shut-in-walk? c))
            w (await (walk-stand! c cell stand (cond-> {} (not shut?) (assoc :ways #{:walk :clear-seal}))))]
        (if (and (= :blocked w) (or shut? (shut-in-walk? c)))
          (hole-blocked! c)
          (do (ctx/update-mem! c dissoc :hole-blocked)
              (if (= :partial w) :continue :again)))))))


(defn ^:async reach-cell!
  "Walk to a stand for the buildable cell; where no ground stand within :reach of it exists, raise a temporary one (jobs.lib.build.scaffold)."
  [c cells cell]
  (let [body (u/self-pos c)
        facing (when-not (or (:click cell) (:dig? cell)) (facing-of (:want cell)))
        stands (plan/grounded-stands (:primitives c) (:pos cell) (js/Math.floor (:y body)) facing (set (map :pos cells)) (set (:bad-stands (ctx/mem c))))
        grounded (reaching c stands (:pos cell))]
    (or (when-not (seq grounded) (await (scaffold/start! c cells cell (result-data c cells))))
        (await (walk-to! c cells cell)))))

(defn ^:async look-for-support!
  "Look at the unseen floor of the nearest refused cell: from where the body stands when the cell is in reach, else walk to it first."
  [c cells looks]
  (let [[cell n] (first (sort-by #(u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos (first %)))) looks))]
    (if (<= (u/eye-dist (u/self-pos c) (:pos cell)) (plan/reach))
      (do (ctx/update-mem! c update :support-looked (fnil conj []) n)
          (await (dig-look/look-at! c n))
          :again)
      (await (walk-to! c cells cell)))))

(defn ^:async build-step!
  "One step of the build (:again, or :continue while a walk waits): place or dig what is in reach, else walk, else finish."
  [c cells]
  (let [_ (ctx/update-mem! c retry-walks (u/self-pos c) false)
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
        near (in-reach c layer)
        mine (body-cells (u/self-pos c))
        _ (ctx/update-mem! c update :stood (fnil into #{}) mine)
        feet (first (sort mine))
        on-ground (not-any? #(= (update (:pos %) 1 inc) feet) cells)
        underfoot (when on-ground (first (filter #(mine (:pos %)) wanted)))
        ;; standing on a cell of the plan itself, the body has no ground to leave from until nothing else is left to place
        standing (let [cell (first (filter #(mine (:pos %)) wanted))]
                   (when (and cell (not= (:pos cell) (:stepped-off (ctx/mem c)))) cell))
        dig-near (in-dig-reach c digs)
        nearest #(first (sort-by (fn [cell] (u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos cell)))) %))]
    (cond
      (<= (settings/get settings ::budget-streak) (:budget-streak (ctx/mem c) 0))
      (ctx/end c :nothing-possible :unreachable
               {:placed (:placed (ctx/mem c) 0)
                :text (str "build of " (:plan (:args c)) " stopped: " (:budget-streak (ctx/mem c)) " cells in a row had no stand the body could walk to (the search ran out of its node budget each time)")})
      (scaffold/working? c) (await (scaffold/step! c (give-up)))
      (scaffold/stale? c) (do (scaffold/begin-clean! c) :again)
      (:leave (ctx/mem c)) (await (leave! c))
      (and (< (:off-bed (ctx/mem c) 0) (settings/get settings ::off-bed-tries)) (off-low-floor c))
      (do (ctx/update-mem! c #(-> % (assoc :leave (off-low-floor c) :leave-range 0) (update :off-bed (fnil inc 0)))) :again)
      (and underfoot (not= (:pos underfoot) (:stepped-off (ctx/mem c))))
      (do (ctx/update-mem! c assoc :leave (exit-point cells (u/self-pos c)) :stepped-off (:pos underfoot))
          :again)
      (seq near) (do (await (watch/watch! c {}))
                     (warn-way-in! c cells near)
                     (loop [left near]
                       (if (seq left)
                         (if (seals? c (:pos (first left)))
                           (do (ctx/update-mem! c #(-> (count-fail % (:pos (first left)) :unreachable (give-up))
                                                       (assoc :leave (exit-point cells (u/self-pos c)))))
                               :again)
                           (if (= :continue (await (place-one! c (first left) (give-up))))
                             :continue
                             (recur (rest left))))
                         :again)))
      (seq dig-near) (loop [left dig-near]
                       (if (seq left)
                         (if (= :continue (await (dig-one! c (first left) (give-up))))
                           :continue
                           (recur (rest left)))
                         :again))
      (= :up (scaffold/phase c)) (do (scaffold/finish-up! c cells todo (nearest layer) (give-up)) :again)
      (seq todo) (await (reach-cell! c cells (nearest layer)))
      (seq digs) (await (walk-to! c cells (nearest digs)))
      (seq (support-looks c wanted)) (await (look-for-support! c cells (support-looks c wanted)))
      (seq (unseen cells given-up)) (await (walk-to! c cells (nearest (unseen cells given-up))))
      standing (do (ctx/update-mem! c assoc :leave (exit-point cells (u/self-pos c)) :stepped-off (:pos standing))
                   :again)
      :else (let [before (ctx/mem c)]
              (ctx/update-mem! c retry-walks (u/self-pos c) true)
              (if (= before (ctx/mem c)) (finish! c cells) :again)))))

(defn ^:async fetch-step!
  "Fetch what the plan lacks (jobs.lib.need) when a fetch is due: :continue while the child waits on the world, :again
  once it ended (a failed one, a limit too, is booked), nil when nothing is due."
  [c]
  (when-let [w (plan/problem c)]
    (let [o (assoc (need/fetch-opts c) :explore (:explore (:args c)) :for-plan (:plan (:args c)))
          bed? (and (= :need (:reason w)) (wood/bed? (:item w)))
          e (await (cond
                     bed? (need/blocks! c wood/bed-names (:count w) o)
                     (= :need (:reason w)) (need/item! c (:item w) (:count w) o)
                     :else (need/tool! c {:block (:block w)} o)))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else (do (book-failed! c w e) :again)))))

(defn ^:async step [c]
  (let [{:keys [trouble wood]} (planned c)]
    (if trouble
      :continue
      (do (when (and wood (nil? (:wood (ctx/mem c)))) (ctx/update-mem! c assoc :wood wood))
          (or (await (fetch-step! c))
              (let [cells (:cells (planned c))]
                (settle-placing! c cells)
                (await (build-step! c cells))))))))

(defn ^:async round
  "The whole attempt: loop the steps until the plan is built or nothing more applies (finish); :continue only while a
  walk or a fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))

(defn ^:async cleanup
  "However the call ends: take the scaffold back."
  [c _]
  (or (await (scaffold/take-back! c)) (ctx/end c :done nil {})))
