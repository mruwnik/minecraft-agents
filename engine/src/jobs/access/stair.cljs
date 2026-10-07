(ns jobs.access.stair
  (:require [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.escape :as escape]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [engine.path.executor :as executor]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]))

(def doc
  "Dig a 1-wide stair :down or :up along :heading from where the body stands, :steps steps or to feet height :y.
  The check waits (:no-tool, :no-free-slot) when the next dig lacks a pickaxe or room for the drop.

  A step cuts three cells, top first, so the body can walk one block forward and one down (or up) with full
  headroom. Then the body walks into the step (a go-to child). Before cutting the next step it plans the
  way back to the stair's first cell on a fresh pathWorld. The way must be whole and walkable by the executor
  (no gap, door or swim), else the stair stops :no-way-back.

  Cells are read as the body senses them (felt, seen, remembered). A cell it has not seen is taken for stone (dig to
  see): the stair looks at each step before judging it and at each cell it dug, and the stops below fire on what is
  then seen.

  A step's floor must be solid, the cell under it neither air nor fluid (:cave-below), and everything loaded.
  A floor of air gets a carried building block (blocks/building-blocks, never an ore or valuable) after the
  rules' may-place?. The cell under a placed floor is not judged. With no such block: :no-floor with :filler
  :none. A lava or water floor is never bridged.

  Each step is judged when chosen and each cell again right before its dig. Stops:
  - Access refusals with their reason: :zone :claim :footprint :not-loaded :no-zones. :ignore-zones? skips all
    but :not-loaded.
  - :fluid-in-cut: a cut cell holds a fluid.
  - :crop: a crop, stem or farmland in a cut cell (never dug, zone or not).
  - :undercuts-way: a cut cell is the floor of a stair this body cut earlier (memory :stair-way, written at every
    end with the floors of the steps walked).
  - :unbreakable: bedrock, barrier, portal frames, command blocks.
  - :no-tool: a pickaxe block with no pickaxe carried.
  - :inventory-full: the drop has no room.
  - :refills: a cell refilled 3 times.
  - :hazard: a hazard not in :accept (below).
  - :off-stair: the body is off the stair line.
  - :lava-exposed: lava beside an open cell of the step (or in one) with :on-lava :stop.
  - :lava-unsealed: that lava could not be filled (:on-lava :seal, the default): the place child's outcome in :place,
    or still lava after max-seals places. The body first steps back to the stair's cell before (:backed-off) and
    digs no more.

  Exposed lava (beside the feet, the head or an open cut cell, or in one) is filled with a building block
  (blocks/building-blocks, a jobs.blocks.place child; it fetches one when :fetch allows), sources behind first, one
  stair.sealed info each; then the stair goes on. Lava seen beside a cell not yet dug is a hazard as below.

  Hazards are the rules' (one per fluid beside) plus a falling block over the top cut of the next column.
  :accept is a set of :water :lava :falling-block :under-feet, default #{}. Water beside the cut is not taken
  by default (it can flow into the cut and onto the body's cell, which the walker cannot leave). Lava never is
  in practice, and a falling block would land on the body or refill the cut. :under-feet never comes up, since
  the stair never digs the block it stands on.

  One call cuts the whole stair; it yields :continue only while a fetch or walk child waits on the world, or after
  max-steps digs, steps and fetch rounds. The body's cell is the progress: a resumed call finds its step from where
  the body stands on the stair line. Dug cells are left and recorded.

  Hands over {:status :done|:stopped :reason kw :steps n :at [x y z] :dug [{:cell :block}]} plus detail (:cell
  :hazards :zone :walk ...), also as a stair.done info or stair.stopped warn event.

  :fetch (default true; jobs.lib.fetch; false waits :no-tool): the :no-tool wait is not waited out; the call runs jobs.items.get-tool
  for the block (child :fetch) first, then walks back to the cell it stood on (child :fetch-back) and goes on. A
  parent's stair child does not fetch.")

(def args
  {:dir {:doc ":down or :up" :default :down}
   :heading {:doc ":north :east :south or :west" :default nil}
   :steps {:doc "steps to cut; or give :y" :default nil}
   :y {:doc "feet height to end at, instead of :steps" :default nil}
   :accept {:doc "hazards taken: #{:water :lava :falling-block :under-feet}" :default #{}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :note {:doc "a map: each cell dug is written to the tidy ledger at once with it (jobs.lib.escape/note-hole!; go-to's escalation)" :default nil}
   :fetch {:doc "get a missing pickaxe instead of waiting :no-tool (jobs.lib.fetch), and a block to seal lava with: true, a set of kinds or a map of limits" :default true}
   :on-lava {:doc "exposed lava: :seal (fill it with a building block, then go on) or :stop" :default :seal}})

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})
(def rises {:down -1 :up 1})
(def max-cell-digs 3)
(def max-steps "Digs, steps, bridges and fetch rounds of one call before it gives the round back with :continue." 400)
(def stack-size 64)
(def unbreakable #{"bedrock" "barrier" "end_portal_frame" "end_portal" "nether_portal" "command_block" "structure_block" "jigsaw"})

(defn add [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])

(defn delta
  "One step's move [dx dy dz]."
  [dir heading]
  (let [[dx dz] (headings heading)] [dx (rises dir) dz]))

(defn step-cells
  "The cells of the step from feet: {:next feet cell after the step, :cut cells to clear top first, :floor, :under}."
  [feet dir heading]
  (let [d (delta dir heading)
        n (add feet d)
        cut (if (= :down dir)
              [(add n [0 2 0]) (add n [0 1 0]) n]
              [(add feet [0 2 0]) (add n [0 1 0]) n])]
    {:next n :cut cut :floor (add n [0 -1 0]) :under (add n [0 -2 0])}))

(defn stair-index
  "i when feet is the stair's cell after i steps from origin, 0 <= i <= steps; else nil."
  [origin feet dir heading steps]
  (let [[dx dy dz] (delta dir heading)
        i (+ (* dx (- (feet 0) (origin 0))) (* dz (- (feet 2) (origin 2))))]
    (when (and (<= 0 i steps) (= feet (add origin [(* i dx) (* i dy) (* i dz)])))
      i)))

(def crop-names
  "Blocks a stair never cuts: a farm's crops, stems and the farmland they stand on (the farm is another's work even
  when no zone says so)."
  #{"wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "attached_melon_stem" "attached_pumpkin_stem"
    "nether_wart" "sweet_berry_bush" "torchflower_crop" "pitcher_crop" "farmland"})

(defn hazard-key
  "The :accept key of a hazard: a fluid beside is :lava or :water (bubble columns are water)."
  [{:keys [reason fluid]}]
  (if (= :fluid-adjacent reason) (if (= "lava" fluid) :lava :water) reason))

(defn stair-hazards
  "The hazard the stair adds to the rules' for cell: a falling block over it, unless that cell is cut too (the rules
  see one only over the body's own column; the body walks into the next one)."
  [block-at cell cut]
  (let [above (add cell [0 1 0])
        n (block-at above)]
    (when (and (rules/falling? n) (not (some #{above} cut)))
      [{:reason :falling-block :block n :at above}])))

(defn cell-verdict
  "The dig verdict of cell: the rules' refusal, else {:ok true :hazards [...]} with the stair's hazards added."
  [in cut]
  (let [v (rules/may-dig? in)]
    (if-not (:ok v)
      v
      (let [hs (vec (distinct (concat (:hazards v) (stair-hazards (:block-at in) (:cell in) cut))))]
        (cond-> {:ok true} (seq hs) (assoc :hazards hs))))))

(def way-policy {:cap 30 :ttl :forever})

(defn way-floors
  "The floors of the steps a stair walked (not the start's own ground), from its origin, direction and steps reached."
  [origin dir heading steps]
  (let [d (delta dir heading)]
    (vec (for [i (range 1 (inc (or steps 0)))] (add (add origin (mapv #(* i %) d)) [0 -1 0])))))

(defn known-floor
  "The first cut cell that is a floor of a stair the body made earlier (ways: the set of those floors) and is still
  solid as the body sees it now (a floor dug away since, or put back as air, no longer carries a way)."
  [block-at cut ways]
  (first (filter #(and (contains? ways %) (rules/solid-floor? block-at %)) cut)))

(defn ways-of
  "The set of floors of the stairs this body cut earlier in its current dimension (memory :stair-way)."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (mapcat #(when (= dim (:dim (:data %))) (:floors (:data %))) (ctx/entries c :stair-way)))))

(defn stop-of
  "Why the step cannot go on, as {:reason ...detail}, or nil when every cell may be cut.
  in: the rules' input without :cell. cells :bridged? true: the floor was placed by the stair, what is under it is not judged; :up? true: it never is (a stair up
  digs no floor)."
  [{:keys [block-at] :as in} {:keys [cut floor under bridged? up? ways]} accept]
  (let [floor-cell (known-floor block-at cut (into (set (:ways in)) ways))
        fluid-cell (first (filter #(rules/fluids (block-at %)) cut))
        crop-cell (first (filter #(crop-names (block-at %)) cut))]
    (cond
      (some #(nil? (block-at %)) (conj cut floor under))
      {:reason :not-loaded :cell (first (filter #(nil? (block-at %)) (conj cut floor under)))}
      fluid-cell {:reason :fluid-in-cut :cell fluid-cell :fluid (block-at fluid-cell)}
      crop-cell {:reason :crop :cell crop-cell :block (block-at crop-cell)}
      floor-cell {:reason :undercuts-way :cell floor-cell :block (block-at floor-cell)
                  :why "the cell is the floor of a stair this body cut earlier; cutting it breaks the way"}
      (not (rules/solid-floor? block-at floor)) {:reason :no-floor :cell floor :block (block-at floor)}
      (and (not bridged?) (not up?) (let [n (block-at under)] (or (rules/air n) (rules/fluids n))))
      {:reason :cave-below :cell under :block (block-at under)}
      :else
      (some (fn [cell]
              (when-not (rules/air (block-at cell))
                (let [v (cell-verdict (assoc in :cell cell) cut)]
                  (cond
                    (not (:ok v)) (assoc (dissoc v :ok) :cell cell)
                    (not-every? (comp accept hazard-key) (:hazards v))
                    {:reason :hazard :cell cell :hazards (:hazards v)}))))
            cut))))

(defn room-for?
  "A free slot, or a carried stack of item with room."
  [p item]
  (or (pos? (u/free-slots p))
      (some #(and (= item (:name %)) (< (:count %) stack-size)) (u/inventory p))))

(defn no-tool?
  "Whether block needs a tool to clear (jobs.lib.tools/needs-tool-to-clear?) and none is carried."
  [p block]
  (tools/needs-tool-to-clear? p block))

(defn access-world
  "The social half of the rules' input (jobs.lib.access/zone-input): zones, claims, footprints, the body's name and
  the clock, and the job's :ignore-zones? arg."
  [c]
  (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c)) :own-plans-ok? true}))

(defn feet-of
  "The cell [x y z] the body stands on (jobs.lib.reach/standing-cell: the planner's start on a block's edge)."
  [c]
  (let [{:keys [x y z]} (reach/standing-cell (:primitives c))]
    [x y z]))

(def hidden-guess "What a cell the body has not sensed is taken for: rock, so it is dug to see." "stone")

(defn sensed-at
  "A block-at fn [x y z] -> name over what the body senses (jobs.lib.util/sensed): guess for a cell it has not sensed,
  nil when the cell is not loaded (the rules' :not-loaded)."
  [p guess]
  (fn [[x y z]] (when-let [b (u/sensed p {:x x :y y :z z})] (if (true? (.-unknown b)) guess (.-name b)))))

(defn rules-in
  "The rules' input at feet: :block-at reads unsensed cells as hidden-guess; :column-at reads them as air (a column
  scanned from the sky down for a stand, jobs.access.tunnel/surface)."
  [c feet]
  (merge {:block-at (sensed-at (:primitives c) hidden-guess) :column-at (sensed-at (:primitives c) "air")
          :feet feet :ledger #{} :ways (ways-of c)}
         (access-world c)))

(defn ^:async look-at!
  "Turn the head to cell's centre, so perception glances it and its 6 neighbours; nothing for primitives that do not
  sense (they read blockAt)."
  [c [x y z]]
  (when (some? (.-sensedAt (:primitives c)))
    (await (ctx/act c :look (clj->js {:pos {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}})))))

(defn unknown?
  "Whether the body has not sensed cell [x y z] (loaded, never seen or too old to trust)."
  [p [x y z]]
  (true? (some-> (u/sensed p {:x x :y y :z z}) .-unknown)))

(defn ^:async look-ahead!
  "Once per step from feet (memory :looked): look at the next cell, then at each cut cell still unknown, so what a
  player would see of the step is seen when it is judged."
  [c feet next cut]
  (when-not (= [feet next] (:looked (ctx/mem c)))
    (ctx/update-mem! c assoc :looked [feet next])
    (await (look-at! c next))
    (loop [cells cut]
      (when-let [cell (first cells)]
        (when (unknown? (:primitives c) cell) (await (look-at! c cell)))
        (recur (rest cells))))))

(defn ^:async see-round!
  "After a dig of cell: look into it, then at each neighbour still unknown, the faces the dig laid open."
  [c cell]
  (await (look-at! c cell))
  (loop [ds rules/neighbour-deltas]
    (when-let [d (first ds)]
      (let [n (add cell d)]
        (when (unknown? (:primitives c) n) (await (look-at! c n))))
      (recur (rest ds)))))

(defn ^:async peek!
  "Before digging cell: when it is still unknown, look at it once (memory :peeked) and give :again so the step is
  judged on what shows; else nil."
  [c cell]
  (when (and (unknown? (:primitives c) cell) (not= cell (:peeked (ctx/mem c))))
    (ctx/update-mem! c assoc :peeked cell)
    (await (look-at! c cell))
    :again))

(defn exposed-lava
  "The lava cells a step lays open: lava beside the feet, the head or a cut cell that is open (air or fluid) now,
  then lava in one of those; sources behind first."
  [block-at feet cut]
  (let [open (into [feet (add feet [0 1 0])] (filter #(let [n (block-at %)] (or (rules/air n) (rules/fluids n))) cut))
        open? (set open)
        lava? #(= "lava" (block-at %))
        beside (for [o open d rules/neighbour-deltas :let [n (add o d)] :when (and (not (open? n)) (lava? n))] n)]
    (distinct (concat beside (filter lava? open)))))

(def max-seals "Places per lava cell before the seal counts as failed." 3)

(defn ^:async seal!
  "Fill lava cell with a building block (a jobs.blocks.place child, fetching one when the job's :fetch allows). :again
  once placed (event kind, info), :continue while the child waits on the world, else the stop :lava-unsealed."
  [c kind block-at cell]
  (let [tries (get-in (ctx/mem c) [:seals cell] 0)
        [x y z] cell]
    (if (>= tries max-seals)
      {:reason :lava-unsealed :cell cell :tries tries}
      (do
        ;; a waiting child is resumed on the same cell: only its first round counts a try
        (ctx/update-mem! c #(cond-> (assoc % :sealing cell)
                              (not= cell (:sealing %)) (update-in [:seals cell] (fnil inc 0))))
        (let [outcome (await (blocks/place-cell! c {:x x :y y :z z} nil
                                                 {:any-of blocks/building-blocks :fetch (:fetch (:args c))
                                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
          (if (= :continue outcome)
            :continue
            (do (ctx/update-mem! c dissoc :sealing)
                (if (#{:placed :already} outcome)
                  (do (ctx/emit! c kind :info {:cell cell :now (block-at cell) :text (str "sealed lava at " (pr-str cell))})
                      :again)
                  {:reason :lava-unsealed :cell cell :place outcome}))))))))

(defn ^:async lava-step!
  "Exposed lava by the job's :on-lava: nil when there is none; :stop gives the stop :lava-exposed, else the first is
  sealed (seal!, event kind)."
  [c kind block-at feet cut]
  (when-let [cell (first (exposed-lava block-at feet cut))]
    (if (= :stop (:on-lava (:args c)))
      {:reason :lava-exposed :cell cell :fluid "lava"}
      (await (seal! c kind block-at cell)))))

(defn target-steps
  "Steps to cut from the args and the start feet, or {:error text}."
  [{:keys [dir heading steps y]} [_ fy _]]
  (cond
    (not (rises dir)) {:error "dir must be :down or :up"}
    (not (headings heading)) {:error "heading must be :north :east :south or :west"}
    (and (int? steps) (pos? steps)) steps
    (and (int? y) (pos? (* (rises dir) (- y fy)))) (js/Math.abs (- y fy))
    :else {:error "give :steps > 0, or :y beyond the feet in :dir"}))

(defn finish!
  "Hand over the result and end: :done with reason :done, else :stopped (warn)."
  [c reason detail]
  (let [m (ctx/mem c)
        steps (or (:at-step m) 0)
        result (merge {:status (if (= :done reason) :done :stopped) :reason reason :steps steps
                       :at (feet-of c) :dug (:dug m [])}
                      detail)]
    (ctx/result! c result)
    (when-let [{:keys [origin]} (when (pos? steps) m)]
      (let [{:keys [dir heading]} (:args c)]
        (ctx/remember! c :stair-way {:dim (.-dimension (.self (:primitives c)))
                                     :floors (way-floors origin dir heading steps)} way-policy)))
    (if (= :done reason)
      (ctx/emit! c :stair.done :info (assoc result :text (str "stair " (name (:dir (:args c))) " done, " steps " steps")))
      (ctx/emit! c :stair.stopped :warn
                 (assoc result :text (str "stair stopped after " steps " steps: " (name reason)
                                          (some->> (:cell detail) pr-str (str " at "))
                                          (some->> (:why detail) (str ": "))))))
    :done))

(defn record-dug
  "Memory after a dig intent: the cell is recorded as dug when it no longer holds its block."
  [m block-at]
  (if-let [{:keys [cell block]} (:digging m)]
    (cond-> (dissoc m :digging)
      (not= block (block-at cell)) (update :dug (fnil conj []) {:cell cell :block block}))
    m))

(defn ^:async way-back
  "nil when a whole plan the executor can walk leads from the body to origin on a fresh pathWorld, else the stop (a
  promise: the search yields to the event loop)."
  [c origin]
  (let [pw (wworld/path-world (:primitives c))]
    (if (nil? pw)
      {:reason :no-way-back :why :unsupported}
      (let [{:keys [r steps beyond]} (await (wplan/plan-within! c pw origin 0 walk/default-weight (wworld/body-policy c)))
            status (.-status r)
            stop (fn [refused] {:reason :no-way-back :why :refused :kind (:kind refused) :step (:at refused)})]
        (cond
          beyond (stop beyond)
          (not= "found" status) {:reason :no-way-back :why (keyword status) :planner (some-> (.-reason r) keyword)}
          :else (some-> (executor/refusal (wworld/body-policy c) steps) stop))))))

(defn need
  "What the next cell to dig lacks, as a reason map for ctx/wait, or nil: :no-tool (a pickaxe), :no-free-slot (no room
  for the drop). Judged for the step from the feet when the body is at the stair's start or on it; bad args and the
  rules' refusals are left to the round."
  [c]
  (let [{:keys [dir heading]} (:args c)
        p (:primitives c)
        feet (feet-of c)
        {:keys [origin target]} (ctx/mem c)
        on-stair? (or (nil? origin) (some? (stair-index origin feet dir heading target)))]
    (when (and on-stair? (rises dir) (headings heading))
      (let [block-at (:block-at (rules-in c feet))
            {:keys [cut]} (step-cells feet dir heading)
            block (some #(let [n (block-at %)] (when (and n (not (rules/air n)) (not (unbreakable n))) n)) cut)]
        (cond
          (nil? block) nil
          (no-tool? p block) {:reason :no-tool :tool (tools/needed-kind p block) :block block}
          (not (room-for? p (blocks/item-name {:block block}))) {:reason :no-free-slot :block block})))))

(defn refusal
  "Before anything is cut: the stop of the first step when a zone, claim or plan refuses its cut, else nil."
  [c]
  (let [{:keys [dir heading]} (:args c)
        {:keys [origin target]} (ctx/mem c)
        feet (feet-of c)]
    (when (and (or (nil? origin) (and (= feet origin) (empty? (:dug (ctx/mem c)))))
               (rises dir) (headings heading))
      (let [in (rules-in c feet)
            cells (-> (step-cells feet dir heading) (assoc :up? (= :up dir)))
            stop (stop-of in cells (set (:accept (:args c))))]
        (when (#{:zone :claim :footprint} (:reason stop)) stop)))))

(defn check
  "True, or a wait for what the next dig lacks (see need), or :refused when a zone, claim or plan refuses the first
  step. As a child (leave-tunnel, tunnel, dig-in) it runs and stops with the same reason in its result, which its
  parent reads."
  [c]
  (if-let [stop (refusal c)]
    (access/decline! c :stair.declined "stair" (assoc (access/refusal-fields [stop]) :reason :refused))
    (if-let [lack (need c)] (fetch/check c 'jobs.access.stair lack) true)))

(defn ^:async dig!
  "Equip the best tool, check the cell again, write the intent and dig it (a blocks.dig child). :continue (dug, go on), :yield (the child waits on the world), or a stop map."
  [c in cell cut accept]
  (let [p (:primitives c)
        block ((:block-at in) cell)
        tries (get-in (ctx/mem c) [:tries cell] 0)]
    (cond
      (>= tries max-cell-digs) {:reason :refills :cell cell :block block}
      (unbreakable block) {:reason :unbreakable :cell cell :block block}
      (no-tool? p block) {:reason :no-tool :cell cell :block block :tool (tools/needed-kind p block)}
      (not (room-for? p (blocks/item-name {:block block}))) {:reason :inventory-full :cell cell :block block}
      :else
      (do
        (await (tools/equip! c block))
        (let [v (cell-verdict (assoc in :cell cell) cut)
              block ((:block-at in) cell)]
          (cond
            (not (:ok v)) (assoc (dissoc v :ok) :cell cell)
            (not-every? (comp accept hazard-key) (:hazards v)) {:reason :hazard :cell cell :hazards (:hazards v)}
            (rules/air block) :continue
            :else
            (do
              ;; a waiting child is resumed on the same cell: only the first round of a dig counts a try
              (ctx/update-mem! c #(cond-> (assoc % :digging {:cell cell :block block} :counted cell)
                                    (not= cell (:counted %)) (update-in [:tries cell] (fnil inc 0))))
              (let [[x y z] cell
                    outcome (await (blocks/dig-cell! c {:x x :y y :z z} {:accept #{:fluid-adjacent :falling-block :under-feet}
                                                                         :ignore-zones? true}))]
                (if (= :continue outcome)
                  :yield
                  (do
                    (ctx/update-mem! c #(-> % (dissoc :counted) (record-dug (:block-at in))))
                    (when-let [tag (when (= :dug outcome) (:note (:args c)))] (escape/note-hole! c tag cell block))
                    (when (#{:dug :missing} outcome) (await (see-round! c cell)))
                    (case outcome
                      (:dug :missing) :continue
                      {:reason :dig-failed :cell cell :block block :dig outcome})))))))))))

(defn ^:async bridge!
  "Place carried filler on the missing floor of the step. :again, or a stop map."
  [c in {:keys [floor]}]
  (let [item (blocks/pick c blocks/building-blocks)
        verdict (rules/may-place? (assoc in :cell floor))
        [x y z] floor]
    (cond
      (nil? item) {:reason :no-floor :cell floor :block ((:block-at in) floor) :filler :none
                   :why (str "no floor and no filler block carried (" (str/join ", " (take 4 blocks/building-blocks)) " ...)")}
      (not (:ok verdict)) (assoc (dissoc verdict :ok) :cell floor)
      :else
      (do (await (ctx/act c :place #js {:pos #js {:x x :y y :z z} :item item}))
          (if (rules/air ((:block-at (rules-in c (feet-of c))) floor))
            {:reason :place-failed :cell floor :item item}
            (do (ctx/update-mem! c update :bridged (fnil conj #{}) floor)
                :again))))))

(defn ^:async walk-into!
  "Walk the body into cell [x y z] with a go-to child in slot (no escalation: the dig is this job's own): :continue
  while it walks, else the child's result."
  [c slot cell]
  (if (= :done (await (ctx/call-child c slot 'jobs.movement.go-to {:pos cell :range 0 :escalate false})))
    (ctx/child-result c slot)
    :continue))

(defn ^:async step!
  "Walk into the cut step. :again, :continue while the walk waits, or a stop map."
  [c next-feet]
  (let [r (await (walk-into! c :walk next-feet))]
    (cond
      (= :continue r) :continue
      (and (:arrived r) (= next-feet (feet-of c)))
      (do (ctx/emit! c :stair.step :info {:at next-feet :text (str "stepped to " (pr-str next-feet))})
          :again)
      :else {:reason :step-failed :cell next-feet :walk r})))

(defn ^:async work!
  "One bounded piece of the stair from the body's place on it: check the way back, dig one cell or take the step."
  [c]
  (let [{:keys [dir heading]} (:args c)
        accept (set (:accept (:args c)))
        {:keys [origin target checked]} (ctx/mem c)
        feet (feet-of c)
        in (rules-in c feet)
        i (stair-index origin feet dir heading target)]
    (ctx/update-mem! c record-dug (:block-at in))
    (cond
      (nil? i) {:reason :off-stair :cell feet :origin origin
                :offset (mapv - feet origin)
                :why (str "the body is " (pr-str (mapv - feet origin)) " from the stair's start " (pr-str origin)
                          ", off its line")}
      :else
      (do
        (ctx/update-mem! c assoc :at-step i)
        (if-let [stop (when (and (pos? i) (not= i checked)) (await (way-back c origin)))]
          stop
          (do
            (ctx/update-mem! c assoc :checked i)
            (if (= i target)
              (or (await (lava-step! c :stair.sealed (:block-at in) feet [])) :finished)
              (let [{:keys [next cut] :as cells} (-> (step-cells feet dir heading)
                                                           (as-> cs (assoc cs :up? (= :up dir)
                                                                 :bridged? (contains? (:bridged (ctx/mem c)) (:floor cs)))))
                    _ (await (look-ahead! c feet next cut))
                    stop (or (await (lava-step! c :stair.sealed (:block-at in) feet cut)) (stop-of in cells accept))]
                (if (and (= :no-floor (:reason stop)) (rules/air (:block stop)))
                  (await (bridge! c in cells))
                  (or stop
                    (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
                      (or (await (peek! c cell))
                          (let [r (await (dig! c in cell cut accept))] (case r :continue :again :yield :continue r)))
                      (await (step! c next)))))))))))))

(defn back-cell
  "The stair's cell one step back toward its origin from where the body stands, or nil at the origin or off the line."
  [c]
  (let [{:keys [dir heading]} (:args c)
        {:keys [origin target]} (ctx/mem c)
        i (when origin (stair-index origin (feet-of c) dir heading target))]
    (when (and i (pos? i))
      (add origin (mapv #(* (dec i) %) (delta dir heading))))))

(defn ^:async back-off!
  "After a failed seal: walk one step back up the stair (a go-to child), then finish with the stop, :backed-off true
  when the body got there."
  [c]
  (let [{:keys [stop cell]} (:backoff (ctx/mem c))
        r (when cell (await (walk-into! c :back cell)))]
    (if (= :continue r)
      :continue
      (finish! c (:reason stop) (assoc (dissoc stop :reason) :backed-off (boolean (and cell (= cell (feet-of c)))))))))

(defn ^:async next!
  "One piece of the stair: a fetch round, the start, a dig, a bridge, a seal or a step. :again, :continue (a child
  waits, or the next cell lacks a tool or room with nothing to fetch: the check waits) or :done."
  [c]
  (let [m (ctx/mem c)
        r (when-not (:backoff m) (await (fetch/step! c 'jobs.access.stair (need c) {:return? true})))]
    (cond
      (:backoff m) (await (back-off! c))
      r r
      (and (:origin m) (need c)) :continue
      (nil? (:origin m))
      (let [feet (feet-of c)
            target (target-steps (:args c) feet)]
        (if (map? target)
          (finish! c :bad-args {:why (:error target)})
          (do (ctx/update-mem! c assoc :origin feet :target target :dug [])
              :again)))
      :else
      (let [r (await (work! c))]
        (cond
          (#{:again :continue} r) r
          (= :finished r) (finish! c :done {})
          (= :lava-unsealed (:reason r)) (do (ctx/update-mem! c assoc :backoff {:stop r :cell (back-cell c)}) :again)
          :else (finish! c (:reason r) (dissoc r :reason)))))))

(defn ^:async round
  "The whole stair: next! until it ends, a pace between pieces; :continue after max-steps of them."
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (next! c) :continue)))))
