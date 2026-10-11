(ns jobs.access.stair
  (:require [jobs.lib.submitted :as submitted]
            [jobs.lib.child :as child]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [plan.shape :as shape]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.escape :as escape]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.lava :as lava]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.bridge :as bridge]
            [jobs.lib.look :as look]
            [jobs.lib.dig-look :refer [unknown? look-at! settle! see-round!]]
            [jobs.lib.tools :as tools]
            [jobs.lib.trip :as trip]
            [jobs.lib.walk :as walk]))

(def doc
  "Dig a 1-wide stair :down or :up along :heading (nil: the way the body faces) from where the body stands, :steps steps or to feet height :y.
  The check waits (:no-tool, :no-free-slot) when the next dig lacks a pickaxe or room for the drop.
  A stair down first stocks torches when under 8 are carried (jobs.lib.trip/torches-ready!; :stock-torches false skips it, as does :fetch off).

  A step cuts three cells, top first, so the body can walk one block forward and one down (or up) with full
  headroom. Then the body walks into the step (a go-to child). Before cutting the next step it plans the
  way back to the stair's first cell on a fresh pathWorld. The way must be whole and walkable by the executor
  (no gap, door or swim), else the stair stops :no-way-back.

  Cells are read as the body senses them (felt, seen, remembered). A cell it has not seen is taken for stone (dig to
  see): the stair looks at each step before judging it and at each cell it dug, and the stops below fire on what is
  then seen.

  A step's floor must be solid, the cell under it not a fluid, nor air when no building block is carried (:cave-below), and everything loaded.
  A floor of air gets a carried building block (blocks/building-blocks, never an ore or valuable) after the
  rules' may-place?, against a neighbour, or from the cell under it first when only that has a face (jobs.lib.bridge/bridge-plans; a plan whose face was unseen air gives way to the next); a cave floor with no block beside or under it stops :no-support-floor (the underside of the block over it cannot be aimed at).
  The cell under a placed floor is not judged. With no such block: :no-floor with :filler :none; with no face to place
  against at any of them (a void pocket): :no-support-floor with :cell, :tried and :why. A lava or water floor is never bridged. A go-to stair (:note) writes its placed floors to the scaffold ledger
  (purpose :stair-floor) for jobs.access.cleanup to take back once go-to's job ended (setting take-back-floors?).

  Each step is judged when chosen and each cell again right before its dig. Stops:
  - Access refusals with their reason: :zone :claim :footprint :not-loaded :no-zones. :ignore-zones? skips all
    but :not-loaded. A cell of a plan this body made refuses :footprint too unless :spare-own-builds is false.
  - :fluid-in-cut: a cut cell holds a fluid.
  - :crop: a crop, stem or farmland in a cut cell (never dug, zone or not).
  - :base-block: a chest, bed, furnace, table, door or other base block in a cut cell (never dug, anyone's).
  - :undercuts-way: a cut cell is the floor of a stair this body cut earlier (memory :stair-way, written at every
    end with the floors of the steps walked).
  - :unbreakable: bedrock, barrier, portal frames, command blocks.
  - :no-tool: a pickaxe block with no pickaxe carried.
  - :inventory-full: the drop has no room.
  - :refills: a cell refilled 3 times (with a :why when the block does not fall: something put it back).
  - :hazard: a hazard not in :accept (below).
  - :off-stair: the body is off the stair line.
  - :lava-exposed: lava beside an open cell of the step (or in one) with :on-lava :stop.
  - :floor-unseen: the floor of the step is still unseen once its cut is open and looked at once more.
  - :lava-unsealed: that lava could not be filled (:on-lava :seal, the default): the place child's outcome in :place,
    or still lava after max-seals places (a seal, then one reseal when lava flows back). The body first steps back to the stair's cell before (:backed-off) and
    digs no more.
  - :water-unplugged: a cell the stair dug turned out to touch seen water (:water not in :accept) and could not be filled.

  Water a dig lays open (a cell the stair dug now touches seen water, :water not in :accept) is plugged at once: the cell
  is filled with a building block (same child and fetch as the lava seal, event stair.plugged), and the stair stops :hazard
  with :plugged <cell>.

  Exposed lava (beside the feet, the head or an open cut cell, or in one) is filled with a building block
  (blocks/building-blocks, a jobs.blocks.place child; it fetches one when :fetch allows), sources behind first, one
  stair.sealed info each; then the stair goes on. Before it steps into an open cut with an unseen cell beside it, it
  waits a lava flow delay since the last dig and looks at the cut once more. Lava seen beside a cell not yet dug is a hazard as below.

  Hazards are the rules' (one per fluid beside) plus a falling block over the top cut of the next column.
  :accept is a set of :water :lava :falling-block, default #{}. Water beside the cut is not taken
  by default (it can flow into the cut and onto the body's cell, which the walker cannot leave). Lava never is
  in practice, and a falling block would land on the body or refill the cut. The stair never digs the block it
  stands on.

  One call cuts the whole stair; it yields :continue only while a fetch or walk child waits on the world, or after
  max-steps digs, steps and fetch rounds. The body's cell is the progress: a resumed call finds its step from where
  the body stands on the stair line. Dug cells are left and recorded.

  Before each step it digs the exposed ores within reach (:pick-up-ores, jobs.lib.ore-pick) and returns to its place.

  Hands over {:status :done|:stopped :reason kw :steps n :at [x y z] :dug [{:cell :block}] :ores [{:cell :block :got}]} plus detail (:cell
  :hazards :zone :walk ...), also as a stair.done info or stair.stopped warn event.

  :fetch (default true; jobs.lib.fetch; false waits :no-tool): the :no-tool wait is not waited out; the call runs jobs.items.get-tool
  for the block (child :fetch) first, then walks back to the cell it stood on (child :fetch-back) and goes on. A
  parent's stair child does not fetch.
  :spent-ok (default false): a dig whose every carried tool is down to 3 uses (jobs.lib.trip/spent-tool?) stops {:reason :tool-spent :tool <kind>}
  before the last tool breaks deep down; true digs on.
  :by-hand (default false): a block no carried tool clears is dug bare-handed anyway (slow, its drop lost): go-to's
  last resort when it has no pickaxe. Also done once the fetch of a tool for that block failed.")

(a/defargs args
  {:from {:doc "where the body stood at submit (jobs.mjs adds it): the job waits :moved-since-submit before its first round when the body is then over jobs.lib.submitted/limit away" :spec a/position? :default nil}
   :here {:doc "true: run where the body is, whatever :from says" :spec boolean? :default false}
   :dir {:doc ":down or :up" :spec #{:down :up} :default :down}
   :heading {:doc ":north :east :south or :west; nil: the way the body faces" :spec #{:north :east :south :west} :default nil}
   :steps {:doc "steps to cut; or give :y" :spec (a/int-in 1 nil) :default nil}
   :y {:doc "feet height to end at, instead of :steps" :spec (a/num-in nil nil) :default nil}
   :accept {:doc "hazards taken: #{:water :lava :falling-block}" :spec (a/coll-of #{:water :lava :falling-block}) :default #{}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :note {:doc "a map (go-to's escalation): each cell dug in another's zone, claim or plan is written to the tidy ledger at once with it (jobs.lib.escape/note-hole!), each floor placed to the scaffold ledger (take-back-floor?)" :spec map? :default nil}
   :fetch {:doc "get a missing pickaxe instead of waiting :no-tool (jobs.lib.fetch), and a block to seal lava with: true, a set of kinds or a map of limits" :spec fetch/option? :default true}
   :on-lava {:doc "exposed lava: :seal (fill it with a building block, then go on) or :stop" :spec #{:seal :stop} :default :seal}
   :by-hand {:doc "dig a block no carried tool clears bare-handed (slow, the drop is lost) instead of waiting :no-tool" :spec boolean? :default false}
   :spent-ok {:doc "dig on with tools down to jobs.lib.trip's ::spent-uses (3) uses instead of stopping :tool-spent before the last breaks; true for a way out" :spec boolean? :default false}
   :stock-torches {:doc "a stair down first gets torches when few are carried (jobs.lib.trip/torches-ready!, needs :fetch); false for a stair a job runs as its own child" :spec boolean? :default true}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach on the way; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}
   :spare-own-builds {:doc "a cell of a plan this body made is never dug (stops :footprint naming the plan); false: it may be" :spec boolean? :default true}
   :through {:doc "the id of a mineshaft plan the stair runs down: its footprint is no obstacle (jobs.lib.shafts)" :spec string? :default nil}})

(a/defargs settings
  {::max-cell-digs {:default 3 :doc "Digs of one cell before the stair stops :refills." :spec (a/int-in 0 nil)}
   ::max-fall-digs {:default 12 :doc "Digs of one cell a falling block (gravel, sand) keeps refilling before the stair stops :refills." :spec (a/int-in 0 nil)}
   ::max-steps {:default 400 :doc "Digs, steps, bridges and fetch rounds of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}
   ::take-back-floors? {:default true :doc "A floor block a go-to stair (:note) places goes into the scaffold ledger for jobs.access.cleanup to take back; false keeps it." :spec boolean?}})
(defn max-cell-digs [] (settings/get settings ::max-cell-digs))
(defn max-fall-digs [] (settings/get settings ::max-fall-digs))
(defn max-steps [] (settings/get settings ::max-steps))

(defn stair-index
  "i when feet is the stair's cell after i steps from origin, 0 <= i <= steps; else nil."
  [origin feet dir heading steps]
  (let [[dx dy dz] (shape/delta dir heading)
        i (+ (* dx (- (feet 0) (origin 0))) (* dz (- (feet 2) (origin 2))))]
    (when (and (<= 0 i steps) (= feet (shape/add origin [(* i dx) (* i dy) (* i dz)])))
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
  (let [above (shape/add cell [0 1 0])
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
  (let [d (shape/delta dir heading)]
    (vec (for [i (range 1 (inc (or steps 0)))] (shape/add (shape/add origin (mapv #(* i %) d)) [0 -1 0])))))

(defn hazards-why
  "The hazards in words: \"falling-block gravel at [x y z]; ...\"."
  [hazards]
  (str/join "; " (map (fn [h] (str (name (:reason h)) (some->> (:block h) (str " ")) (some->> (:at h) pr-str (str " at ")))) hazards)))

(defn known-floor
  "The first cut cell that is a floor of a stair the body made earlier (ways: the set of those floors) and is still
  solid as the body sees it now (a floor dug away since, or put back as air, no longer carries a way)."
  [block-at cut ways]
  (first (filter #(and (contains? ways %) (rules/solid-floor? block-at %)) cut)))

(defn stop-of
  "Why the step cannot go on, as {:reason ...detail}, or nil when every cell may be cut.
  in: the rules' input without :cell; its :unseen? (fn cell -> true when never sensed) makes a floor the body has not
  seen a stop :floor-unseen once the cut is open, not a guess. cells :bridged? true: the floor was placed by the stair, what is under it is not judged; :filler? true: air under a solid floor is no stop (the next floor is bridged); :up? true: it never is (a stair up
  digs no floor)."
  [{:keys [block-at] :as in} {:keys [cut floor under bridged? up? filler? ways]} accept]
  (let [floor-cell (known-floor block-at cut (into (set (:ways in)) ways))
        fluid-cell (first (filter #(rules/fluids (block-at %)) cut))
        crop-cell (first (filter #(crop-names (block-at %)) cut))
        unseen-cell (when-let [unseen? (:unseen? in)]
                      (when (and (every? #(rules/air (block-at %)) cut) (unseen? floor)) floor))]
    (cond
      (some #(nil? (block-at %)) (conj cut floor under))
      {:reason :not-loaded :cell (first (filter #(nil? (block-at %)) (conj cut floor under)))}
      fluid-cell {:reason :fluid-in-cut :cell fluid-cell :fluid (block-at fluid-cell)}
      crop-cell {:reason :crop :cell crop-cell :block (block-at crop-cell)}
      floor-cell {:reason :undercuts-way :cell floor-cell :block (block-at floor-cell)
                  :why "the cell is the floor of a stair this body cut earlier; cutting it breaks the way"}
      unseen-cell {:reason :floor-unseen :cell unseen-cell}
      (not (rules/solid-floor? block-at floor)) {:reason :no-floor :cell floor :block (block-at floor)}
      (and (not bridged?) (not up?) (let [n (block-at under)] (or (and (rules/air n) (not filler?)) (rules/fluids n))))
      {:reason :cave-below :cell under :block (block-at under)}
      :else
      (some (fn [cell]
              (when-not (rules/air (block-at cell))
                (let [v (cell-verdict (assoc in :cell cell) cut)]
                  (cond
                    (not (:ok v)) (assoc (dissoc v :ok) :cell cell)
                    (not-every? (comp accept hazard-key) (:hazards v))
                    {:reason :hazard :cell cell :hazards (:hazards v) :why (hazards-why (:hazards v))}))))
            cut))))

(defn by-hand?
  "Whether block is dug bare-handed: :by-hand asks for it, or the fetch of a tool for it failed (the way out must open
  even with no pickaxe to be had)."
  [c block]
  (and (tools/needs-tool-to-clear? (:primitives c) block)
       (or (:by-hand (:args c))
           (fetch/failed? c 'jobs.access.stair {:reason :no-tool :block block}))))

(defn lacks-tool?
  "A tool is needed and not carried, and the block is not dug by hand (by-hand?)."
  [c block]
  (and (not (by-hand? c block)) (tools/needs-tool-to-clear? (:primitives c) block)))

(defn drop-room?
  "Room for block's drop, or no drop to keep: a :by-hand dig with no tool that harvests it loses it."
  [c block]
  (or (by-hand? c block)
      (blocks/room-for? (:primitives c) (blocks/item-name {:block block}))))

(defn access-world
  "The social half of the rules' input (jobs.lib.access/zone-input): zones, claims, footprints, the body's name and
  the clock, and the job's :ignore-zones? arg."
  [c]
  (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c)) :own-plans-ok? (false? (:spare-own-builds (:args c))) :except (:through (:args c))}))

(defn rules-in
  "The rules' input at feet: :block-at reads unsensed cells as hidden-guess; :column-at reads them as air (a column
  scanned from the sky down for a stand, jobs.access.tunnel.line/surface)."
  [c feet]
  (merge {:floor-at (access/seen-at (:primitives c)) :block-at (access/sensed-at (:primitives c) blocks/hidden-guess) :column-at (access/sensed-at (:primitives c) "air")
          :feet feet :ledger #{} :ways (ledger/ways-of c) :keep-base? true}
         (access-world c)))

(defn judged-in
  "in for the step about to be taken: a floor the body has not seen stops it (:floor-unseen)."
  [c in]
  (assoc in :unseen? #(unknown? (:primitives c) %)))

(defn ^:async look-ahead!
  "Once per step from feet (memory :looked): look at the next cell and the floor under it, at each cut cell still unknown,
  and at the unknown neighbours of the feet and head, so what a player would see of the step is seen when it is judged."
  [c feet next cut]
  (when-not (= [feet next] (:looked (ctx/mem c)))
    (ctx/update-mem! c assoc :looked [feet next])
    (await (look-at! c next))
    (loop [cells (concat cut (map #(shape/add next [0 % 0]) [-1 -2])
                         (for [b [feet (shape/add feet [0 1 0])] d rules/neighbour-deltas] (shape/add b d)))]
      (when-let [cell (first cells)]
        (when (unknown? (:primitives c) cell) (await (look-at! c cell)))
        (recur (rest cells))))))

(defn ^:async judge!
  "stop-of for the step about to be taken. A floor still unseen once the cut is open gets one more look (memory
  :floor-looked) and is judged again, so a glance that missed is not a final stop."
  [c in cells accept]
  (let [stop (stop-of (judged-in c in) cells accept)
        cell (:cell stop)]
    (if (and (= :floor-unseen (:reason stop)) (not= cell (:floor-looked (ctx/mem c))))
      (do (ctx/update-mem! c assoc :floor-looked cell)
          (await (look-at! c cell))
          (stop-of (judged-in c in) cells accept))
      stop)))

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
  (let [open (into [feet (shape/add feet [0 1 0])] (filter #(let [n (block-at %)] (or (rules/air n) (rules/fluids n))) cut))
        open? (set open)
        lava? #(= "lava" (block-at %))
        beside (for [o open d rules/neighbour-deltas :let [n (shape/add o d)] :when (and (not (open? n)) (lava? n))] n)]
    (distinct (concat beside (filter lava? open)))))

(defn ^:async lava-step!
  "Exposed lava by the job's :on-lava: nil when there is none; :stop gives the stop :lava-exposed, else the first is
  sealed (jobs.lib.lava/seal!, event kind)."
  [c kind block-at feet cut]
  (when-let [cell (first (exposed-lava block-at feet cut))]
    (if (= :stop (:on-lava (:args c)))
      {:reason :lava-exposed :cell cell :fluid "lava"}
      (await (lava/seal! c kind block-at cell {:fetch (:fetch (:args c)) :ignore-zones? (:ignore-zones? (:args c))})))))

(defn exposed-water
  "The cells this stair dug (memory :dug) among the cut that are open now and touch seen water, each with the water
  cell: [cell water], water in the cell itself counted. Empty when :water is accepted."
  [block-at dug accept cut]
  (if (accept :water)
    []
    (let [dug? (set (map :cell dug))]
      (vec (for [cell cut :when (dug? cell)
                 :let [n (block-at cell)]
                 :when (or (rules/air n) (rules/fluids n))
                 w (cons cell (map #(shape/add cell %) rules/neighbour-deltas))
                 :when (= "water" (block-at w))]
             [cell w])))))

(defn ^:async water-step!
  "Water a dig laid open: a cell the stair dug that now touches seen water is filled with a building block at once
  (jobs.lib.lava/seal!, event stair.plugged), before the water pours in. nil when there is none; :continue while the
  place child waits; the stop {:reason :hazard :cell :plugged :hazards} once plugged, else {:reason :water-unplugged}."
  [c block-at accept cut]
  (when-let [[cell w] (first (exposed-water block-at (:dug (ctx/mem c)) accept cut))]
    (let [r (await (lava/seal! c :stair.plugged block-at cell {:fetch (:fetch (:args c)) :ignore-zones? (:ignore-zones? (:args c))}))]
      (cond
        (= :continue r) :continue
        (= :again r) {:reason :hazard :cell cell :plugged cell :hazards [{:reason :fluid-adjacent :fluid "water" :at w}]}
        :else (assoc r :reason :water-unplugged)))))

(defn target-steps
  "Steps to cut from the args and the start feet, or {:error text}."
  [{:keys [dir heading steps y]} [_ fy _]]
  (cond
    (not (shape/rises dir)) {:error "dir must be :down or :up"}
    (not (shape/headings heading)) {:error "heading must be :north :east :south or :west"}
    (and (int? steps) (pos? steps)) steps
    (and (int? y) (pos? (* (shape/rises dir) (- y fy)))) (js/Math.abs (- y fy))
    :else {:error "give :steps > 0, or :y beyond the feet in :dir"}))

(defn finish!
  "Hand over the result and end: :done with reason :done, else :stopped (warn)."
  [c reason detail]
  (let [m (ctx/mem c)
        steps (or (:at-step m) 0)
        result (merge {:status (if (= :done reason) :done :stopped) :reason reason :steps steps
                       :at (reach/standing-vec (:primitives c)) :dug (:dug m []) :ores (vec (ore-pick/picked c))}
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

(defn need
  "What the next cell to dig lacks, as a reason map for ctx/wait, or nil: :no-tool (a pickaxe), :no-free-slot (no room
  for the drop). Judged for the step from the feet when the body is at the stair's start or on it; bad args and the
  rules' refusals are left to the round."
  [c]
  (let [{:keys [dir heading]} (:args c)
        p (:primitives c)
        feet (reach/standing-vec (:primitives c))
        {:keys [origin target]} (ctx/mem c)
        on-stair? (or (nil? origin) (some? (stair-index origin feet dir heading target)))]
    (when (and on-stair? (shape/rises dir) (shape/headings heading))
      (let [block-at (:block-at (rules-in c feet))
            {:keys [cut]} (shape/step-cells feet dir heading)
            block (some #(let [n (when-not (unknown? p %) (block-at %))] (when (and n (not (rules/air n)) (not (rules/unbreakable n))) n)) cut)]
        (cond
          (nil? block) nil
          (lacks-tool? c block) {:reason :no-tool :tool (tools/needed-kind p block) :block block}
          (not (drop-room? c block)) {:reason :no-free-slot :block block})))))

(defn refusal
  "Before anything is cut: the stop of the first step when a zone, claim or plan refuses its cut, else nil."
  [c]
  (let [{:keys [dir heading]} (:args c)
        {:keys [origin target]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))]
    (when (and (or (nil? origin) (and (= feet origin) (empty? (:dug (ctx/mem c)))))
               (shape/rises dir) (shape/headings heading))
      (let [in (rules-in c feet)
            cells (-> (shape/step-cells feet dir heading) (assoc :up? (= :up dir)))
            stop (stop-of in cells (set (:accept (:args c))))]
        (when (#{:zone :claim :footprint} (:reason stop)) stop)))))

(defn with-heading
  "c with the :heading arg filled when the call gave none: the heading the first round chose (job memory), else the way
  the body faces."
  [c]
  (if (:heading (:args c))
    c
    (assoc-in c [:args :heading] (keyword (or (:heading (ctx/mem c)) (look/facing (:primitives c)))))))

(defn check
  "True, or a wait for what the next dig lacks (see need), or :refused when a zone, claim or plan refuses the first
  step. As a child (leave-tunnel, tunnel, dig-in) it runs and stops with the same reason in its result, which its
  parent reads."
  [c0]
  (let [c (with-heading c0)
        stop (refusal c)]
    (cond
      (some? (submitted/away-wait c)) false
      stop (access/decline! c :stair.declined "stair" (assoc (access/refusal-fields [stop]) :reason :refused))
      :else (if-let [lack (need c)] (fetch/check c 'jobs.access.stair lack) (trip/torches-check c)))))

(defn ^:async dig!
  "Equip the best tool, check the cell again, write the intent and dig it (a blocks.dig child). :continue (dug, go on), :yield (the child waits on the world), or a stop map."
  [c in cell cut accept]
  (let [p (:primitives c)
        block ((:block-at in) cell)
        tries (get-in (ctx/mem c) [:tries cell] 0)]
    (cond
      (>= tries (if (rules/falling? block) (max-fall-digs) (max-cell-digs))) (cond-> {:reason :refills :cell cell :block block}
                                   (not (rules/falling? block))
                                   (assoc :why "put back by something else, e.g. the night shelter closing an opened refuge again"))
      (rules/unbreakable block) {:reason :unbreakable :cell cell :block block}
      (lacks-tool? c block) {:reason :no-tool :cell cell :block block :tool (tools/needed-kind p block)}
      (and (not (:spent-ok (:args c))) (trip/spent-tool? p block)) {:reason :tool-spent :cell cell :block block :tool (tools/needed-kind p block)}
      (not (drop-room? c block)) {:reason :inventory-full :cell cell :block block}
      :else
      (do
        (await (tools/equip! c block))
        (let [v (cell-verdict (assoc in :cell cell) cut)
              block ((:block-at in) cell)]
          (cond
            (not (:ok v)) (assoc (dissoc v :ok) :cell cell)
            (not-every? (comp accept hazard-key) (:hazards v)) {:reason :hazard :cell cell :hazards (:hazards v) :why (hazards-why (:hazards v))}
            (rules/air block) :continue
            :else
            (do
              ;; a waiting child is resumed on the same cell: only the first round of a dig counts a try
              (ctx/update-mem! c #(cond-> (assoc % :digging {:cell cell :block block} :counted cell)
                                    (not= cell (:counted %)) (update-in [:tries cell] (fnil inc 0))))
              (let [[x y z] cell
                    outcome (await (blocks/dig-cell! c {:x x :y y :z z} {:accept #{:fluid-adjacent :lava-adjacent :falling-block}
                                                                         :on-lava :leave :ignore-zones? true}))]
                (if (= :continue outcome)
                  :yield
                  (do
                    (ctx/update-mem! c #(-> % (dissoc :counted) (record-dug (:block-at in))))
                    (when-let [tag (when (= :dug outcome) (:note (:args c)))] (escape/note-hole! c tag cell block))
                    (when (#{:dug :missing} outcome)
                      (ctx/update-mem! c assoc :dug-at (ctx/now c))
                      (await (see-round! c cell)))
                    (case outcome
                      (:dug :missing) :continue
                      {:reason :dig-failed :cell cell :block block :dig outcome})))))))))))

(defn take-back-floor?
  "Whether a floor this stair places goes into the scaffold ledger: a go-to stair (:note), unless the setting keeps it."
  [c]
  (boolean (and (:note (:args c)) (settings/get settings ::take-back-floors?))))

(defn ^:async bridge!
  "Place carried filler on the missing floor of the step (bridge/bridge-plans, a plan whose placing found no support gives way
  to the next). :again, or a stop map (:no-support-floor when no plan works)."
  [c in {:keys [floor]}]
  (let [item (blocks/pick c blocks/building-blocks)
        plans (bridge/bridge-plans (:block-at in) floor (bridge/eye-y c))]
    (cond
      (nil? item) {:reason :no-floor :cell floor :block ((:block-at in) floor) :filler :none
                   :why (str "no floor and no filler block carried (" (str/join ", " (take 4 blocks/building-blocks)) " ...)")}
      (empty? plans) {:reason :no-support-floor :cell floor :tried [floor (update floor 1 dec)]
                      :why (str "no block beside or under " (pr-str floor) " to place " item " against: the floor opens into a void")}
      :else (or (await (bridge/place-plans! c in item plans (take-back-floor? c))) :again))))

(defn ^:async step!
  "Walk into the cut step. :again, :continue while the walk waits, or a stop map."
  [c next-feet]
  (let [r (await (child/walk-into! c :walk next-feet))]
    (cond
      (= :continue r) :continue
      (and (:arrived r) (= next-feet (reach/standing-vec (:primitives c))))
      (do (ctx/emit! c :stair.step :info {:at next-feet :text (str "stepped to " (pr-str next-feet))})
          :again)
      :else {:reason :step-failed :cell next-feet :walk r})))

(defn ^:async work!
  "One bounded piece of the stair from the body's place on it: check the way back, dig one cell or take the step."
  [c]
  (let [{:keys [dir heading]} (:args c)
        accept (set (:accept (:args c)))
        {:keys [origin target checked]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))
        in (rules-in c feet)
        i (stair-index origin feet dir heading target)]
    (ctx/update-mem! c record-dug (:block-at in))
    (cond
      (nil? i) {:reason :off-stair :cell feet :origin origin
                :offset (mapv - feet origin)
                :why (str "the body is " (pr-str (mapv - feet origin)) " from the stair's start " (pr-str origin)
                          ", off its line")}
      (= :continue (await (ore-pick/pick! c))) :continue
      :else
      (do
        (ctx/update-mem! c assoc :at-step i)
        (if-let [stop (when (and (pos? i) (not= i checked) (not= i target)) (await (walk/way-back c origin)))]
          stop
          (do
            (ctx/update-mem! c assoc :checked i)
            (if (= i target)
              (or (await (lava-step! c :stair.sealed (:block-at in) feet [])) :finished)
              (let [{:keys [next cut] :as cells} (-> (shape/step-cells feet dir heading)
                                                           (as-> cs (assoc cs :up? (= :up dir)
                                                                 :bridged? (contains? (:bridged (ctx/mem c)) (:floor cs))
                                                                 :filler? (some? (blocks/pick c blocks/building-blocks)))))
                    _ (await (look-ahead! c feet next cut))
                    stop (or (await (lava-step! c :stair.sealed (:block-at in) feet cut))
                             (await (water-step! c (:block-at in) accept cut))
                             (await (judge! c in cells accept)))]
                (if (and (= :no-floor (:reason stop)) (rules/air (:block stop)))
                  (await (bridge! c in cells))
                  (or stop
                    (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
                      (or (await (peek! c cell))
                          (let [r (await (dig! c in cell cut accept))] (case r :continue :again :yield :continue r)))
                      (or (await (settle! c cut)) (await (step! c next))))))))))))))

(defn back-cell
  "The stair's cell one step back toward its origin from where the body stands, or nil at the origin or off the line."
  [c]
  (let [{:keys [dir heading]} (:args c)
        {:keys [origin target]} (ctx/mem c)
        i (when origin (stair-index origin (reach/standing-vec (:primitives c)) dir heading target))]
    (when (and i (pos? i))
      (shape/add origin (mapv #(* (dec i) %) (shape/delta dir heading))))))

(defn ^:async back-off!
  "After a failed seal: walk one step back up the stair (a go-to child), then finish with the stop, :backed-off true
  when the body got there."
  [c]
  (let [{:keys [stop cell]} (:backoff (ctx/mem c))
        r (when cell (await (child/walk-into! c :back cell)))]
    (if (= :continue r)
      :continue
      (finish! c (:reason stop) (assoc (dissoc stop :reason) :backed-off (boolean (and cell (= cell (reach/standing-vec (:primitives c))))))))))

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
      (let [feet (or (some-> (:start m) vec) (reach/standing-vec (:primitives c)))
            target (target-steps (:args c) feet)]
        (if (map? target)
          (finish! c :bad-args {:why (:error target)})
          ;; stone stands for the cells to dig: unseen cells are taken for stone (see the ns doc)
          (do
            ;; the spare's fetch may walk the body off: the stair starts where it was asked, not where the fetch ended
            (ctx/update-mem! c assoc :start feet)
            (if (= :continue (or (await (when (and (= :down (:dir (:args c))) (:stock-torches (:args c))) (trip/torches-ready! c)))
                                   (await (trip/tool-ready! c "stone"))))
              :continue
              (if (and (not= feet (reach/standing-vec (:primitives c))) (= :continue (await (child/walk-into! c :start feet))))
                :continue
                (do (ctx/update-mem! c assoc :origin feet :target target :dug [])
                    :again))))))
      :else
      (let [r (await (work! c))]
        (cond
          (#{:again :continue} r) r
          (= :finished r) (finish! c :done {})
          (= :lava-unsealed (:reason r)) (do (ctx/update-mem! c assoc :backoff {:stop r :cell (back-cell c)}) :again)
          :else (finish! c (:reason r) (dissoc r :reason)))))))

(defn ^:async round
  "The whole stair: next! until it ends, a pace between pieces; :continue after max-steps of them."
  [c0]
  (let [c (with-heading c0)
        n (atom 0)]
    (submitted/begin! c)
    (when-not (:heading (:args c0)) (ctx/update-mem! c assoc :heading (name (:heading (:args c)))))
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (next! c) :continue)))))
