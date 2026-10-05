(ns jobs.access.stair
  (:require [clojure.string :as str]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.fetch :as fetch]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [engine.path.executor :as executor]
            [engine.path.walk :as walk]
            [jobs.survival.dig-in :as dig-in]
            [jobs.gather.mine :as mine]))

(def doc
  "Dig a 1-wide stair :down or :up along :heading from where the body stands, :steps steps or to feet height :y.
  The check waits (:no-tool, :no-free-slot) when the next dig lacks a pickaxe or room for the drop.

  A step cuts three cells, top first, so the body can walk one block forward and one down (or up) with full
  headroom. Then the body walks into the step (jobs.debug.walk-plan). Before cutting the next step it plans the
  way back to the stair's first cell on a fresh pathWorld. The way must be whole and walkable by the executor
  (no gap, door or swim), else the stair stops :no-way-back.

  A step's floor must be solid, the cell under it neither air nor fluid (:cave-below), and everything loaded.
  A floor of air gets a carried building block (dig-in/building-blocks, never an ore or valuable) after the
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

  Hazards are the rules' (one per fluid beside) plus a falling block over the top cut of the next column.
  :accept is a set of :water :lava :falling-block :under-feet, default #{}. Water beside the cut is not taken
  by default (it can flow into the cut and onto the body's cell, which the walker cannot leave). Lava never is
  in practice, and a falling block would land on the body or refill the cut. :under-feet never comes up, since
  the stair never digs the block it stands on.

  The body's cell is the progress: a resumed round finds its step from where the body stands on the stair
  line. Dug cells are left and recorded.

  Hands over {:status :done|:stopped :reason kw :steps n :at [x y z] :dug [{:cell :block}]} plus detail (:cell
  :hazards :zone :walk ...), also as a stair.done info or stair.stopped warn event.

  :fetch (default false; engine.jobs.fetch): the :no-tool wait is not waited out; the rounds run jobs.items.get-tool
  for the block (child :fetch) first, then walks back to the cell it stood on (child :fetch-back) and goes on. A
  parent's stair child does not fetch.")

(def args
  {:dir {:doc ":down or :up" :default :down}
   :heading {:doc ":north :east :south or :west" :default nil}
   :steps {:doc "steps to cut; or give :y" :default nil}
   :y {:doc "feet height to end at, instead of :steps" :default nil}
   :accept {:doc "hazards taken: #{:water :lava :falling-block :under-feet}" :default #{}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :fetch {:doc "get a missing pickaxe instead of waiting :no-tool (engine.jobs.fetch): true, a set of kinds or a map of limits" :default false}})

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})
(def rises {:down -1 :up 1})
(def max-cell-digs 3)
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
  "The first cut cell that is a floor of a stair the body made earlier (ways: the set of those floors)."
  [cut ways]
  (first (filter ways cut)))

(defn stop-of
  "Why the step cannot go on, as {:reason ...detail}, or nil when every cell may be cut.
  in: the rules' input without :cell. cells :bridged? true: the floor was placed by the stair, what is under it is not judged."
  [{:keys [block-at] :as in} {:keys [cut floor under bridged? ways]} accept]
  (let [floor-cell (known-floor cut (or ways #{}))
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
      (and (not bridged?) (let [n (block-at under)] (or (rules/air n) (rules/fluids n))))
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

(defn no-tool? [p block]
  (and (= "pickaxe" (tools/tool-kind block))
       (not-any? #(re-find #"_pickaxe$" (:name %)) (u/inventory p))))

(defn access-world
  "The social half of the rules' input (engine.jobs.access/zone-input): zones, claims, footprints, the body's name and
  the clock, and the job's :ignore-zones? arg."
  [c]
  (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c))}))

(defn feet-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn rules-in [c feet]
  (merge {:block-at (fn [[x y z]] (u/block-name (:primitives c) {:x x :y y :z z})) :feet feet :ledger #{}}
         (access-world c)))

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
        (ctx/remember! c :stair-way {:floors (way-floors origin dir heading steps)} way-policy)))
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

(defn way-back
  "nil when a whole plan the executor can walk leads from the body to origin on a fresh pathWorld, else the stop."
  [c origin]
  (let [pw (walk/path-world (:primitives c))]
    (if (nil? pw)
      {:reason :no-way-back :why :unsupported}
      (let [{:keys [r steps beyond]} (walk/plan-within c pw origin 0 walk/default-weight)
            status (.-status r)
            stop (fn [refused] {:reason :no-way-back :why :refused :kind (:kind refused) :step (:at refused)})]
        (cond
          beyond (stop beyond)
          (not= "found" status) {:reason :no-way-back :why (keyword status) :planner (some-> (.-reason r) keyword)}
          :else (some-> (executor/refusal executor/policy steps) stop))))))

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
          (no-tool? p block) {:reason :no-tool :tool "pickaxe" :block block}
          (not (room-for? p (mine/item-name {:block block}))) {:reason :no-free-slot :block block})))))

(defn check
  "True, or a wait for what the next dig lacks (see need). Only for a stair that is itself listed: as a child (leave-tunnel,
  tunnel, dig-in) it runs and stops with the same reason in its result, which its parent reads."
  [c]
  (if-let [lack (need c)] (fetch/check c 'jobs.access.stair lack) true))

(defn ^:async dig!
  "Equip the best tool, check the cell again, write the intent and dig it. :continue, or a stop map."
  [c in cell cut accept]
  (let [p (:primitives c)
        block ((:block-at in) cell)
        tries (get-in (ctx/mem c) [:tries cell] 0)]
    (cond
      (>= tries max-cell-digs) {:reason :refills :cell cell :block block}
      (unbreakable block) {:reason :unbreakable :cell cell :block block}
      (no-tool? p block) {:reason :no-tool :cell cell :block block :tool "pickaxe"}
      (not (room-for? p (mine/item-name {:block block}))) {:reason :inventory-full :cell cell :block block}
      :else
      (do
        (await (tools/equip-for! c block))
        (let [v (cell-verdict (assoc in :cell cell) cut)
              block ((:block-at in) cell)]
          (cond
            (not (:ok v)) (assoc (dissoc v :ok) :cell cell)
            (not-every? (comp accept hazard-key) (:hazards v)) {:reason :hazard :cell cell :hazards (:hazards v)}
            (rules/air block) :continue
            :else
            (do
              (ctx/update-mem! c #(-> % (assoc :digging {:cell cell :block block}) (update-in [:tries cell] (fnil inc 0))))
              (let [[x y z] cell
                    status (.-status (await (ctx/act c :dig #js {:pos #js {:x x :y y :z z}})))]
                (await (tools/note-wear! c))
                (ctx/update-mem! c record-dug (:block-at in))
                (if (#{"dug" "missing"} status)
                  :continue
                  {:reason :dig-failed :cell cell :block block :dig status})))))))))

(defn ^:async bridge!
  "Place carried filler on the missing floor of the step. :continue, or a stop map."
  [c in {:keys [floor]}]
  (let [item (dig-in/pick c dig-in/building-blocks)
        verdict (rules/may-place? (assoc in :cell floor))
        [x y z] floor]
    (cond
      (nil? item) {:reason :no-floor :cell floor :block ((:block-at in) floor) :filler :none
                   :why (str "no floor and no filler block carried (" (str/join ", " (take 4 dig-in/building-blocks)) " ...)")}
      (not (:ok verdict)) (assoc (dissoc verdict :ok) :cell floor)
      :else
      (do (await (ctx/act c :place #js {:pos #js {:x x :y y :z z} :item item}))
          (if (rules/air ((:block-at (rules-in c (feet-of c))) floor))
            {:reason :place-failed :cell floor :item item}
            (do (ctx/update-mem! c update :bridged (fnil conj #{}) floor)
                :continue))))))

(defn ^:async step!
  "Walk into the cut step. :continue, or a stop map."
  [c next-feet]
  (if (= :done (await (ctx/call-child c :walk 'jobs.debug.walk-plan {:to next-feet})))
    (let [r (ctx/child-result c :walk)]
      (if (and (= :arrived (:status r)) (= next-feet (feet-of c)))
        (do (ctx/emit! c :stair.step :info {:at next-feet :text (str "stepped to " (pr-str next-feet))})
            :continue)
        {:reason :step-failed :cell next-feet :walk r}))
    :continue))

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
        (if-let [stop (when (and (pos? i) (not= i checked)) (way-back c origin))]
          stop
          (do
            (ctx/update-mem! c assoc :checked i)
            (if (= i target)
              :finished
              (let [{:keys [next cut] :as cells} (-> (step-cells feet dir heading)
                                                           (as-> cs (assoc cs :bridged? (contains? (:bridged (ctx/mem c)) (:floor cs))
                                                       :ways (set (mapcat #(:floors (:data %)) (ctx/entries c :stair-way))))))
                    stop (stop-of in cells accept)]
                (if (and (= :no-floor (:reason stop)) (rules/air (:block stop)))
                  (await (bridge! c in cells))
                  (or stop
                    (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
                      (await (dig! c in cell cut accept))
                      (await (step! c next)))))))))))))

(defn ^:async round [c]
  (let [m (ctx/mem c)
        r (await (fetch/step! c 'jobs.access.stair (need c) {:return? true}))]
    (cond
      r r
      (nil? (:origin m))
      (let [feet (feet-of c)
            target (target-steps (:args c) feet)]
        (if (map? target)
          (finish! c :bad-args {:why (:error target)})
          (do (ctx/update-mem! c assoc :origin feet :target target :dug [])
              :continue)))
      :else
      (let [r (await (work! c))]
        (cond
          (= :continue r) :continue
          (= :finished r) (finish! c :done {})
          :else (finish! c (:reason r) (dissoc r :reason)))))))
