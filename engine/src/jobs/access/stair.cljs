(ns jobs.access.stair
  (:require [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [engine.path.executor :as executor]
            [jobs.debug.walk-plan :as walk-plan]
            [jobs.gather.mine :as mine]))

(def doc
  "Dig a 1-wide stair :steps steps (or down/up to feet height :y) :down or :up along :heading, from where the body
  stands. A step cuts three cells top first so the body can walk one block forward and one down (or up) with full
  headroom: down, the next column at the old head, the old feet and the new feet height; up, the cell over the head
  and the next column at the new feet and head height. Then it walks into the step with jobs.debug.walk-plan and,
  before cutting the next one, plans the way back to the stair's first cell on a fresh pathWorld: it must be found
  whole and walkable by the executor (no gap, door or swim), else the stair stops :no-way-back.
  Each step is judged when chosen and every cell again right before its dig: the next floor must be a solid floor
  (else :no-floor: this slice places nothing), the cell under it neither air nor fluid (:cave-below) and loaded;
  a cut cell holding a fluid stops (:fluid-in-cut). Access refusals stop with their reason (:zone, :footprint,
  :not-loaded, :no-zones). Hazards are engine.access.rules' (one per fluid beside) plus one the stair adds for its
  geometry: a falling block over the top cut of the next column, the column the body walks into. Accepted hazards
  (:accept, a set of :water :lava :falling-block :under-feet) default to #{}: water beside the cut is not taken
  (live, it flowed into the cut and onto the body's cell, which the walker cannot leave: no swimming; named, the
  stair digs and stops :fluid-in-cut a round later), lava never is, a falling block would land on the body or refill
  the cut, and :under-feet never comes up (the stair never digs the block it stands on), so seeing it is a bug.
  A pickaxe block with no pickaxe carried stops :no-tool (by hand stone drops nothing); a dig whose drop has no
  room stops :inventory-full; a cell refilled 3 times stops :refills. The body's cell is the progress: a resumed
  round finds its step from where the body stands on the stair line (off it: :off-stair). Dug cells are left and
  recorded. Hands over {:status :done|:stopped :reason kw :steps n :at [x y z] :dug [{:cell :block}]} plus detail
  (:cell :hazards :zone :walk ...), also as a :stair.done info or :stair.stopped warn event.")

(def args
  {:dir {:doc ":down or :up" :default :down}
   :heading {:doc ":north :east :south or :west" :default nil}
   :steps {:doc "steps to cut; or give :y" :default nil}
   :y {:doc "feet height to end at, instead of :steps" :default nil}
   :accept {:doc "hazards taken: #{:water :lava :falling-block :under-feet}" :default #{}}})

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})
(def rises {:down -1 :up 1})
(def max-cell-digs 3)
(def stack-size 64)

(defn check [_c] true)

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

(defn stop-of
  "Why the step cannot go on, as {:reason ...detail}, or nil when every cell may be cut.
  in: the rules' input without :cell."
  [{:keys [block-at] :as in} {:keys [cut floor under]} accept]
  (let [fluid-cell (first (filter #(rules/fluids (block-at %)) cut))]
    (cond
      (some #(nil? (block-at %)) (conj cut floor under))
      {:reason :not-loaded :cell (first (filter #(nil? (block-at %)) (conj cut floor under)))}
      fluid-cell {:reason :fluid-in-cut :cell fluid-cell :fluid (block-at fluid-cell)}
      (not (rules/solid-floor? block-at floor)) {:reason :no-floor :cell floor :block (block-at floor)}
      (let [n (block-at under)] (or (rules/air n) (rules/fluids n)))
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
  "{:zones :footprints} for the rules: the world's zone list (nil: never read) and every active plan's cells."
  [c]
  {:zones (ctx/zones c) :footprints (ctx/footprints c)})

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
    (if (= :done reason)
      (ctx/emit! c :stair.done :info (assoc result :text (str "stair " (name (:dir (:args c))) " done, " steps " steps")))
      (ctx/emit! c :stair.stopped :warn
                 (assoc result :text (str "stair stopped after " steps " steps: " (name reason)
                                          (some->> (:cell detail) pr-str (str " at "))))))
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
  (let [pw (walk-plan/path-world (:primitives c))]
    (if (nil? pw)
      {:reason :no-way-back :why :unsupported}
      (let [{:keys [r steps beyond]} (walk-plan/plan-within c pw origin 0 (:default (:weight walk-plan/args)))
            status (.-status r)
            stop (fn [refused] {:reason :no-way-back :why :refused :kind (:kind refused) :step (:at refused)})]
        (cond
          beyond (stop beyond)
          (not= "found" status) {:reason :no-way-back :why (keyword status) :planner (some-> (.-reason r) keyword)}
          :else (some-> (executor/refusal executor/policy steps) stop))))))

(defn ^:async dig!
  "Equip the best tool, check the cell again, write the intent and dig it. :continue, or a stop map."
  [c in cell cut accept]
  (let [p (:primitives c)
        block ((:block-at in) cell)
        tries (get-in (ctx/mem c) [:tries cell] 0)
        tool (tools/best-tool (map :name (u/inventory p)) block)]
    (cond
      (>= tries max-cell-digs) {:reason :refills :cell cell :block block}
      (no-tool? p block) {:reason :no-tool :cell cell :block block :tool "pickaxe"}
      (not (room-for? p (mine/item-name {:block block}))) {:reason :inventory-full :cell cell :block block}
      :else
      (do
        (when (and tool (not= tool (.-held (.self p))))
          (await (ctx/act c :equip #js {:item tool :dest "hand"})))
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
                (ctx/update-mem! c record-dug (:block-at in))
                (if (#{"dug" "missing"} status)
                  :continue
                  {:reason :dig-failed :cell cell :block block :dig status})))))))))

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
      (nil? i) {:reason :off-stair :cell feet}
      :else
      (do
        (ctx/update-mem! c assoc :at-step i)
        (if-let [stop (when (and (pos? i) (not= i checked)) (way-back c origin))]
          stop
          (do
            (ctx/update-mem! c assoc :checked i)
            (if (= i target)
              :finished
              (let [{:keys [next cut] :as cells} (step-cells feet dir heading)]
                (or (stop-of in cells accept)
                    (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
                      (await (dig! c in cell cut accept))
                      (await (step! c next))))))))))))

(defn ^:async round [c]
  (let [m (ctx/mem c)]
    (if (nil? (:origin m))
      (let [feet (feet-of c)
            target (target-steps (:args c) feet)]
        (if (map? target)
          (finish! c :bad-args {:why (:error target)})
          (do (ctx/update-mem! c assoc :origin feet :target target :dug [])
              :continue)))
      (let [r (await (work! c))]
        (cond
          (= :continue r) :continue
          (= :finished r) (finish! c :done {})
          :else (finish! c (:reason r) (dissoc r :reason)))))))
