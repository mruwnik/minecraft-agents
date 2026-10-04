(ns jobs.access.tunnel
  (:require [engine.access.ledger :as ledger]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.util :as u]
            [engine.placement :as placement]
            [jobs.access.stair :as stair]
            [jobs.build.from-plan :as from-plan]))

(def doc
  "Cut a way to stand beside a buried block :target [x y z] and stop there, the target the next cell ahead at feet
  height. The way is one straight line along a heading through the target's column: from an entry stand on the
  surface (the column's top cell over a solid floor, two open cells; no entry from water), a 1-wide stair down (or
  up) to the target's height with jobs.access.stair as a called child (:y the target's height, so it resumes from
  any step), then a flat run 1 wide and 2 high (head cell first) up to the stand. Never a shaft, and the run never
  passes under its own stair. The choice tries the four headings and, per heading, the entry distances 1 to
  :max-length; a distance fits when the stair's steps are at most the distance less one. Every cell of a fitting
  line is judged from the loaded blocks before anything is walked or dug, step by step as the stair judges its own
  (engine.access.rules: zones, other plans' footprints, unloaded; a fluid in a cut; a fluid beside a cut or a
  falling block over one, taken only when named in :accept, default #{}; the next floor solid; the cell under it
  neither air nor fluid), plus the target's own column as a last run step: the target and the cell over it, the
  target's floor. At the stand the cell over the target is dug (so after the caller digs the target its cell is 2
  high and can be walked into, to pick up a drop that landed out of reach); the target itself is left. The
  shortest valid line wins; among equals one that leaves the floor under the body's start uncut, then the entry
  nearest the body. No valid line stops before any walk or dig with the
  reason all judged lines share (:zone, :footprint, :hazard, :cave-below, :no-floor, :fluid-in-cut, :not-loaded,
  :no-zones), :too-far when no line fits within :max-length, else :no-approach with :headings (each heading's first
  reason). Then the body walks to the entry (jobs.debug.walk-plan; failing: :walk-in-failed), the stair child cuts
  and walks its steps, and each run step is judged again right before each dig (stair's dig!: :hazard, :zone,
  :no-tool, :inventory-full, :refills, :dig-failed). Before each further run step, and at the stand, the way back to
  the entry is planned on a fresh pathWorld and must be whole and walkable (else :no-way-back, and the body stays
  where it is). Any other stop after the body has entered walks it back to the entry first (:out true when it
  arrived): a dead end (:keep false) through jobs.access.leave-tunnel as a child (the torches taken back, the mouth
  sealed; its result in :leave), a kept tunnel by the plain walk. The body's cell is the progress: on the stair line the stair child goes on, on the run line the run, at
  the stand it is done; anywhere else it walks to the entry. A cell dug before a cut is air and is not dug again.
  Dug cells are left and recorded. A nil zone list declines the check (one tunnel.declined warn).
  Torches go in on the way: the line's cells are numbered 0 (entry) to n (stand), the target n+1, and a torch site is
  a cell index s whose torch hangs in the head cell of s on a side wall (a wall torch; else a floor torch in the feet
  cell), placed from cell s+1 so the body is never in its own place. Sites are chosen when the line is: 0, then each
  time the farthest s whose cell s+1 the torch before still lights (light 14 less the taxicab distance, from the
  nearer of head and feet), until every cell 0..n+1 is lit: a flat run every 11 cells, a stair every 5 steps, none
  when the entry is the stand. The stair is cut in segments up to cell s+1 of the next site so the torch goes in
  between them (the way back to the entry is planned before each segment). A site is placed when due (the body on
  s+1, nothing hanging in its cells: the world is the record, so a restart does not hang it twice); a site the body
  already passed is :unlit :passed. With no torch carried, or the place refused (engine.access.rules may-place?,
  no wall or floor to hang on: :no-support) or failed (:place-failed), the site is :unlit with its reason and the
  tunnel goes on dark, one tunnel.unlit warn for the first. :keep (default false: a dead end) writes each torch to
  the scaffold ledger (purpose :tunnel-torch, before the place) for jobs.access.leave-tunnel to take back; :keep true
  leaves them and the tunnel as they are.
  Hands over {:status :done|:stopped :reason :reached|kw :target :entry :heading :stand :at [x y z] :dug [{:cell
  :block}] :inside bool :keep bool :line {:entry :heading :dir :steps :run :stand :target} :torches [{:cell :site
  :block}] (those standing now, from the world) :unlit [{:cell :site :reason}]} (:inside: the body is off the entry,
  on the way in; false when it never got there) plus detail, also as a :tunnel.done info or :tunnel.stopped warn event.")

(def args
  {:target {:doc "the buried block [x y z]" :default nil}
   :max-length {:doc "longest line, in blocks along the heading from the entry to the target" :default 24}
   :accept {:doc "hazards taken: #{:water :lava :falling-block}" :default #{}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :keep {:doc "a tunnel that stays: torches left and the tunnel left open; false (a dead end): torches go into the scaffold ledger for jobs.access.leave-tunnel to take back" :default false}})

(def heading-order [:north :east :south :west])

(defn check [c]
  (if (and (nil? (ctx/zones c)) (not (:ignore-zones? (:args c))))
    (access/decline! c :tunnel.declined "tunnel" {:reason :no-zones})
    true))

(defn ahead
  "The cell n along heading from cell, same height."
  [cell heading n]
  (let [[dx dz] (stair/headings heading)]
    (stair/add cell [(* n dx) 0 (* n dz)])))

(defn open-cell?
  "Nothing to stand in: air or a plant a body walks through."
  [n]
  (boolean (and n (rules/replaceable n) (not (rules/fluids n)))))

(defn surface
  "The feet height of the top stand of column [x z], looking down from hi to lo: the first cell that is not open must
  be a solid floor with two open cells over it. nil when it is not (a fluid, a cave roof above hi), or a cell on the
  way is not loaded."
  [block-at [x z] lo hi]
  (loop [y hi]
    (when (>= y lo)
      (let [n (block-at [x y z])]
        (cond
          (nil? n) nil
          (open-cell? n) (recur (dec y))
          (and (<= (+ y 2) hi) (rules/solid-floor? block-at [x y z])) (inc y)
          :else nil)))))

(defn run-cells
  "The cells of one flat run step from feet: {:next :cut (head first) :floor :under}."
  [feet heading]
  (let [n (ahead feet heading 1)]
    {:next n :cut [(stair/add n [0 1 0]) n] :floor (stair/add n [0 -1 0]) :under (stair/add n [0 -2 0])}))

(defn line
  "The steps of a line, as [feet cells] each: the stair from entry, then the run; with :end (the stair's last cell)
  and :stand."
  [{:keys [entry heading dir steps run]}]
  (let [stair-feet (take (inc steps) (iterate #(:next (stair/step-cells % dir heading)) entry))
        end (last stair-feet)
        run-feet (take (inc run) (iterate #(ahead % heading 1) end))]
    {:steps (concat (map (fn [f] [f (stair/step-cells f dir heading)]) (butlast stair-feet))
                    (map (fn [f] [f (run-cells f heading)]) (butlast run-feet)))
     :end end
     :stand (last run-feet)}))

(defn line-cells
  "The feet cells of the line of plan: index 0 the entry to n (steps + run) the stand, then the target as n+1."
  [plan]
  (let [{:keys [steps stand]} (line plan)]
    (-> (mapv first steps) (conj stand) (conj (:target plan)))))

(defn taxi [a b] (reduce + (map #(js/Math.abs (- %1 %2)) a b)))

(defn light-between
  "The light of a torch for site s at cell k of cells: 14 less the taxicab distance, from the nearer of head and feet."
  [cells s k]
  (let [feet (cells s)]
    (- 14 (max (taxi (stair/add feet [0 1 0]) (cells k)) (taxi feet (cells k))))))

(defn torch-light [plan s k] (light-between (line-cells plan) s k))

(defn torch-sites
  "The site indices of the torches of plan: [] when the entry is the stand, else 0 and each time the largest s after
  the last whose cell s+1 the last still lights (s up to n-1), until every cell 0..n+1 has light."
  [plan]
  (let [cells (line-cells plan)
        n (- (count cells) 2)
        lit? (fn [sites k] (some #(pos? (light-between cells % k)) sites))
        dark? (fn [sites] (not-every? #(lit? sites %) (range 0 (+ n 2))))]
    (if (zero? n)
      []
      (loop [sites [0]]
        (let [at (peek sites)
              nxt (last (filter #(pos? (light-between cells at (inc %))) (range (inc at) n)))]
          (if (and (dark? sites) nxt) (recur (conj sites nxt)) sites))))))

(defn line-stop
  "The first stop on the line of plan, or nil when every step and the target may be cut."
  [in plan accept]
  (let [{:keys [steps stand]} (line plan)]
    (or (some (fn [[feet cells]] (stair/stop-of (assoc in :feet feet) cells accept)) steps)
        (stair/stop-of (assoc in :feet stand) (run-cells stand (:heading plan)) accept))))

(defn fits
  "The plan of the line along heading whose entry is n before the target, when the stair fits in it; else nil."
  [block-at target heading n max-length]
  (let [[tx ty tz] target
        [ex _ ez] (ahead target heading (- n))
        y (surface block-at [ex ez] (- ty max-length) (+ ty max-length 1))]
    (when (and y (<= (js/Math.abs (- y ty)) (dec n)))
      (let [d (- y ty)
            plan {:entry [ex y ez] :heading heading :dir (if (neg? d) :up :down) :steps (js/Math.abs d)
                  :run (- n 1 (js/Math.abs d)) :length n :target [tx ty tz]}]
        (assoc plan :stand (:stand (line plan)))))))

(defn best-on-heading
  "{:plan p} for the shortest valid line along heading, else {:stop s} (the shortest fitting line's stop, or
  :too-far when none fits)."
  [in target heading max-length accept]
  (let [fitting (keep #(fits (:block-at in) target heading % max-length) (range 1 (inc max-length)))
        judged (map (fn [p] [p (line-stop in p accept)]) fitting)]
    (if-let [[p] (first (filter (comp nil? second) judged))]
      {:plan p}
      {:stop (or (second (first judged)) {:reason :too-far})})))

(defn cuts-floor?
  "Whether the line of plan cuts the floor under feet (the body would come back to a hole where it stood)."
  [plan feet]
  (let [floor (stair/add feet [0 -1 0])]
    (boolean (some (fn [[_ {:keys [cut]}]] (some #{floor} cut)) (:steps (line plan))))))

(defn dist2 [[x y z] [a b c]] (+ (* (- x a) (- x a)) (* (- y b) (- y b)) (* (- z c) (- z c))))

(defn approach
  "The line to cut to target (the plan map, see fits) from rules input in (without :cell and :feet), the body at
  feet; else {:reason ...} (see the doc), with :headings when the headings fail for different reasons."
  [in target feet max-length accept]
  (if (and (nil? (:zones in)) (not (:ignore-zones? in)))
    {:reason :no-zones}
    (let [per (into {} (map (fn [h] [h (best-on-heading in target h max-length accept)])) heading-order)
          plans (keep (comp :plan per) heading-order)]
      (if (seq plans)
        (first (sort-by (juxt :length #(cuts-floor? % feet) #(dist2 feet (:entry %))) plans))
        (let [stops (into {} (map (fn [[h r]] [h (:stop r)])) per)
              real (remove #(= :too-far (:reason %)) (vals stops))
              reasons (set (map :reason real))]
          (cond
            (empty? real) {:reason :too-far}
            (= 1 (count reasons)) (first real)
            :else {:reason :no-approach :headings (into {} (map (fn [[h s]] [h (:reason s)])) stops)}))))))

(defn feet-of [c] (stair/feet-of c))

(def torch-blocks #{"torch" "wall_torch"})

(defn head-of [cell] (stair/add cell [0 1 0]))

(defn torch-cell
  "The cell of site s (head, then feet) that holds a torch in the world, or nil."
  [block-at cells s]
  (first (filter #(torch-blocks (block-at %)) [(head-of (cells s)) (cells s)])))

(defn standing-torches
  "[{:cell :site :block}] for the sites of plan whose torch stands now."
  [block-at plan]
  (let [cells (line-cells plan)]
    (vec (keep (fn [s] (when-let [cell (torch-cell block-at cells s)] {:cell cell :site s :block (block-at cell)}))
               (:sites plan)))))

(defn finish!
  "Hand the result over and end: :done when reached, else :stopped (warn)."
  [c reason detail]
  (let [{:keys [plan dug unlit]} (ctx/mem c)
        feet (feet-of c)
        result (merge (cond-> {:status (if (= :reached reason) :done :stopped) :reason reason :target (:target (:args c))
                               :at feet :dug (or dug []) :inside (boolean (and plan (not= feet (:entry plan))))
                               :keep (boolean (:keep (:args c))) :torches [] :unlit (or unlit [])}
                        plan (assoc :line (select-keys plan [:entry :heading :dir :steps :run :stand :target])
                                    :torches (standing-torches (:block-at (stair/rules-in c feet)) plan)))
                      (select-keys plan [:entry :heading :stand])
                      detail)]
    (ctx/result! c result)
    (if (= :reached reason)
      (ctx/emit! c :tunnel.done :info (assoc result :text (str "tunnel reached " (pr-str (:stand plan)))))
      (ctx/emit! c :tunnel.stopped :warn
                 (assoc result :text (str "tunnel stopped: " (name reason) (some->> (:cell detail) pr-str (str " at "))))))
    :done))

(defn run-index
  "k when feet is the run's cell k steps from end, 0 <= k <= run; else nil."
  [end feet heading run]
  (first (filter #(= feet (ahead end heading %)) (range 0 (inc run)))))

(defn ^:async walk-to!
  "Walk to cell with a walk-plan child in slot: :continue while it walks, else the child's result."
  [c slot cell]
  (if (= :done (await (ctx/call-child c slot 'jobs.debug.walk-plan {:to cell})))
    (ctx/child-result c slot)
    :continue))

(defn ^:async run-step!
  "Dig one cell of the next run step or walk into it: :continue, or a stop map."
  [c feet]
  (let [{:keys [plan]} (ctx/mem c)
        accept (set (:accept (:args c)))
        in (stair/rules-in c feet)
        {:keys [next cut] :as cells} (run-cells feet (:heading plan))]
    (or (stair/stop-of in cells accept)
        (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
          (await (stair/dig! c in cell cut accept))
          (let [r (await (walk-to! c :walk next))]
            (cond
              (= :continue r) :continue
              (and (= :arrived (:status r)) (= next (feet-of c)))
              (do (ctx/emit! c :tunnel.step :info {:at next :text (str "tunnel step to " (pr-str next))}) :continue)
              :else {:reason :step-failed :cell next :walk r}))))))

(defn ^:async open-over-target!
  "At the stand: the target's step judged again and the cell over the target dug; :reached once it is open."
  [c feet]
  (let [accept (set (:accept (:args c)))
        in (stair/rules-in c feet)
        {:keys [cut] :as cells} (run-cells feet (:heading (:plan (ctx/mem c))))
        over (first cut)]
    (or (stair/stop-of in cells accept)
        (if (rules/air ((:block-at in) over))
          :reached
          (await (stair/dig! c in over cut accept))))))

(defn line-index
  "The index of feet on the line of cells (0..n), or nil."
  [cells feet]
  (first (keep-indexed (fn [i cell] (when (and (= cell feet) (< i (dec (count cells)))) i)) cells)))

(defn segment-end
  "The line index the stair child cuts to from k: the standing cell (s+1) of the next site s >= k, else the stair's end."
  [{:keys [sites steps]} k]
  (min steps (or (some #(when (>= % k) (inc %)) sites) steps)))

(defn ^:async stair-part!
  "One round of the stair child over the segment from line index k, also its last one at the segment's end (it hands
  over what it dug): :continue, or a stop map when the stair stopped. :stair-done once the body stands at the stair's
  end."
  [c {:keys [heading dir] :as plan} k]
  (let [cells (line-cells plan)
        r (await (ctx/call-child c :stair 'jobs.access.stair
                                 {:dir dir :heading heading :y ((cells (segment-end plan k)) 1)
                                  :accept (set (:accept (:args c))) :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (if (not= :done r)
      :continue
      (let [res (ctx/child-result c :stair)
            done? (= :done (:status res))]
        (ctx/update-mem! c #(-> % (update :dug (fnil into []) (:dug res))
                                (assoc :stair-done (and done? (= (cells (:steps plan)) (feet-of c))))))
        (if done?
          :continue
          (assoc (select-keys res [:cell :hazards :zone :claim :plan :fluid :block :tool :walk :why]) :reason (:reason res)
                 :in :stair))))))

(defn segment-start?
  "Whether no stair child is in flight: the next call cuts a new segment."
  [c]
  (empty? (get-in (ctx/mem c) [:children :stair])))

(defn book-unlit!
  "Book site s as :unlit with reason, at the cell its torch would take (cell, else the site's head cell); the first of
  the tunnel warns."
  [c plan s reason cell]
  (let [cell (or cell (head-of ((line-cells plan) s)))
        first? (empty? (:unlit (ctx/mem c)))]
    (ctx/update-mem! c update :unlit (fnil conj []) {:cell cell :site s :reason reason})
    (when first?
      (ctx/emit! c :tunnel.unlit :warn {:reason reason :cell cell
                                        :text (str "tunnel torch left out at " (pr-str cell) ": " (name reason))}))
    nil))

(defn side-dirs
  "The unit steps to the left and to the right of heading."
  [heading]
  (let [[dx dz] (stair/headings heading)] [[dz 0 (- dx)] [(- dz) 0 dx]]))

(defn facing-name [d] (some (fn [[n v]] (when (= v d) n)) placement/steps))

(defn torch-choice
  "How to hang the torch of site s from the eye: a wall torch in the head cell on the first side wall (left, then
  right of the heading) that takes it, else a floor torch in the feet cell: {:cell :block :click}, or {:refused
  :no-support}."
  [plan s eye block-at]
  (let [cells (line-cells plan)
        feet (cells s)
        head (head-of feet)
        world (fn [cell] (some->> (block-at cell) (hash-map :name)))
        wall (fn [d] (assoc (placement/click {:block "wall_torch" :facing (facing-name (mapv - d))} head eye world)
                            :cell head :block "wall_torch"))
        floor (assoc (placement/click "torch" feet eye world) :cell feet :block "torch")]
    (or (first (remove :refused (map wall (side-dirs (:heading plan)))))
        (if (:refused floor) {:refused :no-support} floor))))

(defn torches-carried [p]
  (reduce + (map :count (filter #(= "torch" (:name %)) (u/inventory p)))))

(defn ^:async hang!
  "Hang the torch of site s from the body's place, or book the site :unlit with why not. The ledger entry (a dead
  end) is written before the place and confirmed when the cell holds the torch. :continue after a place."
  [c in plan s]
  (let [p (:primitives c)
        block-at (:block-at in)
        choice (torch-choice plan s (from-plan/eye (u/self-pos c)) block-at)
        cell (:cell choice)
        verdict (when cell (rules/may-place? (assoc in :cell cell)))
        reason (cond (zero? (torches-carried p)) :no-torches
                     (:refused choice) (:refused choice)
                     (not (:ok verdict)) (:reason verdict))]
    (if reason
      (book-unlit! c plan s reason cell)
      (let [keep? (:keep (:args c))
            intended (ledger/intend (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
                                    {:cell cell :item (:block choice) :before "air" :job (:id c) :purpose :tunnel-torch})
            _ (when-not keep? (ledger/remember! c intended))
            r (await (ctx/act c :place (clj->js {:pos (zipmap [:x :y :z] cell) :item "torch"
                                                 :click (from-plan/js-click (:click choice))})))
            held? (or (= "placed" (.-status r)) (torch-blocks (block-at cell)))]
        (when-not keep? (ledger/remember! c (if held? (ledger/confirm intended cell) (ledger/reconcile intended block-at))))
        (when-not held? (book-unlit! c plan s :place-failed cell))
        :continue))))

(defn ^:async torch-step!
  "With the body on line index k: book the sites it passed without a torch, and hang the one due (site k-1, not
  :unlit, nothing hanging in its cells). :continue after a place, else nil."
  [c k]
  (let [{:keys [plan unlit]} (ctx/mem c)
        in (stair/rules-in c (feet-of c))
        cells (line-cells plan)
        booked (set (map :site unlit))
        pending (remove #(or (booked %) (torch-cell (:block-at in) cells %)) (filter #(<= (inc %) k) (:sites plan)))]
    (doseq [s (filter #(< (inc %) k) pending)] (book-unlit! c plan s :passed nil))
    (when-let [due (first (filter #(= (inc %) k) pending))]
      (await (hang! c in plan due)))))

(defn ^:async work!
  "One bounded piece of the way from the body's place: :reached, :continue or a stop map."
  [c]
  (let [{:keys [plan checked stair-done]} (ctx/mem c)
        {:keys [entry heading dir steps run]} plan
        cells (line-cells plan)
        {:keys [end stand]} (line plan)
        feet (feet-of c)
        k (line-index cells feet)
        j (run-index end feet heading run)
        i (when (pos? steps) (stair/stair-index entry feet dir heading steps))]
    (ctx/update-mem! c stair/record-dug (:block-at (stair/rules-in c feet)))
    (or (when k (await (torch-step! c k)))
        (cond
          (and i (not stair-done) (or (< i steps) (not (segment-start? c))))
          (if-let [stop (when (and (pos? i) (not= i checked) (segment-start? c)) (stair/way-back c entry))]
            stop
            (do (when (segment-start? c) (ctx/update-mem! c assoc :checked i))
                (await (stair-part! c plan i))))
          j (if-let [stop (when (and (not= feet entry) (not= (+ steps j) checked)) (stair/way-back c entry))]
              stop
              (do (ctx/update-mem! c assoc :checked (+ steps j))
                  (if (= feet stand) (await (open-over-target! c feet)) (await (run-step! c feet)))))
          :else (let [r (await (walk-to! c :in entry))]
                  (cond
                    (= :continue r) :continue
                    (and (= :arrived (:status r)) (= entry (feet-of c))) :continue
                    :else {:reason :walk-in-failed :cell entry :walk r :outside true}))))))

(defn way-out
  "What jobs.access.leave-tunnel needs of the tunnel so far: {:line :dug :torches}."
  [c]
  (let [{:keys [plan dug]} (ctx/mem c)]
    {:line (select-keys plan [:entry :heading :dir :steps :run :stand :target]) :dug (or dug [])
     :torches (standing-torches (:block-at (stair/rules-in c (feet-of c))) plan)}))

(defn ^:async retreat!
  "Leave after a stop: a dead end through jobs.access.leave-tunnel (torches back, mouth sealed), a kept tunnel by the
  plain walk back to the entry; then finish with the stop."
  [c]
  (let [{:keys [stop plan way-out]} (ctx/mem c)
        keep? (:keep (:args c))
        r (if keep?
            (await (walk-to! c :out (:entry plan)))
            (if (= :done (await (ctx/call-child c :out 'jobs.access.leave-tunnel
                                           {:tunnel way-out :ignore-zones? (boolean (:ignore-zones? (:args c)))})))
              (ctx/child-result c :out)
              :continue))]
    (cond
      (= :continue r) :continue
      keep? (finish! c (:reason stop) (assoc (dissoc stop :reason) :out (= (:entry plan) (feet-of c)) :walk-out r))
      :else (finish! c (:reason stop) (assoc (dissoc stop :reason) :out (= (:entry plan) (feet-of c)) :leave r)))))

(defn stop!
  "After a stop: finish where the body is (outside, at the entry, or no way back), else walk out first."
  [c {:keys [reason outside] :as stop}]
  (if (or outside (= :no-way-back reason) (= (feet-of c) (:entry (:plan (ctx/mem c)))))
    (finish! c reason (cond-> (dissoc stop :reason :outside) outside (assoc :inside false)))
    (do (ctx/update-mem! c assoc :stop stop :way-out (way-out c)) :continue)))

(defn bad-target? [t] (not (and (vector? t) (= 3 (count t)) (every? int? t))))

(defn choose!
  "Judge the lines and keep the best, or finish with why there is none."
  [c]
  (let [{:keys [target max-length accept]} (:args c)
        feet (feet-of c)]
    (if (bad-target? target)
      (finish! c :bad-args {:why "target must be [x y z] of integers"})
      (let [a (approach (dissoc (stair/rules-in c feet) :feet) target feet max-length (set accept))]
        (if (:entry a)
          (do (ctx/update-mem! c assoc :plan (assoc a :sites (torch-sites a)) :dug [])
              (ctx/emit! c :tunnel.plan :info (assoc a :text (str "tunnel from " (pr-str (:entry a)) " " (name (:heading a))
                                                                  ", " (:steps a) " steps, run " (:run a))))
              :continue)
          (finish! c (:reason a) (dissoc a :reason)))))))

(defn ^:async round [c]
  (let [m (ctx/mem c)]
    (cond
      (nil? (:plan m)) (choose! c)
      (:stop m) (await (retreat! c))
      :else (let [r (await (work! c))]
              (cond
                (= :continue r) :continue
                (= :reached r) (finish! c :reached {})
                :else (stop! c r))))))
