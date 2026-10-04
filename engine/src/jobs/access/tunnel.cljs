(ns jobs.access.tunnel
  (:require [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [jobs.access.stair :as stair]))

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
  neither air nor fluid), plus the target cell as the last opening (a fluid beside it would flood the stand). The
  shortest valid line wins, ties to the entry nearest the body. No valid line stops before any walk or dig with the
  reason all judged lines share (:zone, :footprint, :hazard, :cave-below, :no-floor, :fluid-in-cut, :not-loaded,
  :no-zones), :too-far when no line fits within :max-length, else :no-approach with :headings (each heading's first
  reason). Then the body walks to the entry (jobs.debug.walk-plan; failing: :walk-in-failed), the stair child cuts
  and walks its steps, and each run step is judged again right before each dig (stair's dig!: :hazard, :zone,
  :no-tool, :inventory-full, :refills, :dig-failed). Before each further run step, and at the stand, the way back to
  the entry is planned on a fresh pathWorld and must be whole and walkable (else :no-way-back, and the body stays
  where it is). Any other stop after the body has entered walks it back to the entry first (:out true when it
  arrived). The body's cell is the progress: on the stair line the stair child goes on, on the run line the run, at
  the stand it is done; anywhere else it walks to the entry. A cell dug before a cut is air and is not dug again.
  Dug cells are left and recorded. A nil zone list declines the check (one tunnel.declined warn). Hands over
  {:status :done|:stopped :reason :reached|kw :target :entry :heading :stand :at [x y z] :dug [{:cell :block}]}
  plus detail, also as a :tunnel.done info or :tunnel.stopped warn event.")

(def args
  {:target {:doc "the buried block [x y z]" :default nil}
   :max-length {:doc "longest line, in blocks along the heading from the entry to the target" :default 24}
   :accept {:doc "hazards taken: #{:water :lava :falling-block}" :default #{}}})

(def heading-order [:north :east :south :west])

(defn check [c]
  (if (nil? (ctx/zones c))
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

(defn target-stop
  "Why opening the target from the stand would not do, or nil."
  [in target stand accept]
  (let [v (stair/cell-verdict (assoc in :cell target :feet stand) [target])]
    (cond
      (nil? ((:block-at in) target)) {:reason :not-loaded :cell target}
      (not (:ok v)) (assoc (dissoc v :ok) :cell target)
      (not-every? (comp accept stair/hazard-key) (:hazards v)) {:reason :hazard :cell target :hazards (:hazards v)})))

(defn line-stop
  "The first stop on the line of plan, or nil when every step and the target may be cut."
  [in plan accept]
  (let [{:keys [steps stand]} (line plan)]
    (or (some (fn [[feet cells]] (stair/stop-of (assoc in :feet feet) cells accept)) steps)
        (target-stop in (:target plan) stand accept))))

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

(defn dist2 [[x y z] [a b c]] (+ (* (- x a) (- x a)) (* (- y b) (- y b)) (* (- z c) (- z c))))

(defn approach
  "The line to cut to target (the plan map, see fits) from rules input in (without :cell and :feet), the body at
  feet; else {:reason ...} (see the doc), with :headings when the headings fail for different reasons."
  [in target feet max-length accept]
  (if (nil? (:zones in))
    {:reason :no-zones}
    (let [per (into {} (map (fn [h] [h (best-on-heading in target h max-length accept)])) heading-order)
          plans (keep (comp :plan per) heading-order)]
      (if (seq plans)
        (first (sort-by (juxt :length #(dist2 feet (:entry %))) plans))
        (let [stops (into {} (map (fn [[h r]] [h (:stop r)])) per)
              real (remove #(= :too-far (:reason %)) (vals stops))
              reasons (set (map :reason real))]
          (cond
            (empty? real) {:reason :too-far}
            (= 1 (count reasons)) (first real)
            :else {:reason :no-approach :headings (into {} (map (fn [[h s]] [h (:reason s)])) stops)}))))))

(defn feet-of [c] (stair/feet-of c))

(defn finish!
  "Hand the result over and end: :done when reached, else :stopped (warn)."
  [c reason detail]
  (let [{:keys [plan dug]} (ctx/mem c)
        result (merge {:status (if (= :reached reason) :done :stopped) :reason reason :target (:target (:args c))
                       :at (feet-of c) :dug (or dug [])}
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

(defn ^:async stair-part!
  "One round of the stair child, also its last one at the stair's end (it hands over what it dug): :continue, or a
  stop map when the stair stopped."
  [c {:keys [heading dir target]}]
  (let [r (await (ctx/call-child c :stair 'jobs.access.stair
                                 {:dir dir :heading heading :y (target 1) :accept (set (:accept (:args c)))}))]
    (if (not= :done r)
      :continue
      (let [res (ctx/child-result c :stair)]
        (ctx/update-mem! c #(-> % (update :dug (fnil into []) (:dug res)) (assoc :stair-done (= :done (:status res)))))
        (if (= :done (:status res))
          :continue
          (assoc (select-keys res [:cell :hazards :zone :plan :fluid :block :tool :walk :why]) :reason (:reason res)
                 :in :stair))))))

(defn ^:async work!
  "One bounded piece of the way from the body's place: :reached, :continue or a stop map."
  [c]
  (let [{:keys [plan checked stair-done]} (ctx/mem c)
        {:keys [entry heading dir steps run]} plan
        {:keys [end stand]} (line plan)
        feet (feet-of c)
        j (run-index end feet heading run)
        i (when (pos? steps) (stair/stair-index entry feet dir heading steps))]
    (ctx/update-mem! c stair/record-dug (:block-at (stair/rules-in c feet)))
    (cond
      (and i (not stair-done)) (await (stair-part! c plan))
      j (if-let [stop (when (and (not= feet entry) (not= j checked)) (stair/way-back c entry))]
          stop
          (do (ctx/update-mem! c assoc :checked j)
              (if (= feet stand) :reached (await (run-step! c feet)))))
      :else (let [r (await (walk-to! c :in entry))]
              (cond
                (= :continue r) :continue
                (and (= :arrived (:status r)) (= entry (feet-of c))) :continue
                :else {:reason :walk-in-failed :cell entry :walk r :outside true})))))

(defn ^:async retreat!
  "Walk back to the entry after a stop, then finish with it."
  [c]
  (let [{:keys [stop plan]} (ctx/mem c)
        r (await (walk-to! c :out (:entry plan)))]
    (if (= :continue r)
      :continue
      (finish! c (:reason stop) (assoc (dissoc stop :reason) :out (= (:entry plan) (feet-of c)) :walk-out r)))))

(defn stop!
  "After a stop: finish where the body is (outside, at the entry, or no way back), else walk out first."
  [c {:keys [reason outside] :as stop}]
  (if (or outside (= :no-way-back reason) (= (feet-of c) (:entry (:plan (ctx/mem c)))))
    (finish! c reason (dissoc stop :reason :outside))
    (do (ctx/update-mem! c assoc :stop stop) :continue)))

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
          (do (ctx/update-mem! c assoc :plan a :dug [])
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
