(ns jobs.build.from-plan.walk
  "How jobs.build.from-plan gets the body to a cell: stands, walks, tunnels through rock and the ways out of a hole or a shut-in pocket."
  (:require [jobs.lib.child :as child]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.aim :as aim]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.near :as near]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.build.plan :as plan :refer [facing-of stands-for]]
            [jobs.lib.build.scaffold :as scaffold]
            [jobs.build.from-plan.cells :refer [in-reach in-dig-reach exit-point buried? reaching seeing stand-reach rock-bound? retreat-dists step-back needs-tool?]]
            [jobs.build.from-plan.book :refer [walk-fail result-data]]))

(a/defargs settings
  {::give-up {:default 3 :doc "Refused places or failed walks after which a cell is given up." :spec (a/int-in 1 nil)}
   ::hole-depth {:default 2 :doc "Levels of walls round the body from its feet up that make it a body in a hole, whose walks may climb out." :spec (a/int-in 1 nil)}
   ::cell-s {:default 3 :doc "Seconds one cell of the plan is guessed to take, for the quote and the stay a walk to a stand is priced over." :spec (a/num-in 0 nil)}
   ::pocket-cells {:default 256 :doc "Walkable cells at most which a body whose walk ended shut in counts as shut in a pocket (its walks then climb or dig out); a larger closed room is built in." :spec (a/int-in 1 nil)}})

(defn give-up [] (settings/get settings ::give-up))

(defn cell-s "Seconds one cell of the plan is guessed to take: the quote's figure, and the stay per cell a stand is priced over." [] (settings/get settings ::cell-s))

(defn trace!
  "Note the branch a step took and its cell, for the :stalled ending; kept off the job memory so it never counts as progress."
  [c branch cell]
  (some-> (:trace c) (vreset! [branch cell])))

(defn ^:async leave!
  "One go-to call (child :leave) out of the plan's footprint to the :leave cell (within :leave-range, default 1); dropped once the walk ends (arrived
  or not: the next step judges where the body stands; a walk that did not arrive gives up the plan cell the body stands in, :unreachable). :continue while the walk waits, else :again."
  [c]
  (let [[x y z] (:leave (ctx/mem c))
        r (await (child/call-walk* c :leave {:pos {:x x :y y :z z} :range (if (:mouth (ctx/mem c)) 0 (:leave-range (ctx/mem c) 1)) :ways (cond-> #{:walk :clear-seal} (:mouth (ctx/mem c)) (into [:stair :dig-out])) :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (when-not (= :continue r)
      (ctx/update-mem! c #(cond-> (dissoc % :leave :leave-range :leave-for)
                            (and (not= :done (:kind r)) (:stepped-off %)) (assoc-in [:given-up (:stepped-off %)] :unreachable)
                            (and (not= :done (:kind r)) (:leave-for %)) (plan/count-fail (:leave-for %) :unreachable (give-up)))))
    (if (= :continue r) :continue :again)))

(defn in-hole?
  "Whether the body stands in a hole at least ::hole-depth deep (escape/pit-depth); afloat in water it is not in one."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    (and (not (breath/in-water? (:primitives c)))
         (<= (settings/get settings ::hole-depth) (escape/pit-depth (escape/block-at-of (:primitives c)) [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])))))

(defn shut-in-end
  "End the build :shut-in: go-to, with every way out allowed, gave up with the body still shut in."
  [c]
  (ctx/end c :nothing-possible :shut-in
           {:placed (:placed (ctx/mem c) 0)
            :text (str "build of " (:plan (:args c)) " stopped: the body is shut in and go-to found no way out")}))

(defn sealed-in-shelter?
  "Whether the last walk was refused because the body is sealed in its own shelter, which the night job opens by day: no verdict on the cell or the body's way out."
  [c]
  (let [e (near/last-walk c)]
    (boolean (or (:inside-own-shelter (:data e)) (:inside-own-shelter (:cause e))))))

(declare shut-in-walk?)

(defn chunk-pending?
  "Whether the cell's chunk is not loaded (just after a login, or out of range): it will load, so the build waits for it."
  [p pos]
  (and (some? (.-sensedAt p)) (nil? (u/sensed p (zipmap [:x :y :z] pos)))))

(defn ^:async walk-stand!
  "Walk to stand beside cell with go-near options opts; a walk that is blocked (the stand is then bad), or an arrival
  with the cell unseen (:unseen, :buried; a chunk not loaded yet is waited for) or out of reach, counts a failure. Resolves to go-near's answer; :partial
  (no failure) when the body is sealed in its own shelter. A blocked walk to a stand two or more above the body is a
  climb, not a failure: the stand is only ruled out and the cell sent to the scaffold, which plans stands that see it."
  [c cell stand opts]
  (let [limit (give-up)
        w (await (near/go-near! c (zipmap [:x :y :z] stand) 0 opts))
        climb? (and (not (:dig? cell)) (not (in-hole? c)) (<= (+ 2 (js/Math.floor (:y (u/self-pos c)))) (second stand)))]
    (when (and (= :blocked w) (not (sealed-in-shelter? c)))
      (ctx/update-mem! c #(cond-> (update % :bad-stands (fnil conj []) stand)
                            (not climb?) (walk-fail (:pos cell) :unreachable limit (u/self-pos c))
                            (= :budget (:why (:data (near/last-walk c)))) (update :budget-streak (fnil inc 0))
                            (or climb? (not (or (:dig? cell) (in-hole? c) (shut-in-walk? c) (= :stuck (:why (:data (near/last-walk c)))))))
                            (update :needs-scaffold (fnil conj #{}) (:pos cell)))))
    (when (#{:there :partial} w) (ctx/update-mem! c dissoc :budget-streak))
    (when (and (= :there w) (nil? (:found cell)))
      (let [pos (:pos cell)]
        ;; a cell outside the view cone is seen only by turning to it
        (when (dig-look/unknown? (:primitives c) pos) (await (dig-look/look-at! c pos)))
        (when (nil? (u/seen-block (:primitives c) (zipmap [:x :y :z] pos)))
          (when-not (chunk-pending? (:primitives c) pos)
            (ctx/update-mem! c walk-fail pos (if (buried? (:primitives c) pos) :buried :unseen) limit (u/self-pos c))))))
    (when (and (= :there w) (:found cell) (empty? (if (:dig? cell) (in-dig-reach c [cell]) (in-reach c [cell]))))
      (ctx/update-mem! c walk-fail (:pos cell) :unreachable limit (u/self-pos c)))
    (cond
      (and (= :blocked w) (sealed-in-shelter? c)) :partial
      (and (= :there w) (nil? (:found cell)) (chunk-pending? (:primitives c) (:pos cell))) :partial
      :else w)))

(defn in-pocket?
  "Whether the body stands in a closed room of at most ::pocket-cells walkable cells, over the world go-to's plans read toward
  the last walk's goal (wworld/unseen-kind of escape/walk-spec): it confirms go-to's shut-in verdict."
  [c]
  (let [p (:primitives c)]
    (reach-room/enclosed? p (wworld/unseen-kind p (escape/walk-spec c (near/last-goal c))) #{} (settings/get settings ::pocket-cells))))

(defn shut-in-walk?
  "Whether the last walk ended because the body is shut in (go-to refuses to dig or build out of a restricted walk) and the
  room it can walk in is a pocket of at most ::pocket-cells: a larger closed hall still has room to bridge or pillar in."
  [c]
  (boolean (and (:shut-in (:data (near/last-walk c))) (in-pocket? c))))

(defn exit-of
  "Where the body leaves a cell it stands in: in a block it cut into rock, one step back toward the tunnel mouth (the cell
  it left is then filled from there); else outside the plan's box."
  [c cells]
  (let [mouth (:mouth (ctx/mem c))
        feet (mapv js/Math.floor (let [b (u/self-pos c)] [(:x b) (:y b) (:z b)]))]
    (or (when mouth (step-back (retreat-dists (:primitives c) mouth cells) feet))
        mouth
        (exit-point cells (u/self-pos c)))))

(defn shut-in?
  "Whether rock holds cell in, or a dig or place there found no stand that sees it after the body cut into rock."
  [c cell]
  (or (rock-bound? (:primitives c) cell) (contains? (:sightless (ctx/mem c)) (:pos cell))))

(defn ^:async tunnel-to!
  "A cell shut in by rock: with a pickaxe, cut a way (jobs.access.tunnel) to the deepest such cell, so the body stands in the
  plan's cells and fills them while backing out; without one the cell is given up :no-tool at once. :continue while the
  tunnel waits, :again once it is cut or failed (a failure counts against the cell), nil when the cell is open to air or
  the body already stands at the end of the way. A cell beyond the body's reach with faces never seen is walked to first (rock-bound?)."
  [c cells cell]
  (let [p (:primitives c)
        closed (merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        shut? #(shut-in? c %)]
    (when (shut? cell)
      (let [pos (:pos cell)
            near? (<= (u/eye-dist (u/self-pos c) pos) (plan/reach))]
        (if (and near? (not (contains? (:rock-looked (ctx/mem c)) pos)))
          (do (ctx/update-mem! c update :rock-looked (fnil conj #{}) pos)
              (await (dig-look/look-at! c pos))
              :again)
          (if (needs-tool? p cell)
            (let [[x y z] pos
                  by-rock? (some #(= :no-tool (get (:given-up (ctx/mem c)) %)) [[(inc x) y z] [(dec x) y z] [x (inc y) z] [x (dec y) z] [x y (inc z)] [x y (dec z)]])]
              (cond
                (or near? by-rock?)
                (do (ctx/update-mem! c assoc-in [:given-up pos] :no-tool)
                    :again)))
            (let [body (u/self-pos c)
                  _ (ctx/update-mem! c update :mouth #(or % (mapv js/Math.floor [(:x body) (:y body) (:z body)])))
                  open (filter #(and (not= :match (:answer %)) (not (:companion %)) (not (contains? closed (:pos %))) (shut? %)) cells)
                  deepest (if (seq open) (apply max-key #(u/dist body (zipmap [:x :y :z] (:pos %))) open) cell)
                  r (await (ctx/call c :tunnel 'jobs.access.tunnel {:target (:pos deepest) :through (:plan (:args c)) :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
            (cond
              (= :continue r) :continue
              (and (= :done (:kind r)) (= :already (:reason (:data r)))) nil
              (= :done (:kind r)) (do (ctx/update-mem! c #(-> (update % :sightless disj (:pos deepest)) (update-in [:tunnels (:pos deepest)] (fnil inc 0)))) :again)
              :else (do (ctx/update-mem! c walk-fail (:pos deepest) :unreachable (give-up) (u/self-pos c)) :again)))))))))

(defn stand-choices
  "{:all :inner} of cell: the stands to place or dig it from (stands-for), and with none standable beside it, the cells
  beside it standable when the plan's own cells count as open (inner)."
  [c cells cell]
  (let [body-y (js/Math.floor (:y (u/self-pos c)))
        planned (set (map :pos cells))
        bad (set (:bad-stands (ctx/mem c)))
        facing (when-not (or (:click cell) (:dig? cell)) (facing-of (:want cell)))]
    {:all (stands-for (:primitives c) (:pos cell) body-y facing planned bad)
     :inner (when-not (plan/grounded-stands (:primitives c) (:pos cell) body-y facing planned bad)
              (seq (remove bad (plan/grounded (:primitives c) (plan/stand-cells (:pos cell) (second (:pos cell)) facing #{})))))}))

(defn place-here?
  "Whether the cell in reach is placed from where the body stands: its eye sees it, or no stand of stand-choices does
  (the place job's own walk then searches wider for one that sees it)."
  [c cells cell]
  (or (aim/body-sees? c (:pos cell))
      (empty? (seeing c (:all (stand-choices c cells cell)) (:pos cell)))))

(defn- open-cells
  "The open cells of cell's layer: those still to place or dig the same way, not given up."
  [c cells cell]
  (let [closed (merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        y (second (:pos cell))]
    (filter #(and (= y (second (:pos %))) (= (boolean (:dig? cell)) (boolean (:dig? %))) (not= :match (:answer %))
                  (not (:companion %)) (not (contains? closed (:pos %))))
            cells)))

(defn stand-eye [[x sy z]] [(+ x 0.5) (+ sy game/eye-height) (+ z 0.5)])

(defn stand-price
  "Seconds stand costs the build (jobs.lib.cost): the walk there, plus cover-tour-s from its eye over the open cells of
  cell's layer, those it reaches wherever it stands in the cell (as reaching) placed from it and the rest priced by the walks on."
  [c cells cell stand]
  (let [[x sy z] stand]
    (+ (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) {:x (+ x 0.5) :y sy :z (+ z 0.5)})}))
       (cost/cover-tour-s (stand-eye stand) (map #(mapv (partial + 0.5) (:pos %)) (open-cells c cells cell)) (stand-reach)))))

(defn stand-stay-s
  "Seconds the build stays at stand: cell-s for each open cell of cell's layer it reaches from there (the cell itself at least)."
  [c cells cell stand]
  (let [eye (stand-eye stand)
        reached (count (filter #(<= (apply js/Math.hypot (map - eye (mapv (partial + 0.5) (:pos %)))) (stand-reach)) (open-cells c cells cell)))]
    (* (max 1 reached) (cell-s))))

(defn ^:async walk-to-stand!
  "Walk to the cheapest (stand-price) stand cell beside cell, among those that see it first (:continue while the walk waits, else :again); a cell that cannot be walked to, or is still out of reach (or unseen, as :unseen) on arrival, counts a failure."
  [c cells cell]
  (trace! c :walk-to! (:pos cell))
  (let [{:keys [all inner]} (stand-choices c cells cell)
        stands (or (seq (seeing c all (:pos cell))) (seq (reaching c all (:pos cell))) inner all)
        stand (second (first (sort-by first (map (juxt #(stand-price c cells cell %) identity) stands))))]
    (cond
      (not stand)
      (do (ctx/update-mem! c walk-fail (:pos cell) :unreachable (give-up) (u/self-pos c))
          :again)
      :else
      (let [shut? (or (in-hole? c) (shut-in-walk? c))
            w (await (walk-stand! c cell stand (cond-> {:stay-s (stand-stay-s c cells cell stand)}
                                               (not shut?) (assoc :ways (cond-> #{:walk :clear-seal} (sh-inside/sheltered-in c) (into [:stair :dig-out]))))))]
        ;; an arrival in reach that does not see the cell, where another stand would, is a bad stand: else the step walks to it again
        (when (and (= :there w) (:found cell) (not (:dig? cell)) (seq (in-reach c [cell])) (not (place-here? c cells cell)))
          (ctx/update-mem! c #(-> (update % :bad-stands (fnil conj []) stand) (walk-fail (:pos cell) :unreachable (give-up) (u/self-pos c)))))
        (cond
          (and shut? (= :blocked w) (:shut-in (:data (near/last-walk c)))) (shut-in-end c)
          (= :partial w) :continue
          :else :again)))))


(defn ^:async walk-to!
  "Tunnel toward a cell with rock between it and every stand, else walk-to-stand!."
  [c cells cell]
  (or (await (tunnel-to! c cells cell)) (await (walk-to-stand! c cells cell))))

(defn ^:async reach-cell!
  "Walk to a stand for the buildable cell; where no ground stand within :reach of it exists, raise a temporary one (jobs.lib.build.scaffold)."
  [c cells cell]
  (let [body (u/self-pos c)
        facing (when-not (or (:click cell) (:dig? cell)) (facing-of (:want cell)))
        stands (plan/grounded-stands (:primitives c) (:pos cell) (js/Math.floor (:y body)) facing (set (map :pos cells)) (set (:bad-stands (ctx/mem c))))
        grounded (when-not (contains? (:needs-scaffold (ctx/mem c)) (:pos cell)) (reaching c stands (:pos cell)))]
    (or (await (tunnel-to! c cells cell))
        (when-not (seq grounded) (await (scaffold/start! c cells cell (result-data c cells))))
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
