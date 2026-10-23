(ns jobs.lib.flee-search
  "The one priced flee search: where a body fleeing its chasers walks to.
  Candidates: ring ends (x-z discs at ::bearings headings, reach blocks off: the planner finds the height, so a slope, a
  stair or a cliff is its business) and the targets a caller names (a goal, a home). Each is planned by the go-to
  planner over what the body has seen, costed as go-to's walks are (known dangers, darkness, unseen cells), within
  ::max-nodes (a named target ::target-max-nodes) and the ring ends together within ::ring-nodes, ring headings away
  from the chasers first. A heading with no route gives the end of the planner's partial route, a pocket (:found? false). Ring
  routes plan through unseen cells as through seen ones (no rock guess), a flee step walks only the seen part.
  Each end is priced (the planner's price plus the death the route risks past the known threats, a death about where
  the body stands: cost/route-death-cost, plus its unseen part at the worse case: unseen-price) and scored against
  the chasers: :arrive, the seconds the first of them takes to reach it (cost/arrival-s over its way, its threat
  field's; the body timed at cost/flee-walk-speed, cost/flee-stop-s between steps); :caught?, whether one lands
  a blow on the route (cost/caught-on?); :score, arrive less price. pick chooses a flee step's end."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [engine.path.planner.base :as base]
            [engine.path.planner-tuned :as planner]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threat-field :as field]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::bearings {:default 12 :doc "Headings round the body a flee search plans a ring end at." :spec (a/int-in 1 64)}
   ::horizon-s {:default 8 :doc "Seconds of a flight (reach) whose distance is the ring a flee search plans its ends on: a way that leads that far is open, one that stops short a pocket (8 s at the flee pace, about 22 blocks: a dead end that far ahead shows)." :spec (a/num-in 0.5 nil)}
   ::max-nodes {:default 600 :doc "Planner nodes one ring end's search may expand: past it the end is its partial route's." :spec (a/int-in 50 nil)}
   ::target-max-nodes {:default 2400 :doc "Planner nodes the search of a named target (a goal, a home) may expand." :spec (a/int-in 50 nil)}
   ::open-share {:default 0.5 :doc "Share of the reach a ring end's partial route must cover to count as a way on (a lake, a tree or a wall at the ring); one that stops shorter is a pocket." :spec (a/num-in 0 1)}
   ::step-s {:default 1 :doc "Seconds a flee step walks of the chosen route before the flight stops to choose again (the cell of the route that far in; a cover end is walked to whole)." :spec (a/num-in 0.1 nil)}
   ::ring-nodes {:default 7200 :doc "Planner nodes the ring ends of one flee search may expand together; the ring headings not planned by then are left out (the ones most away from the chasers are planned first)." :spec (a/int-in 50 nil)}
   ::hazard-clearance {:default 2.5 :doc "Blocks a flee route keeps from a remembered hazard: a route with a cell nearer is not taken." :spec (a/num-in 0 nil)}})

(defn reach
  "Blocks to the ring of a flee search for a body with food food: what a flight covers in ::horizon-s, its steps of
  ::step-s at cost/flee-walk-speed with cost/flee-stop-s between them."
  [food]
  (let [step (settings/get settings ::step-s)]
    (* (cost/flee-walk-speed food) (settings/get settings ::horizon-s) (/ step (+ step (cost/flee-stop-s))))))

(defn away
  "The unit [ux uz] from `from` away from the chasers ({:pos}), each weighing 1/distance; [1 0] when they cancel out."
  [from chasers]
  (let [[sx sz] (reduce (fn [[ax az] {:keys [pos]}]
                          (let [dx (- (:x from) (:x pos)) dz (- (:z from) (:z pos))
                                d (max 1 (js/Math.hypot dx dz))]
                            [(+ ax (/ dx (* d d))) (+ az (/ dz (* d d)))]))
                        [0 0] chasers)
        n (js/Math.hypot sx sz)]
    (if (pos? n) [(/ sx n) (/ sz n)] [1 0])))

(defn ring
  "The ring ends round from: n columns {:x :z} reach blocks off, the one most away from the chasers first and the rest
  by how far they turn from it."
  [from chasers reach n]
  (let [[ux uz] (away from chasers)
        a0 (js/Math.atan2 uz ux)
        turn (fn [i] (* (if (odd? i) 1 -1) (js/Math.ceil (/ i 2)) (/ (* 2 js/Math.PI) n)))]
    (->> (range n)
         (map (fn [i] (let [a (+ a0 (turn i))]
                        {:x (js/Math.floor (+ (:x from) (* reach (js/Math.cos a))))
                         :z (js/Math.floor (+ (:z from) (* reach (js/Math.sin a))))})))
         distinct
         vec)))

(defn timed-cells
  "The cells {:x :y :z :t} of planner steps, :t the seconds into the route: its seconds spread over its length, plus
  cost/flee-stop-s for each ::step-s walked before (a flight stops to choose its next step)."
  [steps seconds]
  (let [cells (mapv (fn [^js s] {:x (.-x s) :y (.-y s) :z (.-z s)}) steps)
        legs (map u/dist cells (rest cells))
        total (reduce + 0 legs)
        at (reductions + 0 legs)
        step (settings/get settings ::step-s)
        stop (cost/flee-stop-s)]
    (mapv (fn [cell d] (let [walk (if (pos? total) (* seconds (/ d total)) 0)]
                         (assoc cell :t (+ walk (* stop (js/Math.floor (/ walk step)))))))
          cells at)))

(defn pace
  "How much longer than the planner's seconds a flight's steps by a body with food food take on a route: the planner
  times a sprint, a flight's steps go at cost/flee-walk-speed."
  [food]
  (/ (cost/body-speed 20) (cost/flee-walk-speed food)))

(defn dwelt
  "Timed cells (timed-cells) with :s, the seconds the body spends in each (what death-chance reads)."
  [cells]
  (mapv (fn [prev cell] (assoc cell :s (- (:t cell) (:t prev 0)))) (cons nil cells) cells))

(defn routed
  "A planner result as a flee end {:found? :end :s :priced :cells}, its seconds (:s, the cells' :t) times pace; nil when
  it ends where the body stands (no route nearer)."
  [^js r pace]
  (let [^js path (.-path r)
        steps (some-> path .-steps array-seq)]
    (when (and (contains? #{"found" "partial"} (.-status r)) (next steps))
      (let [^js cost (.-cost path)
            s (* pace (.-seconds cost))
            cells (timed-cells steps s)]
        {:found? (= "found" (.-status r)) :end (select-keys (peek cells) [:x :y :z]) :s s
         :priced (or (.-priced cost) (.-seconds cost)) :cells cells}))))

(defn cell-of [{:keys [x y z]}] [x y z])

(defn seen-split
  "Route r (routed) with :seen-n, how many of its cells from the start the body has seen (feet and head), and :unseen,
  the first cell past them it has not ([x y z], nil when it saw them all): a flee step walks only the seen part
  (step-of), the rest is priced as unseen (unseen-price)."
  [p {:keys [cells] :as r}]
  (let [unseen (fn [{:keys [x y z]}] (some #(when-not (u/seen-block p {:x x :y % :z z}) [x % z]) [y (inc y)]))
        i (or (first (keep-indexed #(when (and (pos? %1) (unseen %2)) %1) cells)) (count cells))]
    (cond-> (assoc r :seen-n i)
      (< i (count cells)) (assoc :unseen (unseen (nth cells i))))))

(defn unseen-price
  "Seconds the unseen part of r (seen-split) adds to its price (cost/unseen-way-cost: from its last seen cell on, kit
  the body's, underground? whether it is under ground), trapped-at a fn of that cell: what the chasers met there on the way back cost; 0 when it saw it all."
  [{:keys [cells seen-n s unseen]} kit trapped-at underground?]
  (if-not unseen
    0
    (let [split (nth cells (dec seen-n))]
      (cost/unseen-way-cost {:unseen-s (- s (:t split)) :trapped (trapped-at split) :kit kit :underground? underground?}))))

(defn ^:async plan-to!
  "The planner's result for query over pw, its search within max-nodes nodes, in slices with yields between them."
  [c pw policy query max-nodes]
  (let [options (wplan/with-drops (wplan/plan-options pw (walk/default-weight) (executor/planner-limits policy (wworld/solid-fn pw))
                                                      (assoc (wplan/box-of policy) :maxNodes max-nodes))
                                  policy (wworld/landing-seen (:primitives c)))
        ^js p (planner/create-plan (.-snapshot pw) query options)]
    (loop []
      (wplan/stop-if-cut! c)
      (if (.step p wplan/chunk-expansions)
        (.result p)
        (do (await (wplan/yield!)) (recur))))))

(defn query
  "The planner query to target: a named one ({:pos :range}) within its range, a ring end ({:x :z}) anywhere in its
  column within a block."
  [c target]
  (if-let [{:keys [x y z]} (:pos target)]
    (wplan/plan-query c (mapv js/Math.floor [x y z]) (:range target 1))
    (doto (wplan/plan-query c [(:x target) 0 (:z target)] 1)
      (-> .-goal (unchecked-set "kind" "column")))))

(defn costed
  "The pathWorld a flee route plans over: what the body has seen (go-to's :unknown, the cell goal spared by the rock
  guess; with priced? no rock guess: an unseen cell keeps the planner's unknown price and the search adds its worse
  case, unseen-price), costed as go-to's walks but for the threats keep? leaves out (the chasers: their danger is the
  race); nil when the body cannot sense the world."
  [c goal keep? priced?]
  (some-> (wworld/path-world (:primitives c) (wworld/with-seeds (cond-> (wworld/unknown-spec) priced? wworld/without-rock) goal nil))
          (#(wworld/costed-world c % {:dangers? (wworld/dangers?) :dark? (wworld/dark?) :keep? keep?}))))

(defn floor-cell [pos] (mapv #(js/Math.floor (% pos)) [:x :y :z]))

(defn ^:async price!
  "Each of targets (named ones {:pos :range :kind}, then ring ends {:x :z}) planned from where the body stands (see the ns
  doc): [{:target :found? :open? :end :s :priced :price :cells :seen-n :unseen}] (:open? found, or a ring end whose
  partial route covers ::open-share of reach), the ones the planner found no way nearer to, and the ring ends
  left when ::ring-nodes ran out (named ones have their own node cap), left out; its meta {:nodes :planned} the nodes
  expanded and the targets planned. opts {:y-margin :reach :chasers}: blocks past body and end the routes may plan, blocks
  to the ring, the chasers (their danger is the race, so death-chance weighs only the other known threats; met on the
  way back from an unseen dead end, they catch the body: unseen-price)."
  [c targets {:keys [y-margin reach chasers]}]
  (let [p (:primitives c)
        from (u/self-pos c)
        open-at (* (settings/get settings ::open-share) (or reach 0))
        chased (set (map #(floor-cell (:pos %)) chasers))
        other? #(not (chased (floor-cell (:pos %))))
        ring-world (costed c nil other? true)
        policy (cond-> (wworld/body-policy c) y-margin (assoc :y-margin y-margin))
        body (threats/death-body c)
        class {:underground? (sh-sense/buried? p) :night? (sh-sense/dark-outside? p) :threats (threats/threats-of c other?)}
        caught-by (mapv #(assoc % :caught 1) (threats/threats-of c (complement other?)))
        trapped-at (fn [cell] (cost/route-death-cost body (assoc class :cells [cell] :threats caught-by) cell))
        kit (wworld/kit-of c)
        slow (pace (.-food (.self p)))]
    (if (nil? ring-world)
      []
      (loop [[t & more] targets out [] nodes 0 ring-left (settings/get settings ::ring-nodes) planned 0]
        (if (or (nil? t) (and (nil? (:pos t)) (<= ring-left 0)))
          (with-meta out {:nodes nodes :planned planned})
          (let [pw (if-let [pos (:pos t)] (costed c (floor-cell pos) other? false) ring-world)
                ^js res (await (plan-to! c pw policy (query c t)
                                         (if (:pos t) (settings/get settings ::target-max-nodes) (min ring-left (settings/get settings ::max-nodes)))))
                spent (or (.-expanded res) 0)
                r (some->> (routed res slow) (seen-split p))]
            (recur more (if r
                          (conj out (assoc r :target t
                                             :open? (or (:found? r) (and (nil? (:pos t)) (>= (u/dist from (:end r)) open-at)))
                                             :price (+ (:priced r)
                                                       (cost/route-death-cost body (assoc class :cells (dwelt (:cells r))) from)
                                                       (unseen-price r kit trapped-at (:underground? class)))))
                          out)
                   (+ nodes spent)
                   (cond-> ring-left (nil? (:pos t)) (- spent))
                   (inc planned))))))))

(defn way-at
  "The blocks of chaser's way ({:way}: field/way-fn) to pos, nil when not known."
  [{:keys [way]} {:keys [x y z]}]
  (when way (way (js/Math.floor x) (js/Math.floor y) (js/Math.floor z))))

(defn arrive-at
  "Seconds the first of chasers ({:name :pos :way}) takes to reach pos (cost/arrival-s over its way, way-at), ##Inf
  with none."
  [chasers pos]
  (reduce min ##Inf (map #(cost/arrival-s (:name %) (:pos %) pos (way-at % pos)) chasers)))

(defn with-ways
  "chasers each with :way, the field/way-fn of its threat field to within cost/melee-reach, when it has a :mob name
  (a shot spot has none) other than a creeper or a shooter (a blast or an arrow passes where it cannot walk: the
  straight line stands)."
  [c chasers]
  (mapv (fn [{:keys [mob pos] :as ch}]
          (cond-> ch (and mob (not= "creeper" mob) (not= :projectile (threat/damage-type mob)))
            (assoc :way (field/way-fn (field/field-of c {:mob mob :pos pos}) (floor-cell pos) (cost/melee-reach)))))
        chasers))

(def unreached-s
  "The arrival a :score counts for an end no chaser can get to: past any real one, so such ends rank first and among
  themselves by price."
  1e6)

(defn scored
  "priced (price!) scored against chasers [{:name (mob-kind) :pos}] from where the body stands (from): :arrive (the
  seconds the first chaser takes to the end, ##Inf with none), :gain (arrive less the chasers' arrival at from: what
  the walk buys over standing still; 0 where no chaser can get to the body already), :caught? (by a chaser not :calm?, on the route past where the body stands, with no race margin: a margin is for a
  race to one place, and a chaser a few blocks off would cut every first step) and :score (see the ns doc)."
  [chasers from priced]
  (let [here (arrive-at chasers from)]
    (mapv (fn [{:keys [end cells price] :as e}]
          (let [arrive (arrive-at chasers end)]
            (assoc e :arrive arrive :gain (if (js/isFinite here) (- arrive here) 0)
                     :caught? (boolean (some (fn [ch] (cost/caught-on? {:chaser (:name ch) :pos (:pos ch) :route (rest cells) :margin-s 0 :way #(way-at ch %)})) (remove :calm? chasers)))
                     :score (- (min arrive unreached-s) price))))
        priced)))

(defn covers
  "Ends on the routes of scored where cover first holds (covered?: a cell -> whether it is out of every ranged mob's line
  of fire): each route cut there and scored again against chasers, :cover? true."
  [chasers from covered? ends]
  (scored chasers from
          (keep (fn [{:keys [cells s price priced] :as e}]
                  (when-let [i (first (keep-indexed #(when (and (pos? %1) (covered? (select-keys %2 [:x :y :z]))) %1) cells))]
                    (let [part (subvec cells 0 (inc i))
                          share (if (pos? s) (/ (:t (peek part)) s) 1)]
                      (assoc e :end (select-keys (peek part) [:x :y :z]) :cells part :cover? true :found? true :open? true
                               :s (:t (peek part)) :price (* share price) :priced (* share priced)))))
                ends)))

(defn hazard-near?
  "Whether a cell of route cells lies within ::hazard-clearance of one of hazards (positions)."
  [hazards cells]
  (let [r (settings/get settings ::hazard-clearance)]
    (boolean (some (fn [cell] (some #(< (u/dist % cell) r) hazards)) cells))))

(defn step-of
  "The cell a flee step walks to of a picked end entry: of its route's seen part (:seen-n cells, at least the first past
  the start: a step into the dark is one cell), the first cell ::step-s or more in that is not in refused ([x y z]
  cells go-to refused) nor where the route starts, else the part's last; a cover end whole; nil when they are all
  refused."
  ([e] (step-of e #{}))
  ([{:keys [end cells cover? seen-n]} refused]
   (let [s (settings/get settings ::step-s)
         start (some-> (first cells) cell-of)
         walk (subvec cells 0 (min (count cells) (max 2 (or seen-n (count cells)))))
         ok #(when-not (or (contains? refused (cell-of %)) (= start (cell-of %))) (select-keys % [:x :y :z]))]
     (if cover?
       (ok end)
       (or (some #(when (>= (:t %) s) (ok %)) walk) (ok (peek walk)))))))

(defn choose
  "The end entry a flee step takes among scored ends (scored, covers), its :step the cell the step walks to (step-of,
  past the cells go-to refused: refused [x y z]); nil when cornered. Left out: caught ones, those (but cover) that gain
  nothing on the chasers over standing still, pockets that gain less than their walk takes, those with every step
  cell refused, those never? holds of (at the step cell or the end, as for wet?), those passing near hazards; wet ones (wet?) while a dry one is left, all with
  dry-only?; then the ends each of avoid (cell -> bool, most wanted first) holds of, while that leaves some: the route
  there is the planner's, its first steps often shared by every end. Of the rest, in this order: a :cover? end, a
  preferred one (prefer?: end entry -> bool, e.g. a safe home), an :open? end, and only when none of those is left (a
  closed cave, a wall's foot) a pocket; the best :score of each. Open ways that every chaser cuts leave the pockets
  out: they are traps."
  [ends {:keys [refused never? hazards wet? dry-only? avoid prefer?]}]
  (let [either (fn [f] (fn [e] (or (f (:step e)) (f (:end e)))))
        ok (->> ends
                (remove :caught?)
                (filter #(or (:cover? %) (pos? (:gain %))))
                (filter #(or (:open? %) (> (:gain %) (:s %))))
                (keep #(when-let [step (step-of % (or refused #{}))] (assoc % :step step)))
                (remove #(and never? ((either never?) %)))
                (remove #(hazard-near? hazards (:cells %))))
        dry (if wet? (remove (either wet?) ok) ok)
        ok (if (or dry-only? (seq dry)) dry ok)
        ok (reduce (fn [os bad?] (let [kept (remove #(bad? (:end %)) os)] (if (seq kept) kept os))) ok avoid)
        best (fn [xs] (when (seq xs) (apply max-key :score (reverse xs))))
        ways (filter #(and (:open? %) (nil? (:pos (:target %)))) ends)
        traps? (and (seq ways) (every? :caught? ways))]
    (or (best (filter :cover? ok))
        (when prefer? (best (filter prefer? ok)))
        (best (filter :open? ok))
        (when-not traps? (best ok)))))

(defn into-unseen?
  "Whether the step of end entry e (choose) is cut short by a cell the body has not seen: its route's seen part ends
  less than ::step-s in (a cover end is walked to whole)."
  [{:keys [unseen cover? cells seen-n]}]
  (boolean (and unseen (not cover?) (< (:t (nth cells (dec seen-n))) (settings/get settings ::step-s)))))

(defn pick
  "The cell the flee step choose takes walks to, nil when cornered."
  [ends opts]
  (:step (choose ends opts)))

(defn ^:async search!
  "The flee search from where the body stands: the targets (named, {:pos :range :kind}) and the ring ends at reach
  (default: reach for the body's food) priced (price!) and scored against chasers ([{:name :pos}]), with covers on
  their routes when covered? is given. opts {:reach :y-margin :covered?}."
  [c chasers targets {:keys [covered?] :as opts}]
  (let [self (.self (:primitives c))
        from (u/self-pos c)
        chasers (with-ways c chasers)
        r (or (:reach opts) (reach (.-food self)))
        t0 (js/performance.now)
        priced (await (price! c (into (vec targets) (ring from chasers r (settings/get settings ::bearings))) (assoc opts :reach r :chasers chasers)))
        ends (scored chasers from priced)]
    (ctx/emit! c :retreat.search-work :debug (assoc (meta priced) :ms (js/Math.round (- (js/performance.now) t0)) :targets (count targets)))
    (cond-> ends covered? (into (covers chasers from covered? ends)))))

(defn unseen-ahead
  "The first unseen cells ([x y z], nearest first) of ends' routes: where to look before the flight counts itself
  cornered."
  [ends]
  (->> ends (sort-by :s) (keep :unseen) distinct seq))

(defn caught-chance
  "The chance a flight from where search ends (search!) ran is caught (cost/flight-caught-chance): its ring routes the
  ways (a pocket is none: the chaser follows it in), those a chaser catches or that gain nothing on the chasers cut,
  and how many of the rest run unseen; underground? whether the body is under ground."
  [ends underground?]
  (let [ways (filter #(and (:open? %) (not (:cover? %)) (nil? (:pos (:target %)))) ends)
        cut? #(or (:caught? %) (not (pos? (:gain %))))
        left (remove cut? ways)]
    (cost/flight-caught-chance {:ways (count ways) :caught (count (filter cut? ways))
                                :unseen (count (filter :unseen left))
                                :underground? underground?})))

(defn named
  "The scored end of search ends planned to the named target at pos, nil when none."
  [ends pos]
  (first (filter #(= pos (:pos (:target %))) ends)))

(defn beaten-to?
  "Whether a chaser gets to pos before a body with food food at from could, walking straight there (cost/wins-race? lost
  at cost/body-speed): no route there can be safe."
  [chasers from food pos]
  (not (cost/wins-race? (/ (u/dist from pos) (cost/body-speed food)) (arrive-at chasers pos))))

(defn safe-why
  "Why the walk to a named target is no safe run, its scored end e: :no-way (no route there, or only part of one),
  :past-hostile (a chaser catches the route), :hostile-nearer (one gets there first: cost/wins-race? lost); nil when safe."
  [e]
  (cond
    (not (:found? e)) :no-way
    (:caught? e) :past-hostile
    (not (cost/wins-race? (:s e) (:arrive e))) :hostile-nearer))
