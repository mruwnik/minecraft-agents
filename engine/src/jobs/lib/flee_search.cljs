(ns jobs.lib.flee-search
  "The one priced flee search: where a body fleeing its chasers walks to.
  Candidates: ring ends (x-z discs at ::bearings headings, reach blocks off, where the chasers give the body up: the planner finds the height, so a slope, a
  stair or a cliff is its business) and the targets a caller names (a goal, a home). Each is planned by the go-to
  planner over what the body has seen, costed as go-to's walks are (known dangers, darkness, unseen cells), within
  ::max-nodes (a named target ::target-max-nodes) and the ring ends together within ::ring-nodes, ring headings away
  from the chasers first. A heading with no route gives the end of the planner's partial route, a pocket (:found? false) unless the
  chasers have given the body up there. Ring
  routes plan through unseen cells as through seen ones (no rock guess), a flee step walks only the seen part.
  Each end is priced (the planner's price plus the death the route risks past the known threats, a death about where
  the body stands: cost/route-death-cost, plus the chasers met on the way back from an unseen dead end: unseen-price) and scored against
  the chasers: :arrive, the seconds the first of them takes to reach it (cost/arrival-s over its way, its threat
  field's; the body timed at cost/flee-walk-speed, cost/flee-stop-s between steps); :caught-s, the seconds one is within its blow on the route
  (cost/reach-s), :caught? whether that is any; :score, arrive less price. pick chooses a flee step's end."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [engine.path.planner.base :as base]
            [engine.path.planner-tuned :as planner]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threat-field :as field]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.edges :as edges]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::bearings {:default 12 :doc "Headings round the body a flee search plans a ring end at." :spec (a/int-in 1 64)}
   ::max-nodes {:default 600 :doc "Planner nodes one ring end's search may expand: past it the end is its partial route's." :spec (a/int-in 50 nil)}
   ::target-max-nodes {:default 2400 :doc "Planner nodes the search of a named target (a goal, a home) may expand." :spec (a/int-in 50 nil)}
   ::step-s {:default 1 :doc "Seconds a flee step walks of the chosen route before the flight stops to choose again (the cell of the route that far in; a cover end is walked to whole)." :spec (a/num-in 0.1 nil)}
   ::ring-nodes {:default 7200 :doc "Planner nodes the ring ends of one flee search may expand together; the ring headings not planned by then are left out (the ones most away from the chasers are planned first)." :spec (a/int-in 50 nil)}
   ::hazard-clearance {:default 2.5 :doc "Blocks a flee route keeps from a remembered hazard: a route with a cell nearer is not taken." :spec (a/num-in 0 nil)}})

(defn give-up-at
  "Blocks from chaser {:name :mob} past which it gives the body up (threat/give-up-range of its kind)."
  [{:keys [name mob]}]
  (threat/give-up-range (or mob name)))

(defn given-up?
  "Whether at pos every one of chasers ({:pos}) not :calm? (a calm one does not follow) has given the body up (give-up-at)."
  [chasers pos]
  (every? #(>= (u/dist (:pos %) pos) (give-up-at %)) (remove :calm? chasers)))

(defn reach
  "Blocks to the ring of a flee search from `from`: where the chasers give the body up, the most over chasers not
  :calm? of give-up-at less its distance now; at least one flee step (::step-s at cost/flee-walk-speed of food)."
  [from chasers food]
  (reduce max (* (cost/flee-walk-speed food) (settings/get settings ::step-s))
          (map #(- (give-up-at %) (u/dist from (:pos %))) (remove :calm? chasers))))

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

(defn swum
  "Cells (routed's) each with :wet?, whether the body swims there as the body knows it (its feet and the cell under in
  breath/swimmable, as breath/swimming?): the race there is against a swimmer (cost/reach-s)."
  [p cells]
  (let [wet? #(contains? breath/swimmable (u/seen-name p %))]
    (mapv #(assoc % :wet? (and (wet? %) (wet? (update % :y dec)))) cells)))

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
  "Seconds the unseen part of r (seen-split) adds to its planner price (whose mask prices the unseen cells themselves,
  cost/unseen-share): cost/dead-end-cost of counts (wworld/way-counts) and trapped-at its last seen cell, what the
  chasers met there on the way back cost; 0 when it saw it all."
  [{:keys [cells seen-n unseen]} counts trapped-at]
  (if-not unseen
    0
    (cost/dead-end-cost counts (trapped-at (nth cells (dec seen-n))))))

(defn ^:async plan-to!
  "The planner's result for query over pw, its search within max-nodes nodes, in slices with yields between them; the
  landings it asked about are kept in landing-memo (wworld/landing-seen's)."
  [c pw policy query max-nodes landing-memo]
  (let [options (wplan/with-drops (wplan/plan-options pw (walk/default-weight) (executor/planner-limits policy (wworld/solid-fn pw))
                                                      (assoc (wplan/box-of policy) :maxNodes max-nodes))
                                  policy (wworld/landing-seen (:primitives c) landing-memo))
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
  guess; with priced? no rock guess: an unseen cell keeps the planner's unseen price and the search adds the chasers
  met on the way back from a dead end, unseen-price), costed as go-to's walks but for the threats keep? leaves out (the chasers: their danger is the
  race); nil when the body cannot sense the world."
  [c goal keep? priced?]
  (some-> (wworld/path-world (:primitives c) (wworld/with-seeds (cond-> (wworld/unknown-spec) priced? wworld/without-rock) goal nil))
          (#(wworld/costed-world c % {:dangers? (wworld/dangers?) :dark? (wworld/dark?) :keep? keep?}))))

(defn floor-cell [pos] (mapv #(js/Math.floor (% pos)) [:x :y :z]))

(defn cell-key-of
  "A threat as the key of a kept search: its mob, its cell and weight to 1/20 (the fields and tolls it prices are cell-wide)."
  [{:keys [mob pos weight]}]
  [mob (floor-cell pos) (/ (js/Math.round (* 20 (or weight 1))) 20)])

(defonce ^:private kept-prices (js/WeakMap.))

(defn gather
  "Every input of price-all!'s result, read once: {:dim :writes (the remembered world) :pos :kit :food :body :class :caught-by
  (the threats priced, with their reach fns) :policy (wworld/body-policy: the job's args, effects, oxygen and fire
  through it) :targets :y-margin :chasers (their cells) :dangers (the planner's: cells, fades and radii as they come to) :tolls
  (the steps the walker failed at) :near :sky (the sky's light darkening) :difficulty :values :scope}.
  What a price reads that is time-dependent enters as the value it computes to: the death fade through :dangers; the
  landings the planner asked about are kept with the result and read again (wworld/landing-reads-hold?)."
  [c st targets {:keys [y-margin chasers]} chased other?]
  (let [p (:primitives c)
        raw (look/raw-of p)]
    {:dim (some-> ^js st .-dim) :writes (some-> ^js st .-writes) :pos (u/self-pos c) :kit (wworld/kit-of c) :food (.-food (.self p))
     :body (threats/death-body c)
     :class {:underground? (sh-sense/buried? p) :night? (sh-sense/dark-outside? p) :threats (threats/threats-of c other?)}
     :caught-by (mapv #(assoc % :caught 1) (threats/threats-of c (complement other?)))
     :policy (cond-> (wworld/body-policy c) y-margin (assoc :y-margin y-margin))
     :targets targets :y-margin y-margin :chasers chasers :chased chased
     :dangers (some-> (threats/planner-dangers c) js->clj)
     :tolls (second (edges/tolls p)) :near (look/near-of p) :sky (some-> raw look/sky-subtract) :difficulty (threat/difficulty)
     :values (:values @settings/state) :scope (js->clj (.getStore settings/call-scope))}))

(defn priced-key
  "The inputs of a search (gather) as the key of its kept result: its threats by cell and weight (cell-key-of), its chasers
  by name, mob, calm and cell, its policy without the fns it carries (the air it counts is the remembered world's)."
  [in]
  (-> in
      (update-in [:class :threats] #(mapv cell-key-of %))
      (update :caught-by #(mapv cell-key-of %))
      (update :chasers #(mapv (juxt :name :mob :calm? :pos) %))
      (update :policy #(into {} (remove (comp fn? val)) %))))

(declare price-all!)

(defn ^:async price!
  "Each of targets (named ones {:pos :range :kind}, then ring ends {:x :z}) planned from where the body stands (see the ns
  doc): [{:target :found? :open? :end :s :priced :price :cells :seen-n :unseen}] (:open? found, or a ring end whose
  partial route ends where the chasers have given the body up: given-up?), the ones the planner found no way nearer to, and the ring ends
  left when ::ring-nodes ran out (named ones have their own node cap), left out; its meta {:nodes :planned :unrouted} the nodes
  expanded, the targets planned and why the ones with no route had none (the planner's reason: count). opts {:y-margin :chasers}: blocks past body and end the routes may plan, the chasers (their danger is the race, so death-chance weighs only the other known threats; met on the
  way back from an unseen dead end, they catch the body: unseen-price). Kept for the body while it asks the same of the
  same inputs (gather, priced-key)."
  [c targets {:keys [chasers] :as opts}]
  (let [chased (set (map #(floor-cell (:pos %)) chasers))
        other? #(not (chased (floor-cell (:pos %))))
        st (some-> (aget (:primitives c) "perception") :st)
        in (gather c st targets opts chased other?)
        k (when st (priced-key in))
        hit (when st (.get kept-prices st))]
    (if (and k (= k (:k hit)) (wworld/landing-reads-hold? (:primitives c) (:landings hit)))
      (:v hit)
      (let [landings (js/Map.)
            v (await (price-all! c in other? landings))]
        (when st (.set kept-prices st {:k k :v v :landings landings}))
        v))))

(defn ^:async price-all!
  "price! without the kept result, over the inputs gathered (gather); the landings its plans asked about are kept in landings."
  [c {:keys [targets chasers pos body class caught-by policy food]} other? landings]
  (let [p (:primitives c)
        ring-world (costed c nil other? true)
        trapped-at (fn [cell] (cost/route-death-cost body (assoc class :cells [cell] :threats caught-by) cell))
        counts (wworld/way-counts p)
        slow (pace food)]
    (if (nil? ring-world)
      []
      (loop [[t & more] targets out [] nodes 0 ring-left (settings/get settings ::ring-nodes) planned 0 unrouted {}]
        (if (or (nil? t) (and (nil? (:pos t)) (<= ring-left 0)))
          (with-meta out {:nodes nodes :planned planned :unrouted unrouted})
          (let [pw (if-let [target (:pos t)] (costed c (floor-cell target) other? false) ring-world)
                ^js res (await (plan-to! c pw policy (query c t)
                                         (if (:pos t) (settings/get settings ::target-max-nodes) (min ring-left (settings/get settings ::max-nodes)))
                                         landings))
                spent (or (.-expanded res) 0)
                r (some->> (routed res slow) (seen-split p) (#(update % :cells (partial swum p))))]
            (recur more (if r
                          (conj out (assoc r :target t
                                             :open? (or (:found? r) (and (nil? (:pos t)) (given-up? chasers (:end r))))
                                             :price (+ (:priced r)
                                                       (cost/route-death-cost body (assoc class :cells (dwelt (:cells r))) pos)
                                                       (unseen-price r counts trapped-at))))
                          out)
                   (+ nodes spent)
                   (cond-> ring-left (nil? (:pos t)) (- spent))
                   (inc planned)
                   (cond-> unrouted (nil? r) (update (.-reason res) (fnil inc 0))))))))))

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
  "chasers each with :way, the field/way-fn of its threat field over its give-up range (the ring's extent) to within cost/melee-reach, when it has a :mob name
  (a shot spot has none) other than a creeper or a shooter (a blast or an arrow passes where it cannot walk: the
  straight line stands)."
  [c chasers]
  (mapv (fn [{:keys [mob pos] :as ch}]
          (cond-> ch (and mob (not= "creeper" mob) (not= :projectile (threat/damage-type mob)))
            (assoc :way (field/way-fn (field/field-of c {:mob mob :pos pos :extent (give-up-at ch)}) (floor-cell pos) (cost/melee-reach)))))
        chasers))

(def unreached-s
  "The arrival a :score counts for an end no chaser can get to: past any real one, so such ends rank first and among
  themselves by price."
  1e6)

(defn scored
  "priced (price!) scored against chasers [{:name (mob-kind) :pos}] from where the body stands (from): :arrive (the
  seconds the first chaser takes to the end, ##Inf with none), :gain (arrive less the chasers' arrival at from: what
  the walk buys over standing still; 0 where no chaser can get to the body already), :caught-s (the most seconds a
  chaser not :calm? is within its blow on the route past the cells it reaches without moving (those the flight's start
  pays, cost.answer/flee-mob), cost/reach-s: no race margin, a margin is for a race to one place), :caught? (one lands a blow: caught-s over 0) and :score (see the ns doc)."
  [chasers from priced]
  (let [here (arrive-at chasers from)]
    (mapv (fn [{:keys [end cells price] :as e}]
          (let [arrive (arrive-at chasers end)
                caught-s (reduce max 0 (map (fn [ch]
                                              (let [way #(way-at ch %)
                                                    past (drop-while #(zero? (cost/arrival-s (:name ch) (:pos ch) % (way %))) cells)]
                                                (cost/reach-s {:chaser (:name ch) :pos (:pos ch) :route (vec past) :way way})))
                                            (remove :calm? chasers)))]
            (assoc e :arrive arrive :gain (if (js/isFinite here) (- arrive here) 0)
                     :caught-s caught-s :caught? (pos? caught-s)
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

(defn gets-away?
  "Whether a flight on scored end e outruns its chasers by the end: an open ring way that gains on them and whose end
  no chaser gets to before the body does."
  [e]
  (and (:open? e) (not (:cover? e)) (nil? (:pos (:target e))) (pos? (:gain e)) (> (:arrive e) (:s e))))

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
  (default: reach, where the chasers give the body up) priced (price!) and scored against chasers ([{:name :pos}]), with covers on
  their routes when covered? is given. opts {:reach :y-margin :covered?}."
  [c chasers targets {:keys [covered?] :as opts}]
  (let [t0 (js/performance.now)
        self (.self (:primitives c))
        from (u/self-pos c)
        chasers (with-ways c chasers)
        ways-ms (js/Math.round (- (js/performance.now) t0))
        r (or (:reach opts) (reach from chasers (.-food self)))
        priced (await (price! c (into (vec targets) (ring from chasers r (settings/get settings ::bearings))) (assoc opts :reach r :chasers chasers)))
        ends (scored chasers from priced)]
    (ctx/emit! c :retreat.search-work :debug (assoc (meta priced) :ms (js/Math.round (- (js/performance.now) t0)) :ways-ms ways-ms :targets (count targets)))
    (with-meta (cond-> ends covered? (into (covers chasers from covered? ends))) (meta priced))))

(defn unseen-ahead
  "The first unseen cells ([x y z], nearest first) of ends' routes but those in glanced (looked at this flight already:
  each once a flight): where the flight looks before it counts itself cornered; nil when none is left."
  [ends glanced]
  (->> ends (sort-by :s) (keep :unseen) distinct (remove (set glanced)) seq))

(defn caught-chance
  "{:chance :seconds} of a flight from where search ends (search!) ran: the chance it is caught
  (cost/flight-caught-chance) and the seconds its chasers are within their blow when it is (:caught-s; nil: the whole
  flight). A way is a ring route that is open or runs on into cells not seen (a pocket is none: the chaser follows it in)
  and gains on the chasers, none of which gets to its end before the body. The flight takes the best-scored way not
  caught: the chance is that of a way the race calls free, the prior (plus its dead end at counts, wworld/way-counts,
  when it runs unseen); a catch lasts the least seconds the race keeps a chaser in reach over the caught headings (nil
  with none caught). The other headings only choose the way. With every way caught: chance 1, over the seconds of the way
  it gets away on least so (else of a ring route into the unseen; a closed pocket's: the whole flight), :cornered? when
  choose (with opts, the flight's own) takes none of the ends and no unseen cell is left to look at (unseen-ahead, glanced
  the cells the flight looked at: the flight would be the cornered step, no walk away); with one, the flight looks first,
  and a way may be there at way-on-chance: the chance is that of an unseen way (flight-caught-chance). Nothing routed:
  the prior with the unseen worse case."
  [ends counts opts glanced]
  (let [ring (filter #(and (nil? (:pos (:target %))) (not (:cover? %))) ends)
        left (filter #(and (or (:open? %) (:unseen %)) (not (:caught? %)) (pos? (:gain %)) (> (:arrive %) (:s %))) ring)
        ways (filter gets-away? ends)
        best (when (seq left) (apply max-key :score left))
        caught (filter :caught? ring)]
    (cond
      best (cond-> {:chance (cost/flight-caught-chance {:way? true :unseen? (some? (:unseen best)) :counts counts})}
             (seq caught) (assoc :seconds (reduce min (map :caught-s caught))))
      (empty? ring) {:chance (cost/flight-caught-chance {:way? true :unseen? true :counts counts})}
      :else (let [timed (or (seq ways) (seq (filter :unseen ring)))
                  free? (some? (choose ends opts))
                  look? (and (not free?) (unseen-ahead ends glanced))]
              (cond-> {:chance (if look? (cost/flight-caught-chance {:way? true :unseen? true :counts counts}) 1)
                       :cornered? (not (or free? look?))}
                timed (assoc :seconds (reduce min (map :caught-s timed))))))))

(defn way-up-chance
  "{:chance} of a flight that takes a stair up it has seen (respond.cover/way-up): a seen way, at wworld/way-counts counts."
  [counts]
  {:chance (cost/flight-caught-chance {:way? true :unseen? false :counts counts})})

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
