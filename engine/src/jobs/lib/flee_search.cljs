(ns jobs.lib.flee-search
  "The one priced flee search: where a body fleeing its chasers walks to.
  Candidates: ring ends (x-z discs at ::bearings headings, reach blocks off, where the chasers give the body up: the planner finds the height, so a slope, a
  stair or a cliff is its business) and the targets a caller names (a goal, a home). Each is planned by the go-to
  planner over what the body has seen, costed as go-to's walks are (known dangers, darkness, unseen cells), within
  ::max-nodes (a named target ::target-max-nodes) and the ring ends together within ::ring-nodes, ring headings away
  from the chasers first. A heading with no route gives the end of the planner's partial route, a pocket (:found? false) unless the
  chasers have given the body up there. Ring
  routes plan through unseen cells (no rock guess), each priced by the chance it is rock (wworld/shut-cell-fn), a flee step walks only the seen part.
  Each end is priced (the planner's price plus the death the route risks from the known threats not chasing and the mobs
  met spawning on it (spawn-light/spawns-over), a death about where the body stands: cost/route-death-cost, plus, scored, a dead end where it runs unseen: dead-end) and scored against
  the chasers: :arrive, the seconds the first of them takes to reach it (cost/arrival-s over its way, its threat
  field's; the body timed at cost/flee-walk-speed, cost/flee-stop-s between steps); :caught-s, the seconds one is within its blow on the route
  (cost/reach-s), :caught? whether that is any; :score, arrive less price. pick chooses a flee step's end."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [jobs.lib.clock :as clock]
            [jobs.lib.memory-age :as memory-age]
            [engine.path.planner.base :as base]
            [engine.path.planner-tuned :as planner]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.threat-field :as field]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as lreach]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.edges :as edges]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
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
  "The cells {:x :y :z :t :g} of planner steps, :t the seconds into the route: its seconds spread over its length, plus
  cost/flee-stop-s for each ::step-s walked before (a flight stops to choose its next step), :stops how many; :g the planner's price to the cell."
  [steps seconds]
  (let [cells (mapv (fn [^js s] {:x (.-x s) :y (.-y s) :z (.-z s) :g (.-g s)}) steps)
        legs (map u/dist cells (rest cells))
        total (reduce + 0 legs)
        at (reductions + 0 legs)
        step (settings/get settings ::step-s)
        stop (cost/flee-stop-s)]
    (mapv (fn [cell d] (let [walk (if (pos? total) (* seconds (/ d total)) 0)
                             stops (js/Math.floor (/ walk step))]
                         (assoc cell :t (+ walk (* stop stops)) :stops stops)))
          cells at)))

(defn unstopped
  "Timed cells (timed-cells) re-timed as one walk with no stops between steps (a cover end is walked to whole)."
  [cells]
  (let [stop (cost/flee-stop-s)]
    (mapv #(-> % (update :t - (* stop (:stops % 0))) (dissoc :stops)) cells)))

(defn pace
  "How much longer than the planner's seconds a flight's steps take on a route: the planner times the gait the body's food
  allows (a walk when it cannot sprint, wworld/body-policy), a flight's steps go at cost/flee-walk-speed of that gait."
  []
  (/ (cost/body-speed 20) (cost/flee-walk-speed 20)))

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
  (step-of), the rest is priced as unseen (dead-end)."
  [p {:keys [cells] :as r}]
  (let [unseen (fn [{:keys [x y z]}] (some #(when-not (u/seen-block p {:x x :y % :z z}) [x % z]) [y (inc y)]))
        i (or (first (keep-indexed #(when (and (pos? %1) (unseen %2)) %1) cells)) (count cells))]
    (cond-> (assoc r :seen-n i)
      (< i (count cells)) (assoc :unseen (unseen (nth cells i))))))

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
  guess; with priced? no rock guess: an unseen cell keeps the planner's unseen price and the search adds a dead end
  there, dead-end), costed as go-to's walks but for the threats keep? leaves out (the chasers: their danger is the
  race); nil when the body cannot sense the world."
  [c goal keep? priced?]
  (some-> (wworld/path-world (:primitives c) (wworld/with-seeds (cond-> (wworld/unknown-spec c) priced? wworld/without-rock) goal nil))
          (#(wworld/costed-world c % {:dangers? (wworld/dangers?) :dark? (wworld/dark?) :keep? keep?}))))

(defn floor-cell [pos] (mapv #(js/Math.floor (% pos)) [:x :y :z]))

(defn cell-key-of
  "A threat as the key of a kept search: its mob, its cell and weight to 1/20 (the fields and tolls it prices are cell-wide)."
  [{:keys [mob pos weight]}]
  [mob (floor-cell pos) (/ (js/Math.round (* 20 (or weight 1))) 20)])

(defonce ^:private kept-prices (js/WeakMap.))

(defn gather
  "Every input of price-all!'s result, read once: {:dim :memory (the remembered world) :pos :kit :food :body :covers :others (the
  known threats not chasing, with their reach fns) :caught-by
  (the threats priced, with their reach fns) :policy (wworld/body-policy: the job's args, effects, oxygen and fire
  through it) :targets :y-margin :chasers (their cells) :dangers (the planner's: cells, fades and radii as they come to) :tolls
  (the steps the walker failed at) :near :sky (the sky's light darkening) :difficulty :values :scope}.
  What a price reads that is time-dependent enters as the value it computes to: the death fade through :dangers; the
  landings the planner asked about are kept with the result and read again (wworld/landing-reads-hold?)."
  [c st targets {:keys [y-margin chasers covers covers-key]} chased other?]
  (let [p (:primitives c)
        raw (look/raw-of p)]
    {:dim (some-> ^js st .-dim) :memory (some-> ^js st memory-age/epoch) :pos (u/self-pos c) :kit (wworld/kit-of c) :food (.-food (.self p))
     :body (threats/death-body c) :covers covers :covers-key covers-key
     :others (threats/threats-of c other?)
     :caught-by (mapv #(assoc % :caught 1) (threats/threats-of c (complement other?)))
     :policy (cond-> (wworld/body-policy c) y-margin (assoc :y-margin y-margin))
     :targets targets :y-margin y-margin :chasers chasers :chased chased
     :dangers (some-> (threats/planner-dangers c) js->clj)
     :tolls (second (edges/tolls p)) :near (look/near-of p) :sky (some-> raw look/sky-subtract) :difficulty (threat/difficulty)
     :values (:values @settings/state) :scope (js->clj (.getStore settings/call-scope))}))

(defn priced-key
  "The inputs of a search (gather) as the key of its kept result: its threats by cell and weight (cell-key-of), its chasers
  by name, mob, held item, calm, shooter and cell, its policy without the fns it carries (the air it counts is the remembered world's), its
  covers by their :covers-key (the covers are only built at a dead end)."
  [in]
  (-> in
      (update :others #(mapv cell-key-of %))
      (update :caught-by #(mapv cell-key-of %))
      (update :chasers #(mapv (juxt :name :mob :held :calm? :shooter? :pos :burns :speed) %))
      (update :policy #(into {} (remove (comp fn? val)) %))
      (dissoc :covers)))

(declare price-all! arrive-at shooter?)

(defn ^:async price!
  "Each of targets (named ones {:pos :range :kind}, then ring ends {:x :z}) planned from where the body stands (see the ns
  doc): [{:target :found? :open? :end :s :priced :price :cells :seen-n :unseen :counts :trapped}] (:open? found, or a ring end whose
  partial route ends where the chasers have given the body up: given-up?), the ones the planner found no way nearer to, and the ring ends
  left when ::ring-nodes ran out (named ones have their own node cap), left out; its meta {:nodes :planned :unrouted :plan-ms :cut?} the nodes
  expanded, the targets planned and why the ones with no route had none (the planner's reason: count), plan-ms the
  milliseconds in its plans, :cut? when the search's meter (opts :meter, a jobs.lib.reach/meter: the flight step's one
  search, Q141) ran out before every target was planned. Each plan's nodes stop at what the meter has left
  (cost/search-cell-s a node) and take off it their seconds, the clock's or the nodes' price when more; the search ends
  with the best so far once those seconds reach the first chaser's time to contact (a decision made after contact is no
  escape). opts {:y-margin :chasers :covers :covers-key :meter}: blocks past body and end the routes may plan, the chasers (their danger is the race, so death-chance weighs only the other known threats; met at an
  unseen dead end, they corner the body: :trapped, the cheapest cornered answer to them (cost/cornered-answers: the fight,
  or a cover of (:covers) {way {:open-s :seconds :fails}}, a fn called at the first dead end only, those the cornered step
  would take where the body stands; :covers-key what they depend on past the body and its kit), with no mob known the death of
  being caught at its last seen cell, and :counts, wworld/way-counts, for dead-end). Kept for the body while it asks the same of the
  same inputs (gather, priced-key), but for a cut one."
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
            v (await (price-all! c in other? landings (:meter opts)))]
        (when (and st (not (:cut? (meta v)))) (.set kept-prices st {:k k :v v :landings landings}))
        v))))

(defn ^:async price-all!
  "price! without the kept result, over the inputs gathered (gather); the landings its plans asked about are kept in landings."
  [c {:keys [targets chasers pos body covers others caught-by policy]} other? landings meter]
  (let [p (:primitives c)
        ring-world (costed c nil other? true)
        cornered (delay (when-let [mobs (seq (keep #(when (:mob %) {:distance 0 :name (:mob %) :burns (:burns %)}) chasers))]
                          (let [fight (cost/fight-answer {:weapon (:weapon body) :equipment (:equipment body) :health (:health body) :mobs mobs})]
                            (apply min (vals (:prices (cost/decide body (cost/cornered-answers {:equipment (:equipment body) :mobs mobs :fight fight :covers (when covers (covers))}) pos)))))))
        trapped-at (fn [cell] (or @cornered (cost/route-death-cost body {:cells [cell] :threats caught-by} cell)))
        counts (wworld/way-counts p)
        spawn-kill (delay (cost/spawn-kill body pos))
        share (spawn-light/share-of p false nil)
        slow (pace)
        contact-s (arrive-at (remove (some-fn :calm? shooter?) chasers) pos)
        named-n (count (filter :pos targets))]
    (if (nil? ring-world)
      []
      (loop [[t & more] targets out [] nodes 0 ring-left (settings/get settings ::ring-nodes) planned 0 unrouted {} plan-ms 0 own-s 0]
        (let [most (when t (if (:pos t) (settings/get settings ::target-max-nodes) (min ring-left (settings/get settings ::max-nodes))))
              cap (when most (binding [lreach/*search* meter] (lreach/metered-cap most :node-s)))
              cut? (and t (or (:pos t) (pos? ring-left)) (zero? cap))]
          (if (or (nil? t) (and (nil? (:pos t)) (<= ring-left 0)) cut?
                  (and (> planned named-n) (<= contact-s own-s)))
            (do (when cut? (lreach/spend! meter 0 true))
                (with-meta out (cond-> {:nodes nodes :planned planned :unrouted unrouted :plan-ms (js/Math.round plan-ms)} cut? (assoc :cut? true))))
            (let [pw (if-let [target (:pos t)] (costed c (floor-cell target) other? false) ring-world)
                  t1 (clock/now)
                  ^js res (await (plan-to! c pw policy (query c t) cap landings))
                  ms (- (clock/now) t1)
                  spent (or (.-expanded res) 0)
                  ;; the clock's, or the nodes' price when more: a clock that does not move with the CPU (a fake's) still ends the search
                  s (max (/ ms 1000) (* spent (cost/search-cell-s)))
                  _ (lreach/spend! meter s false)
                  r (some->> (routed res slow) (seen-split p))]
            (recur more (if r
                          (conj out (assoc r :target t
                                             :open? (or (:found? r) (and (nil? (:pos t)) (given-up? chasers (:end r))))
                                             :price (+ (:priced r)
                                                       (let [spawns (spawn-light/spawns-over p (:cells r) share)]
                                                         (cost/route-death-cost body {:cells (dwelt (:cells r)) :threats others :spawns spawns :spawn-kill (when (pos? spawns) @spawn-kill)} pos)))
                                             :counts counts
                                             :trapped (when (:unseen r) (trapped-at (nth (:cells r) (dec (:seen-n r)))))))
                          out)
                   (+ nodes spent)
                   (cond-> ring-left (nil? (:pos t)) (- spent))
                   (inc planned)
                   (cond-> unrouted (nil? r) (update (.-reason res) (fnil inc 0)))
                   (+ plan-ms ms)
                   (+ own-s s)))))))))

(defn shooter?
  "Whether chaser ch only shoots: a shot spot (:shooter?, where an unseen shot came from) or a mob {:mob :held} that
  lands no blow (threat/strikes?). It runs no race (caught-s, the search's contact deadline, trap-s): its danger is its
  line of fire, the flight's mark (respond.goal/choose-opts)."
  [{:keys [mob held] :as ch}]
  (boolean (or (:shooter? ch) (and mob (not (threat/strikes? {:name mob :held held}))))))

(defn way-at
  "The blocks of chaser's way ({:way}: field/way-fn) to pos, nil when not known."
  [{:keys [way]} {:keys [x y z]}]
  (when way (way (js/Math.floor x) (js/Math.floor y) (js/Math.floor z))))

(defn swim-at
  "The blocks of chaser's straight line to pos ({:swim} from with-ways: a fn of a point) the body knows to be water, 0 with none."
  [{:keys [swim]} pos]
  (if swim (swim pos) 0))

(defn arrive-at
  "Seconds the first of chasers ({:name :pos :way}) takes to reach pos (cost/arrival-s over its way, way-at), ##Inf
  with none."
  [chasers pos]
  (reduce min ##Inf (map #(cost/arrival-s (:name %) (:pos %) pos (way-at % pos) (swim-at % pos) (:speed %)) chasers)))

(defn swim-fn
  "A fn of a point to the blocks of the straight line from `from` to it (sampled each half block) that the body knows to be
  water (feet and the cell under in breath/swimmable, as breath/swimming?): a chaser is timed at its swim speed only
  over water that is shown, else at its land speed. Memoised per point: a search scores the same cells many times; wet
  (a Map of cell to whether it is water) is shared by the chasers of a search, whose lines to one end overlap."
  [c from wet]
  (let [p (:primitives c)
        wet-cell? (fn [x y z]
                    (let [k (base/cell-key x y z)]
                      (if (.has wet k)
                        (.get wet k)
                        (let [at (fn [dy] (contains? breath/swimmable (u/seen-name p {:x x :y (+ dy y) :z z})))
                              w (boolean (and (at 0) (at -1)))]
                          (.set wet k w)
                          w))))
        {fx :x fy :y fz :z} from]
    (memoize
     (fn [to]
       (let [n (max 1 (js/Math.ceil (* 2 (u/dist from to))))
             {tx :x ty :y tz :z} to
             wet-at? (fn [i] (let [f (/ i n)]
                               (wet-cell? (js/Math.floor (+ fx (* f (- tx fx)))) (js/Math.floor (+ fy (* f (- ty fy)))) (js/Math.floor (+ fz (* f (- tz fz)))))))]
         (* 0.5 (count (filter wet-at? (range (inc n))))))))))

(defn with-ways
  "chasers each with :way, the field/way-fn of its threat field over its give-up range (the ring's extent) to within cost/melee-reach, when it has a :mob name
  (a shot spot has none) that walks to strike (threat/strikes?, a bow mob seen holding a sword included) other than a
  creeper or a blaze: a blast, an arrow or a flyer passes where it cannot walk, the straight line stands."
  [c chasers]
  (let [wet (js/Map.)]
    (mapv (fn [{:keys [mob pos] :as ch}]
            (cond-> (assoc ch :swim (swim-fn c pos wet))
              (and mob (not (#{"creeper" "blaze"} mob)) (threat/strikes? {:name mob :held (:held ch)}))
              (assoc :way (field/way-fn (field/field-of c {:mob mob :held (:held ch) :pos pos :extent (give-up-at ch)}) (floor-cell pos) (cost/melee-reach)))))
          chasers)))

(def unreached-s
  "The arrival a :score counts for an end no chaser can get to: past any real one, so such ends rank first and among
  themselves by price."
  1e6)

(defn at-time
  "The point of timed route ([{:x :y :z :t}]) at second t: linear between its points, its last one past the end."
  [route t]
  (let [[before after] (split-with #(< (:t %) t) route)
        p (last before) q (first after)]
    (cond (nil? q) (last route)
          (nil? p) q
          :else (let [k (/ (- t (:t p)) (- (:t q) (:t p)))]
                  (merge-with (fn [a b] (+ a (* k (- b a)))) (select-keys p [:x :y :z]) (select-keys q [:x :y :z]))))))

(defn fuse-blast
  "The distance a creeper's blast reaches a body walking timed route (centred cells the creeper does not already touch) at, nil when it does not hurt:
  the creeper lights at the first point it reaches within cost/swell-start of the body, stands still where it lit (vanilla's
  SwellGoal stops its walk), the fuse burns while the body stays within cost/fuse-cancel-range of that spot, and the blast hurts
  where the body is then (cost/blast-damage); a body whose route ends first stands at its end."
  [ch route]
  (let [starts (cost/swim-arrivals (:name ch) (:pos ch) route #(way-at ch %) #(swim-at ch %) (:speed ch) cost/swell-start)
        lit (map first (drop-while (fn [[p a]] (< (:t p) a)) (map vector route starts)))
        p0 (first lit)]
    (when p0
      (let [gap (u/dist (:pos ch) p0)
            k (/ cost/swell-start gap)
            spot (if (<= gap cost/swell-start)
                   (:pos ch)
                   (merge-with (fn [at from] (+ at (* k (- from at)))) (select-keys p0 [:x :y :z]) (select-keys (:pos ch) [:x :y :z])))
            end (+ (:t p0) cost/fuse-s)
            burning (take-while #(<= (:t %) end) lit)]
        (let [d (u/dist (at-time lit end) spot)]
          (when (and (every? #(<= (u/dist % spot) cost/fuse-cancel-range) burning) (pos? (cost/blast-damage d)))
            d))))))

(defn route-past
  "The centred cells of timed cells past those chaser ch reaches without moving."
  [ch cells]
  (let [way #(way-at ch %)]
    (mapv cost/centred (drop-while #(zero? (cost/arrival-s (:name ch) (:pos ch) (cost/centred %) (way %) (swim-at ch %) (:speed ch))) cells))))

(defn caught-blast
  "{:caught-s :blast-d} of timed cells against chasers: caught-s is the most seconds a chaser not :calm? is within its blow,
  past the cells it reaches without moving (those the flight's start pays, cost.answer/flee-mob): cost/reach-s, no race margin
  (a margin is for a race to one place). A shooter? lands no blow. A creeper blows only when its fuse runs its length
  (fuse-blast, walked once: cost/fuse-s over it, its distance to :blast-d, the least over the creepers; nil none)."
  [chasers cells]
  (let [per (map (fn [ch]
                   (let [way #(way-at ch %)
                         route (route-past ch cells)]
                     (if (= "creeper" (:name ch))
                       (let [d (fuse-blast ch route)] {:s (if d cost/fuse-s 0) :d d})
                       {:s (cost/reach-s {:chaser (:name ch) :pos (:pos ch) :route route :way way :swim #(swim-at ch %) :speed (:speed ch)})})))
                 (remove (some-fn :calm? shooter?) chasers))
        ds (keep :d per)]
    (cond-> {:caught-s (reduce max 0 (map :s per))}
      (seq ds) (assoc :blast-d (reduce min ds)))))

(defn blast-d
  "The least distance a creeper of chasers blasts a body walking timed cells at (fuse-blast), nil when none hurts."
  [chasers cells]
  (:blast-d (caught-blast chasers cells)))

(defn caught-s
  "The most seconds a chaser not :calm? is within its blow on timed cells (caught-blast)."
  [chasers cells]
  (:caught-s (caught-blast chasers cells)))

(defn trap-s
  "What a dead end at the first unseen cell of scored end e costs it when it traps the body: its :trapped, and the
  arrival of the chasers that walk (not shooter?) at its end past theirs at its last seen cell (they get to the dead end then)."
  [chasers {:keys [cells seen-n end trapped]}]
  (let [walkers (remove shooter? chasers)
        at #(min (arrive-at walkers (cost/centred %)) unreached-s)]
    (+ trapped (max 0 (- (at end) (at (nth cells (dec seen-n))))))))

(defn back-via
  "The walk back of a flight on scored end r that finds its way shut at its first unseen cell, along other way e
  (cost/dead-end-cost's backs): from its last seen cell back to where e's route leaves r's, then on along e. :s its
  seconds back to the fork (the way in is r's price, the unseen cells' walk back their unseen-share), e's price past
  the fork and the arrival r's end scores past e's, plus fire (a fn of a timed route {:cells :end}: the seconds of
  death the shooters' fire on it risks, respond.goal/fire-price; nil none) of the walk back, timed from the turn; :caught-s
  that of the whole flight (caught-s); :unseen? e's, :trapped its trap-s."
  ([chasers r e] (back-via chasers r e nil))
  ([chasers r e fire]
  (let [seen (subvec (:cells r) 0 (:seen-n r))
        f (dec (count (take-while true? (map #(= (cell-of %1) (cell-of %2)) seen (:cells e)))))
        last-t (:t (peek seen))
        at-fork (- (* 2 last-t) (:t (nth seen f)))
        e-fork (:t (nth (:cells e) f))
        on (- (:price e) (:dead-end e 0))
        back (-> []
                 (into (map #(assoc % :t (- (* 2 last-t) (:t %)))) (rseq (subvec seen f (dec (count seen)))))
                 (into (map #(update % :t + (- at-fork e-fork))) (subvec (:cells e) (inc f))))]
    {:s (+ (- at-fork last-t) (if (pos? (:s e)) (* on (- 1 (/ e-fork (:s e)))) on)
           (- (min (:arrive r) unreached-s) (min (:arrive e) unreached-s))
           (if fire (fire {:cells (mapv #(update % :t - last-t) back) :end (:end e)}) 0))
     :caught-s (caught-s chasers (into seen back))
     :unseen? (some? (:unseen e)) :trapped (when (:unseen e) (trap-s chasers e))})))

(defn dead-end-race
  "cost/dead-end-race {:cost :chance} of a dead end at the first unseen cell of scored end r; nil when r does not reach
  a cell unseen. Its :counts and trap-s, backs along the ways out of pool (open or running on unseen, not caught, not a cover end, not through that
  cell; back-via, with fire)."
  ([chasers r pool] (dead-end-race chasers r pool nil))
  ([chasers r pool fire]
  (when (and (:trapped r) (< (:seen-n r) (count (:cells r))))
    (let [shut (cell-of (nth (:cells r) (:seen-n r)))
          out? #(and (or (:open? %) (:unseen %)) (not (:caught? %)) (not (:cover? %)) (not-any? (fn [c] (= shut (cell-of c))) (:cells %)))]
      (cost/dead-end-race (:counts r) (trap-s chasers r) (map #(back-via chasers r % fire) (filter out? pool)))))))

(defn dead-end
  "Seconds a dead end at the first unseen cell of scored end r adds to its price (dead-end-race); 0 when r does not reach
  a cell unseen."
  [chasers r pool]
  (:cost (dead-end-race chasers r pool) 0))

(defn scored
  "priced (price!) scored against chasers [{:name (mob-kind) :pos}] from where the body stands (from): :arrive (the
  seconds the first chaser takes to the end, ##Inf with none), :gain (arrive less the chasers' arrival at from: what
  the walk buys over standing still; 0 where no chaser can get to the body already), :caught-s (caught-s of its route),
  :caught? (one lands a blow: caught-s over 0), :caught-seen? (caught on its seen cells alone: no look past them frees it), :dead-end (dead-end, the ways out those of pool, default the ends
  scored; the walks back priced with fire, back-via), on :price, and :score (see the ns doc)."
  ([chasers from priced] (scored chasers from priced nil nil))
  ([chasers from priced pool] (scored chasers from priced pool nil))
  ([chasers from priced pool fire]
   (let [here (arrive-at chasers from)
         raced (mapv (fn [{:keys [end cells seen-n] :as e}]
                       (let [arrive (arrive-at chasers (cost/centred end))
                             {c-s :caught-s d :blast-d} (caught-blast chasers cells)]
                         (assoc e :arrive arrive :gain (if (js/isFinite here) (- arrive here) 0) :caught-s c-s :blast-d d :caught? (pos? c-s)
                                :caught-seen? (and (pos? c-s) (pos? (caught-s chasers (subvec cells 0 (min (count cells) (or seen-n (count cells))))))))))
                     priced)]
     (mapv (fn [e]
             (let [race (dead-end-race chasers e (or pool raced) fire)
                   d (:cost race 0)
                   price (+ (:price e) d)]
               (assoc e :dead-end d :dead-chance (:chance race) :price price :score (- (min (:arrive e) unreached-s) price))))
           raced))))

(defn covers
  "Ends on the routes of scored where cover first holds (covered?: a cell -> whether it is out of every ranged mob's line
  of fire): each route cut there, timed with no stops (unstopped: walked whole) and scored again against chasers, its dead end against the ways of ends (fire as scored's), :cover? true."
  ([chasers from covered? ends] (covers chasers from covered? ends nil))
  ([chasers from covered? ends fire]
  (scored chasers from
          (keep (fn [{:keys [cells s price priced dead-end] :as e}]
                  (when-let [i (first (keep-indexed #(when (and (pos? %1) (covered? (select-keys %2 [:x :y :z]))) %1) cells))]
                    (let [part (unstopped (subvec cells 0 (inc i)))
                          share (if (pos? s) (/ (:t (peek part)) s) 1)]
                      (assoc e :end (select-keys (peek part) [:x :y :z]) :cells part :cover? true :found? true :open? true
                               :s (:t (peek part)) :price (* share (- price dead-end)) :priced (* share priced)))))
                ends)
          ends fire)))

(defn hazard-near?
  "Whether a cell of route cells lies within ::hazard-clearance of one of hazards (positions)."
  [hazards cells]
  (let [r (settings/get settings ::hazard-clearance)]
    (boolean (some (fn [cell] (some #(< (u/dist % cell) r) hazards)) cells))))

(defn with-route
  "Step cell step with its route's cells from the start to it, [{:x :y :z :g}], as its :route meta (go-to's :route; none
  when cells do not reach it)."
  [step cells]
  (let [[before [at]] (split-with #(not= (cell-of step) (cell-of %)) cells)]
    (cond-> step at (with-meta {:route (mapv #(select-keys % [:x :y :z :g]) (concat before [at]))}))))

(defn go-to-route
  "Route (a step's :route, [{:x :y :z :g}]) from the body's cell on re-priced as go-to's walk prices it: an only-search
  (walk.search/only-route) of its cells over the world go-to plans in, every known threat tolled (the flee search left the
  chasers out), to within range of its last cell. The route with these :g, nil when the body is off it or that search finds
  no way (go-to then plans as usual). Once per chosen step."
  [c route range]
  (let [goal (mapv (last route) [:x :y :z])
        ^js from (.-from (wplan/plan-query c goal range))
        cells (wsearch/seed-from route [(.-x from) (.-y from) (.-z from)])
        pw (when cells (costed c nil nil false))
        ^js r (when pw (wsearch/only-route c pw goal range (walk/default-weight) (wworld/body-policy c) cells))]
    (when (and r (= "found" (.-status r)))
      (mapv (fn [^js s] {:x (.-x s) :y (.-y s) :z (.-z s) :g (.-g s)}) (array-seq (.-steps (.-path r)))))))

(defn step-of
  "The cell a flee step walks to of a picked end entry: of its route's seen part (:seen-n cells, at least the first past
  the start: a step into the dark is one cell), the first cell ::step-s or more in that is not in refused ([x y z]
  cells go-to refused) nor where the route starts, else the part's last; a cover end whole; nil when they are all
  refused. Its route to it rides as meta (with-route)."
  ([e] (step-of e #{}))
  ([{:keys [end cells cover? seen-n]} refused]
   (let [s (settings/get settings ::step-s)
         start (some-> (first cells) cell-of)
         walk (subvec cells 0 (min (count cells) (max 2 (or seen-n (count cells)))))
         ok #(when-not (or (contains? refused (cell-of %)) (= start (cell-of %))) (select-keys % [:x :y :z]))]
     (some-> (if cover?
               (ok end)
               (or (some #(when (>= (:t %) s) (ok %)) walk) (ok (peek walk))))
             (with-route cells)))))

(defn gets-away?
  "Whether a flight on scored end e outruns its chasers by the end: an open ring way that gains on them and whose end
  no chaser gets to before the body does."
  [e]
  (and (:open? e) (not (:cover? e)) (nil? (:pos (:target e))) (pos? (:gain e)) (> (:arrive e) (:s e))))

(defn at-step-or-end
  "A fn of end entry e (choose): whether f (cell -> bool) holds of e's step cell or its end."
  [f]
  (fn [e] (boolean (or (f (:step e)) (f (:end e))))))

(defn choose
  "The end entry a flee step takes among scored ends (scored, covers), its :step the cell the step walks to (step-of,
  past the cells go-to refused: refused [x y z]); nil when cornered. Left out: caught ones, those (but cover) that gain
  nothing on the chasers over standing still, pockets that gain less than their walk takes, those with every step
  cell refused, those never? holds of (at the step cell or the end) and those passing near hazards. Priced, not left
  out: each of marks ({:holds? (end entry -> bool) :s seconds}, e.g. at-step-or-end of a cell test) that holds of an
  end costs it its :s (a number, or a fn of the end entry), the death the mark risks (a negative :s credits the death a place removes: a closed room), off
  its :score. The end with the highest score so priced wins,
  covers, homes and open ways alike. A pocket only when no open way is left uncut: open ways that every chaser cuts leave the pockets out, they are traps."
  [ends {:keys [refused never? hazards marks]}]
  (let [ok (->> ends
                (remove :caught?)
                (filter #(or (:cover? %) (pos? (:gain %))))
                (filter #(or (:open? %) (> (:gain %) (:s %))))
                (keep #(when-let [step (step-of % (or refused #{}))] (assoc % :step step)))
                (remove #(and never? ((at-step-or-end never?) %)))
                (remove #(hazard-near? hazards (:cells %)))
                (map (fn [e] (assoc e :net (- (:score e) (transduce (comp (filter (fn [m] ((:holds? m) e))) (map (fn [m] (let [s (:s m)] (if (fn? s) (s e) s))))) + 0 marks))))))
        ways (filter #(and (:open? %) (nil? (:pos (:target %)))) ends)
        traps? (and (seq ways) (every? :caught? ways))
        open (filter :open? ok)
        pool (cond (seq open) open traps? nil :else ok)]
    (when (seq pool) (apply max-key :net (reverse pool)))))

(defn into-unseen?
  "Whether the step of end entry e (choose) walks into a cell the body has not seen: its route's seen part ends less than
  ::step-s in, or, for a cover end (walked to whole), before the cover."
  [{:keys [unseen cover? cells seen-n]}]
  (boolean (and unseen (if cover?
                         (< seen-n (count cells))
                         (< (:t (nth cells (dec seen-n))) (settings/get settings ::step-s))))))

(defn pick
  "The cell the flee step choose takes walks to, nil when cornered."
  [ends opts]
  (:step (choose ends opts)))

(defn ^:async search!
  "The flee search from where the body stands: the targets (named, {:pos :range :kind}) and the ring ends at reach
  (default: reach, where the chasers give the body up) priced (price!) and scored against chasers ([{:name :pos}]), with covers on
  their routes when covered? is given, the walks back from dead ends priced with :fire (scored). opts {:reach :y-margin
  :covered? :covers :covers-key :fire :meter} (covers, meter: price!'s)."
  [c chasers targets {:keys [covered? fire] :as opts}]
  (let [t0 (clock/now)
        self (.self (:primitives c))
        from (u/self-pos c)
        chasers (with-ways c chasers)
        ways-ms (js/Math.round (- (clock/now) t0))
        r (or (:reach opts) (reach from chasers (.-food self)))
        priced (await (price! c (into (vec targets) (ring from chasers r (settings/get settings ::bearings))) (assoc opts :reach r :chasers chasers)))
        ends (scored chasers from priced nil fire)]
    (ctx/emit! c :retreat.search-work :debug (assoc (meta priced) :ms (js/Math.round (- (clock/now) t0)) :ways-ms ways-ms :targets (count targets)))
    (with-meta (cond-> ends covered? (into (covers chasers from covered? ends fire))) (meta priced))))

(defn unseen-ahead
  "The first unseen cells ([x y z], nearest first) of ends' routes but those in glanced (looked at this flight already:
  each once a flight) and those of routes caught before them (:caught-seen?): where the flight looks before it counts
  itself cornered; nil when none is left."
  [ends glanced]
  (->> ends (remove :caught-seen?) (sort-by :s) (keep :unseen) distinct (remove (set glanced)) seq))

(defn door-s
  "The seconds of end e's route up to its first cell door? (cell -> bool) holds of, else the whole route: a shut door ends the exposure."
  [e door?]
  (or (when door? (:t (first (filter door? (:cells e))))) (:s e)))

(defn cut? "Whether search ends (search!) were cut by the search's meter before every target was planned." [ends] (boolean (:cut? (meta ends))))

(defn boxed?
  "Whether the search planned routes and the planner found every one's region explored with no way on (price!'s meta:
  all planned, all unrouted as exhausted, none left by a cut, cut?): what is round the body is seen and shut, no unseen
  way left to hope for."
  [ends]
  (let [{:keys [planned unrouted]} (meta ends)]
    (boolean (and (pos? (or planned 0)) (= planned (get unrouted "exhausted" 0)) (not (cut? ends))))))

(defn caught-chance
  "{:chance :seconds :blast-d} of a flight from where search ends (search!) ran: the chance it is caught
  (cost/flight-caught-chance) and the seconds its chasers are within their blow when it is (:caught-s; nil: the whole
  flight). A way is a ring route that is open or runs on into cells not seen (a pocket is none: the chaser follows it in)
  and gains on the chasers, none of which gets to its end before the body. The flight takes the best-scored way not
  caught: the chance is that of a way the race calls free, the prior (plus its dead end at counts, wworld/way-counts,
  when it runs unseen); a catch lasts the least seconds the race keeps a chaser in reach over the caught headings (nil
  with none caught), a creeper's blast landing at the least distance of them (:blast-d; nil none). The other headings only choose the way. With every way caught: chance 1, over the seconds of the way
  it gets away on least so (else of a ring route into the unseen; a closed pocket's: the whole flight), :cornered? when
  choose (with opts, the flight's own) takes none of the ends and no unseen cell is left to look at (unseen-ahead, glanced
  the cells the flight looked at: the flight would be the cornered step, no walk away); with one, or headings a cut
  search left unplanned (cut?), a way may be there at way-on-chance: the chance is that of an unseen way
  (flight-caught-chance), with :look the cells to look at. Nothing routed:
  the prior with the unseen worse case, unless every route was exhausted (boxed?): cornered. A flight that takes the preferred end (a safe home) uncaught
  is a known route to a refuge: {:chance 0 :refuge-s seconds of the route up to its door}, priced as a cover (cost/refuge-answer)."
  [ends counts opts glanced]
  (let [picked (choose ends opts)
        refuge? (and picked (not (:caught? picked)) (boolean (some-> (:prefer? opts) (#(% picked)))))
        ring (filter #(and (nil? (:pos (:target %))) (not (:cover? %))) ends)
        left (filter #(and (or (:open? %) (:unseen %)) (not (:caught? %)) (pos? (:gain %)) (> (:arrive %) (:s %))) ring)
        ways (filter gets-away? ends)
        best (when (seq left) (apply max-key :score left))
        caught (filter :caught? ring)]
    (cond
      refuge? {:chance 0 :refuge-s (door-s picked (:door? opts))}
      best (cond-> {:chance (cost/flight-caught-chance {:way? true :unseen? (some? (:unseen best)) :counts counts :dead-chance (:dead-chance best)})}
             (seq caught) (assoc :seconds (reduce min (map :caught-s caught)))
             (some :blast-d caught) (assoc :blast-d (reduce min (keep :blast-d caught))))
      (and (empty? ring) (boxed? ends)) {:chance 1 :cornered? true}
      (empty? ring) {:chance (cost/flight-caught-chance {:way? true :unseen? true :counts counts})}
      :else (let [timed (or (seq ways) (seq (filter :unseen ring)))
                  free? (some? (choose ends opts))
                  look? (and (not free?) (unseen-ahead ends glanced))
                  ;; headings a cut left unplanned may hold a way, at the unseen one's chance
                  open? (or look? (cut? ends))]
              (cond-> {:chance (if open? (cost/flight-caught-chance {:way? true :unseen? true :counts counts}) 1)
                       :cornered? (not (or free? open?))}
                look? (assoc :look look?)
                timed (assoc :seconds (reduce min (map :caught-s timed)))
                (some :blast-d timed) (assoc :blast-d (reduce min (keep :blast-d timed))))))))

(defn way-up-chance
  "{:chance} of a flight that takes a stair up it has seen (respond.cover/way-up): a seen way, at wworld/way-counts counts."
  [counts]
  {:chance (cost/flight-caught-chance {:way? true :unseen? false :counts counts})})

(defn named
  "The scored end of search ends planned to the named target at pos, nil when none."
  [ends pos]
  (first (filter #(= pos (:pos (:target %))) ends)))

(defn beaten-to?
  "Whether a chaser gets to pos before a body with food food at from could, walking straight there at a flight's pace
  (cost/wins-race? lost at cost/flee-pace): no route there can be safe."
  [chasers from food pos]
  (not (cost/wins-race? (/ (u/dist from (cost/centred pos)) (cost/flee-pace food)) (arrive-at chasers (cost/centred pos)))))

(defn safe-why
  "Why the walk to a named target is no safe run, its scored end e: :no-way (no route there, or only part of one),
  :past-hostile (a chaser catches the route), :hostile-nearer (one gets there first: cost/wins-race? lost); nil when safe."
  [e]
  (cond
    (not (:found? e)) :no-way
    (:caught? e) :past-hostile
    (not (cost/wins-race? (:s e) (:arrive e))) :hostile-nearer))
