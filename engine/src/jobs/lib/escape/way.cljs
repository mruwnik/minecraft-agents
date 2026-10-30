(ns jobs.lib.escape.way
  "go-to's escalation picks a way for a body that cannot walk to its goal (jobs.lib.escape reads the cells):
  - wall-spot, door-spot: the cheapest cell beside a wall, or with a door it may dig (priced-spot), to walk to first.
  - price: the seconds an escalation's way takes (jobs.lib.cost); way-s: that, the climb left, the walk on and its dangers.
  - choose: the escalation for a body here and a goal; way-spot: where to walk when none can be made here.
  Cells are [x y z]."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.choose :as choose]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.reach :as reach]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(declare choose way-end)

(defn walk-s [a b] (:cost (cost/walk-cost {:distance (apply js/Math.hypot (map - b a))})))

(defn rest-s-fn
  "(rest-s cell): jobs.lib.cost seconds of the walk on from cell to goal along the straight line, its unseen cells at
  the unseen price (wworld/unseen-share)."
  [p goal]
  (let [kind-at (reach/seen-lookup p reach/kind-of :ground)
        share (wworld/unseen-share p)]
    (memoize (fn [cell] (+ (walk-s cell goal) (cost/unseen-route-s share kind-at (map #(zipmap [:x :y :z] %) (escape/line-cells cell goal))))))))

(defn priced-spot
  "The cheapest cell (jobs.lib.cost seconds: the walk there, plus the walk on to goal along the straight line with its
  unseen cells at the unseen price, wworld/unseen-share, rest-s) other than feet and the cells in visited (already walked to)
  for which (take? cell g rest-s), g its walk's seconds, over the cells steps (level-steps) reaches from feet: tried
  cheapest first, at most cap tried (nil: no cap). nil when none."
  [p feet goal steps take? visited cap]
  (let [rest-s (rest-s-fn p goal)]
    (loop [open (sorted-set [(rest-s feet) 0 0 feet]) best {feet 0} n 1 tried 0]
      (when-let [[_ _ g cell :as top] (first open)]
        (let [open (disj open top)
              try? (and (= g (best cell)) (not= cell feet) (not (visited cell)))]
          (cond
            (< (best cell) g) (recur open best n tried)
            (and try? (take? cell g rest-s)) cell
            (and cap (>= tried cap)) nil
            :else (let [nbrs (keep #(let [g2 (+ g (walk-s cell %))] (when (< g2 (get best % js/Infinity)) [% g2])) (steps cell))]
                    (recur (into open (map-indexed (fn [i [c g2]] [(+ g2 (rest-s c)) (+ n i) g2 c]) nbrs))
                           (into best nbrs) (+ n (count nbrs)) (cond-> tried try? inc)))))))))

(defn wall-spot
  "The cheapest cell (priced-spot through standable cells) with a solid side: one whose side toward goal is solid
  when there is one, else one with any. nil when none. Cells in visited (already walked to) do not count."
  ([p feet goal] (wall-spot p feet goal #{}))
  ([p feet goal visited]
   (let [block-at (escape/block-at-of p)
         steps (escape/level-steps p feet false)
         toward? (fn [cell _ _] (when-let [d (escape/heading cell goal)] (escape/solid? (block-at (escape/ahead cell d 1)))))]
     (or (priced-spot p feet goal steps toward? visited nil)
         (priced-spot p feet goal steps (fn [cell _ _] (escape/walled-side? block-at cell)) visited nil)))))

(defn door-spot
  "The cheapest cell (priced-spot through standable cells) from which a wall straight toward goal is a door (door)
  whose every cell is diggable? and whose far side is cheaper to goal from there than from feet, the walk there
  counted: where to walk when the wall at feet
  cannot be dug (an iron door or another's build in line). nil when none. Cells in visited (already walked to) do not
  count."
  [p feet goal diggable visited]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        take? (fn [cell g rest-s] (when-let [d (escape/heading cell goal)]
                                    (when-let [gap (escape/door block-at unseen? cell d)]
                                      (and (every? diggable (:cells gap)) (< (+ g (rest-s (:through gap))) (rest-s feet))))))]
    (priced-spot p feet goal (escape/level-steps p feet false) take? visited nil)))

(defn carried-build-n
  "How many blocks of the stair's floor fillers (choose/fillers :build) the body carries."
  [p]
  (let [names (set (choose/fillers :build))]
    (transduce (comp (filter #(names (:name %))) (map :count)) + 0 (u/inventory p))))

(defn stair-route-s
  "Seconds the stair way e takes from feet, each step as jobs.lib.dig-route (route-answer) prices one: jobs.lib.cost/stair-leg-pricer
  of its cuts (dig-cost), a fill for an open floor (a fetch of the blocks, with none carried) and an unseen cut or floor,
  its unseen step in the price (cw/step-price), as the route's search does."
  [p feet {:keys [heading steps]}]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        leg-s (cost/stair-leg-pricer (wworld/unseen-share p))
        fill-s (cw/fill-cost (carried-build-n p))
        cut-s (fn [cell] (let [n (block-at cell)]
                           (cond (dig-route/open? n) nil
                                 (nil? (dig-route/cell-cost n)) ##Inf
                                 :else (or (cost/dig-cost (or n "stone")) ##Inf))))]
    (reduce (fn [total cuts]
              (let [floor (escape/add (nth cuts 2) [0 -1 0])
                    leg (leg-s (map cut-s cuts) (when (dig-route/open? (block-at floor)) fill-s) (boolean (some unseen? (cons floor cuts))))]
                (+ total leg)))
            0 (partition 3 (escape/stair-cuts feet (escape/heading-dirs (keyword heading)) steps)))))

(defn price
  "Seconds way e from feet is reckoned to take (jobs.lib.cost): a stair, a pillar, a bridge or a fill by its blocks, a dug door or
  route by what its cells take to dig with the tool carried, plus a step a cell; 0 for a step that is no span. A stair,
  door or route adds what its cuts destroy (cost/destroyed-fn); with own-s, a cell it prices over 0, the body's own build,
  is left out of that: the caller adds its price."
  ([p feet e] (price p feet e nil))
  ([p feet {:keys [step heading] :as e} own-s]
    (let [block-at (delay (escape/block-at-of p))
          destroyed (delay (cost/destroyed-fn (fn [[x y z]] (u/sensed p {:x x :y y :z z}))))
          lost-s (fn [cells] (transduce (comp (remove #(or (dig-route/open? (@block-at %)) (and own-s (pos? (own-s %)))))
                                              (map #(@destroyed (@block-at %) % false)))
                                        + 0 cells))
          dig-s (fn [cells] (+ (cost/dig-cells-s (map #(tools/clear-ms p (@block-at %)) cells)) (lost-s cells)))]
      (case step
        :stair (+ (stair-route-s p feet e) (* (:aside e 0) (or (cost/dig-cost "stone") 1))
                  (lost-s (escape/stair-cuts feet (escape/heading-dirs (keyword heading)) (:steps e))))
        :pillar (cost/pillar-s (wworld/unseen-share p) (:height e) (escape/unseen-pillar-blocks p feet (:height e)))
        :bridge (cw/bridge-cost (:length e) (:short e 0))
        :fill-gap (cw/fill-cost (:count e 1))
        :dig-route (dig-s (map :cell (:cells e)))
        :clear-path (let [{:keys [cells fall]} (escape/door @block-at (escape/unseen-of p) feet (escape/heading-dirs (:heading e)))
                          self (.self p)]
                      (+ (dig-s cells)
                         (if fall (cost/hp-cost (.-health self) (cost/fall-damage (cost/equipment-of (.-equipment self)) fall)) 0)))
      0))))

(defn new-stair-step-s
  "Seconds one step of a stair not yet laid out (no heading chosen) takes: its three cuts priced as stone, and its cells
  unseen (the worse case)."
  [p]
  ((cost/stair-leg-pricer (wworld/unseen-share p)) (repeat 3 (or (cost/dig-cost "stone") 0)) nil true))

(defn rise-left
  "Blocks from end ([x y z]) up to the stand a step below goal's height."
  [end goal]
  (max 0 (- (second goal) (second end) 1)))

(defn rise-left-s
  "Seconds the climb from end up rise-left to goal takes: the cheaper of as many new stair steps (new-stair-step-s) and a
  pillar (cost/pillar-s: its unseen blocks, those past the carried climb blocks left fetched), none when a cell over end
  it climbs into is seen solid (rock overhead); a stair is none (##Inf) when a seen cell over end is not diggable? (may-dig?)."
  ([p end goal left] (rise-left-s p end goal left (constantly true)))
  ([p end goal left may-dig?]
  (let [n (rise-left end goal)
        block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        over (map #(escape/up end %) (range 2 (+ n 2)))
        open? (every? #(or (unseen? %) (bs/passable? (block-at %))) over)
        cuttable? (every? #(or (unseen? %) (escape/diggable? block-at may-dig? %)) over)
        stair-s (if cuttable? (* n (new-stair-step-s p)) ##Inf)]
    (cond (zero? n) 0
          open? (min stair-s (cost/pillar-s (wworld/unseen-share p) n (escape/unseen-pillar-blocks p end n) (max 0 (- n left))))
          :else stair-s))))

(defn stair-floors
  "The floor cells of stair way e's steps from feet."
  [feet {:keys [heading steps]}]
  (map #(escape/add (nth % 2) [0 -1 0]) (partition 3 (escape/stair-cuts feet (escape/heading-dirs (keyword heading)) steps))))

(defn spent-blocks
  "How many of the carried climb block item way e from feet places: a pillar's height, a stair's open floors when item
  is one of its floor fillers."
  [p feet item e]
  (case (:step e)
    :pillar (:height e)
    :stair (if (some #{item} (choose/fillers :build))
             (count (filter #(dig-route/open? ((escape/block-at-of p) %)) (stair-floors feet e)))
             0)
    0))

(defn way-cells
  "The cells way e from feet takes the body through, feet first: a pillar's column, a stair's steps, else end (a door's far
  side)."
  [feet {:keys [step height heading steps]} end]
  (into [feet]
        (case step
          :pillar (map #(escape/up feet %) (range 1 (inc height)))
          :stair (let [[dx dz] (escape/heading-dirs (keyword heading))] (map #(escape/add feet [(* % dx) % (* % dz)]) (range 1 (inc steps))))
          [end])))

(defn way-s
  "Seconds way e (choose's) made from feet takes the body to goal (jobs.lib.cost): its price (an approach: its walk, then
  the door toward goal from its cell when there is one), the climb left from where it leaves the body (rise-left-s, with
  the count of carried climb block item it leaves), the walk on from the top of that climb (rest-s, rest-s-fn), and, with
  risk-s, what the dangers on its cells cost over those seconds ((risk-s cells goal work-s): go-to's :risk-s)."
  [p feet goal e {:keys [rest-s risk-s item count may-dig?] :or {may-dig? (constantly true)}}]
  (let [[walk from e] (if (= :approach (:step e))
                        (let [pos (:pos e)
                              door (some->> (escape/heading pos goal) escape/heading-names (hash-map :step :clear-path :heading))]
                          [(walk-s feet pos) pos (when (and door (way-end p pos goal door)) door)])
                        [0 feet e])
        end (if e (way-end p from goal e) from)
        left (- (or count 0) (if e (spent-blocks p from item e) 0))
        work (+ walk (if e (price p from e) 0) (rise-left-s p end goal left may-dig?))
        top (escape/up end (rise-left end goal))]
    (+ work (rest-s top)
       (if risk-s (risk-s (vec (dedupe (concat [feet] (if e (way-cells from e end) [from]) (map #(escape/up end %) (range 1 (inc (rise-left end goal)))) [top]))) goal work) 0))))

(def way-spot-cap
  "Standable cells way-spot tries choose from at most."
  12)

(defn way-end
  "Where way e (choose's) made from cell leaves the body: a pillar's top, a stair's last step, a clear-path's far
  side; nil for any other step."
  [p cell goal {:keys [step height heading steps]}]
  (case step
    :pillar (escape/up cell height)
    :stair (let [[dx dz] (escape/heading-dirs heading)] (escape/add cell [(* steps dx) steps (* steps dz)]))
    :clear-path (:through (escape/door (escape/block-at-of p) (escape/unseen-of p) cell (escape/heading-dirs heading)))
    nil))

(defn way-spot
  "The cheapest cell (priced-spot through standable cells a step up or down included, at most way-spot-cap tried) from
  which choose makes a way (a pillar, stair or clear-path) toward goal: where to walk when no way can be made from feet
  itself, e.g. a body inside its hut beside the doorway it cannot cut through the wall to. A clear-path, stair or pillar must
  leave the walk there and on from its end (way-end) cheaper than the walk from feet (a stair or pillar up toward a higher goal); one out of a pit or room counts as it is. nil when none."
  [p feet goal may-dig? opts visited]
  (let [opts (update opts :skip (fnil conj #{}) :approach)
        take? (fn [cell g rest-s]
                (let [e (choose p cell goal may-dig? opts)
                      end (way-end p cell goal e)]
                  (case (:step e)
                    :clear-path (boolean (and end (< (+ g (rest-s end)) (rest-s feet))))
                    (:stair :pillar) (or (<= (goal 1) (cell 1)) (< (+ g (rest-s end)) (rest-s feet)))
                    false)))]
    (priced-spot p feet goal (escape/level-steps p feet true) take? visited way-spot-cap)))

(defn choose
  "How a body at feet (a cell) that cannot walk to goal (a cell) can make a way, cheapest first. It digs
  any block in cells (may-dig? cell)
  allows (go-to: no other owner's zone or claim); opts :pillar-item ({:item :count} or nil, escape/pillar-item) replaces the climb block read from the inventory. opts :skip is a set of steps that already failed (a stair heading
  that failed: [:stair :east]): the next one is chosen; opts :drop-steps, steps never weighed among the ways (the wall walk is still offered); opts :approached, the cells an approach already walked to; opts :stands, the feet cells of the body's own stairs (a ledge stair's first floor never lands on one: the stair job refuses :undercuts-way).
  - the cheapest (way-s; opts :risk-s prices their dangers) of the ways that can be made:
    - {:step :pillar :height n :item}: in a pit d deep, the column over it seen clear to the top, at least n pillar
      blocks carried: n the rim-pillar (d - 1, then a step onto the rim), d when no side has a rim to step onto;
      beside a wall (a pit too wide to be walled on three sides), n the rim-pillar.
    - {:step :stair :heading kw :steps n}: a block in front to stair up on, n how far the goal is above or the pit's
      depth, whichever is more, at most max-depth (the pit's depth when the longer cut is not all diggable?), every
      cell of its cut diggable?.
    - {:step :clear-path :heading kw}: a wall straight toward the goal, with floor beyond, every cell diggable?, not
      skipped (door-key; a bare :clear-path skips every door).
    - {:step :approach :pos cell}: no door at feet: a wall in line it cannot dig: to door-spot; in the free part of a
      shut door's cell, step back off the panel (door-back).
    else the first of these:
  - {:step :approach :pos cell}: no side of the body's cell is solid: walk to wall-spot first.
  - on a ledge (no side solid, no wall-spot) under a higher goal with a pillar block carried: a stair whose first
    floor the stair job lays.
  - a stair with no pit and no goal above (a sealed tunnel, a goal through thick rock): the deepest diggable one, up
    to max-depth.
  - the pit's pillar with an unseen or solid cell in the column over it.
  - {:step :none :why kw}: none of these. :why :no-headroom: a pit the carried blocks would pillar out of but for
    the block over the head; :no-dig: nothing allowed to dig; :no-wall: no wall to walk to."
  ([p feet goal] (choose p feet goal (constantly true)))
  ([p feet goal may-dig?] (choose p feet goal may-dig? nil))
  ([p feet goal may-dig? {:keys [skip approached stands drop-steps] :or {skip #{} approached #{} stands #{}} :as opts}]
   (let [approached (or approached #{})
         block-at (escape/block-at-of p)
         dir (escape/heading feet goal)
         depth (escape/pit-depth block-at (escape/unseen-of p) feet)
         {:keys [item count]} (if (contains? opts :pillar-item) (:pillar-item opts) (escape/carried-pillar-item p))
         rim-n (when (and item (not (skip :pillar))) (escape/rim-pillar block-at (escape/unseen-of p) feet dir))
         height (when (pos? depth) (min depth (or rim-n depth)))
         pillar-blocks? (and (not (skip :pillar)) height item (>= count height))
         headroom? (bs/passable? (block-at (escape/up feet 2)))
         open-above? (and pillar-blocks? (every? #(bs/passable? (block-at (escape/up feet %))) (range 2 (+ height 2))))
         rise (min (escape/max-depth) (max depth (- (second goal) (second feet))))
         stair-dirs (remove #(skip [:stair (escape/heading-names %)]) (distinct (cond->> escape/cardinals dir (cons dir))))
         [stair rise] (if (or (skip :stair) (not (pos? rise)))
                        [nil rise]
                        (or (some (fn [n] (some-> (escape/stair-heading block-at feet stair-dirs n may-dig?) (vector n)))
                                  (distinct (filter pos? [rise depth])))
                            [nil rise]))
         climb (delay (when-not (or (skip :stair) (pos? rise))
                        (some (fn [n] (some-> (escape/stair-heading block-at feet stair-dirs n may-dig?) (vector n)))
                              (range (escape/max-depth) 0 -1))))
         ledge (delay (when (and item (not (skip :stair)) (pos? rise) (not (escape/walled-side? block-at feet)))
                        (escape/stair-heading block-at feet (remove #(contains? stands (escape/ahead feet % 1)) stair-dirs) rise may-dig? true)))
         door-failed? (and dir (skip (escape/door-key feet (escape/heading-names dir))))
         gap (when (and dir (not (skip :clear-path)) (not door-failed?))
               (escape/door block-at (escape/unseen-of p) feet dir))
         ;; door-spot is for a wall in line that may not be dug; one that was dug and failed is no cause to try its neighbour
         spot (delay (when (and dir (not (skip :approach)) (not (skip :clear-path)) (not door-failed?) (escape/walled-side? block-at feet))
                       (door-spot p feet goal #(escape/diggable? block-at may-dig? %)
                                  (into approached (escape/skipped-door-cells skip)))))
         wall (delay (wall-spot p feet goal approached))
         back (delay (when-not (or (skip :approach) (skip :clear-path)) (escape/door-back p feet approached)))
         door? (and gap (every? #(escape/diggable? block-at may-dig? %) (:cells gap)))
         rim (when (and rim-n (>= count rim-n)) rim-n)
         ways (distinct (cond-> []
                          (and open-above? headroom?) (conj {:step :pillar :height height :item item})
                          stair (conj {:step :stair :heading stair :steps rise})
                          rim (conj {:step :pillar :height rim :item item})
                          door? (conj {:step :clear-path :heading (escape/heading-names dir)})
                          ;; door-spot and door-back stand for a door at feet that cannot be dug
                          (and (not door?) @spot) (conj {:step :approach :pos @spot})
                          (and (not door?) @back) (conj {:step :approach :pos @back})))
         ways (remove #(contains? (set drop-steps) (:step %)) ways)]
     (cond
       (seq ways)
       (let [priced {:rest-s (rest-s-fn p goal) :risk-s (:risk-s opts) :item item :count count :may-dig? may-dig?}]
         (second (reduce (fn [[best-s best :as cur] w] (let [s (way-s p feet goal w priced)] (if (< s best-s) [s w] cur)))
                         [##Inf (first ways)] ways)))

       (and (not (skip :approach)) (not (and (skip :clear-path) (skip :stair))) (not (escape/walled-side? block-at feet)) @wall)
       {:step :approach :pos @wall}

       @ledge
       {:step :stair :heading @ledge :steps rise}

       (and (not (skip :approach)) (not (escape/walled-side? block-at feet)))
       {:step :none :why :no-wall}

       @climb
       (let [[heading n] @climb] {:step :stair :heading heading :steps n})

       (and pillar-blocks? headroom?)
       {:step :pillar :height height :item item}

       pillar-blocks? {:step :none :why :no-headroom}

       :else {:step :none :why :no-dig}))))
