(ns jobs.lib.escape.way
  "go-to's escalation picks a way for a body that cannot walk to its goal (jobs.lib.escape reads the cells):
  - wall-spot: the cheapest cell beside a wall (priced-spot), to walk to first.
  - price: the seconds an escalation's way takes (jobs.lib.cost); way-s: that, the climb left, the walk on and its dangers.
  - choose: the escalation for a body here and a goal; way-spot: where to walk when none can be made here.
  Cells are [x y z]."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.choose :as choose]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.cost.value :as value]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.reach :as reach]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(declare choose way-end)

(defn walk-s [a b] (:cost (cost/walk-cost {:distance (apply js/Math.hypot (map - b a))})))

(def read-line-cells "Line cells read for their seen state; the rest of a farther goal's line is priced all unseen, unread." 512)

(defn rest-s-fn
  "(rest-s cell): jobs.lib.cost seconds of the walk on from cell to goal along the straight line, its unseen cells at
  the unseen price (wworld/unseen-share); only the first read-line-cells of the line are looked at."
  [p goal]
  (let [kind-at (reach/seen-lookup p reach/kind-of :ground)
        share (wworld/unseen-share p)]
    (memoize (fn [cell]
               (let [n (js/Math.ceil (apply js/Math.hypot (map - goal cell)))
                     far (max 0 (- n 1 read-line-cells))]
                 (+ (walk-s cell goal)
                    (cost/unseen-route-s share kind-at (map #(zipmap [:x :y :z] %) (take read-line-cells (escape/line-cells cell goal))))
                    (* far (cost/unseen-step-s share))))))))

(defn priced-spot
  "The cheapest cell (jobs.lib.cost seconds: the walk there, plus the walk on to goal along the straight line with its
  unseen cells at the unseen price, wworld/unseen-share, rest-s) other than feet and the cells in visited (already walked to)
  for which (take? cell g rest-s), g its walk's seconds, over the cells steps (level-steps) reaches from feet: tried
  cheapest first, so the first taken is the cheapest; the search ends, nil, when the cheapest open cell's price is more than
  bound-s affords (cost/affords?; nil: no bound). nil when none."
  [p feet goal steps take? visited bound-s]
  (let [rest-s (rest-s-fn p goal)]
    (loop [open (sorted-set [(rest-s feet) 0 0 feet]) best {feet 0} n 1]
      (when-let [[f _ g cell :as top] (first open)]
        (let [open (disj open top)]
          (cond
            (and bound-s (not (cost/affords? f bound-s))) nil
            (< (best cell) g) (recur open best n)
            (and (not= cell feet) (not (visited cell)) (take? cell g rest-s)) cell
            :else (let [nbrs (keep #(let [g2 (+ g (walk-s cell %))] (when (< g2 (get best % js/Infinity)) [% g2])) (steps cell))]
                    (recur (into open (map-indexed (fn [i [c g2]] [(+ g2 (rest-s c)) (+ n i) g2 c]) nbrs))
                           (into best nbrs) (+ n (count nbrs))))))))))

(defn wall-spot
  "The cheapest cell (priced-spot through standable cells, within bound-s) with a solid side: one whose side toward goal is
  solid when there is one, else one with any. nil when none. Cells in visited (already walked to) do not count."
  [p feet goal visited bound-s]
  (let [block-at (escape/block-at-of p)
        steps (escape/level-steps p false)
        toward? (fn [cell _ _] (when-let [d (escape/heading cell goal)] (escape/solid? (block-at (escape/ahead cell d 1)))))]
    (or (priced-spot p feet goal steps toward? visited bound-s)
        (priced-spot p feet goal steps (fn [cell _ _] (escape/walled-side? block-at cell)) visited bound-s))))

(defn carried-build-n
  "How many blocks of the stair's floor fillers (choose/fillers :build) the body carries."
  [p]
  (let [names (set (choose/fillers :build))]
    (transduce (comp (filter #(names (:name %))) (map :count)) + 0 (u/inventory p))))

(defn loose-run
  "Lazy seq of [cell w name] for the cells up base's column (base is [x y z], the top of what the body has seen there):
  w the chance the cell is loose ground (falling: sand, gravel) and name the block it is. A seen cell is whole or not
  (w 1 or 0, its own name); an unseen cell over a run of h loose cells is loose at value/loose-over-loose-chance of h,
  times the chance of the cells under it, as the block under it. The run ends at a seen cell that is not loose and at an
  unseen one with no loose cell under it (w 0 from there: no prior to read, the caller's own guess prices it)."
  [p base]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        under (fn [] (loop [h 0 cell base] (if (and (< h 64) (not (unseen? cell)) (rules/falling? (block-at cell))) (recur (inc h) (escape/add cell [0 -1 0])) h)))
        step (fn step [cell h w name]
               (when (< h 64)
                 (let [cell (escape/add cell [0 1 0])]
                   (cond (not (unseen? cell)) (let [n (block-at cell)]
                                                (when (rules/falling? n) (cons [cell 1 n] (lazy-seq (step cell (inc h) 1 n)))))
                         (nil? name) nil
                         :else (let [w (* w (value/loose-over-loose-chance h))]
                                 (cons [cell w name] (lazy-seq (step cell (inc h) w name))))))))
        h0 (under)]
    (step base h0 1 (when (pos? h0) (block-at base)))))

(defn stair-route-s
  "Seconds the stair way e takes from feet, each step as jobs.lib.dig-route (route-answer) prices one: jobs.lib.cost/stair-leg-pricer
  of its cuts (the carried tool's dig time, an unseen one at the chance it is solid), a fill for an open floor (a fetch of the blocks, with none carried) and an unseen cut or floor,
  its unseen step in the price (cw/step-price), as the route's search does. A falling block (gravel, sand) is dug like any
  cut (jobs.access.stair digs it again as it falls), and each one stacked over a column's top cut is one dig more: a seen
  one whole, an unseen one over loose ground at the chance it and those under it are loose too (loose-run)."
  [p feet {:keys [heading steps]}]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        leg-s (cost/stair-leg-pricer (wworld/unseen-share p))
        fill-s (cw/fill-cost (carried-build-n p))
        solid-share (escape/dig-share-fn p)
        dig-s (fn [n] (/ (tools/clear-ms p n) 1000))
        cut-s (fn [cell] (let [n (block-at cell)]
                           (cond (dig-route/open? n) nil
                                 (and (nil? (dig-route/cell-cost n)) (not (rules/falling? n))) ##Inf
                                 :else (* (solid-share cell) (dig-s (or n "stone"))))))
        cuts (escape/stair-cuts feet (escape/heading-dirs (keyword heading)) steps)
        tops (vals (reduce (fn [m [x y z]] (update m [x z] #(if (and % (> (% 1) y)) % [x y z]))) {} cuts))
        stack-s (fn [top] (transduce (map (fn [[_ w n]] (* w (dig-s n)))) + 0 (loose-run p top)))]
    (reduce (fn [total cuts]
              (let [floor (escape/add (nth cuts 2) [0 -1 0])
                    leg (leg-s (map cut-s cuts) (when (dig-route/open? (block-at floor)) fill-s) (boolean (some unseen? (cons floor cuts))))]
                (+ total leg)))
            (transduce (map stack-s) + 0 tops) (partition 3 cuts))))

(defn pillar-way-s
  "Seconds a pillar of height blocks up from feet takes, short of them fetched: cost/pillar-s, its knock
  (escape/pillar-knock), and a dig of each cell over it the head climbs into that the body has not seen, at the
  chance it is loose ground over a seen loose cell (loose-run) else solid (escape/dig-share-fn)."
  [p feet height short]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        solid-share (escape/dig-share-fn p)
        loose (into {} (map (fn [[cell w n]] [cell [w n]])) (loose-run p (escape/up feet 1)))
        roof-s (transduce (comp (map #(escape/up feet %)) (filter unseen?)
                                (map (fn [cell] (let [[w n] (get loose cell [0])]
                                                  (+ (if n (* w (/ (tools/clear-ms p n) 1000)) 0)
                                                     (* (- 1 w) (solid-share cell) (/ (tools/clear-ms p (block-at cell)) 1000)))))))
                          + 0 (range 2 (+ height 2)))]
    (+ (cost/pillar-s (wworld/unseen-share p) height (escape/unseen-pillar-blocks p feet height) short)
       (escape/pillar-knock p feet height)
       roof-s)))

(defn price
  "Seconds way e from feet is reckoned to take (jobs.lib.cost): a stair, a pillar, a bridge or a fill by its blocks, a dug
  route by what its cells take to dig with the tool carried (an unseen cell at the chance it is solid), plus a step a cell; 0 for a step that is no span. A stair
  or route adds what its cuts destroy (cost/destroyed-fn); with own-s, a cell it prices over 0, the body's own build,
  is left out of that: the caller adds its price."
  ([p feet e] (price p feet e nil))
  ([p feet {:keys [step heading] :as e} own-s]
    (let [block-at (delay (escape/block-at-of p))
          destroyed (delay (cost/destroyed-fn (fn [[x y z]] (u/sensed p {:x x :y y :z z}))))
          lost-s (fn [cells] (transduce (comp (remove #(or (dig-route/open? (@block-at %)) (and own-s (pos? (own-s %)))))
                                              (map #(@destroyed (@block-at %) % false)))
                                        + 0 cells))]
      (case step
        :stair (+ (stair-route-s p feet e) (* (:aside e 0) (/ (tools/clear-ms p "stone") 1000))
                  (lost-s (escape/stair-cuts feet (escape/heading-dirs (keyword heading)) (:steps e))))
        :pillar (pillar-way-s p feet (:height e) 0)
        :bridge (cw/bridge-cost (:length e) (:short e 0))
        :fill-gap (cw/fill-cost (:count e 1))
        :dig-route (let [solid-share (escape/dig-share-fn p)
                         unseen? (escape/unseen-of p)]
                     (+ (* (count (filter unseen? (map :cell (:cells e)))) (cost/unseen-step-s (wworld/unseen-share p)))
                        (cost/dig-cells-s (map #(* (solid-share %) (tools/clear-ms p (@block-at %))) (map :cell (:cells e))))
                        (transduce (map #(* (solid-share %) (lost-s [%]))) + 0 (map :cell (:cells e)))))
      0))))

(defn new-stair-step-s
  "Seconds one step of a stair not yet laid out (no heading chosen) takes: its three cuts priced as stone with the carried tool, and its cells
  unseen (the worse case)."
  [p]
  ((cost/stair-leg-pricer (wworld/unseen-share p)) (repeat 3 (/ (tools/clear-ms p "stone") 1000)) nil true))

(defn rise-left
  "Blocks from end ([x y z]) up to the stand a step below goal's height."
  [end goal]
  (max 0 (- (second goal) (second end) 1)))

(defn rise-left-s
  "Seconds the climb from end up rise-left to goal takes: the cheaper of as many new stair steps (new-stair-step-s) and a
  pillar (pillar-way-s, the blocks past the carried climb blocks left fetched), none when a cell over end
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
          open? (min stair-s (pillar-way-s p end n (max 0 (- n left))))
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
  "The cells way e from feet takes the body through, feet first: a pillar's column, a stair's steps, else end."
  [feet {:keys [step height heading steps]} end]
  (into [feet]
        (case step
          :pillar (map #(escape/up feet %) (range 1 (inc height)))
          :stair (let [[dx dz] (escape/heading-dirs (keyword heading))] (map #(escape/add feet [(* % dx) % (* % dz)]) (range 1 (inc steps))))
          [end])))

(defn way-s
  "Seconds way e (choose's) made from feet takes the body to goal (jobs.lib.cost): its price (an approach: its walk), the climb left from where it leaves the body (rise-left-s, with
  the count of carried climb block item it leaves), the walk on from the top of that climb (rest-s, rest-s-fn), and, with
  risk-s, what the dangers on its cells cost over those seconds ((risk-s cells goal work-s): go-to's :risk-s)."
  [p feet goal e {:keys [rest-s risk-s item count may-dig?] :or {may-dig? (constantly true)}}]
  (let [[walk from e] (if (= :approach (:step e))
                        [(walk-s feet (:pos e)) (:pos e) nil]
                        [0 feet e])
        end (if e (way-end p from goal e) from)
        left (- (or count 0) (if e (spent-blocks p from item e) 0))
        work (+ walk (if e (price p from e) 0) (rise-left-s p end goal left may-dig?))
        top (escape/up end (rise-left end goal))]
    (+ work (rest-s top)
       (if risk-s (risk-s (vec (dedupe (concat [feet] (if e (way-cells from e end) [from]) (map #(escape/up end %) (range 1 (inc (rise-left end goal)))) [top]))) goal work) 0))))

(defn way-end
  "Where way e (choose's) made from cell leaves the body: a pillar's top, a stair's last step; nil for any other step."
  [p cell goal {:keys [step height heading steps]}]
  (case step
    :pillar (escape/up cell height)
    :stair (let [[dx dz] (escape/heading-dirs heading)] (escape/add cell [(* steps dx) steps (* steps dz)]))
    nil))

(defn way-spot
  "The cheapest cell (priced-spot through standable cells a step up or down included, within the call's :bound-s) from
  which choose makes a way (a pillar or stair) toward goal: where to walk when no way can be made from feet
  itself, e.g. a body inside its hut beside the doorway it cannot cut through the wall to. A stair or pillar must
  leave the walk there and on from its end (way-end) cheaper than the walk from feet (a stair or pillar up toward a higher goal); one out of a pit or room counts as it is. nil when none."
  [p feet goal may-dig? opts visited]
  (let [opts (update opts :skip (fnil conj #{}) :approach)
        take? (fn [cell g rest-s]
                (let [e (choose p cell goal may-dig? opts)
                      end (way-end p cell goal e)]
                  (case (:step e)
                    (:stair :pillar) (or (<= (goal 1) (cell 1)) (< (+ g (rest-s end)) (rest-s feet)))
                    false)))]
    (priced-spot p feet goal (escape/level-steps p true) take? visited (:bound-s opts))))

(defn choose
  "How a body at feet (a cell) that cannot walk to goal (a cell) can make a way, cheapest first. It digs
  any block in cells (may-dig? cell)
  allows (go-to: no other owner's zone or claim); opts :pillar-item ({:item :count} or nil, escape/pillar-item) replaces the climb block read from the inventory. opts :skip is a set of steps that already failed (a stair heading
  that failed: [:stair :east]): the next one is chosen; opts :drop-steps, of :pillar and :stair the ones never chosen, as if skipped, but the wall walk is still offered; opts :approached, the cells an approach already walked to; opts :stands, the feet cells of the body's own stairs (a ledge stair's first floor never lands on one: the stair job refuses :undercuts-way).
  - the cheapest (way-s; opts :risk-s prices their dangers) of the ways that can be made:
    - {:step :pillar :height n :item}: in a pit d deep, the column over it clear or unseen to the top (its unseen blocks priced), at least n pillar
      blocks carried: n the rim-pillar (d - 1, then a step onto the rim), d when no side has a rim to step onto;
      beside a wall (a pit too wide to be walled on three sides), n the rim-pillar.
    - {:step :stair :heading kw :steps n}: a block in front to stair up on, n how far the goal is above or the pit's
      depth, whichever is more, at most max-depth (the pit's depth when the longer cut is not all diggable?), every
      cell of its cut diggable?.
    else the first of these:
  - {:step :approach :pos cell}: no side of the body's cell is solid: walk to wall-spot first.
  - on a ledge (no side solid, no wall-spot) under a higher goal with a pillar block carried: a stair whose first
    floor the stair job lays.
  - a stair with no pit and no goal above (a sealed tunnel, a goal through thick rock): the deepest diggable one, up
    to max-depth.
  - the pit's pillar with a seen solid cell in the column over it.
  - {:step :none :why kw}: none of these. :why :no-headroom: a pit the carried blocks would pillar out of but for
    the block over the head; :no-dig: nothing allowed to dig; :no-wall: no wall to walk to."
  ([p feet goal] (choose p feet goal (constantly true)))
  ([p feet goal may-dig?] (choose p feet goal may-dig? nil))
  ([p feet goal may-dig? {:keys [skip approached stands drop-steps] :or {skip #{} approached #{} stands #{}} :as opts}]
   (let [approached (or approached #{})
         ;; a dropped step is gone from every branch; only the wall walk's gate reads skip alone (failed steps)
         off (into skip drop-steps)
         block-at (escape/block-at-of p)
         dir (escape/heading feet goal)
         depth (escape/pit-depth block-at (escape/unseen-of p) feet)
         {:keys [item count]} (if (contains? opts :pillar-item) (:pillar-item opts) (escape/carried-pillar-item p))
         rim-n (when (and item (not (off :pillar))) (escape/rim-pillar block-at (escape/unseen-of p) feet dir))
         height (when (pos? depth) (min depth (or rim-n depth)))
         pillar-blocks? (and (not (off :pillar)) height item (>= count height))
         headroom? (bs/passable? (block-at (escape/up feet 2)))
         open-above? (and pillar-blocks? (let [unseen? (escape/unseen-of p)] (every? #(or (and (unseen? %) (may-dig? %)) (bs/passable? (block-at %))) (map #(escape/up feet %) (range 2 (+ height 2))))))
         rise (min (escape/max-depth) (max depth (- (second goal) (second feet))))
         stair-dirs (remove #(skip [:stair (escape/heading-names %)]) (distinct (cond->> escape/cardinals dir (cons dir))))
         [stair rise] (if (or (off :stair) (not (pos? rise)))
                        [nil rise]
                        (or (some (fn [n] (some-> (escape/stair-heading block-at feet stair-dirs n may-dig?) (vector n)))
                                  (distinct (filter pos? [rise depth])))
                            [nil rise]))
         climb (delay (when-not (or (off :stair) (pos? rise))
                        (some (fn [n] (some-> (escape/stair-heading block-at feet stair-dirs n may-dig?) (vector n)))
                              (range (escape/max-depth) 0 -1))))
         ledge (delay (when (and item (not (off :stair)) (pos? rise) (not (escape/walled-side? block-at feet)))
                        (escape/stair-heading block-at feet (remove #(contains? stands (escape/ahead feet % 1)) stair-dirs) rise may-dig? true)))
         wall (delay (wall-spot p feet goal approached (:bound-s opts)))
         rim (when (and rim-n (>= count rim-n)) rim-n)
         ways (distinct (cond-> []
                          open-above? (conj {:step :pillar :height height :item item})
                          stair (conj {:step :stair :heading stair :steps rise})
                          rim (conj {:step :pillar :height rim :item item})))]
     (cond
       (and (seq ways) (not (next ways)))
       (first ways)

       (seq ways)
       (let [priced {:rest-s (rest-s-fn p goal) :risk-s (:risk-s opts) :item item :count count :may-dig? may-dig?}]
         (second (reduce (fn [[best-s best :as cur] w] (let [s (way-s p feet goal w priced)] (if (< s best-s) [s w] cur)))
                         [##Inf (first ways)] ways)))

       (and (not (skip :approach)) (not (skip :stair)) (not (escape/walled-side? block-at feet)) @wall)
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
