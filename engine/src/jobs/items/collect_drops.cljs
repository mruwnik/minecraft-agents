(ns jobs.items.collect-drops
  (:require [clojure.string :as str]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.game :as game]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.body :as body]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.look :as look]
            [jobs.lib.loose :as loose]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.sense :as sense]
            [jobs.lib.torch :as torch]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def doc
  "Collect the nearest matching dropped item, in one call, until none is left within :radius of where the job began (the work area: a walk to an item
  does not move it, so drops far away, e.g. other bodies', are never chased).
  An item is tried once; one that is gone, cannot be reached or picked up is skipped. A drop that slid to another cell while the body
  walked or dug to it is judged again there (at most ::max-moves times).
  An item more than 2 blocks off (not above the feet), or more than a block lower than the feet, is walked to first (a jobs.movement.go-to child, range 1, which prices
  falls and dangers against the worth of the drops in sight, :worth); one it cannot reach is left, with a debug collect-drops.left {:reason :unreachable}. A nearer one whose
  straight way crosses a drop (a shaft between) is walked to the same way. A body whose centre overhangs a drop first steps onto
  the block holding it up; failing that the drop is left {:reason :unsafe-floor}.
  A drop in water with a known hostile within the hostile radius of it is left, with {:reason :water-danger}.
  An item lower than the feet in a hole the caller dug (:holes) is walked to only when it lies one block down at most and
  the body has seen the floor under it solid and safe and the cells round it (no lava, fluid or cave opening, nothing
  hazardous beside it), the walk dropping one block at most; else it is left, with a debug collect-drops.left
  {:pos :reason :unseen-floor|:unseen-cell|:unsafe-floor|:too-deep}. A drop still falling is judged at rest.
  With :search, when fewer than the call's :least (default 1) items (those named in :least-items, else any; a gain of :least-items in the inventory since the start counts, so a drop that flew to the body ends the search) were collected and no item is in sight (trunks or terrain hide drops), the body walks (a go-to child)
  to standable cells round the anchor, nearest first (a cell whose floor is unseen counts when nothing seen forbids it: walking there is how it gets looked at; not behind a seen solid block on the way from the anchor, which is rock), and looks again at each, until an item is seen or the cells within :search
  blocks are tried. With :dig a body shut in a room digs out by go-to's priced escalation (:clear-path :stair :dig-out). A search that go-to made wait :search-waits calls in a row ends with :reason :search-waited.
  While it runs it holds its work area (jobs.lib.loose :collecting), so the :loose-items reflex leaves those drops to it.
  With :ids, ids not in sight when none is left to try are booked as one :left {:reason :unseen :ids [...] :last {id [x y z]}}, the cell each was last seen in.
  A drop more than a block above the feet that the act cannot reach (a column top) is walked up to with go-to's :pillar and :stair ways, then collected; one go-to cannot stand beside (a wall niche) is pillared to from the feet when it lies 2 to 6 above and within 1 cell sideways (carried blocks).
  With :dig, a drop go-to cannot reach, or one too deep in the dug hole, is dug to once: the fewest cells (jobs.lib.dig-route,
  over the blocks the body has seen, an unseen one taken for stone) that open a way to it, each a jobs.blocks.dig child (zones, claims, hazards
  are its rules), then the walk is tried again. A dig that ends :unseen (the dark hides the cell) sets a carried torch in the body's cell once per call, looks again and digs on. No such way leaves the drop :no-route, a dig that fails with that dig's outcome (:refused, :cannot, :unreachable ...).
  With :near, a sweep that sees no drop first looks at that spot, and climbs to it (go-to :pillar/:stair) when it lies over a block above the feet. A sweep without :ids that sees no drop turns round once from each cell it ends in (entities are sensed only in the view cone) before it ends; a dig's :ids sweep does not (the dig looked already, and a danger's time is short).
  With :expect n, a sweep that took fewer than n waits ::expect-wait-ms and sweeps again, at most ::expect-rounds times (a drop still falling or not yet seen); the call is not done while it waits.
  Needs nothing; the sweep is the whole job. Ends :done with data {:collected n}, the number of items that entered the
  inventory, with :left [{:pos :reason}] for those (also in :loose-left, so the :loose-items reflex leaves them); none in
  sight at all is :done with 0 (an open sweep, without :near). Drops tried, a :search that found none, or a named :near place with nothing in it, and none collected: :nothing-possible :none-collected, same data;
  :nothing-possible :search-waited when the search was held up.
  Quote: what is in sight within :radius (yields by name), time to walk to each, unbounded with :search or :dig (also the items it may yet find).")

(a/defargs args
  {:radius {:doc "search radius in blocks; the ::radius setting when nil" :spec (a/num-in 0 nil) :default nil}
   :items {:doc "item names to collect; everything when nil" :spec (a/coll-of a/item?) :default nil}
   :near {:doc "{:x :y :z} the work area is centred on, instead of where the body stands when the job begins" :spec ::a/pos :default nil}
   :search {:doc "blocks round the anchor to walk to and look from when no drop is in sight and fewer than the call's :least were collected; 0: no search" :spec (a/num-in 0 nil) :default 0}
   :least-items {:doc "item names that count towards the call's :least; every collected item when nil" :spec (a/coll-of a/item?) :default nil}
   :ids {:doc "entity ids to collect (only those); any item when nil" :spec (a/coll-of number?) :default nil}
   :dig {:doc "dig a way to a drop that cannot be walked to (the cells are left for the caller's tidy)" :spec boolean? :default false}
   :expect {:doc "items the caller knows lie within :radius (cells it just cleared): a sweep that took fewer waits ::expect-wait-ms and sweeps again, at most ::expect-rounds times (a drop still falling or not yet seen)" :spec (a/int-in 0 nil) :default 0}
   :look-holes? {:doc "look at each of :holes (and down its column) once before the round turns round" :spec boolean? :default false}
   :holes {:doc "cells the caller dug: an item lower than the feet at or under one of them, or one column beside, is in that hole"
           :spec (a/coll-of a/position?) :default nil}})

(def target [:ids :near])

(def defaults {:least 1})

(declare radius-of held-count)

(defn place
  "Where the job goes: the cell of the nearest seen item it is after, nil when none is in sight (a search for unseen ones roams)."
  [c]
  (let [{:keys [items ids]} (:args c)
        wanted? (fn [e] (and (or (nil? ids) (some #{(.-id e)} ids))
                             (or (nil? items) (some #{(some-> (.-item e) .-name)} items))))
        feet (sh-sense/feet (:primitives c))]
    (some->> (loose/seen (:primitives c) {:radius (radius-of c)})
             (filter wanted?)
             (map loose/item-pos)
             (sort-by #(u/dist feet %))
             first
             (#(update-vals % js/Math.floor)))))

(def fit? ready/ok)

(a/defargs settings
  {::max-settle-waits {:default 10 :doc "Checks of a hole drop that still moves before it counts as settled." :spec (a/int-in 0 nil)}
   ::walk-s-per-block {:default 0.4 :doc "Seconds a quote counts to walk one block to a drop." :spec (a/num-in 0 nil)}
   ::dig-max-cells {:default 16 :doc "The most cells :dig digs to reach one drop; a dearer route leaves it." :spec (a/int-in 0 nil)}
   ::dig-margin {:default 3 :doc "How far outside the box round the body and the drop the dig route may wander, in blocks." :spec (a/int-in 0 nil)}
   ::replans {:default 4 :doc "How often a dig route is planned again after a dig of it failed, the cells before it dug." :spec (a/int-in 0 nil)}
   ::dig-max-fall {:default 3 :doc "The longest fall, in blocks, the dig route plans." :spec (a/int-in 0 nil)}
   ::pile-reach {:default 10 :doc "How far from the place of an open death a :near spot still counts as its pile, in blocks." :spec (a/num-in 0 nil)}
   ::radius {:default 16 :doc "How far from the work area a drop is looked for when the call gives no :radius, in blocks." :spec (a/num-in 0 nil)}
   ::expect-rounds {:default 3 :doc "How often a sweep that took fewer items than the call's :expect is made again." :spec (a/int-in 0 nil)}
   ::expect-wait-ms {:default 500 :doc "How long the body waits before a sweep is made again for the call's :expect, in ms." :spec (a/int-in 0 nil)}
   ::max-moves {:default 2 :doc "How often a drop that slid to another cell while the body went to it is tried again, after a walk or dig that did not reach it." :spec (a/int-in 0 nil)}
   ::search-waits {:default 5 :doc "How many calls in a row a :search walk may wait on go-to (the night, a danger) before the search is given up." :spec (a/int-in 1 nil)}})

(defn search-waits [] (settings/get settings ::search-waits))

(defn at-death-pile?
  "Whether :near lies within ::pile-reach of the place of a death whose drops have not despawned: a pile never waits for light (the despawn clock runs)."
  [c]
  (let [view (ctx/view c)
        near (some->> (:near (:args c)) ((fn [n] (if (map? n) n (u/cell-pos (vec n))))))]
    (boolean (and near
                  (some (fn [d] (and (pos? (game/despawn-left-ticks d view))
                                     (some-> (:pos (:data d)) (u/dist near) (<= (settings/get settings ::pile-reach)))))
                        (body/open-deaths view))))))

(def safe?
  (let [base (price/staying :work (ready/safe-at-or-roaming place))
        pile-ok (contract/ok :pile "a death pile does not wait for light")]
    (with-meta (fn [c] (if (at-death-pile? c) pile-ok (base c)))
      (update (meta base) ::ready/exposure (fn [f] (fn [c] (if (at-death-pile? c) pile-ok (f c))))))))

(defn max-moves [] (settings/get settings ::max-moves))

(defn radius-of [c] (or (:radius (:args c)) (settings/get settings ::radius)))

(defn gained-count
  "The items an act result says entered the inventory (only those named in names when given)."
  ([r] (gained-count r nil))
  ([r names]
   (->> (array-seq (or (.-gained r) #js []))
        (filter #(or (nil? names) (names (.-name %))))
        (map #(.-count %))
        (reduce + 0))))

(defn candidates
  "The wanted items within :radius of the anchor that are not skipped, nearest first."
  [c anchor]
  (let [radius (radius-of c)
        ids (:ids (:args c))
        only-ids (some-> ids set)
        wanted (some-> (:items (:args c)) set)
        skipped (set (:skipped (ctx/mem c)))]
    (->> (look/seen-items (:primitives c) {:radius (+ radius (u/dist anchor (u/self-pos c))) :max (if (or only-ids wanted) 1024 32)})
         (filter #(if-let [p (.-pos %)] (<= (u/dist anchor (u/pos-of p)) radius) true))
         (remove #(skipped (.-id %)))
         (filter #(or (nil? only-ids) (only-ids (.-id %))))
         (filter #(or (nil? wanted) (wanted (some-> (.-item %) .-name)))))))

(defn item-cell [item] (let [{:keys [x y z]} (u/pos-of (.-pos item))] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn note-last-cells!
  "Remember, per wanted :ids id, the cell it was last seen in (job memory :last-cells), for a drop that then goes out of
  sight (down a shaft)."
  [c items]
  (when (:ids (:args c))
    (ctx/update-mem! c update :last-cells #(into (or % {}) (keep (fn [e] (when (.-pos e) [(.-id e) (item-cell e)]))) items))))

(defn feet-y [c] (js/Math.floor (:y (u/self-pos c))))

(defn below-feet? [c item] (boolean (and (.-pos item) (< (second (item-cell item)) (feet-y c)))))

(defn at-hole?
  "Whether item lies at or under a cell of :holes, in its column or one of the eight beside (a drop that fell through the
  hole lands a column over), at any height: a drop still falling out of the dug cell is not yet lower than the feet."
  [c item]
  (let [[x y z] (when (.-pos item) (item-cell item))]
    (boolean (and x
                  (some #(let [{hx :x hy :y hz :z} (a/cell %)]
                           (and (<= (abs (- x (js/Math.floor hx))) 1) (<= (abs (- z (js/Math.floor hz))) 1) (<= y (js/Math.floor hy))))
                        (:holes (:args c)))))))

(defn in-hole?
  "Whether item is at a hole (at-hole?) and lower than the feet."
  [c item]
  (and (below-feet? c item) (at-hole? c item)))

(defn deep? [c item] (boolean (and (.-pos item) (> (- (feet-y c) (second (item-cell item))) 1))))

(defn max-settle-waits [] (settings/get settings ::max-settle-waits))

(defn nothing-under?
  "Whether the cell under item is air or not seen: a drop resting on a seen solid cell is not falling."
  [c item]
  (let [[x y z] (item-cell item)
        under (u/seen-name (:primitives c) {:x x :y (dec y) :z z})]
    (or (nil? under) (boolean (bs/air under)))))

(defn settling?
  "Whether a hole drop is still moving: its cell differs from the one noted for its id in job memory (noted now, at most
  ::max-settle-waits times); judged where it comes to rest, not mid-fall."
  [c item]
  (let [id (.-id item)
        cell (item-cell item)
        {:keys [cell-seen n] :or {n 0}} (get-in (ctx/mem c) [:settle id])
        moving? (and (not= cell cell-seen) (< n (max-settle-waits)))]
    (when moving?
      (ctx/update-mem! c assoc-in [:settle id] {:cell-seen cell :n (inc n)}))
    moving?))

(defn hole-problem
  "Why the body must not walk down to item, in a dug hole: :too-deep (more than one block down), :unseen-floor (the
  floor under it not seen), :unseen-cell (its cell, the head cell over it or a cell beside not seen) or :unsafe-floor
  (no seen solid floor to stand on, a hazard at feet or head, or a fluid beside); nil when it may."
  [c item]
  (let [p (:primitives c)
        [x y z] (item-cell item)
        at (fn [dx dy dz] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
        sides (map #(apply at %) [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])]
    (if (> (- (feet-y c) y) 1)
      :too-deep
      (let [unseen (filterv #(dig-look/unknown? p (u/cell-vec %)) (concat [(at 0 -1 0) (at 0 0 0) (at 0 1 0)] sides))]
        (cond
          (some #{(at 0 -1 0)} unseen) :unseen-floor
          (seq unseen) :unseen-cell
          (or (not (reach/standable-cell? p (at 0 0 0) (reach/lookup p)))
              (some #(bs/harms-in-cell (u/seen-name p %)) [(at 0 0 0) (at 0 1 0)])
              (some #(bs/fluids (u/seen-name p %)) (concat [(at 0 0 0) (at 0 -1 0)] sides)))
          :unsafe-floor)))))

(defn ^:async hole-problem!
  "hole-problem of item, decided again after a look at each cell it found unseen (jobs.lib.sense/decide!)."
  [c item]
  (await (sense/decide! c :collect-drops.hole #(hole-problem c item) {:retry? some?})))

(defn high? [c item] (boolean (and (.-pos item) (> (- (second (item-cell item)) (feet-y c)) 1))))

(defn far?
  "Whether item lies beyond the act's last step (over 2 blocks off) and not above the feet: its walk goes through go-to, which prices the route."
  [c item]
  (boolean (and (.-pos item) (not (high? c item)) (> (u/dist (sh-sense/feet (:primitives c)) (u/pos-of (.-pos item))) 2))))

(defn gap-on-way?
  "Whether the act's own walk to item (it plans on the raw world, unpriced) would cross a drop: from the feet column to
  the one before the item's, along the straight line, a column with no seen solid block at the feet height or the two
  under it. Such a walk goes through go-to, which prices the fall."
  [c item]
  (let [p (:primitives c)
        {fx :x fy :y fz :z} (u/self-pos c)
        [ix _ iz] (item-cell item)
        {px :x pz :z} (u/pos-of (.-pos item))
        y (js/Math.floor fy)
        n (js/Math.ceil (* 4 (js/Math.hypot (- px fx) (- pz fz))))
        cols (distinct (for [i (range (inc n))
                             :let [t (/ i (max n 1))]]
                         [(js/Math.floor (+ fx (* t (- px fx)))) (js/Math.floor (+ fz (* t (- pz fz))))]))]
    (boolean (some (fn [[x z]] (not-any? #(sh-sense/solid-at? p {:x x :y % :z z}) [y (dec y) (- y 2)]))
                   (remove #{[ix iz]} cols)))))

(defn ^:async footing!
  "A body whose centre overhangs a drop (held up by the edge of a side block, reach/standing-cell) first steps onto
  the middle of that block's cell: the act's walk starts from the cell under the centre and drops the body. true when
  the centre is over a floor (now or after the step)."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        {:keys [x y z] :as stand} (reach/standing-cell p)]
    (or (= stand feet)
        (sh-sense/solid-at? p (update feet :y dec))
        ;; raw moveTo kept: a step within the cell the body already stands on (go-to counts it there already)
        (do (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0.15}))
            (sh-sense/solid-at? p (update (sh-sense/feet p) :y dec))))))

(defn drops-worth
  "Seconds the drops in sight round the anchor are worth (jobs.lib.cost/item-value): what a walk to them may risk."
  [c]
  (:value (cost/item-value (for [e (candidates c (or (:anchor (ctx/mem c)) (:near (:args c)) (u/self-pos c)))
                                 :let [{:keys [name count]} (js->clj (.-item e) :keywordize-keys true)]]
                             {:name name :count (or count 1)}))))

(defn ^:async walk-down!
  "Walk within a block of item, more than a block lower than the feet (or, with ways, higher: a column top is climbed by
  go-to's pillar or stair): one go-to call (child/run!*, slot :walk). :arrived, :continue (go-to waits on the world) or :unreachable."
  ([c item] (walk-down! c item #{:walk :clear-seal}))
  ([c item ways]
  (let [r (await (child/run!* c :walk 'jobs.movement.go-to {:pos (item-cell item) :range 1 :ways ways :worth (drops-worth c)}))]
    (cond
      (= :continue r) :continue
      (and (= :done (:kind r)) (:arrived (:data r))) :arrived
      :else :unreachable))))

(defn pillar-height
  "How many blocks to pillar to bring the feet level with item: it lies above the feet by 2 to 6 and within 1 cell sideways (a drop is picked up from a block away); else nil."
  [c item]
  (let [[x y z] (item-cell item)
        [fx fy fz] (u/cell-vec (u/self-pos c))
        dy (- y fy)]
    (when (and (<= 2 dy 6) (<= (max (abs (- x fx)) (abs (- z fz))) 1))
      dy)))

(defn ^:async pillar-to!
  "A drop in a niche with nothing to stand on beside it (go-to answers :unreachable): the body stacks carried blocks under itself until its feet are level with it, as a player does. One pillar child (slot :pillar, carried blocks only). :arrived, :continue while it waits, else nil."
  [c item]
  (when-let [h (pillar-height c item)]
    (let [r (await (child/run!* c :pillar 'jobs.access.pillar {:height h :ways #{:carried}}))]
      (cond (= :continue r) :continue
            (= :done (:kind r)) :arrived))))

(defn open-above?
  "Whether the nearest seen cell above cell in its column (within 8) is open."
  [block-at [x y z]]
  (boolean (some-> (first (keep #(block-at [x % z]) (range (inc y) (+ y 9)))) dig-route/open?)))

(defn drop-route
  "dig-route/route from a feet cell to the cell of a drop lying at goal; block-at answers a cell's name or nil when never
  seen; a cell never seen is taken for stone (a rim sees no floor down a dug column), except under a seen open cell: it may
  be air of any depth (a trench), so no step lands on it. A drop at rest has a solid floor, seen or not. With an owner (a job
  id) a search that spends its nodes is kept and gone on at the next call: {:problem :searching} till it ends."
  ([block-at from goal] (drop-route nil block-at from goal))
  ([owner block-at from [gx gy gz :as goal]]
   ((if owner (partial dig-route/route-resumed [owner :drop-route from goal]) dig-route/route)
    (fn [cell]
      (or (block-at cell)
          (when (or (= cell [gx (dec gy) gz]) (not (open-above? block-at cell))) "stone")))
    from goal
    {:max-digs (settings/get settings ::dig-max-cells) :margin (settings/get settings ::dig-margin)
     :max-fall (settings/get settings ::dig-max-fall)})))

(defn ^:async look-pit!
  "A route over cells never seen takes them for stone, so a drop in a pit is dug to round the pit: first a look at the unsensed
  cells of the box between the feet and the drop, nearest to the drop first (jobs.lib.dig-look), as a player would lean over
  the edge."
  [c from [gx gy gz :as goal]]
  (let [[fx fy fz] from
        rng (fn [a b] (range (min a b) (inc (max a b))))
        cells (for [x (rng fx gx) z (rng fz gz) y (range (dec gy) (inc fy))] [x y z])]
    (await (dig-look/look-unknown! c (sort-by #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal)) cells)))))

(defn replans [] (settings/get settings ::replans))

(defn cause-reason [e] (when (map? e) (:reason (:cause e))))

(defn ^:async dig-cells!
  "One dig child per cell of cells, in order: :dug when all are open, else {:out outcome :cell cell :n cells dug before it}."
  [c cells]
  (loop [cells cells n 0]
    (if-let [cell (first cells)]
      (let [e (await (blocks/dig-cell-ending! c cell {:need-drop true}))
            o (if (= :unseen (cause-reason e)) :unseen (blocks/dig-outcome e))]
        (if (#{:dug :missing} o)
          (recur (rest cells) (inc n))
          {:out o :cell cell :n n}))
      :dug)))

(defn ^:async light-here!
  "A dig stopped on a cell it could not see: a body carrying torches sets one in its own cell, once per call (a player lights the
  dark before looking down it), so the cell can be looked at. :continue while the place child waits, :lit after it ended, nil when not due."
  [c]
  (when (and (not (:lit (ctx/mem c))) (pos? (torch/torches-held (:primitives c))))
    (let [o (await (blocks/place-cell! c (zipmap [:x :y :z] (u/cell-vec (u/self-pos c))) "torch" {}))]
      (when-not (= :continue o) (ctx/update-mem! c assoc :lit true))
      (if (= :continue o) :continue :lit))))

(defn ^:async dig-to!
  "Dig the route to item (jobs.lib.dig-route). A dig that fails after others of its route were dug (a cell beyond the light was
  never seen) is planned again from where the body now stands, at most ::replans times. :dug when every cell is open now,
  :continue while a dig waits on the world, else why not: :no-route or the failed dig's outcome (:refused, :cannot, :unreachable ...)."
  [c item]
  (let [p (:primitives c)]
    (loop [pass 0]
      (let [from (u/cell-vec (u/self-pos c))
            _ (await (look-pit! c from (item-cell item)))
            r (drop-route (:id c) (fn [[x y z]] (u/seen-name p {:x x :y y :z z})) from (item-cell item))]
        (case (:problem r)
          :searching :continue
          :no-route :no-route
          nil
          (let [d (await (dig-cells! c (:dig r)))]
            (cond
              (= :dug d) :dug
              (and (pos? (:n d)) (< pass (replans)) (not= :continue (:out d)))
              (do (await (dig-look/look-at! c (:cell d))) (recur (inc pass)))
              (= :unseen (:out d))
              (let [l (await (light-here! c))]
                (if l
                  (if (= :continue l) :continue (do (await (look-pit! c from (item-cell item))) (recur pass)))
                  :unseen))
              :else (:out d))))))))

(defn moved-item
  "item as seen now when it lies in another cell than the one it was judged in (a drop slides while the body digs to it), else nil."
  [c item]
  (let [now (first (filter #(= (.-id item) (.-id %)) (look/seen-items (:primitives c) {:radius (radius-of c) :max 64})))]
    (when (and now (.-pos now) (not= (item-cell now) (item-cell item))) now)))

(defn leave!
  "Leave item where it lies: why is the hole-problem. Its id is remembered (loose/left!), so the :loose-items reflex does not fetch it after."
  [c item why]
  (let [[x y z] (item-cell item)
        cell {:x x :y y :z z}]
    (ctx/emit! c :collect-drops.left :debug {:pos cell :reason why :item (some-> (.-item item) .-name)
                                             :text (str "left a drop at " x "," y "," z ": " (name why))})
    (loose/left! c [(.-id item)] (loose/options {}))
    (ctx/update-mem! c update :left (fnil conj []) {:pos cell :reason why})))

(defn leave-unseen!
  "The wanted :ids that were neither picked up nor skipped are not in sight (a pit, a wall): booked as one :left entry."
  [c]
  (let [gone (set (:skipped (ctx/mem c)))
        ids (vec (remove gone (:ids (:args c))))
        lasts (select-keys (:last-cells (ctx/mem c)) ids)]
    (when (seq ids)
      (ctx/emit! c :collect-drops.left :debug {:reason :unseen :ids ids :text (str "left " (count ids) " drops not in sight")})
      (ctx/update-mem! c update :left (fnil conj [])
                       (cond-> {:reason :unseen :ids ids}
                         (seq lasts) (assoc :last lasts))))))

(defn unseen-open?
  "Whether cell may be stood on though some of its feet, head or floor are unseen: every seen one is as a standable cell needs
  it, at least one is unseen. Walking there is how the body gets to look."
  [p {:keys [x y z]}]
  (let [[floor feet head] (map #(u/seen-block p {:x x :y % :z z}) [(dec y) y (inc y)])
        free? #(or (nil? %) (identical? :open (reach/kind-of %)))]
    (and (not (and floor feet head))
         (or (nil? floor) (identical? :solid (reach/kind-of floor)))
         (free? feet) (free? head))))

(defn walled-off?
  "Whether a seen opaque solid block (not leaves, which can be looked through) stands on the line from the anchor to the cell at d steps
  (dx, dz) away, at feet or head height: what lies behind it is rock, not somewhere to walk to and look."
  [p [ax ay az] d dx dz]
  (some (fn [[k dy]]
          (let [b (u/seen-block p {:x (+ ax (* k dx)) :y (+ ay dy) :z (+ az (* k dz))})]
            (and b (= :solid (reach/kind-of b)) (not (str/ends-with? (.-name b) "_leaves")))))
        (for [k (range 1 d) dy [0 1]] [k dy])))

(defn search-cells
  "Cells round the anchor at 1, 2 and :search blocks (1: the hole a dig left in a leaf wall, where the body steps in to look) in the eight directions, nearest to the body first: per column the
  standable one at the anchor's height or one above or below, else (nothing seen there, a wall in the way) the anchor's height
  when what is seen of it allows standing (unseen-open?)."
  [c anchor]
  (let [p (:primitives c)
        {:keys [search]} (:args c)
        [ax ay az] (u/cell-vec anchor)
        dists (distinct [(min 1 search) (min 2 search) search])
        cells (for [d dists dx [-1 0 1] dz [-1 0 1] :when (not (and (zero? dx) (zero? dz)))
                    dy [0 1 -1]
                    :let [cell {:x (+ ax (* d dx)) :y (+ ay dy) :z (+ az (* d dz))}]
                    :when (reach/seen-standable? p cell)]
                cell)
        seen-columns (set (map (juxt :x :z) cells))
        unseen (for [d dists dx [-1 0 1] dz [-1 0 1] :when (not (and (zero? dx) (zero? dz)))
                     :let [cell {:x (+ ax (* d dx)) :y ay :z (+ az (* d dz))}]
                     :when (and (not (seen-columns [(:x cell) (:z cell)])) (not (walled-off? p [ax ay az] d dx dz)) (unseen-open? p cell))]
                 cell)
        by-column (vals (group-by (juxt :x :z) (concat cells unseen)))]
    (->> by-column (map first) (sort-by #(u/dist (u/self-pos c) %)))))

(defn wanted-count
  "How many of the :least-items came in: what the acts booked, or more when the inventory gained more of them since the job
  began (a drop that flies to the body while it searches)."
  [c]
  (let [m (ctx/mem c)]
    (max (:wanted m 0)
         (reduce + 0 (for [[n start] (:start-held m)] (max 0 (- (held-count c n) start)))))))

(defn least
  "The call's :least as a number (:all: no number is enough, the search tries every cell)."
  [c]
  (let [l (or (:least (ctx/frozen-call c)) 1)]
    (if (= :all l) ##Inf l)))

(defn search-ways
  "go-to ways of a search walk: with :dig a body shut in (its own seal or shelter, a closed room) may dig out, priced by go-to's escalation."
  [c]
  (cond-> #{:walk :clear-seal}
    (:dig (:args c)) (into [:clear-path :stair :dig-out])))

(defn ^:async search-step!
  "Walks to the next unvisited search cell (one go-to call, slot :search; a cell go-to cannot reach is dropped).
  :continue while go-to waits on the world, :waited after search-waits calls in a row did, :walked after a cell, nil when none is left.
  Once the queue is spent the cell nearest the anchor is visited again: a drop is sensed late (pickup delay) and may only show from there."
  [c anchor]
  (let [m (ctx/mem c)
        seed! (fn [q] (ctx/update-mem! c assoc :search-queue q) q)
        queue (cond
                (nil? (:search-queue m)) (seed! (vec (search-cells c anchor)))
                (and (empty? (:search-queue m)) (not (:search-revisited m)))
                (do (ctx/update-mem! c assoc :search-revisited true)
                    (seed! (vec (take 1 (sort-by #(u/dist (u/cell-pos (u/cell-vec anchor)) %) (search-cells c anchor))))))
                :else (:search-queue m))]
    (when-let [cell (first queue)]
      (let [r (await (child/run!* c :search 'jobs.movement.go-to {:pos (u/cell-vec cell) :range (if (reach/seen-standable? (:primitives c) cell) 0 1) :ways (search-ways c)}))]
        (if (= :continue r)
          (if (>= (:search-waited (ctx/mem c) 0) (search-waits))
            :waited
            (do (ctx/update-mem! c update :search-waited (fnil inc 0))
                :continue))
          (do (ctx/update-mem! c #(-> % (update :search-queue (comp vec rest)) (dissoc :search-waited)))
              :walked))))))

(defn expecting?
  "Whether the call's :expect is not met and sweeps are left to wait for the rest."
  [c]
  (let [m (ctx/mem c)]
    (and (< (:collected m 0) (:expect (:args c))) (< (:expect-tries m 0) (settings/get settings ::expect-rounds)))))

(defn done?
  "Ok once no wanted item lies in sight within :radius of the anchor and none was left unseen, the :search (when one is
  owed) is over and the sweep took something or skipped nothing; else later."
  [c]
  (let [m (ctx/mem c)
        n (:collected m 0)
        anchor (or (:anchor m) (:near (:args c)) (u/self-pos c))
        in-sight (count (candidates c anchor))
        waiting (cond
                  (pos? in-sight) :collecting
                  (seq (:left m)) :left
                  (seq (remove (set (:skipped m)) (:ids (:args c)))) :ids-unseen
                  (and (pos? (:search (:args c))) (< (wanted-count c) (least c))) :searching
                  (expecting? c) :expecting
                  (and (zero? n) (or (seq (:skipped m)) (pos? (:search (:args c))) (:near (:args c)))) :skipped
                  (and (nil? (:ids (:args c))) (not (look/surveyed? c :cell))) :looking)]
    (if waiting
      (contract/later waiting (str "collect-drops: " n " collected, " in-sight " in sight") :units :items :done n :left in-sight :result {:collected n})
      (contract/ok :collected (str "collect-drops: " n " collected, no drop in sight") :units :items :done n :left 0 :result {:collected n}))))

(defn quote-of
  "What is in sight within :radius of the anchor: yields by name, time to walk to each (unbounded when a :search or a :dig may go on)."
  [c]
  (let [{:keys [near search dig]} (:args c)
        items (candidates c (or near (u/self-pos c)))
        n (count items)
        mean (* (settings/get settings ::walk-s-per-block)
                (reduce + 0 (map #(u/dist (u/self-pos c) (u/pos-of (.-pos %))) (filter #(.-pos %) items))))
        names (frequencies (keep #(some-> (.-item %) .-name) items))]
    {:uses {:job-time [0 mean (if (or (pos? search) dig) ##Inf (* 3 mean))]}
     :yields {:items (update-vals names (fn [k] [0 k k]))}
     :done [0 n (if (or (pos? search) dig) ##Inf n)]}))

(defn cleanup
  "However the job ends: the :collecting hold on its area is released."
  [c _]
  (loose/release! c)
  (ctx/end c :done nil))

(defn collected-count
  "Items that entered the inventory: booked by the acts, or the :least-items gained since the start when more (wanted-count)."
  [c]
  (max (:collected (ctx/mem c) 0) (wanted-count c)))

(defn data-of [c]
  (cond-> {:collected (collected-count c)}
    (seq (:left (ctx/mem c))) (assoc :left (:left (ctx/mem c)))))

(defn none-text
  "What the sweep saw at the named place when nothing came in."
  [c]
  (let [m (ctx/mem c)]
    (str "collect-drops: nothing collected at " (pr-str (:near (:args c))) ": "
         (if (or (seq (:left m)) (seq (:skipped m)))
           "items were there but out of reach"
           "no items in sight there (taken or despawned)"))))

(defn ^:async swept!
  "The ending of a sweep with nothing left to try: done, or nothing-possible when nothing came in after items were skipped, a search ran or a :near place was named. Fewer collected than :expect waits and sweeps again (:again)."
  [c]
  (let [m (ctx/mem c)]
    (cond
      (expecting? c)
      (do (await (ctx/act c :wait #js {:ms (settings/get settings ::expect-wait-ms)}))
          (ctx/update-mem! c update :expect-tries (fnil inc 0))
          :again)
      (and (zero? (collected-count c)) (or (seq (:skipped m)) (pos? (:search (:args c))) (:near (:args c))))
      (ctx/end c :nothing-possible :none-collected (cond-> (data-of c) (:near (:args c)) (assoc :text (none-text c))))
      :else (ctx/end c :done nil (data-of c)))))

(defn ^:async search-or-end!
  "No drop in sight: one search step (:continue while go-to waits, :again after a cell), else the end of the sweep."
  [c anchor]
  (let [r (await (search-step! c anchor))]
    (cond
      (= :continue r) :continue
      (and r (not= :waited r)) :again
      (= :waited r) (ctx/end c :nothing-possible :search-waited (data-of c))
      :else (await (swept! c)))))

(defn held-count [c item-name]
  (transduce (comp (filter #(= item-name (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn water-danger?
  "Whether item lies in water with a known hostile (jobs.lib.danger/known-hostiles) within the hostile radius of it: a player does not swim to it."
  [c item]
  (let [p (:primitives c)
        [x y z] (item-cell item)
        spot (u/pos-of (.-pos item))]
    (and (= "water" (u/seen-name p {:x x :y y :z z}))
         (boolean (some #(<= (u/dist spot (danger-q/mob-pos p %)) (danger-q/hostile-radius))
                        (danger-q/known-hostiles p (+ (danger-q/ranged-radius) (danger-q/hostile-radius)) {}))))))

(defn ^:async try-take!
  "One try at a drop: walk, dig or collect as its place asks (a stuck collect digs and tries once more). {:wait true} while a child
  waits on the world, else {:acts [act results] :left why-or-nil :retry? bool}."
  [c item]
  (let [item-name (some-> (.-item item) .-name)
        hole? (in-hole? c item)
        moving? (and (at-hole? c item) (nothing-under? c item) (settling? c item))
        why (when-not (await (footing! c)) :unsafe-floor)
        why (or why (when (and hole? (not moving?)) (await (hole-problem! c item))))
        walk (when (and (not why) (not hole?) (or (deep? c item) (far? c item) (and (not (high? c item)) (gap-on-way? c item))))
               (await (walk-down! c item)))
        dig? (and (:dig (:args c)) (or (= :unreachable walk) (= :too-deep why)))
        dug (when dig? (await (dig-to! c item)))
        dig-why (when-not (#{nil :dug :continue} dug) dug)
        walk (if (= :dug dug) (await (walk-down! c item)) walk)
        why (if (= :dug dug) nil why)
        collect! (fn ^:async collect* []
                   (let [before (held-count c item-name)
                         r (await (ctx/act c :collect (clj->js {:id (.-id item) :maxDropDown 1})))
                         got (- (held-count c item-name) before)]
                     ;; the act can answer unreachable for a drop that flew to the body meanwhile
                     (if (and (= "unreachable" (.-status r)) (pos? got))
                       (clj->js {:status "ok" :gained [{:name item-name :count got}]})
                       r)))
        r (when-not (or moving? (= :continue walk) (= :continue dug) why (= :unreachable walk)) (await (collect!)))
        stuck? (and r (= "unreachable" (.-status r)))
        up? (and stuck? (high? c item))
        walked-up (when up? (await (walk-down! c item #{:walk :pillar :stair :clear-seal})))
        up (if (= :unreachable walked-up) (or (await (pillar-to! c item)) walked-up) walked-up)
        near-walk (when (and stuck? (not up?) (nil? walk)) (await (walk-down! c item)))
        dug2 (when (and stuck? (not up?) (not= :arrived near-walk) (:dig (:args c))) (await (dig-to! c item)))
        dig2-why (when-not (#{nil :dug :continue} dug2) dug2)
        walk2 (when (= :dug dug2) (await (walk-down! c item)))
        r2 (when (or (= :arrived up) (= :arrived near-walk) (and (= :dug dug2) (not= :continue walk2))) (await (collect!)))
        r2-stuck? (and r2 (= "unreachable" (.-status r2)))]
    (if (or moving? (= :continue walk) (= :continue dug) (= :continue dug2) (= :continue walk2) (= :continue up) (= :continue near-walk))
      {:wait true}
      {:acts (remove nil? [r r2])
       :left (cond
               dig-why dig-why
               why why
               (= :unreachable walk) :unreachable
               dig2-why dig2-why
               (and stuck? (not= :dug dug2) (not= :arrived up) (not= :arrived near-walk)) :unreachable
               r2-stuck? :unreachable)
       :retry? (and (not why) (< (get-in (ctx/mem c) [:moves (.-id item)] 0) (max-moves))
                    (or dig-why (= :unreachable walk) dig2-why stuck? r2-stuck?)
                    (not (and r2 (not r2-stuck?)))
                    (boolean (moved-item c item)))})))

(defn book!
  "Counts what a try brought in (also what the walk down picked up before the act ran) and skips the drop, or lets it be tried again."
  [c item start-held {:keys [acts left retry?]}]
  (let [item-name (some-> (.-item item) .-name)
        least-items (some-> (:least-items (:args c)) set)
        extra (max 0 (- (held-count c item-name) start-held (reduce + 0 (map gained-count acts))))
        wants? (or (nil? least-items) (least-items item-name))]
    (when (and left (not retry?)) (leave! c item left))
    (ctx/update-mem! c (fn [m] (reduce (fn [m r] (-> m (update :collected (fnil + 0) (gained-count r))
                                                    (update :wanted (fnil + 0) (gained-count r least-items))))
                                       (cond-> (update m :collected (fnil + 0) extra)
                                         wants? (update :wanted (fnil + 0) extra))
                                       acts)))
    (if retry?
      (ctx/update-mem! c update-in [:moves (.-id item)] (fnil inc 0))
      (ctx/update-mem! c update :skipped (fnil conj []) (.-id item)))))

(defn ^:async collect-item!
  "One drop: try to take it, then book what came in and skip it; :continue while a child waits on the world, else :again."
  [c item]
  (let [start-held (held-count c (some-> (.-item item) .-name))
        t (if (water-danger? c item) {:left :water-danger} (await (try-take! c item)))]
    (if (:wait t)
      :continue
      (do (book! c item start-held t) :again))))

(defn ^:async look-at-holes!
  "Once per run and hole of :holes, glance at the dug cell and the cell under it, then down the column at each unsensed cell,
  ending on the dug cell again: a drop beside it is outside the view cone of a head turned steeply down. Returns the
  candidates the looks showed."
  [c anchor]
  (let [looked (:holes-looked (ctx/mem c))]
    (loop [holes (remove (set looked) (:holes (:args c)))]
      (when-let [dug (first holes)]
        (ctx/update-mem! c update :holes-looked (fnil conj []) dug)
        (await (dig-look/look-at! c dug))
        (await (dig-look/look-at! c (update dug 1 dec)))
        (await (dig-look/look-unknown! c (map #(update dug 1 - %) [2 3 4])))
        (await (dig-look/look-at! c dug))
        (or (seq (candidates c anchor)) (recur (rest holes)))))))

(defn ^:async look-at-near!
  "With :near: look at that spot; a spot over a block above the feet that shows no drop is walked up to (go-to :pillar/:stair), as a
  player climbs to a pile it knows is there, and looked at again. The items seen, :continue while go-to waits, else nil."
  [c anchor]
  (when-let [near (:near (:args c))]
    (let [look! (fn ^:async look* [] (await (dig-look/look-at! c (u/cell-vec near)))
                  (look/see! c (look/glance-effort) :look-around look/hazards-only)
                  (seq (candidates c anchor)))]
      (or (await (look!))
          (when (> (- (second (u/cell-vec near)) (feet-y c)) 1)
            (let [r (await (child/run!* c :walk 'jobs.movement.go-to {:pos (u/cell-vec near) :range 1 :ways #{:walk :pillar :stair :clear-seal}}))]
              (cond
                (= :continue r) :continue
                (and (= :done (:kind r)) (:arrived (:data r))) (await (look!)))))))))

(defn ^:async step!
  "One step of the sweep: the nearest drop in sight, else a search cell, else the end. :again, :continue or an ending."
  [c anchor]
  (if-not (ctx/alive? c)
    :continue
    (let [items (or (seq (candidates c anchor))
                    (when (:look-holes? (:args c)) (await (look-at-holes! c anchor)))
                    (await (look-at-near! c anchor))
                    (when (nil? (:ids (:args c))) (await (look/survey-until! c :cell #(seq (candidates c anchor)) (look/glance-effort) look/hazards-only))))
          _ (when (sequential? items) (note-last-cells! c items))
          item (when (sequential? items) (first items))]
      (cond
        (= :continue items) :continue
        item (await (collect-item! c item))
        (and (pos? (:search (:args c))) (< (wanted-count c) (least c)) (empty? (:left (ctx/mem c))))
        (await (search-or-end! c anchor))
        :else (do (leave-unseen! c) (await (swept! c)))))))

(defn ^:async round
  "One whole attempt: collects the nearest matching dropped item again and again until none is left in radius.
  Each item is tried once (picked up, gone, unreachable or not): its id is remembered in job memory and skipped,
  and :collected counts the items gained, also on a round that gave up.
  :ids limits it to those entity ids (a dig's own drops)."
  [c]
  (let [anchor (or (:anchor (ctx/mem c))
                   (let [a (or (:near (:args c)) (u/self-pos c))]
                     (ctx/update-mem! c assoc :anchor a :start-held (into {} (map (juxt identity #(held-count c %))) (:least-items (:args c))))
                     (loose/hold! c a (radius-of c))
                     a))]
    (await (pace/steps! c #(step! c anchor)))))

