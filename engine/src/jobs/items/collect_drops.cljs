(ns jobs.items.collect-drops
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.child :as child]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.look :as look]
            [jobs.lib.loose :as loose]
            [jobs.lib.reach :as reach]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.trees :refer [default-radius]]))

(def doc
  "Collect the nearest matching dropped item, in one call, until none is left within :radius of where the job began (the work area: a walk to an item
  does not move it, so drops far away, e.g. other bodies', are never chased).
  An item is tried once; one that is gone, cannot be reached or picked up is skipped.
  An item more than a block lower than the feet is walked to first (a jobs.movement.go-to child, range 1, which prices
  falls and dangers); one it cannot reach is left, with a debug collect-drops.left {:reason :unreachable}.
  An item lower than the feet in a hole the caller dug (:holes) is walked to only when it lies one block down at most and
  the body has seen the floor under it solid and safe and the cells round it (no lava, fluid or cave opening, nothing
  hazardous beside it), the walk dropping one block at most; else it is left, with a debug collect-drops.left
  {:pos :reason :unseen-floor|:unseen-cell|:unsafe-floor|:too-deep}. A drop still falling is judged at rest.
  With :search, when fewer than the call's :least (default 1) items (those named in :least-items, else any) were collected and no item is in sight (trunks or terrain hide drops), the body walks (a go-to child)
  to standable cells round the anchor, nearest first (a cell whose floor is unseen counts when nothing seen forbids it: walking there is how it gets looked at), and looks again at each, until an item is seen or the cells within :search
  blocks are tried. Underground a walk goes on at night; a search that go-to made wait :search-waits calls in a row ends with :reason :search-waited.
  While it runs it holds its work area (jobs.lib.loose :collecting), so the :loose-items reflex leaves those drops to it.
  With :ids, ids not in sight when none is left to try are booked as one :left {:reason :unseen :ids [...] :last {id [x y z]}}, the cell each was last seen in.
  With :dig, a drop go-to cannot reach, or one too deep in the dug hole, is dug to once: the fewest cells (jobs.lib.dig-route,
  over the blocks the body has seen, an unseen one taken for stone) that open a way to it, each a jobs.blocks.dig child (zones, claims, hazards
  are its rules), then the walk is tried again. No such way leaves the drop :no-route, a dig that fails with that dig's outcome (:refused, :cannot, :unreachable ...).
  Needs nothing; the sweep is the whole job. Ends :done with data {:collected n}, the number of items that entered the
  inventory, with :left [{:pos :reason}] for those (also in :loose-left, so the :loose-items reflex leaves them); none in
  sight at all is :done with 0 (an open sweep). Drops tried, or a :search that found none, and none collected: :nothing-possible :none-collected, same data;
  :nothing-possible :search-waited when the search was held up; :cut-off :cut when cut.
  Quote: what is in sight within :radius (yields by name), time to walk to each, unbounded with :search or :dig (also the items it may yet find).")

(a/defargs args
  {:radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :items {:doc "item names to collect; everything when nil" :spec (a/coll-of a/item?) :default nil}
   :near {:doc "{:x :y :z} the work area is centred on, instead of where the body stands when the job begins" :spec ::a/pos :default nil}
   :search {:doc "blocks round the anchor to walk to and look from when no drop is in sight and fewer than the call's :least were collected; 0: no search" :spec (a/num-in 0 nil) :default 0}
   :least-items {:doc "item names that count towards the call's :least; every collected item when nil" :spec (a/coll-of a/item?) :default nil}
   :ids {:doc "entity ids to collect (only those); any item when nil" :spec (a/coll-of number?) :default nil}
   :dig {:doc "dig a way to a drop that cannot be walked to (the cells are left for the caller's tidy)" :spec boolean? :default false}
   :holes {:doc "cells the caller dug: an item lower than the feet at or under one of them, or one column beside, is in that hole"
           :spec (a/coll-of a/position?) :default nil}})

(def target [:ids :near])

(def defaults {:least 1})

(def safe? ready/safe-if-lit)

(def fit? ready/ok)

(a/defargs settings
  {::max-settle-waits {:default 10 :doc "Checks of a hole drop that still moves before it counts as settled." :spec (a/int-in 0 nil)}
   ::walk-s-per-block {:default 0.4 :doc "Seconds a quote counts to walk one block to a drop." :spec (a/num-in 0 nil)}
   ::dig-max-cells {:default 16 :doc "The most cells :dig digs to reach one drop; a dearer route leaves it." :spec (a/int-in 0 nil)}
   ::dig-margin {:default 3 :doc "How far outside the box round the body and the drop the dig route may wander, in blocks." :spec (a/int-in 0 nil)}
   ::dig-max-fall {:default 3 :doc "The longest fall, in blocks, the dig route plans." :spec (a/int-in 0 nil)}
   ::search-waits {:default 5 :doc "How many calls in a row a :search walk may wait on go-to (the night, a danger) before the search is given up." :spec (a/int-in 1 nil)}})

(defn search-waits [] (settings/get settings ::search-waits))

(defn walk-args
  "go-to args for a walk of this job: underground it ignores the dark (not (threatened) only), like the digs that made the hole."
  [c args]
  (let [forms (when (sh-sense/buried? (:primitives c)) (ready/unforced c {:safe? '(not (threatened))}))]
    (cond-> args (seq forms) (assoc :checks forms))))

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
  (let [{:keys [radius ids]} (:args c)
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

(def fluids #{"water" "lava" "bubble_column"})

(defn feet-y [c] (js/Math.floor (:y (u/self-pos c))))

(defn below-feet? [c item] (boolean (and (.-pos item) (< (second (item-cell item)) (feet-y c)))))

(defn in-hole?
  "Whether item lies lower than the feet at or under a cell of :holes, in its column or one of the eight beside (a drop
  that fell through the hole lands a column over)."
  [c item]
  (let [[x y z] (when (.-pos item) (item-cell item))]
    (boolean (and (below-feet? c item)
                  (some #(let [{hx :x hy :y hz :z} (a/cell %)]
                           (and (<= (abs (- x (js/Math.floor hx))) 1) (<= (abs (- z (js/Math.floor hz))) 1) (<= y (js/Math.floor hy))))
                        (:holes (:args c)))))))

(defn deep? [c item] (boolean (and (.-pos item) (> (- (feet-y c) (second (item-cell item))) 1))))

(defn max-settle-waits [] (settings/get settings ::max-settle-waits))

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
          (or (not (reach/standable-cell? p (at 0 0 0)))
              (some #(reach/hazard-blocks (u/seen-name p %)) [(at 0 0 0) (at 0 1 0)])
              (some #(fluids (u/seen-name p %)) (concat [(at 0 0 0) (at 0 -1 0)] sides)))
          :unsafe-floor)))))

(defn ^:async hole-problem!
  "hole-problem of item, decided again after a look at each cell it found unseen (jobs.lib.sense/decide!)."
  [c item]
  (await (sense/decide! c :collect-drops.hole #(hole-problem c item) {:retry? some?})))

(defn ^:async walk-down!
  "Walk within a block of item, more than a block lower than the feet: one go-to call (child/run!*, slot :walk).
  :arrived, :continue (go-to waits on the world) or :unreachable."
  [c item]
  (let [r (await (child/run!* c :walk 'jobs.movement.go-to (walk-args c {:pos (item-cell item) :range 1 :ways #{:walk :clear-seal}})))]
    (cond
      (= :continue r) :continue
      (and (= :done (:kind r)) (:arrived (:data r))) :arrived
      :else :unreachable)))

(defn drop-route
  "dig-route/route from a feet cell to the cell of a drop lying at goal; block-at answers a cell's name or nil when never
  seen; a cell never seen is taken for stone, floors included (a rim sees no floor down a dug column). A drop at rest has a
  solid floor, seen or not."
  [block-at from [gx gy gz :as goal]]
  (dig-route/route (fn [cell] (or (block-at cell) "stone"))
                   from goal
                   {:max-digs (settings/get settings ::dig-max-cells) :margin (settings/get settings ::dig-margin)
                    :max-fall (settings/get settings ::dig-max-fall)}))

(defn ^:async dig-to!
  "Dig the route to item (jobs.lib.dig-route): one jobs.blocks.dig child per cell, in walking order. :dug when every
  cell is open now, :continue while a dig waits on the world, else why not: :no-route or the failed dig's outcome
  (:refused, :cannot, :unreachable ...)."
  [c item]
  (let [p (:primitives c)
        r (drop-route (fn [[x y z]] (u/seen-name p {:x x :y y :z z})) (u/cell-vec (u/self-pos c)) (item-cell item))]
    (if (:problem r)
      :no-route
      (loop [cells (:dig r)]
        (if-let [cell (first cells)]
          (let [o (await (blocks/dig-cell! c cell {:need-drop true}))]
            (cond
              (= :continue o) :continue
              (#{:dug :missing} o) (recur (rest cells))
              :else o))
          :dug)))))

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

(defn seen-standable?
  "Whether cell is standable (jobs.lib.reach/standable-cell?) by the blocks the body has seen: its feet, head and floor cells all seen."
  [p {:keys [x y z] :as cell}]
  (and (every? #(u/seen-block p {:x x :y % :z z}) [(dec y) y (inc y)])
       (reach/standable-cell? p cell (fn [x y z] (reach/kind-of (u/seen-block p {:x x :y y :z z}))))))

(defn unseen-open?
  "Whether cell may be stood on though some of its feet, head or floor are unseen: every seen one is as a standable cell needs
  it, at least one is unseen. Walking there is how the body gets to look."
  [p {:keys [x y z]}]
  (let [[floor feet head] (map #(u/seen-block p {:x x :y % :z z}) [(dec y) y (inc y)])
        free? #(or (nil? %) (identical? :open (reach/kind-of %)))]
    (and (not (and floor feet head))
         (or (nil? floor) (identical? :solid (reach/kind-of floor)))
         (free? feet) (free? head))))

(defn search-cells
  "Cells round the anchor at 2 blocks and at :search blocks in the eight directions, nearest to the body first: per column the
  standable one at the anchor's height or one above or below, else (nothing seen there, a wall in the way) the anchor's height
  when what is seen of it allows standing (unseen-open?)."
  [c anchor]
  (let [p (:primitives c)
        {:keys [search]} (:args c)
        [ax ay az] (u/cell-vec anchor)
        dists (distinct [(min 2 search) search])
        cells (for [d dists dx [-1 0 1] dz [-1 0 1] :when (not (and (zero? dx) (zero? dz)))
                    dy [0 1 -1]
                    :let [cell {:x (+ ax (* d dx)) :y (+ ay dy) :z (+ az (* d dz))}]
                    :when (seen-standable? p cell)]
                cell)
        seen-columns (set (map (juxt :x :z) cells))
        unseen (for [d dists dx [-1 0 1] dz [-1 0 1] :when (not (and (zero? dx) (zero? dz)))
                     :let [cell {:x (+ ax (* d dx)) :y ay :z (+ az (* d dz))}]
                     :when (and (not (seen-columns [(:x cell) (:z cell)])) (unseen-open? p cell))]
                 cell)
        by-column (vals (group-by (juxt :x :z) (concat cells unseen)))]
    (->> by-column (map first) (sort-by #(u/dist (u/self-pos c) %)))))

(defn wanted-count
  "How many of the :least-items came in."
  [c]
  (:wanted (ctx/mem c) 0))

(defn least
  "The call's :least as a number (:all: no number is enough, the search tries every cell)."
  [c]
  (let [l (or (:least (ctx/frozen-call c)) 1)]
    (if (= :all l) ##Inf l)))

(defn ^:async search-step!
  "Walks to the next unvisited search cell (one go-to call, slot :search; a cell go-to cannot reach is dropped).
  :continue while go-to waits on the world, :waited after search-waits calls in a row did, :walked after a cell, nil when none is left."
  [c anchor]
  (let [queue (or (:search-queue (ctx/mem c))
                  (let [q (vec (search-cells c anchor))] (ctx/update-mem! c assoc :search-queue q) q))]
    (when-let [cell (first queue)]
      (let [r (await (child/run!* c :search 'jobs.movement.go-to (walk-args c {:pos (u/cell-vec cell) :range (if (seen-standable? (:primitives c) cell) 0 1) :ways #{:walk :clear-seal}})))]
        (if (= :continue r)
          (if (>= (:search-waited (ctx/mem c) 0) (search-waits))
            :waited
            (do (ctx/update-mem! c update :search-waited (fnil inc 0))
                :continue))
          (do (ctx/update-mem! c #(-> % (update :search-queue (comp vec rest)) (dissoc :search-waited)))
              :walked))))))

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
                  (and (zero? n) (or (seq (:skipped m)) (pos? (:search (:args c))))) :skipped)]
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

(defn ^:async round
  "One whole attempt: collects the nearest matching dropped item again and again until none is left in radius.
  Each item is tried once (picked up, gone, unreachable or not): its id is remembered in job memory and skipped,
  and :collected counts the items gained, also on a round that gave up.
  :ids limits it to those entity ids (a dig's own drops)."
  [c]
  (let [anchor (or (:anchor (ctx/mem c))
                   (let [a (or (:near (:args c)) (u/self-pos c))]
                     (ctx/update-mem! c assoc :anchor a)
                     (loose/hold! c a (:radius (:args c)))
                     a))
        done! (fn [kind cause data] (ctx/end c kind cause data))
        data (fn [] (cond-> {:collected (:collected (ctx/mem c) 0)}
                      (seq (:left (ctx/mem c))) (assoc :left (:left (ctx/mem c)))))
        swept! (fn []
                 (if (and (zero? (:collected (ctx/mem c) 0)) (or (seq (:skipped (ctx/mem c))) (pos? (:search (:args c)))))
                   (done! :nothing-possible :none-collected (data))
                   (done! :done nil (data))))]
    (loop []
      (let [items (when (ctx/alive? c) (candidates c anchor))
            _ (note-last-cells! c items)
            item (first items)]
        (if (and (not item) (ctx/alive? c) (pos? (:search (:args c)))
                 (< (wanted-count c) (least c)) (empty? (:left (ctx/mem c))))
          (let [r (await (search-step! c anchor))]
            (if (= :continue r)
              :continue
              (if (and r (not= :waited r))
                (recur)
                (cond
                  (not (ctx/alive? c)) (done! :cut-off :cut (data))
                  (= :waited r) (done! :nothing-possible :search-waited (data))
                  :else (swept!)))))
          (if-not item
          (do (when (ctx/alive? c) (leave-unseen! c))
              (if (ctx/alive? c) (swept!) (done! :cut-off :cut (data))))
          (let [item-name (some-> (.-item item) .-name)
                held (fn [] (transduce (comp (filter #(= item-name (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))
                start-held (held)
                hole? (in-hole? c item)
                moving? (and hole? (settling? c item))
                why (when (and hole? (not moving?)) (await (hole-problem! c item)))
                walk (when (and (not hole?) (deep? c item)) (await (walk-down! c item)))
                dig? (and (:dig (:args c)) (or (= :unreachable walk) (= :too-deep why)))
                dug (when dig? (await (dig-to! c item)))
                dig-why (when-not (#{nil :dug :continue} dug) dug)
                walk (if (= :dug dug) (await (walk-down! c item)) walk)
                why (if (= :dug dug) nil why)]
            (let [collect! (fn ^:async collect* []
                             (let [before (held)
                                   r (await (ctx/act c :collect (clj->js (cond-> {:id (.-id item)} hole? (assoc :maxDropDown 1)))))
                                   got (- (held) before)]
                               ;; the act can answer unreachable for a drop that flew to the body meanwhile
                               (if (and (= "unreachable" (.-status r)) (pos? got))
                                 (clj->js {:status "ok" :gained [{:name item-name :count got}]})
                                 r)))
                  r (when-not (or moving? (= :continue walk) (= :continue dug) why (= :unreachable walk)) (await (collect!)))
                  stuck? (and r (= "unreachable" (.-status r)))
                  dug2 (when (and stuck? (:dig (:args c))) (await (dig-to! c item)))
                  dig2-why (when-not (#{nil :dug :continue} dug2) dug2)
                  r2 (when (= :dug dug2) (await (collect!)))
                  r2-stuck? (and r2 (= "unreachable" (.-status r2)))]
              (if (or moving? (= :continue walk) (= :continue dug) (= :continue dug2))
                :continue
                (do (cond
                      dig-why (leave! c item dig-why)
                      why (leave! c item why)
                      (= :unreachable walk) (leave! c item :unreachable)
                      dig2-why (leave! c item dig2-why)
                      (and stuck? (not= :dug dug2)) (leave! c item :unreachable)
                      r2-stuck? (leave! c item :unreachable))
                    ;; the walk down can pick the drop up before the collect act runs
                    (let [acts (reduce + 0 (map gained-count (remove nil? [r r2])))
                          extra (max 0 (- (held) start-held acts))
                          wants? (or (nil? (:least-items (:args c))) ((set (:least-items (:args c))) item-name))]
                      (ctx/update-mem! c (fn [m] (reduce (fn [m r] (-> m (update :collected (fnil + 0) (gained-count r))
                                                                      (update :wanted (fnil + 0) (gained-count r (some-> (:least-items (:args c)) set)))))
                                                         (cond-> (update m :collected (fnil + 0) extra)
                                                           wants? (update :wanted (fnil + 0) extra))
                                                         (remove nil? [r r2])))))
                    (ctx/update-mem! c update :skipped (fnil conj []) (.-id item))
                    (recur)))))))))))
