(ns jobs.storage.make-room
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.choose :as choose]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as lb]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.shelter :as sh]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.pace :as pace]
            [jobs.lib.child :as child]
            [jobs.lib.storage :as storage]
            [jobs.lib.foods :as foods]))

(def doc
  "Make room in a nearly full inventory (the inventory-nearly-full reflex) until :free slots are free. One round is
  the whole attempt: it re-reads the inventory before every step and puts away, swaps or tosses until enough is free
  or nothing more may go. It never yields: the walk away (the cleanup export) is waited out inside it.
  What is thrown, like a player: names with no keep first (plain junk blocks such as granite, tuff and gravel first, whole
  big stacks before partial, then the rest by worth: flowers, saplings, leaf litter), then the surplus above the
  keep of cobblestone, dirt and planks; ores, fuel and the like are not thrown (below :toss-below only). Never put away or thrown: tools, weapons, armour and buckets. Food is never thrown and is put away only above
  :keep-food (best food-points first; the body's 3-day food reserve when nil). Golden apples are never put away.
  Cobblestone, dirt and each kind of planks (what shelter, mend, furnace and tool jobs spend) are each kept up to
  :keep-blocks; seeds up to :keep-seeds each; other blocks are not kept.
  Steps, in order:
  1. A chest known within :chest-range (and not marked :chest-unusable) takes what jobs.storage.deposit may put
     away, least worth keeping first: names without a floor before food and building blocks, then the cheapest,
     then the one picked up longest ago. A chest that fails (full, gone, unreachable, refused) is remembered as
     :chest-unusable for ten minutes and the job goes on without it.
  2. With no slot free and an item lying within :swap-radius that is worth more than the cheapest throwable
     stack, that stack is thrown away from the item and the item is collected.
  3. Without a usable chest, the stack of least worth is thrown. A stack qualifies when item-worth is
     below :toss-below, and it is thrown whole and only while the name's floor stays carried. Cheapest first,
     then the one picked up longest ago (:picked-up entries), then the smaller stack. The body turns to the first
     of the four directions with two free cells ahead at eye level and tosses; with none (walled in) nothing is
     thrown or swapped, since it would come straight back.
  After tossing, the cleanup export (however the call ends) walks up to :away blocks from where the items were thrown (a go-to child; the farthest cell back
  that is no seen obstacle, none: no walk), so it does not pick them up again.
  Resume: a toss or swap writes its intent first; a run that finds one (after a cut or a restart) checks the
  inventory before acting again. What was tossed before a cut stays in the result when the job survives the cut (a
  child); the spot to walk away from is body memory, so a cut reflex's refire still walks away from it.
  A done run whose tossed items the body carries again after the walk away has the cleanup ending :taken-back.
  Needs: nothing; a bag with :free slots free is done at once.
  Ends:
  - done {:tossed [{:item :count}] :free :text} once :free slots are free;
  - nothing-possible :short (it acted, nothing more may go) or :nothing-to-go (it may put away or toss nothing),
    or :toss-failed after three failed tosses in a row (data :status); data :free, :tossed and :text. The report counts the free slots.
  Also emits info make-room.tossed (with :junk) and make-room.swapped.")

(a/defargs args
  {:free {:doc "done once at least this many slots are free (above the trigger's 2, so it does not refire at once)" :spec (a/int-in 0 nil) :default 4}
   :chest-range {:doc "the known :chest place is used only within this distance" :spec (a/num-in 0 nil) :default 32}
   :keep-food {:doc "food items kept carried (best food by points first); the body's food reserve (jobs.lib.cost/food-reserve) when nil" :spec (a/int-in 0 nil) :default nil}
   :keep-blocks {:doc "cobblestone, dirt and each kind of planks kept carried, each up to this many" :spec (a/int-in 0 nil) :default 64}
   :keep-seeds {:doc "each kind of seeds kept carried up to this many (a farming body sows them)" :spec (a/int-in 0 nil) :default 16}
   :toss-below {:doc "a stack is tossed to make room only when its item-worth (jobs.lib.cost/item-value per item, seconds of work) is below this: dirt, cobblestone, rotten flesh and seeds go; ores, fuel, tools and food stay" :spec (a/num-in 0 nil) :default 3.2}
   :swap-radius {:doc "when no slot is free, a dropped item worth more than some carried stack within this radius is swapped in" :spec (a/num-in 0 nil) :default 8}
   :away {:doc "after tossing, walk this far away from where the items were thrown" :spec (a/num-in 0 nil) :default 4}
   :ignore-zones? :shared})

(def unusable-policy {:cap 5 :ttl 600000})

(def tossed-policy
  "The spot of a toss, in body memory: a cut reflex loses its job memory, the refire still walks away from it."
  {:cap 1 :ttl 120000})

(def rest-policy
  "When make-room last tossed and which item kinds: the inventory-nearly-full trigger rests (its :rest-s) after it, and jobs.lib.loose leaves those kinds."
  {:cap 8 :ttl 3600000})

(defn item-worth
  "The worth of one item of an inventory entry ({:name :count? ...}): its jobs.lib.cost/item-value per item."
  [item]
  (/ (:value (cost/item-value [item])) (max 1 (or (:count item) 1))))

(def tool-like #{"bucket" "water_bucket" "lava_bucket"})

(def junk-blocks
  "Plain blocks a player throws first when the bag is full."
  #{"dirt" "coarse_dirt" "cobblestone" "cobbled_deepslate" "deepslate" "stone" "granite" "diorite" "andesite"
    "tuff" "gravel" "netherrack" "sand" "red_sand" "calcite" "dripstone_block"})

(defn stack-worth
  "item-worth, but 0 for a plain junk block: crafted-only ones (cobbled_deepslate) have no source in the item base and
  would price above the toss line."
  [item]
  (if (contains? junk-blocks (:name item)) 0 (item-worth item)))

(def cardinals [[1 0] [-1 0] [0 1] [0 -1]])

;; ------------------------------------------------------------------ pure

(defn protected?
  "Never put away, never thrown: tools, weapons, armour, the buckets, torches and golden apples."
  [name]
  (boolean (or (storage/tool? name) (tool-like name) (= "torch" name) (contains? foods/precious name))))

(defn totals
  "{name carried} over all stacks."
  [inventory]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inventory))

;; what shelter, mend, furnace and tool jobs spend
(def shelter-blocks
  (into #{"dirt" "cobblestone"} (filter #(str/ends-with? % "_planks")) (choose/fillers :build)))

(defn shared-floors
  "Floors for names, in priority order, that share one budget: each keeps
  what it carries, up to what is left of the budget."
  [names totals budget]
  (first (reduce (fn [[floors left] n]
                   (let [k (min (get totals n 0) left)]
                     [(assoc floors n k) (- left k)]))
                 [{} budget]
                 names)))

(defn keep-counts
  "{name n}: how many of each carried name stay carried. Protected names keep
  their whole total; food names share keep-food, best food-points first;
  shelter-blocks keep up to keep-blocks each, seeds up to keep-seeds each; the rest 0."
  [inventory {:keys [keep-food keep-blocks keep-seeds]}]
  (let [totals (totals inventory)
        names (keys totals)
        food (->> names
                  (filter #(and (not (protected? %)) (foods/edible? %)))
                  (sort-by (juxt #(- (foods/points %)) identity)))
        food-floors (if keep-food
                      (shared-floors food totals keep-food)
                      (select-keys (cost/food-reserve inventory) food))
        capped (fn [pred n] (into {} (for [[k v] totals :when (pred k)] [k (min v (or n 0))])))]
    (merge (zipmap names (repeat 0))
           (select-keys totals (filter protected? names))
           food-floors
           (capped shelter-blocks keep-blocks)
           (capped #(str/ends-with? % "_seeds") keep-seeds))))

(defn recency
  "{item-name t}: the time of the newest :picked-up entry per item."
  [view]
  (reduce (fn [m {:keys [t data]}] (update m (:item data) (fnil max 0) t))
          {}
          (mem/entries view :picked-up)))

(defn toss-order
  "The stacks that may be thrown, first to throw first: [{:name :count :slot
  :worth :junk}]: plain junk blocks (junk-blocks) first, the biggest stack
  first (whole stacks before partial), then by worth, then when the name was
  last picked up (never is 0, oldest), then count, then slot. A stack qualifies when its name is not
  protected or food, its worth is below max-worth, and its whole count fits in
  what the name carries above its keep, after the stacks of the name that come
  before it in the order: a toss frees a whole slot and never cuts into a floor."
  [inventory keep recency max-worth]
  (let [totals (totals inventory)
        budgets (into {} (map (fn [[n total]] [n (- total (get keep n 0))])) totals)
        candidates (->> inventory
                        (map #(assoc (select-keys % [:name :count :slot]) :worth (stack-worth %) :junk (contains? junk-blocks (:name %))))
                        (remove #(or (protected? (:name %)) (foods/edible? (:name %))))
                        (filter #(< (:worth %) max-worth))
                        (sort-by (fn [s] (let [kept (if (pos? (get keep (:name s) 0)) 1 0)]
                                           (if (:junk s)
                                             [kept 0 0 0 0 (- (:count s)) (:slot s)]
                                             [kept 1 (:worth s) 0 (get recency (:name s) 0) (:count s) (:slot s)])))))]
    (first (reduce (fn [[chosen left] s]
                     (if (<= (:count s) (left (:name s)))
                       [(conj chosen s) (update left (:name s) - (:count s))]
                       [chosen left]))
                   [[] budgets]
                   candidates))))

(defn deposit-names
  "Distinct carried names, not protected, whose total exceeds their keep, in
  the order to put them away: least worth keeping first. Names without a floor
  (keep 0) before names with one (food, building blocks), then the lowest
  item-worth among the name's stacks, then when the name was last
  picked up (recency {name t}; never is 0, oldest first), then its lowest slot
  (the stack's index when it has no :slot)."
  [inventory keep recency]
  (let [totals (totals inventory)
        stacks (map-indexed (fn [i s] (assoc s :slot (or (:slot s) i))) inventory)
        order-key (fn [[n ss]]
                    [(if (pos? (get keep n 0)) 1 0)
                     (apply min (map stack-worth ss))
                     (get recency n 0)
                     (apply min (map :slot ss))])]
    (->> stacks
         (remove #(protected? (:name %)))
         (filter #(> (totals (:name %)) (get keep (:name %) 0)))
         (group-by :name)
         (sort-by order-key)
         (mapv first))))

;; ------------------------------------------------------------------ world

(defn free-cell?
  "The two cells ahead of cell at eye level (feet y + 1) have been seen and are not solid: a toss into a cell never
  looked at may hit a wall."
  [p {:keys [x y z]} [dx dz]]
  (every? #(let [pos {:x (+ x (* % dx)) :y (+ y 1) :z (+ z (* % dz))}]
             (and (some? (u/seen-name p pos)) (not (sh/solid-at? p pos))))
          [1 2]))

(defn direction
  "The first cardinal [dx dz] with free cells ahead, else nil: a toss into a wall comes straight back. With away
  (a position) the cardinals are tried most opposite to it first."
  [p away]
  (let [feet (sh/feet p)
        order (if away
                (sort-by (fn [[dx dz]] (+ (* dx (- (:x away) (:x feet))) (* dz (- (:z away) (:z feet))))) cardinals)
                cardinals)]
    (first (filter #(free-cell? p feet %) order))))

(defn unusable?
  [c chest]
  (boolean (some #(= chest (:pos (:data %))) (ctx/entries c :chest-unusable))))

(defn usable-chest
  "The known chest when within range and not marked unusable, else nil."
  [c]
  (let [chest (mem/place (ctx/view c) :chest)]
    (when (and chest
               (<= (u/dist (u/self-pos c) chest) (:chest-range (:args c)))
               (not (unusable? c chest)))
      chest)))

(defn state
  "What the round decides on: inventory, floors and recency."
  [c]
  (let [inventory (u/inventory (:primitives c))]
    {:inventory inventory
     :keep (keep-counts inventory (:args c))
     :recency (recency (ctx/view c))}))

(defn ground-items
  "Item entities within radius, nearest first: [{:id :name :count :pos}]."
  [c radius]
  (->> (look/seen-items (:primitives c) {:radius radius :max 32})
       (keep (fn [e] (when-let [i (.-item e)]
                       {:id (.-id e) :name (.-name i) :count (.-count i) :pos (u/pos-of (.-pos e))})))))

(defn carried-of
  "Total carried of item."
  [p item]
  (get (totals (u/inventory p)) item 0))

(defn tossed!
  "Record a toss of n item from at, thrown along dir: in :tossed, and as the spot to walk away from (body memory)."
  [c item n at dir]
  (ctx/update-mem! c #(-> % (assoc :acted true)
                          (update :tossed (fnil conj []) {:item item :count n})))
  (ctx/remember! c :make-room-tossed {:at at :dir dir} tossed-policy))

(defn ^:async toss!
  "Turn to dir and throw stack, its intent written first (:tossing, with the item's carried total before). True when it
  went; a failed toss counts in :toss-fails."
  [c stack dir fields]
  (let [{:keys [x y z]} (u/self-pos c)
        at {:x x :y y :z z}
        [dx dz] dir]
    (ctx/update-mem! c assoc
                     :tossing {:item (:name stack) :count (:count stack) :before (carried-of (:primitives c) (:name stack))
                               :at at :dir dir})
    (await (ctx/act c :look (clj->js {:pos {:x (+ x (* 3 dx)) :y (+ y 1.5) :z (+ z (* 3 dz))}})))
    (let [r (await (ctx/act c :toss (clj->js {:item (:name stack) :count (:count stack) :slot (:slot stack)})))]
      (ctx/update-mem! c dissoc :tossing)
      (if (= "tossed" (.-status r))
        (do (ctx/update-mem! c dissoc :toss-fails)
            (tossed! c (:name stack) (:count stack) at dir)
            (ctx/emit! c :make-room.tossed :info
                       (merge {:item (:name stack) :count (:count stack) :worth (:worth stack) :junk (boolean (:junk stack))
                               :text (str "tossed " (:count stack) " " (:name stack))}
                              fields))
            true)
        (do (ctx/update-mem! c #(-> % (update :toss-fails (fnil inc 0)) (assoc :toss-status (.-status r))))
            false)))))

(defn ^:async pick-up-swap!
  "Collect the item a swap made room for (:swap in memory), then forget the swap."
  [c]
  (let [{:keys [id item worth]} (:swap (ctx/mem c))
        r (await (ctx/act c :collect (clj->js {:id id :timeoutS 10})))]
    (ctx/update-mem! c dissoc :swap)
    (ctx/emit! c :make-room.swapped :info {:item item :worth worth :status (.-status r)
                                           :text (str "swap for " item ": " (.-status r))})))

(defn ^:async settle-intents!
  "Intents left by a cut or a restart, inspected before acting again: a toss went out when the item's carried total is
  below what it was before; a swap picks up its item when a slot is free, else is dropped."
  [c]
  (let [p (:primitives c)
        {:keys [tossing swap]} (ctx/mem c)]
    (when tossing
      (ctx/update-mem! c dissoc :tossing)
      (when (< (carried-of p (:item tossing)) (:before tossing))
        (tossed! c (:item tossing) (:count tossing) (:at tossing) (:dir tossing))))
    (when swap
      (if (pos? (u/free-slots p))
        (await (pick-up-swap! c))
        (ctx/update-mem! c dissoc :swap)))))

(defn tossed-spot
  "{:at :dir} of the toss to walk away from, or nil."
  [c]
  (:data (ctx/latest c :make-room-tossed)))

(defn near-spot?
  "Whether the body is still within :away + 8 blocks (XZ) of the toss spot: a spot left far behind is stale."
  [c {:keys [at]}]
  (let [pos (.-pos (.self (:primitives c)))
        reach (+ 8 (:away (:args c)))]
    (<= (js/Math.hypot (- (.-x pos) (:x at)) (- (.-z pos) (:z at))) reach)))

(defn standable?
  "Whether the cell {:x :y :z} is seen standable: feet and head cells seen open, the floor seen solid. A cell the body
  has not seen is no goal (it may be rock, or a drop)."
  [p {:keys [x y z]}]
  (let [seen (fn [dy] (u/seen-name p {:x x :y (+ y dy) :z z}))
        open? (fn [n] (and n (rules/replaceable n) (not (rules/fluids n))))]
    (boolean (and (open? (seen 0)) (open? (seen 1))
                  (seen -1) (rules/solid-floor? (fn [_] (seen -1)) nil)))))

(defn away-goal
  "The cell to step to from the toss spot at: the farthest one up to away blocks back along dir that the body can stand
  on, else nil (a one-wide tunnel may end behind it)."
  [p {:keys [x y z]} away [dx dz]]
  (->> (range away 0 -1)
       (map (fn [d] {:x (- x (* d dx)) :y y :z (- z (* d dz))}))
       (filter #(standable? p %))
       first))

(defn ^:async walk-away!
  "Walk up to :away blocks back from spot, where the items were thrown (a go-to child), so they are not picked up
  again; no walk when no cell behind it can be stood on. Best effort: a walk that does not arrive is not retried. The
  walk is run to its end (child/run!*); the spot is forgotten whatever the result."
  [c {:keys [at dir]}]
  (let [goal (away-goal (:primitives c) at (:away (:args c)) dir)
        r (when goal
            (await (child/run!* c :away 'jobs.movement.go-to {:pos goal :range 1 :ways #{:walk :clear-seal} :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (ctx/forget-where! c :make-room-tossed (constantly true))
    r))

(defn ^:async put-away!
  "One call of the deposit child with names. Once it ends (all put away, or it gave up: that chest is remembered as
  unusable for ten minutes) the chest is done with for this run (run's :chest-done)."
  [c run chest names keep]
  (let [before (u/free-slots (:primitives c))
        e (await (ctx/call c :deposit 'jobs.storage.deposit
                           (merge (select-keys (:args c) [:ignore-zones? :free]) {:chest chest :items names :keep keep :make-chests? true :fetch-chests? false})))]
    (when (get-in e [:data :gave-up])
      (ctx/remember! c :chest-unusable {:pos chest :reason (contract/cause-key e)} unusable-policy))
    (when (> (u/free-slots (:primitives c)) before)
      (ctx/update-mem! c assoc :acted true))
    (when (not= :continue e)
      (swap! run assoc :chest-done true))
    (await (pace/pace!))))

(defn swap-target
  "{:item ground-item :stack stack-to-throw} for the nearest ground item worth
  more than a throwable stack, or nil."
  [c {:keys [inventory keep recency]}]
  (some (fn [g]
          (let [worth (stack-worth g)]
            (when-let [stack (first (toss-order inventory keep recency worth))]
              {:item (assoc g :worth worth) :stack stack})))
        (ground-items c (:swap-radius (:args c)))))

(defn ^:async swap-in!
  "Throw stack away from item (the swap intent written first), then collect item."
  [c {:keys [item stack]}]
  (ctx/update-mem! c assoc :swap {:id (:id item) :item (:name item) :worth (:worth item)})
  (if (await (toss! c stack (direction (:primitives c) (:pos item)) {:for (:name item)}))
    (await (pick-up-swap! c))
    (ctx/update-mem! c dissoc :swap)))

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::step-s {:default 3 :doc "Seconds a quote counts for one deposit call, swap or toss." :spec (a/num-in 0 nil)}})

(def target [:free])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn can-start?
  "Nothing to check: a bag with room is already done, one with nothing to spare ends nothing-possible after looking."
  [_]
  (checks/holds))

(defn slots-short
  "How many more slots must be free for :free."
  [c]
  (max 0 (- (:free (:args c)) (u/free-slots (:primitives c)))))

(defn done?
  "Done when :free slots are free; a run that acted still has its summary to do."
  [c]
  (let [left (slots-short c)
        free (u/free-slots (:primitives c))
        data [:units :slots :done free :left left :result {:tossed (vec (:tossed (ctx/mem c))) :free free}]]
    (cond
      (pos? left) (apply contract/later :short (str "make-room: " left " slot(s) short") data)
      (:acted (ctx/mem c)) (apply contract/later :wrapping-up "make-room: room made, the summary to do" data)
      :else (apply contract/ok :room (str free " free") data))))

(defn quote-of
  "Time for the steps the missing slots take (one each at best); frees up to the missing slots, none at worst."
  [c]
  (let [need (slots-short c)
        step (settings/get settings ::step-s)]
    {:uses {:job-time [0 (* need step) (settings/get settings ::job-time-s)]}
     :done [0 need need]}))

(defn tossed-summary
  "The done event's fields: what was tossed ([{:item :count}]) and a text saying so."
  [m free-now]
  (let [tossed (vec (:tossed m))]
    {:free free-now :tossed tossed
     :text (str free-now " free"
                (when (seq tossed) (str ", tossed " (str/join ", " (map #(str (:count %) " " (:item %)) tossed)))))}))

(def stop-texts
  {:short "nothing more may go"
   :nothing-to-go "nothing it may put away or toss"
   :toss-failed "three tosses failed"})

(defn ^:async finish-end!
  "The :done or :nothing-possible ending, its data the summary."
  [c reason]
  (let [m (ctx/mem c)
        free-now (u/free-slots (:primitives c))
        {:keys [tossed text]} (tossed-summary m free-now)]
    (when (seq tossed) (ctx/remember! c :make-room-rest {:items (vec (distinct (map :item tossed)))} rest-policy))
    (if (nil? reason)
      (ctx/end c :done nil {:tossed tossed :free free-now :text text})
      (ctx/end c :nothing-possible reason (cond-> {:text (str (stop-texts reason) "; " text) :free free-now :tossed tossed}
                                            (= :toss-failed reason) (assoc :status (:toss-status m)))))))

(defn ^:async cleanup
  "However the call ends: an intent a cut left is settled, then it walks away from a toss spot the body is still near
  (a spot left far behind is forgotten). Done, or nothing-possible :taken-back when a done run's tossed items are carried again
  and fewer than :free slots are free."
  [c ending]
  (await (settle-intents! c))
  (let [spot (tossed-spot c)]
    (if (and spot (near-spot? c spot))
      (await (walk-away! c spot))
      (ctx/forget-where! c :make-room-tossed (constantly true))))
  (if (and (= :done (:kind ending)) (seq (:tossed (ctx/mem c))) (< (u/free-slots (:primitives c)) (:free (:args c))))
    (ctx/end c :nothing-possible :taken-back nil)
    (ctx/end c :done nil)))

(defn ^:async step!
  "One unit of work on a fresh look at the inventory: a deposit child call, a swap or a toss (:again), or the end
  (:done, through finish-end!). run is {:chest-done}."
  [c run]
  (let [{:keys [free toss-below]} (:args c)
        p (:primitives c)
        m (ctx/mem c)
        free-now (u/free-slots p)
        {:keys [inventory keep recency] :as st} (state c)
        chest (when-not (:chest-done @run) (usable-chest c))
        names (deposit-names inventory keep recency)
        dir (direction p nil)
        swap (when (and dir (zero? free-now)) (swap-target c st))
        order (when dir (toss-order inventory keep recency toss-below))]
    (cond
      (>= free-now free) (await (finish-end! c nil))
      (>= (:toss-fails m 0) 3) (await (finish-end! c :toss-failed))
      (and chest (seq names)) (do (await (put-away! c run chest names keep)) :again)
      swap (do (await (swap-in! c swap)) :again)
      (seq order) (do (await (toss! c (first order) dir {})) :again)
      :else (await (finish-end! c (if (:acted m) :short :nothing-to-go))))))

(defn ^:async round
  "One whole attempt: settle intents left by a cut, then step! until it ends."
  [c]
  (await (settle-intents! c))
  (let [run (atom {:chest-done false})]
    (await (pace/steps! c #(step! c run)))))
