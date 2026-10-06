(ns jobs.storage.make-room
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.worth :as value]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as res]
            [jobs.storage.deposit :as deposit]
            [jobs.survival.dig-in :as dig-in]
            [jobs.lib.foods :as foods]))

(def doc
  "Make room in a nearly full inventory (the inventory-nearly-full reflex) until :free slots are free. One round is
  the whole attempt: it re-reads the inventory before every step and puts away, swaps or tosses until enough is free
  or nothing more may go. It never yields (:continue).
  What is thrown, like a player: plain junk blocks first (junk-blocks: cobblestone, cobbled deepslate, granite,
  tuff, dirt, gravel...), whole big stacks before partial, then the rest by worth; ores, fuel and the like
  only after the junk. Never put away or thrown: tools, weapons, armour and buckets. Food is never thrown and is put away only above
  :keep-food (best food-points first). Building blocks (the dig-in list, in its order) are kept up to
  :keep-blocks.
  Steps, in order:
  1. A chest known within :chest-range (and not marked :chest-unusable) takes what jobs.storage.deposit may put
     away, least worth keeping first: names without a floor before food and building blocks, then the cheapest,
     then the one picked up longest ago. A chest that fails (full, gone, unreachable, refused) is remembered as
     :chest-unusable for ten minutes and the job goes on without it.
  2. With no slot free and an item lying within :swap-radius that is worth more than the cheapest throwable
     stack, that stack is thrown away from the item and the item is collected.
  3. Without a usable chest, the stack of least worth is thrown. A stack qualifies when jobs.lib.worth/item-worth is
     below :toss-below, and it is thrown whole and only while the name's floor stays carried. Cheapest first,
     then the one picked up longest ago (:picked-up entries), then the smaller stack. The body turns to the first
     of the four directions with two free cells ahead at eye level and tosses.
  After tossing it walks :away blocks from where the items were thrown (a go-to child), so it does not pick them up
  again.
  Resume: a toss or swap writes its intent first; a run that finds one (after a cut or a restart) checks the
  inventory before acting again. What was tossed before a cut stays in the result when the job survives the cut (a
  child); the spot to walk away from is body memory, so a cut reflex's refire still walks away from it.
  Ends:
  - done {:tossed [{:item :count}] :free} once :free slots are free (info make-room.done when it acted);
  - stopped :short (it acted, nothing more may go) or :nothing-to-go (it may put away or toss nothing), with
    :free and :tossed, info make-room.stopped;
  - stopped :stalled after :max-steps steps (warn make-room.stalled) or
    :toss-failed after three failed tosses in a row (warn make-room.toss-failed).
  Also emits info make-room.tossed (with :junk) and make-room.swapped.")

(def args
  {:free {:doc "done once at least this many slots are free (above the trigger's 2, so it does not refire at once)" :default 4}
   :chest-range {:doc "the known :chest place is used only within this distance" :default 32}
   :keep-food {:doc "food items kept carried (best food by points first)" :default 16}
   :keep-blocks {:doc "building blocks kept carried (dig-in's list, in its order)" :default 64}
   :toss-below {:doc "a stack is tossed to make room only when its jobs.lib.worth/item-worth is below this" :default 1}
   :swap-radius {:doc "when no slot is free, a dropped item worth more than some carried stack within this radius is swapped in" :default 8}
   :away {:doc "after tossing, walk this far away from where the items were thrown" :default 4}
   :max-steps {:doc "safety: stop (:stalled, warn make-room.stalled) after this many deposit calls, swaps and tosses in one run" :default 40}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def unusable-policy {:cap 5 :ttl 600000})

(def tossed-policy
  "The spot of a toss, in body memory: a cut reflex loses its job memory, the refire still walks away from it."
  {:cap 1 :ttl 120000})

(def tool-like #{"bucket" "water_bucket" "lava_bucket"})

(def junk-blocks
  "Plain blocks a player throws first when the bag is full."
  #{"dirt" "coarse_dirt" "cobblestone" "cobbled_deepslate" "deepslate" "stone" "granite" "diorite" "andesite"
    "tuff" "gravel" "netherrack" "sand" "red_sand" "calcite" "dripstone_block"})

(def cardinals [[1 0] [-1 0] [0 1] [0 -1]])

;; ------------------------------------------------------------------ pure

(defn protected?
  "Never put away, never thrown: tools, weapons, armour, the buckets and torches."
  [name]
  (boolean (or (deposit/tool? name) (tool-like name) (= "torch" name))))

(defn totals
  "{name carried} over all stacks."
  [inventory]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inventory))

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
  building blocks share keep-blocks in building-blocks order; the rest 0."
  [inventory {:keys [keep-food keep-blocks]}]
  (let [totals (totals inventory)
        names (keys totals)
        food (->> names
                  (filter #(and (not (protected? %)) (foods/edible? %)))
                  (sort-by (juxt #(- (foods/points %)) identity)))
        blocks (filter #(contains? totals %) dig-in/building-blocks)]
    (merge (zipmap names (repeat 0))
           (select-keys totals (filter protected? names))
           (shared-floors food totals keep-food)
           (shared-floors blocks totals keep-blocks))))

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
                        (map #(assoc (select-keys % [:name :count :slot]) :worth (value/item-worth %) :junk (contains? junk-blocks (:name %))))
                        (remove #(or (protected? (:name %)) (foods/edible? (:name %))))
                        (filter #(< (:worth %) max-worth))
                        (sort-by (fn [s] (if (:junk s)
                                           [0 0 (- (:count s)) 0 (:slot s)]
                                           [1 (:worth s) 0 (get recency (:name s) 0) (:count s) (:slot s)]))))]
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
  jobs.lib.worth/item-worth among the name's stacks, then when the name was last
  picked up (recency {name t}; never is 0, oldest first), then its lowest slot
  (the stack's index when it has no :slot)."
  [inventory keep recency]
  (let [totals (totals inventory)
        stacks (map-indexed (fn [i s] (assoc s :slot (or (:slot s) i))) inventory)
        order-key (fn [[n ss]]
                    [(if (pos? (get keep n 0)) 1 0)
                     (apply min (map value/item-worth ss))
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
  "The two cells ahead of cell at eye level (feet y + 1) are not solid."
  [p {:keys [x y z]} [dx dz]]
  (not-any? #(sh/solid-at? p {:x (+ x (* % dx)) :y (+ y 1) :z (+ z (* % dz))}) [1 2]))

(defn direction
  "The first cardinal [dx dz] with free cells ahead, else [1 0]. With away
  (a position) the cardinals are tried most opposite to it first."
  [p away]
  (let [feet (sh/feet p)
        order (if away
                (sort-by (fn [[dx dz]] (+ (* dx (- (:x away) (:x feet))) (* dz (- (:z away) (:z feet))))) cardinals)
                cardinals)]
    (or (first (filter #(free-cell? p feet %) order)) [1 0])))

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

(defn ^:async walk-away!
  "Walk :away blocks back from spot, where the items were thrown (a go-to child), so they are not picked up again. Best
  effort: a walk that does not arrive is not retried. A walk that yields (:continue) keeps the spot; returns the child's
  result."
  [c {:keys [at dir]}]
  (let [away (:away (:args c))
        [dx dz] dir
        goal {:x (- (:x at) (* away dx)) :y (:y at) :z (- (:z at) (* away dz))}]
    (let [r (await (ctx/call-child c :away 'jobs.movement.go-to {:pos goal :range 1 :escalate false}))]
      (when (not= :continue r)
        (ctx/forget-where! c :make-room-tossed (constantly true)))
      r)))

(defn ^:async put-away!
  "One call of the deposit child with names. Once it ends (all put away, or it gave up: that chest is remembered as
  unusable for ten minutes) the chest is done with for this run (run's :chest-done)."
  [c run chest names keep]
  (let [before (u/free-slots (:primitives c))
        r (await (ctx/call-child c :deposit 'jobs.storage.deposit
                                 (merge (select-keys (:args c) [:ignore-zones?]) {:chest chest :items names :keep keep})))
        result (when (= :done r) (ctx/child-result c :deposit))]
    (when (:gave-up result)
      (ctx/remember! c :chest-unusable {:pos chest :reason (:reason result)} unusable-policy))
    (when (> (u/free-slots (:primitives c)) before)
      (ctx/update-mem! c assoc :acted true))
    (when (not= :continue r)
      (swap! run assoc :chest-done true))
    (await (pace/pace!))))

(defn swap-target
  "{:item ground-item :stack stack-to-throw} for the nearest ground item worth
  more than a throwable stack, or nil."
  [c {:keys [inventory keep recency]}]
  (some (fn [g]
          (let [worth (value/item-worth g)]
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

(defn check
  "Fewer than :free slots are free, or the job has acted and has yet to end (a cut run resumes)."
  [c]
  (boolean (or (< (u/free-slots (:primitives c)) (:free (:args c)))
               (:acted (ctx/mem c)))))

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
   :stalled "still short of room after its :max-steps steps"
   :toss-failed "three tosses failed"})

(defn ^:async finish-end!
  "The summary event and the :done or :stopped result."
  [c reason]
  (let [m (ctx/mem c)
        free-now (u/free-slots (:primitives c))
        {:keys [tossed text] :as summary} (tossed-summary m free-now)]
    (if (nil? reason)
      (do (when (:acted m) (ctx/emit! c :make-room.done :info summary))
          (res/finish! c {:tossed tossed :free free-now}))
      (let [text (str (stop-texts reason) "; " text)]
        (if (#{:stalled :toss-failed} reason)
          (ctx/emit! c (keyword (str "make-room." (name reason))) :warn (assoc summary :text text :status (:toss-status m)))
          (ctx/emit! c :make-room.stopped :info (assoc summary :reason reason :text text)))
        (res/stop! c reason text :free free-now :tossed tossed)))))

(defn ^:async end!
  "Walk away from what was tossed, then end: done when :free slots are free (reason nil), else stopped with reason.
  :stalled and :toss-failed are warns (make-room.<reason>), the others info make-room.stopped."
  [c reason]
  (let [walked (when-let [spot (tossed-spot c)] (await (walk-away! c spot)))]
    (if (= :continue walked)
      :continue
      (await (finish-end! c reason)))))

(defn ^:async step!
  "One unit of work on a fresh look at the inventory: a deposit child call, a swap or a toss (:again), or the end
  (:done, through end!). run is {:steps :chest-done}."
  [c run]
  (let [{:keys [free max-steps toss-below]} (:args c)
        p (:primitives c)
        m (ctx/mem c)
        free-now (u/free-slots p)
        {:keys [inventory keep recency] :as st} (state c)
        chest (when-not (:chest-done @run) (usable-chest c))
        names (deposit-names inventory keep recency)
        swap (when (zero? free-now) (swap-target c st))
        order (toss-order inventory keep recency toss-below)]
    (swap! run update :steps inc)
    (cond
      (>= free-now free) (await (end! c nil))
      (>= (:toss-fails m 0) 3) (await (end! c :toss-failed))
      (> (:steps @run) max-steps) (await (end! c :stalled))
      (and chest (seq names)) (do (await (put-away! c run chest names keep)) :again)
      swap (do (await (swap-in! c swap)) :again)
      (seq order) (do (await (toss! c (first order) (direction p nil) {})) :again)
      :else (await (end! c (if (:acted m) :short :nothing-to-go))))))

(defn ^:async round
  "One whole attempt: settle intents left by a cut, then step! until it ends."
  [c]
  (await (settle-intents! c))
  (let [run (atom {:steps 0 :chest-done false})]
    (loop []
      (let [r (await (step! c run))]
        (if (= :again r) (recur) r)))))
