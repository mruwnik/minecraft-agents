(ns jobs.storage.make-room
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]
            [engine.memory :as mem]
            [engine.value :as value]
            [jobs.storage.deposit :as deposit]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.eat :as eat]))

(def doc
  "Make room in a nearly full inventory (the reflex of inventory-nearly-full),
  one step per round, until :free slots are free. Each round asks first
  whether anything is left to do and ends :done when not. Tools, weapons,
  armour and buckets are never put away or thrown away. Food is never thrown
  away and is put away only above :keep-food (best food-points first);
  building blocks (the dig-in list, in its order) only above :keep-blocks.
  Steps, in order: a chest known within :chest-range (and not marked
  :chest-unusable) takes what deposit may put away (jobs.storage.deposit with
  :keep), least worth keeping first: names without a floor before food and
  building blocks, then the cheapest, then the one picked up longest ago; a chest that fails is remembered as :chest-unusable for ten minutes
  and the job goes on without it. Without a usable chest the stack of least
  worth is thrown: engine.value/item-worth below :toss-below, cheapest first,
  the one picked up longest ago first among equals (:picked-up entries), then
  the smaller stack; a stack is thrown only whole and only while the name's
  floor stays carried. The body turns to the first of the four directions with
  two free cells ahead at eye level, looks there and tosses. After a toss,
  once enough slots are free (or nothing is left to throw) it walks :away
  blocks back from where the items were thrown, so it does not pick them up
  again. When no slot is free and an item lies within :swap-radius that is
  worth more than the cheapest throwable stack, that stack is thrown away from
  the item and the item is collected. Emits info make-room.tossed, .swapped,
  .done, .declined; gives up :declined after :max-rounds rounds with a warn
  make-room.stalled, and :declined when there is nothing it may toss. Three
  failed tosses end it with a make-room.toss-failed warn.")

(def args
  {:free {:doc "done once at least this many slots are free (above the trigger's 2, so it does not refire at once)" :default 4}
   :chest-range {:doc "the known :chest place is used only within this distance" :default 32}
   :keep-food {:doc "food items kept carried (best food by points first)" :default 16}
   :keep-blocks {:doc "building blocks kept carried (dig-in's list, in its order)" :default 64}
   :toss-below {:doc "a stack is tossed to make room only when its engine.value/item-worth is below this" :default 1}
   :swap-radius {:doc "when no slot is free, a dropped item worth more than some carried stack within this radius is swapped in" :default 8}
   :away {:doc "after tossing, walk this far away from where the items were thrown" :default 4}
   :max-rounds {:doc "safety: give up (:declined, make-room.stalled) after this many rounds" :default 40}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def unusable-policy {:cap 5 :ttl 600000})

(def tool-like #{"bucket" "water_bucket" "lava_bucket"})

(def cardinals [[1 0] [-1 0] [0 1] [0 -1]])

;; ------------------------------------------------------------------ pure

(defn protected?
  "Never put away, never thrown: tools, weapons, armour and the buckets."
  [name]
  (boolean (or (deposit/tool? name) (tool-like name))))

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
                  (filter #(and (not (protected? %)) (eat/edible %)))
                  (sort-by (juxt #(- (eat/food-points %)) identity)))
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
  :worth}] sorted by worth, then when the name was last picked up (never is
  0, oldest), then count, then slot. A stack qualifies when its name is not
  protected or food, its worth is below max-worth, and its whole count fits in
  what the name carries above its keep, after the stacks of the name that come
  before it in the order: a toss frees a whole slot and never cuts into a floor."
  [inventory keep recency max-worth]
  (let [totals (totals inventory)
        budgets (into {} (map (fn [[n total]] [n (- total (get keep n 0))])) totals)
        candidates (->> inventory
                        (map #(assoc (select-keys % [:name :count :slot]) :worth (value/item-worth %)))
                        (remove #(or (protected? (:name %)) (eat/edible (:name %))))
                        (filter #(< (:worth %) max-worth))
                        (sort-by (juxt :worth #(get recency (:name %) 0) :count :slot)))]
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
  engine.value/item-worth among the name's stacks, then when the name was last
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
  (->> (array-seq (.entities (:primitives c) #js {:radius radius :kind "item" :max 32}))
       (keep (fn [e] (when-let [i (.-item e)]
                       {:id (.-id e) :name (.-name i) :count (.-count i) :pos (u/pos-of (.-pos e))})))))

(defn ^:async toss!
  "Turn to dir, throw stack, remember where. Resolves to :continue (or :done
  when three tosses failed)."
  [c stack dir fields]
  (let [p (:primitives c)
        {:keys [x y z]} (u/self-pos c)
        [dx dz] dir
        _ (await (ctx/act c :look (clj->js {:pos {:x (+ x (* 3 dx)) :y (+ y 1.5) :z (+ z (* 3 dz))}})))
        r (await (ctx/act c :toss (clj->js {:item (:name stack) :count (:count stack) :slot (:slot stack)})))]
    (if (not= "tossed" (.-status r))
      (u/fail! c :make-room.toss-failed (str "toss " (:name stack) ": " (.-status r)))
      (do (ctx/update-mem! c assoc :tossed-at {:x x :y y :z z} :toss-dir dir :walked false :acted true)
          (ctx/emit! c :make-room.tossed :info
                     (merge {:item (:name stack) :count (:count stack) :worth (:worth stack)
                             :text (str "tossed " (:count stack) " " (:name stack))}
                            fields))
          :continue))))

(defn ^:async walk-away!
  "Walk :away blocks back from where the items were thrown."
  [c]
  (let [{:keys [tossed-at toss-dir]} (ctx/mem c)
        away (:away (:args c))
        [dx dz] toss-dir
        _ (await (ctx/act c :moveTo (clj->js {:pos {:x (- (:x tossed-at) (* away dx)) :y (:y tossed-at) :z (- (:z tossed-at) (* away dz))}
                                              :range 1 :timeoutS 10})))]
    (ctx/update-mem! c assoc :walked true)
    :continue))

(defn pending-walk? [c]
  (let [m (ctx/mem c)] (boolean (and (:tossed-at m) (not (:walked m))))))

(defn ^:async pick-up-swap!
  [c]
  (let [{:keys [swap-id swap-item swap-worth]} (ctx/mem c)
        r (await (ctx/act c :collect (clj->js {:id swap-id :timeoutS 10})))]
    (ctx/update-mem! c dissoc :swap-id :swap-item :swap-worth)
    (ctx/emit! c :make-room.swapped :info {:item swap-item :worth swap-worth :status (.-status r)
                                           :text (str "swap for " swap-item ": " (.-status r))})
    :continue))

(defn ^:async deposit!
  [c chest names keep]
  (ctx/update-mem! c assoc :acted true)
  (let [r (await (ctx/call-child c :deposit 'jobs.storage.deposit
                                   (merge (select-keys (:args c) [:ignore-zones?]) {:chest chest :items names :keep keep})))
        result (when (= :done r) (ctx/child-result c :deposit))]
    (when (:gave-up result)
      (ctx/remember! c :chest-unusable {:pos chest :reason (:reason result)} unusable-policy))
    :continue))

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
  [c {:keys [item stack]}]
  (let [away (:pos item)
        r (await (toss! c stack (direction (:primitives c) away) {:for (:name item)}))]
    (when (= :continue r)
      (ctx/update-mem! c assoc :swap-id (:id item) :swap-item (:name item) :swap-worth (:worth item)))
    r))

(defn check
  "Fewer than :free slots are free."
  [c]
  (< (u/free-slots (:primitives c)) (:free (:args c))))

(defn ^:async round
  [c]
  (let [{:keys [free max-rounds toss-below]} (:args c)
        rounds (inc (:rounds (ctx/mem c) 0))
        _ (ctx/update-mem! c assoc :rounds rounds)
        m (ctx/mem c)
        free-now (u/free-slots (:primitives c))
        {:keys [inventory keep recency] :as st} (state c)
        chest (usable-chest c)
        names (deposit-names inventory keep recency)
        order (toss-order inventory keep recency toss-below)
        swap (when (zero? free-now) (swap-target c st))]
    (cond
      (> rounds max-rounds)
      (do (ctx/emit! c :make-room.stalled :warn {:rounds rounds :free free-now :text (str "still short of room after " max-rounds " rounds")})
          :declined)

      (:swap-id m) (await (pick-up-swap! c))

      (and (>= free-now free) (pending-walk? c)) (await (walk-away! c))

      (>= free-now free)
      (do (when (:acted m)
            (ctx/emit! c :make-room.done :info {:free free-now :text (str "room made: " free-now " free")}))
          :done)

      (and chest (seq names)) (await (deposit! c chest names keep))

      swap (await (swap-in! c swap))

      (seq order) (await (toss! c (first order) (direction (:primitives c) nil) {}))

      (pending-walk? c) (await (walk-away! c))

      (:acted m)
      (do (ctx/emit! c :make-room.done :info {:free free-now :short true :text (str "short of room: " free-now " free, nothing more may be tossed")})
          :done)

      :else (do (ctx/emit! c :make-room.declined :info {:free free-now :reason "nothing-to-toss" :text "nothing it may toss"})
                :declined))))
