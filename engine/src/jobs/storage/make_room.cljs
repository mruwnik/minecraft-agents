(ns jobs.storage.make-room
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.storage :as storage]
            [jobs.lib.price :as price]
            [jobs.lib.cost :as cost]
            [jobs.lib.ready :as ready]
            [jobs.storage.make-room.order :as ord]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as lb]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.pace :as pace]
            [jobs.lib.child :as child]
            [jobs.lib.chests :as chests]
            [jobs.lib.walk-pick :as pick]))

(def doc
  "Make room in a nearly full inventory (the inventory-nearly-full reflex) until :free slots are free. One round is
  the whole attempt: it re-reads the inventory before every step and puts away, swaps or tosses until enough is free
  or nothing more may go. It never yields. Done as soon as :free slots are free (done? carries the summary); the cleanup walks away.
  What is thrown, like a player: names with no keep first, cheapest worth first (granite, tuff and gravel before flowers,
  saplings and leaf litter; worth is jobs.lib.cost item-value), then the surplus above the
  keep of cobblestone, dirt and planks; ores, fuel and the like are not thrown (below the toss-below setting only). Never put away or thrown: tools, weapons, armour and buckets. Food is never thrown and is put away only above
  :carry-food (best food-points first; the body's 3-day food reserve when nil). Golden apples are never put away.
  Cobblestone, dirt and each kind of planks (what shelter, mend, furnace and tool jobs spend) are each kept up to
  :carry-blocks; seeds up to :carry-seeds each; other blocks are not kept. Names in :carry-items are kept whole.
  Steps, in order:
  1. The storage chest whose walk go-to quotes cheapest (jobs.lib.walk-pick; not marked :chest-unusable), when that walk costs no more than the
     stacks it saves from the toss are worth (all of them when the toss cannot free enough), takes what jobs.storage.deposit may put
     away, least worth keeping first: names without a floor before food and building blocks, then the cheapest,
     then the one picked up longest ago. A chest that fails (full, gone, unreachable, refused) is remembered as
     :chest-unusable for the unusable-ttl-s setting (600) seconds and the job goes on without it.
  2. With no slot free and an item lying within the swap-radius setting (8) that is worth more than the cheapest throwable
     stack, that stack is thrown away from the item and the item is collected.
  3. Without a usable chest, the stack of least worth is thrown. A stack qualifies when item-worth is
     below the toss-below setting (3.2) (with :stuck?, when its whole worth is below the cost of a chest, which it keeps to put down), and it is thrown whole and only while the name's floor stays carried. Cheapest first,
     then the one picked up longest ago (:picked-up entries), then the smaller stack. The body turns to the first
     of the four directions with two free cells ahead at eye level and tosses; with none (walled in, after one look around) nothing is
     thrown or swapped, since it would come straight back.
  With :stuck? and nothing cheaper than a chest to toss, the dearer stacks go into a chest put down beside the body (deposit puts down a carried chest only; with none the call ends with its reason).
  After tossing, the cleanup export (however the call ends; it also notes the :make-room-rest memory the inventory trigger rests on) walks up to :away blocks from where the items were thrown (a go-to child; the farthest cell back
  that is no seen obstacle; none: a step to the back edge of the cell, out of the pickup reach of items stopped by a wall 1.5 ahead), so it does not pick them up again.
  Resume: a toss or swap writes its intent first; a run that finds one (after a cut or a restart) checks the
  inventory before acting again. What was tossed before a cut stays in the result when the job survives the cut (a
  child); the spot to walk away from is body memory, so a cut reflex's refire still walks away from it.
  A done run whose tossed items the body carries again after the walk away has the cleanup ending :taken-back.
  Needs: nothing; a bag with :free slots free is done at once.
  Ends:
  - done {:tossed [{:item :count}] :free :text} once :free slots are free;
  - nothing-possible :short (it acted, nothing more may go) or :nothing-to-go (it may put away or toss nothing),
    or :toss-failed after the toss-fails setting (3) failed tosses in a row (data :status); data :free, :tossed and :text. The report counts the free slots.
  Also emits info make-room.tossed and make-room.swapped.")

(a/defargs args
  {:free {:doc "done once at least this many slots are free (above the trigger's 2, so it does not refire at once)" :spec (a/int-in 0 nil) :default 4}
   :carry-food {:doc "food items kept carried (best food by points first); the body's food reserve (jobs.lib.cost/food-reserve) when nil" :spec (a/int-in 0 nil) :default nil}
   :carry-blocks {:doc "cobblestone, dirt and each kind of planks kept carried, each up to this many" :spec (a/int-in 0 nil) :default 64}
   :carry-items {:doc "item names kept carried whole, never put away or thrown (a caller's own inputs)" :spec (a/coll-of a/name?) :default []}
   :carry-seeds {:doc "each kind of seeds kept carried up to this many (a farming body sows them)" :spec (a/int-in 0 nil) :default 16}
   :toss-from {:doc "a position {:x :z}: toss away from it (a dig's caller gives a point ahead of the way it goes on, so it does not walk into what it threw)" :spec (a/pred "a position {:x :z}" #(and (map? %) (number? (:x %)) (number? (:z %)))) :default nil}
   :away {:doc "after tossing, walk this far away from where the items were thrown" :spec (a/num-in 0 nil) :default 4}
   :stuck? {:doc "a job cannot go on without a slot: a stack goes when its whole worth is below what a chest costs (the chest and walking to put it down), not by the toss-below line per item" :spec boolean? :default false}
   :ignore-zones? :shared})

(a/defargs settings
  {::toss-below {:default 3.2 :doc "A stack is tossed to make room only when its item-worth (jobs.lib.cost/item-value per item, seconds of work) is below this: dirt, cobblestone, rotten flesh and seeds go; ores, fuel, tools and food stay." :spec (a/num-in 0 nil)}
   ::toss-fails {:default 3 :doc "Failed tosses in a row before the call gives up." :spec (a/int-in 1 nil)}
   ::unusable-ttl-s {:default 600 :doc "Seconds a chest that failed is left alone." :spec (a/num-in 0 nil)}
   ::spot-slack {:default 8 :doc "A toss spot the body is farther than :away plus this from is stale, in blocks." :spec (a/num-in 0 nil)}
   ::swap-radius {:default 8 :doc "When no slot is free, a dropped item worth more than some carried stack within this radius is swapped in." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::step-s {:default 3 :doc "Seconds a quote counts for one deposit call, swap or toss." :spec (a/num-in 0 nil)}})
(defn chest-cost
  "Seconds the other way out costs when no chest is usable: a chest and the walk to put it down (the stuck? line)."
  []
  (+ (:value (cost/item-value [{:name "chest" :count 1}]))
     (:cost (cost/walk-cost {:distance (cost/own-place-blocks)}))))
(defn swap-radius [] (settings/get settings ::swap-radius))

(defn unusable-policy [] {:cap 5 :ttl (* 1000 (settings/get settings ::unusable-ttl-s))})

(def tossed-policy
  "The spot of a toss, in body memory: a cut reflex loses its job memory, the refire still walks away from it."
  {:cap 1 :ttl 120000})

(def rest-policy
  "When make-room last tossed and which item kinds: the inventory-nearly-full trigger rests (its :rest-s) after it, and jobs.lib.loose leaves those kinds."
  {:cap 8 :ttl 3600000})

(def cardinals [[1 0] [-1 0] [0 1] [0 -1]])

;; ------------------------------------------------------------------ world

(defn free-cell?
  "The first n cells ahead of cell at eye level (feet y + 1) have been seen and are not solid: a toss into a cell never
  looked at may hit a wall."
  [p {:keys [x y z]} [dx dz] n]
  (every? #(let [pos {:x (+ x (* % dx)) :y (+ y 1) :z (+ z (* % dz))}]
             (and (some? (u/seen-name p pos)) (not (sh-sense/solid-at? p pos))))
          (range 1 (inc n))))

(defn direction
  "The first cardinal [dx dz] with two free cells ahead, else with one (a stair or a bend in a tunnel has no more:
  the items still land clear of the feet), else nil: a toss into a wall comes straight back. With away (a position)
  the cardinals are tried most opposite to it first."
  [p away]
  (let [feet (sh-sense/feet p)
        order (if away
                (sort-by (fn [[dx dz]] (+ (* dx (- (:x away) (:x feet))) (* dz (- (:z away) (:z feet))))) cardinals)
                cardinals)]
    (some (fn [n] (first (filter #(free-cell? p feet % n) order))) [2 1])))

(defn ^:async direction!
  "direction, after one look around when none is free yet (once per run, tracked in run's :looked): cells never
  looked at, as just after arriving, are not walls."
  [c run]
  (let [p (:primitives c)]
    (or (direction p (:toss-from (:args c)))
        (when-not (:looked @run)
          (swap! run assoc :looked true)
          (await (look/glance! c (vals look/headings)))
          (direction p (:toss-from (:args c)))))))

(defn unusable?
  [c chest]
  (boolean (some #(= chest (:pos (:data %))) (ctx/entries c :chest-unusable))))

(defn toss-cost
  "Seconds of worth the toss throws away to free short slots: the whole stacks of the first short of order; ##Inf when
  order cannot free that many."
  [order short]
  (if (< (count order) short)
    ##Inf
    (reduce + (map #(* (:worth %) (:count %)) (take short order)))))

(defn ^:async usable-chest!
  "The body's storage chest (the :chest place and its group, not marked unusable) whose walk go-to quotes cheapest, when
  that walk costs no more than limit (what the alternative, tossing, throws away); else nil."
  [c limit]
  (let [r (await (pick/best! c (->> (storage/group (ctx/view c))
                                    (map #(zipmap [:x :y :z] %))
                                    (remove #(unusable? c %)))
                            {:bound #(pick/walk-s (u/self-pos c) (chests/walk-args c %))
                             :args #(chests/walk-args c %)
                             :price (fn [_ q] (pick/quoted-s c q))}))]
    (when (and r (<= (:price r) limit))
      (:pick r))))

(defn state
  "What the round decides on: inventory, floors and recency."
  [c]
  (let [inventory (u/inventory (:primitives c))]
    {:inventory inventory
     :keep (ord/keep-counts inventory (cond-> (:args c) (:stuck? (:args c)) (update :carry-items conj "chest")) (reserve/held c :make-room))
     :recency (ord/recency (ctx/view c))}))

(defn ground-items
  "Item entities within radius, nearest first: [{:id :name :count :pos}]."
  [c radius]
  (->> (look/seen-items (:primitives c) {:radius radius :max 32})
       (keep (fn [e] (when-let [i (.-item e)]
                       {:id (.-id e) :name (.-name i) :count (.-count i) :pos (u/pos-of (.-pos e))})))))

(defn carried-of
  "Total carried of item."
  [p item]
  (storage/carried (u/inventory p) item))

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
                       (merge {:item (:name stack) :count (:count stack) :worth (:worth stack)
                               :text (str "tossed " (:count stack) " " (:name stack))}
                              fields))
            true)
        (do (ctx/update-mem! c #(-> % (update :toss-fails (fnil inc 0)) (assoc :toss-status (.-status r))))
            false)))))

(defn ^:async pick-up-swap!
  "Collect the item a swap made room for (:swap in memory) with an items.pick-up-loose child, then forget the swap
  (kept while the child waits on the world, so the next round resumes it)."
  [c]
  (let [{:keys [item worth pos]} (:swap (ctx/mem c))
        _ (when pos ; the toss turned the body away from the item, and loose items are seen only inside the view cone
            (await (ctx/act c :look (clj->js {:pos pos})))
            (look/see! c))
        r (await (child/run!* c :collect 'jobs.items.pick-up-loose {}))]
    (when-not (= :continue r)
      (let [status (if (pos? (or (:collected (:data r)) 0)) "collected" "left")]
        (ctx/update-mem! c dissoc :swap)
        (ctx/emit! c :make-room.swapped :info {:item item :worth worth :status status
                                               :text (str "swap for " item ": " status)})))))

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
        reach (+ (settings/get settings ::spot-slack) (:away (:args c)))]
    (<= (js/Math.hypot (- (.-x pos) (:x at)) (- (.-z pos) (:z at))) reach)))

(defn standable?
  "Whether the cell {:x :y :z} is seen standable: feet and head cells seen open, the floor seen solid. A cell the body
  has not seen is no goal (it may be rock, or a drop)."
  [p {:keys [x y z]}]
  (let [seen (fn [dy] (u/seen-name p {:x x :y (+ y dy) :z z}))
        open? (fn [n] (and n (bs/replaceable n) (not (bs/fluids n))))]
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

(def edge-back
  "How far (blocks) the body steps to the back edge of its cell when it cannot walk away from its toss: a wall 1.5 ahead
  stops the items 1.375 from the cell centre, inside the 1.425 pickup reach; 1.555 is out of it."
  0.18)

(defn ^:async walk-away!
  "Walk up to :away blocks back from spot, where the items were thrown (a go-to child), so they are not picked up
  again; with no cell behind it to stand on, step to the back edge of the cell (edge-back). Best effort: a walk that
  does not arrive is not retried. The walk is run to its end (child/run!*); the spot is forgotten whatever the result."
  [c {:keys [at dir]}]
  (let [goal (away-goal (:primitives c) at (:away (:args c)) dir)
        [dx dz] dir
        r (if goal
            (await (child/run!* c :away 'jobs.movement.go-to {:pos goal :range 1 :ways #{:walk :clear-seal} :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
            (await (ctx/act c :moveTo (clj->js {:pos at :range 0 :offset {:dx (- (* edge-back dx)) :dz (- (* edge-back dz))}}))))]
    (ctx/forget-where! c :make-room-tossed (constantly true))
    r))

(declare slots-short)

(defn room-limit
  "{:names :keep} for the deposit child: names in order until the stacks they empty free `short` slots, and a keep on the
  last name that leaves its other stacks carried, so the chest gets no more than the room needs."
  [inventory names keep short]
  (loop [names names, left short, out [], keep keep]
    (if (or (empty? names) (<= left 0))
      {:names out :keep keep}
      (let [n (first names)
            counts (mapv :count (filter #(= n (:name %)) inventory))
            spare (- (reduce + counts) (get keep n 0))
            movable (count (take-while #(<= % spare) (reductions + counts)))]
        (if (<= movable left)
          (recur (rest names) (- left movable) (conj out n) keep)
          (recur nil 0 (conj out n) (assoc keep n (- (reduce + counts) (reduce + (take left counts))))))))))

(defn ^:async put-away!
  "One call of the deposit child with names. Once it ends (all put away, or it gave up: that chest is remembered as
  unusable for the unusable-ttl-s setting) the chest is done with for this run (run's :chest-done)."
  [c run chest names keep]
  (let [before (u/free-slots (:primitives c))
        room (room-limit (u/inventory (:primitives c)) names keep (slots-short c))
        e (await (ctx/call c :deposit 'jobs.storage.deposit
                           (cond-> (merge (select-keys (:args c) [:ignore-zones?]) {:items (:names room) :carry (:keep room) :make-chests? true :ways #{:carried}})
                                  chest (assoc :chest chest))))]
    (when (and chest (get-in e [:data :gave-up]))
      (ctx/remember! c :chest-unusable {:pos chest :reason (contract/cause-key e)} (unusable-policy)))
    (when (> (u/free-slots (:primitives c)) before)
      (ctx/update-mem! c assoc :acted true))
    (when (not= :continue e)
      (swap! run assoc :chest-done true))
    (await (pace/pace! c))))

(defn swap-target
  "{:item ground-item :stack stack-to-throw} for the nearest ground item worth
  more than a throwable stack, or nil."
  [c {:keys [inventory keep recency]}]
  (some (fn [g]
          (let [worth (ord/item-worth g)]
            (when-let [stack (first (ord/toss-order inventory keep recency worth))]
              {:item (assoc g :worth worth) :stack stack})))
        (ground-items c (swap-radius))))

(defn ^:async seek-swap!
  "Once per run, a look round for a swap target: ground items count only in the view cone."
  [c run st]
  (when-not (:swap-looked @run)
    (swap! run assoc :swap-looked true)
    (await (look/glance-until! c (vals look/headings) #(swap-target c st)))))

(defn ^:async swap-in!
  "Throw stack away from item (the swap intent written first), then collect item."
  [c {:keys [item stack]}]
  (ctx/update-mem! c assoc :swap {:id (:id item) :item (:name item) :worth (:worth item) :pos (:pos item)})
  (if (await (toss! c stack (direction (:primitives c) (:pos item)) {:for (:name item)}))
    (await (pick-up-swap! c))
    (ctx/update-mem! c dissoc :swap)))

(def target [:free])

(def meta {:category :reflex})

(def safe? (price/staying :work ready/safe-here-by-night))
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

(declare tossed-summary)

(defn done?
  "Done when :free slots are free; the result is the summary (what was tossed), so the walk away is left to the cleanup."
  [c]
  (let [left (slots-short c)
        free (u/free-slots (:primitives c))
        summary (tossed-summary (ctx/mem c) free)
        data [:units :slots :done free :left left :result summary]]
    (if (pos? left)
      (apply contract/later :short (str "make-room: " left " slot(s) short") data)
      (apply contract/ok :room (:text summary) data))))

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
   :toss-failed "tosses failed in a row"})

(defn ^:async finish-end!
  "The :done or :nothing-possible ending, its data the summary."
  [c reason]
  (let [m (ctx/mem c)
        free-now (u/free-slots (:primitives c))
        {:keys [tossed text]} (tossed-summary m free-now)]
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
  (when-let [tossed (seq (:tossed (ctx/mem c)))]
    (ctx/remember! c :make-room-rest {:items (vec (distinct (map :item tossed)))} rest-policy))
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
  (let [{:keys [free]} (:args c)
        toss-below (settings/get settings ::toss-below)
        p (:primitives c)
        m (ctx/mem c)
        free-now (u/free-slots p)
        {:keys [inventory keep recency] :as st} (state c)
        names (ord/deposit-names inventory keep recency)
        dir (await (direction! c run))
        swap (when (and dir (zero? free-now)) (or (swap-target c st) (await (seek-swap! c run st))))
        order (if (:stuck? (:args c))
                (ord/toss-order inventory keep recency (chest-cost) true)
                (ord/toss-order inventory keep recency toss-below))]
    (cond
      (>= free-now free) (await (finish-end! c nil))
      (>= (:toss-fails m 0) (settings/get settings ::toss-fails)) (await (finish-end! c :toss-failed))
      :else
      (let [chest (when-not (:chest-done @run)
                    (when-not (contains? @run :chest)
                      (swap! run assoc :chest (await (usable-chest! c (if dir (toss-cost order (slots-short c)) ##Inf)))))
                    (:chest @run))]
        (cond
          (and chest (seq names)) (do (await (put-away! c run chest names keep)) :again)
          (and (:stuck? (:args c)) (not (:chest-done @run)) (seq names) (or (nil? dir) (empty? order)))
          (do (await (put-away! c run nil names keep)) :again)
          swap (do (await (swap-in! c swap)) :again)
          (and dir (seq order)) (do (await (toss! c (first order) dir {})) :again)
          :else (await (finish-end! c (if (:acted m) :short :nothing-to-go))))))))

(defn ^:async round
  "One whole attempt: settle intents left by a cut, then step! until it ends."
  [c]
  (await (settle-intents! c))
  (let [run (atom {:chest-done false :looked false})]
    (await (pace/steps! c #(step! c run)))))
