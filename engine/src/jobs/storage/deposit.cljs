(ns jobs.storage.deposit
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.cost :as cost]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.storage :as storage]))

(def doc
  "Walk to the chest (one go-to) and deposit every stack in one call: the named :items in the order named, or everything but tools
  and armour (saplings included) and the body's 3-day food reserve (jobs.lib.cost/food-reserve) in inventory order.
  :keep leaves at least that many of a name carried. :free ends the call, clean, once that many slots are free. With explicit :items the reserve is ignored.
  Ends with {:gave-up false} when nothing is left to put away, or stopped {:gave-up true :status :stopped :reason r
  :moved n} (n items put in) after three failed transfers (warn chest_unusable) or at once when go-to cannot reach.
  r is the transfer status or \"unreachable\".
  Memory: a :chest argument that took at least one item and finished clean is offered to the :chest place. It is
  recorded when none is recorded or the recorded one is gone, never over a different live one (place.kept
  event; jobs.memory.set-place moves it). A transfer that finds the recorded chest missing retracts it, with one
  chest_missing warn, and ends at once with reason \"missing\". No chest known: waits, reason :no-chest; no chest
  at round time gives up \"no-chest\".
  Zones: a chest in another owner's zone or claim that does not allow :put is refused before the walk and again
  before the transfer. The job ends {:gave-up true :reason :refused :zones [..] :claims [..]} after one
  deposit.refused warn and puts nothing in. :ignore-zones? true skips the check.
  Storage group: with no :chest, or a :chest that is the :chest place or one of the group (memory :storage/chests,
  jobs.lib.storage/group), chests are tried in turn; one that answers full or takes nothing is skipped. When all are
  full and :make-chests? is true, a chest (jobs.blocks.place child, fetching or crafting it by default; :fetch-chests? false only a carried one) goes down
  beside a full one; a refused spot is skipped for the next free cell and joins the group; none to be had ends {:reason \"no-chest-item\"}, no free cell beside
  {:reason \"no-chest-spot\"}, :make-chests? false {:reason \"all-chests-full\"}. Any other :chest is used alone.
  Stock: a chest whose stock is booked in body memory :fetch/stock gets what was put in added (jobs.lib.fetch).")

(a/defargs args
  {:chest {:doc "chest position [x y z] or {:x :y :z}; the known :chest place when nil" :spec ::a/pos :default nil}
   :items {:doc "item names to put away, in this order (the first name with something to spare goes first); everything but tools and armour when nil" :spec (a/coll-of (a/or-of string? keyword?)) :default nil}
   :keep {:doc "{item-name count}: leave at least this many of the name carried" :spec (a/map-of string? number?) :default {}}
   :free {:doc "stop once this many inventory slots are free; all of it when nil" :spec (a/int-in 0 nil) :default nil}
   :make-chests? {:doc "when every storage chest is full, put a new one beside them (carried, else fetched and crafted); false stops with reason all-chests-full" :spec boolean? :default true}
   :fetch-chests? {:doc "a new chest may be fetched or crafted when none is carried; false places only a carried one (no-chest-item otherwise)" :spec boolean? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(defn to-deposit
  "The next carried stack to put away and how many of it: {:stack s :count n}
  or nil. With wanted names, the first carried stack of the first name (in
  wanted's order) that has spare; with none (nil), the first stack in
  inventory order that is not a tool or armour and has spare. A name has spare
  when its carried total exceeds its keep (a map of name to count, default 0);
  n is the stack count, cut so the keep stays carried."
  ([items wanted] (to-deposit items wanted {}))
  ([items wanted keep]
   (let [spare (fn [stack] (- (storage/carried items (:name stack)) (get keep (:name stack) 0)))
         pick (fn [stack] (when (pos? (spare stack))
                            {:stack stack :count (min (:count stack) (spare stack))}))]
     (if wanted
       (some (fn [n] (some #(when (= n (:name %)) (pick %)) items)) wanted)
       (some #(when-not (storage/tool? (:name %)) (pick %)) items)))))

(defn check
  "A chest is known; else waits with reason :no-chest."
  [c]
  (or (boolean (storage/chest-of (ctx/view c) (:args c)))
      (ctx/wait c {:reason :no-chest})))

(defn stop!
  "End stopped: {:gave-up true :reason reason :status :stopped :moved n}, n the items put in so far."
  [c reason]
  (ctx/result! c {:gave-up true :reason reason :status :stopped :moved (:moved (ctx/mem c) 0)})
  :done)

(defn give-up!
  "u/fail!: :again until the third failure in a row, then stop with the reason (a warn of kind first)."
  [c kind text reason]
  (let [r (u/fail! c kind text)]
    (if (= :done r) (stop! c reason) :again)))

(defn retracted?
  "retract-if-missing!, and true when status is \"missing\" at the recorded chest place (so the job ends at once)."
  [c chest status]
  (let [recorded (mem/place (ctx/view c) :chest)]
    (places/retract-if-missing! c :chest chest status)
    (and (= "missing" status) (some? recorded) (= chest recorded))))

(defn refuse!
  "End refused: one deposit.refused warn naming the zones and claims, the refusal as the result."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])]
    (ctx/emit! c :deposit.refused :warn (assoc fields :text (str "deposit refused: " (access/refusal-text fields))))
    (ctx/result! c (access/refused-result verdict))
    :done))

(defn cell-pos [cell] (zipmap [:x :y :z] cell))

(defn chests
  "{:pool cells :open cells :group? bool}: the chests to try. group? when the body's storage group is in play (no :chest
  argument, or one in the group): the :chest argument or place first, then the group nearest first. Else only the
  argument. open is the pool without the ones found full this call."
  [c]
  (let [view (ctx/view c)
        args (:args c)
        main (some-> (storage/chest-of view args) storage/cell-of)
        members (storage/group view)
        group? (or (nil? (:chest args)) (some? (some #{main} members)))
        here (u/self-pos c)
        near (sort-by (fn [[x y z]] (+ (js/Math.abs (- x (:x here))) (js/Math.abs (- y (:y here))) (js/Math.abs (- z (:z here))))) members)
        pool (vec (if group? (distinct (concat (when main [main]) near)) (when main [main])))
        full (:full (ctx/mem c) #{})]
    {:pool pool :open (vec (remove full pool)) :group? group?}))

(defn chest-spots
  "Free cells beside the chests at cells that a chest could stand in: seen air with a seen solid floor, level with the
  chest, not tried before (memory :chest-tried); no zone, claim or plan refuses the place."
  [c cells]
  (let [p (:primitives c)
        tried (:chest-tried (ctx/mem c) #{})
        solid? (fn [n] (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n))))]
    (vec (for [[x y z] cells
               [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
               :let [pos {:x (+ x dx) :y y :z (+ z dz)}]
               :when (and (b/air (u/seen-name p pos))
                          (solid? (u/seen-name p (update pos :y dec)))
                          (not (tried (storage/cell-of pos)))
                          (:ok (access/may? c :place pos)))]
           pos))))

(defn ^:async make-chest!
  "Every chest of the pool is full: put a chest down beside one (a jobs.blocks.place child), remember it in the group.
  :again, :continue while the child waits, or a stop with the reason (all-chests-full when :make-chests? is false,
  no-chest-spot, no-chest-item)."
  [c pool]
  (cond
    (not (:make-chests? (:args c))) (stop! c "all-chests-full")
    :else
    (let [spot (or (:chest-spot (ctx/mem c)) (first (chest-spots c pool)))]
      (cond
        (nil? spot) (stop! c "no-chest-spot")
        (= "chest" (u/seen-name (:primitives c) spot)) ; placed by an earlier round of the child
        (do (ctx/update-mem! c dissoc :chest-spot)
            (storage/add-to-group! c spot)
            (u/progress! c)
            :again)
        :else
        (let [pargs (merge (select-keys (:args c) [:ignore-zones?]) {:pos spot :item "chest"} (when-not (:fetch-chests? (:args c)) {:fetch false}))
              _ (ctx/update-mem! c assoc :chest-spot spot)
              r (await (ctx/call-child c :place 'jobs.blocks.place pargs))
              out (ctx/child-result c :place)]
          (cond
            (and (:placed out) (not= :continue r))
            (do (ctx/update-mem! c dissoc :chest-spot)
                (storage/add-to-group! c spot)
                (u/progress! c)
                :again)
            (= :continue r) :continue
            (= :declined r) (let [why (:reason (b/child-wait c :place 'jobs.blocks.place pargs))]
                              (ctx/update-mem! c #(-> % (dissoc :chest-spot) (update :chest-tried (fnil conj #{}) (storage/cell-of spot))))
                              (ctx/emit! c :deposit.no-chest :warn {:text (str "cannot place a new storage chest at " (pr-str (storage/cell-of spot)) ": " (name (or why :not-ready)))})
                              (cond
                                (= :need why) (stop! c "no-chest-item")
                                (seq (chest-spots c pool)) :again
                                :else (stop! c "no-chest-spot")))
            :else
            (do (ctx/update-mem! c #(-> % (dissoc :chest-spot) (update :chest-tried (fnil conj #{}) (storage/cell-of spot))))
                (give-up! c :chest_unusable "could not place a new storage chest" "no-chest-spot"))))))))

(defn mark-full!
  "The chest took nothing more: skip it for the rest of the call. :again."
  [c chest]
  (ctx/update-mem! c update :full (fnil conj #{}) (storage/cell-of chest))
  :again)

(defn ^:async walk!
  "Within 3 of the chest: nil. Else one go-to call: :continue while it waits on the world, :again once there,
  :unreachable when it gave up."
  [c chest]
  (if (u/within? (u/self-pos c) chest 3)
    nil
    (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos chest :range 3 :escalate false :warn false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
      (cond
        (= :continue r) :continue
        (:arrived (ctx/child-result c :walk)) :again
        :else :unreachable))))

(defn ^:async step!
  "Put the next stack away, walking to the chest first: :again, :continue (go-to waits), or :done."
  [c]
  (let [{:keys [items keep free]} (:args c)
        {:keys [pool open group?]} (chests c)
        chest (some-> (first open) cell-pos)
        inventory (u/inventory (:primitives c))
        pick (when-not (and free (>= (u/free-slots (:primitives c)) free))
               (to-deposit inventory items (if items keep (merge-with max (cost/food-reserve inventory) keep))))]
    (cond
      (nil? pick) (do (when (and (:chest (:args c)) (pos? (:deposited (ctx/mem c) 0)))
                        (places/offer! c :chest (:chest (:args c))))
                      (ctx/result! c {:gave-up false})
                      :done)
      (empty? pool) (give-up! c :chest_unusable "no chest is known" "no-chest")
      (nil? chest) (await (make-chest! c pool))
      (access/container-refusal c :put chest) (refuse! c (access/container-refusal c :put chest))
      :else
      (let [w (await (walk! c chest))]
        (cond
          (= :unreachable w) (give-up! c :chest_unusable "cannot reach the chest" "unreachable")
          w w
          :else
        (if-let [v (access/container-refusal c :put chest)]
          (refuse! c v)
          (let [r (await (ctx/act c :transfer (clj->js {:pos chest :direction "deposit"
                                                         :item (:name (:stack pick)) :count (:count pick)})))]
            (if (= "ok" (.-status r))
              (if (and (pos? (or (.-moved r) 0))
                       (< (storage/carried (u/inventory (:primitives c)) (:name (:stack pick))) (storage/carried inventory (:name (:stack pick)))))
                (do (ctx/update-mem! c #(-> % (update :deposited (fnil inc 0)) (update :moved (fnil + 0) (.-moved r))))
                    (u/progress! c)
                    (fetch/note-moved! c chest (:name (:stack pick)) (.-moved r))
                    :again)
                (if group? (mark-full! c chest) (give-up! c :chest_unusable "nothing moved into the chest" "nothing-moved")))
              (cond
                (and group? (= "full" (.-status r))) (mark-full! c chest)
                (and group? (= "missing" (.-status r)) (not= (storage/cell-of chest) (some-> (mem/place (ctx/view c) :chest) storage/cell-of)))
                (do (storage/drop-from-group! c chest) (mark-full! c chest))
                (retracted? c chest (.-status r)) (stop! c (.-status r))
                :else (give-up! c :chest_unusable (str "chest not usable: " (.-status r)) (.-status r)))))))))))

(defn ^:async round
  "The whole put-away in one call: stacks one after another (the inventory is read again before each, so a restart
  moves nothing twice) until nothing is left or it stops. :continue only while go-to waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async deposit-step [] (await (step! c))))))
