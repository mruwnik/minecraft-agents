(ns jobs.storage.deposit
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.cost :as cost]
            [jobs.lib.found :as found]
            [jobs.lib.stock :as stock]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.storage :as storage]
            [jobs.lib.least :as least]))

(def doc
  "Walk to the chest (one go-to) and deposit every stack in one call: the named :items in the order named, or everything but tools
  and armour (saplings included) and the body's 3-day food reserve (jobs.lib.cost/food-reserve) in inventory order.
  :keep leaves at least that many of a name carried. :free ends the call, clean, once that many slots are free. With explicit :items the reserve is ignored.
  Needs: a chest, the :chest argument or the known :chest place; none known waits :no-chest (can-start?).
  Ends done when nothing is left to put away (also at once when nothing is), or at any other ending when at least the
  call's :least items are put in (:all wants every one). The data of every other ending is
  {:gave-up true :moved n}, n the items put in; the report counts the items still to put away.
  - condition-failed :refused {:zones [..] :claims [..]}: a chest in another owner's zone or claim that does not allow :put,
    checked before the walk and again before the transfer (one deposit.refused warn, nothing put in). :ignore-zones? skips it.
  - condition-failed :missing (no container at the chest; a recorded :chest place is retracted, one chest_missing warn)
    or :no-chest: at once.
  - nothing-possible, the cause the transfer status (:unreachable, :full, :nothing-moved, ..): after three failed transfers in a row
    (warn chest_unusable).
  Memory: a :chest argument that took at least one item and finished clean, not in another body's zone or claim, is offered to the :chest place. It is
  recorded when none is recorded or the recorded one is gone, never over a different live one (place.kept
  event; jobs.memory.set-place moves it).
  Storage group: with no :chest, or a :chest that is the :chest place or one of the group (memory :storage/chests,
  jobs.lib.storage/group), chests are tried in turn; one that answers full or takes nothing is skipped. When all are
  full and :make-chests? is true, a chest (jobs.blocks.place child, fetching or crafting it by default; :fetch-chests? false only a carried one) goes down
  beside a full one and joins the group. A place that ends for a reason of the cell (a zone, the body in it, nothing to place against,
  unreachable, occupied) tries the next free spot (warn deposit.no-chest); a place that ends :need (no chest to be had) ends condition-failed
  :no-chest-item, no free cell beside nothing-possible :no-chest-spot, :make-chests? false nothing-possible :all-chests-full. Any other :chest is used alone.
  Found goods (jobs.lib.found: bulk or containers picked up with no job reason) stay carried, even when named in :items; one
  goods.found notice (:attention :notice) names item, count and the pick-up place.
  Stock: what the chest holds after each transfer is booked in body memory :fetch/stock (jobs.lib.stock), so observe chests shows it.")

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

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds a put-away may last: the walk, the transfers and a chest placed." :spec (a/num-in 0 nil)}
   ::take-s {:default 1 :doc "Seconds a quote counts for one transfer." :spec (a/num-in 0 nil)}
   ::walk-speed {:default 4 :doc "Blocks a second a quote counts for the walk to the chest." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:chest])

(def safe? ready/safe-at-chest)
(def fit? ready/ok)
(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def can-start? checks/chest-known)

(defn spare-items
  "{name n} the call would put away now: the carried names the pick rules allow, each by its spare over the keep."
  [c]
  (let [{:keys [items keep]} (:args c)
        carried (u/inventory (:primitives c))
        goods (found/goods (ctx/view c))
        inventory (remove #(contains? goods (:name %)) carried)
        keeps (if items keep (merge-with max (cost/food-reserve inventory) keep))]
    (loop [left inventory, acc {}]
      (if-let [pick (to-deposit left items keeps)]
        (let [n (:name (:stack pick))]
          (recur (remove #(= n (:name %)) left) (assoc acc n (- (storage/carried left n) (get keeps n 0)))))
        acc))))

(defn offer-owed?
  "Whether the :chest argument took something this call and is still to be offered to the :chest place (not once refused)."
  [c]
  (boolean (and (:chest (:args c)) (pos? (:deposited (ctx/mem c) 0)) (not (:offered (ctx/mem c)))
                (not (access/container-refusal (assoc-in c [:args :ignore-zones?] false) :put (:chest (:args c)))))))

(defn done?
  "Done once nothing spare is left to put away (:free slots met, if given) and the chest argument is offered to the :chest place."
  [c]
  (let [left (reduce + 0 (vals (spare-items c)))
        {:keys [free]} (:args c)
        moved (:moved (ctx/mem c) 0)
        data [:units :items :done moved :left left :result {:moved moved}]]
    (cond
      (and free (>= (u/free-slots (:primitives c)) free) (not (offer-owed? c))) (apply contract/ok :deposited "deposit: free slots met" data)
      (pos? left) (apply contract/later :depositing (str "deposit: " left " spare item(s)") data)
      (offer-owed? c) (apply contract/later :offering "deposit: the chest is still to be offered" data)
      :else (apply contract/ok :deposited (str "deposited " moved) data))))

(defn quote-of
  "Time for the walk and one transfer a name; the chest takes what is spare, up to the free slots it has."
  [c]
  (let [spare (spare-items c)
        chest (some-> (storage/chest-of (ctx/view c) (:args c)))
        walk (if chest (/ (u/dist (u/self-pos c) chest) (settings/get settings ::walk-speed)) 0)
        t (+ walk (* (count spare) (settings/get settings ::take-s)))]
    {:uses {:job-time [(* (count spare) (settings/get settings ::take-s)) t (max t (time-s))]}}))

(defn end!
  "ctx/end, but done when the units of done? meet the call's :least."
  [c kind cause data]
  (let [{:keys [done left]} (nth (done? c) 2)]
    (least/end! c kind cause data done (+ done left))))

(defn stop!
  "End with kind and cause: {:gave-up true :moved n}, n the items put in so far."
  [c kind cause]
  (end! c kind cause {:gave-up true :moved (:moved (ctx/mem c) 0)}))

(defn status-stop!
  "stop! for a status that is not ok: a chest that is not there is a fact, any other status (no route included) a failure."
  [c status]
  (stop! c (if (= "missing" status) :condition-failed :nothing-possible) (keyword status)))

(defn give-up!
  "u/fail!: :again until the third failure in a row, then end nothing-possible with the cause (a warn of kind first)."
  [c kind text cause]
  (let [r (u/fail! c kind text)]
    (if (= :done r) (status-stop! c (name cause)) :again)))

(defn retracted?
  "retract-if-missing!, and true when status is \"missing\" at the recorded chest place (so the job ends at once)."
  [c chest status]
  (let [recorded (mem/place (ctx/view c) :chest)]
    (places/retract-if-missing! c :chest chest status)
    (and (= "missing" status) (some? recorded) (= chest recorded))))

(defn refuse!
  "End refused: one deposit.refused warn naming the zones and claims; they are the cause and the data."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])
        refused (select-keys fields [:zones :claims])]
    (ctx/emit! c :deposit.refused :warn (assoc fields :text (str "deposit refused: " (access/refusal-text fields))))
    (end! c :condition-failed (assoc refused :reason :refused) (assoc refused :gave-up true))))

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

(defn spot-failed!
  "The spot cannot take a chest: it is not tried again. :again while another spot is free, else the no-spot stop."
  [c pool spot]
  (ctx/update-mem! c #(-> % (dissoc :chest-spot) (update :chest-tried (fnil conj #{}) (storage/cell-of spot))))
  (if (seq (chest-spots c pool)) :again (stop! c :nothing-possible :no-chest-spot)))

(defn ^:async make-chest!
  "Every chest of the pool is full: put a chest down beside one (a jobs.blocks.place child), remember it in the group.
  :again, :continue while the child waits, or a stop with the cause (all-chests-full when :make-chests? is false,
  no-chest-spot, no-chest-item)."
  [c pool]
  (cond
    (not (:make-chests? (:args c))) (stop! c :nothing-possible :all-chests-full)
    :else
    (let [spot (or (:chest-spot (ctx/mem c)) (first (chest-spots c pool)))]
      (cond
        (nil? spot) (stop! c :nothing-possible :no-chest-spot)
        (= "chest" (u/seen-name (:primitives c) spot)) ; placed by an earlier round of the child
        (do (ctx/update-mem! c dissoc :chest-spot)
            (storage/add-to-group! c spot)
            (u/progress! c)
            :again)
        :else
        (let [pargs (merge (select-keys (:args c) [:ignore-zones?]) {:pos spot :item "chest"} (when-not (:fetch-chests? (:args c)) {:fetch false}))
              _ (ctx/update-mem! c assoc :chest-spot spot)
              e (await (ctx/call c :place 'jobs.blocks.place pargs))]
          (cond
            (= :continue e) :continue
            (and (= :done (:kind e)) (:placed (:data e)))
            (do (ctx/update-mem! c dissoc :chest-spot)
                (storage/add-to-group! c spot)
                (u/progress! c)
                :again)
            (and (= :nothing-possible (:kind e)) (not= :unreachable (contract/cause-key e)))
            (do (ctx/update-mem! c #(-> % (dissoc :chest-spot) (update :chest-tried (fnil conj #{}) (storage/cell-of spot))))
                (give-up! c :chest_unusable "could not place a new storage chest" :no-chest-spot))
            :else
            (let [why (contract/cause-key e)]
              (ctx/emit! c :deposit.no-chest :warn {:text (str "cannot place a new storage chest at " (pr-str (storage/cell-of spot)) ": " (name why))})
              (if (= :need why)
                (stop! c :condition-failed :no-chest-item)
                (spot-failed! c pool spot)))))))))

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
    (let [e (await (child/call-walk* c :walk {:pos chest :range 3 :ways #{:walk :clear-seal} :warn false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
      (cond
        (= :continue e) :continue
        (:arrived (:data e)) :again
        :else :unreachable))))

(defn found-here!
  "{item pick-up} of the carried found goods (jobs.lib.found), which stay carried; one goods.found notice each, once per call."
  [c carried]
  (let [goods (select-keys (found/goods (ctx/view c)) (map :name carried))]
    (doseq [[item {:keys [pos]}] goods
            :when (not (contains? (:reported (ctx/mem c) #{}) item))]
      (ctx/update-mem! c update :reported (fnil conj #{}) item)
      (ctx/emit! c :goods.found :info
                 {:attention :notice :item item :count (storage/carried carried item) :at pos
                  :text (str "found goods, not deposited: " (storage/carried carried item) " " item
                             (when pos (str " picked up at " (pr-str [(:x pos) (:y pos) (:z pos)]))))}))
    goods))

(defn ^:async step!
  "Put the next stack away, walking to the chest first: :again, :continue (go-to waits), or :done."
  [c]
  (let [{:keys [items keep free]} (:args c)
        {:keys [pool open group?]} (chests c)
        chest (some-> (first open) cell-pos)
        carried (u/inventory (:primitives c))
        keeps (found-here! c carried)
        inventory (remove #(contains? keeps (:name %)) carried)
        pick (when-not (and free (>= (u/free-slots (:primitives c)) free))
               (to-deposit inventory items (if items keep (merge-with max (cost/food-reserve inventory) keep))))]
    (cond
      (nil? pick) (do (when (offer-owed? c)
                        (places/offer! c :chest (:chest (:args c)))
                        (ctx/update-mem! c assoc :offered true))
                      (ctx/end c :done nil))
      (empty? pool) (stop! c :condition-failed :no-chest)
      (nil? chest) (await (make-chest! c pool))
      (access/container-refusal c :put chest) (refuse! c (access/container-refusal c :put chest))
      :else
      (let [w (await (walk! c chest))]
        (cond
          (= :unreachable w) (give-up! c :chest_unusable "cannot reach the chest" :unreachable)
          w w
          :else
        (if-let [v (access/container-refusal c :put chest)]
          (refuse! c v)
          (let [r (await (ctx/act c :transfer (clj->js {:pos chest :direction "deposit"
                                                         :item (:name (:stack pick)) :count (:count pick)})))
                _ (when (.-items r) (stock/note-stock! c chest (.-items r)))]
            (if (= "ok" (.-status r))
              (if (and (pos? (or (.-moved r) 0))
                       (< (storage/carried (u/inventory (:primitives c)) (:name (:stack pick))) (storage/carried inventory (:name (:stack pick)))))
                (do (ctx/update-mem! c #(-> % (update :deposited (fnil inc 0)) (update :moved (fnil + 0) (.-moved r))))
                    (u/progress! c)
                    :again)
                (if group? (mark-full! c chest) (give-up! c :chest_unusable "nothing moved into the chest" :nothing-moved)))
              (cond
                (and group? (= "full" (.-status r))) (mark-full! c chest)
                (and group? (= "missing" (.-status r)) (not= (storage/cell-of chest) (some-> (mem/place (ctx/view c) :chest) storage/cell-of)))
                (do (storage/drop-from-group! c chest) (mark-full! c chest))
                (retracted? c chest (.-status r)) (status-stop! c (.-status r))
                :else (give-up! c :chest_unusable (str "chest not usable: " (.-status r)) (keyword (.-status r))))))))))))

(defn ^:async round
  "The whole put-away in one call: stacks one after another (the inventory is read again before each, so a restart
  moves nothing twice) until nothing is left or it stops. :continue only while go-to waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async deposit-step [] (await (step! c))))))
