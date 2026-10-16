(ns jobs.movement.boat-launch
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Put a boat on the water and get in it (:action :launch, the default), or take one back (:action :recover).

  Launch: the boat item is :item, else the first carried boat or raft; none carried is fetched (jobs.blocks.place :fetch, oak_boat).
  The water cell is :pos, else the nearest water in sight (:radius) that has air above it and room round it for the boat (no bank block within a cell).
  jobs.blocks.place puts the boat down (it walks into reach); then, with :board, jobs.movement.mount boards it. The placed boat is
  remembered, so a restart boards it rather than placing another.
  Impossible: :bad-args (:item is no boat, :pos names no cell). Conditions at the start: :aboard (already on a vehicle), :no-water (none in sight),
  :no-room (water in sight, none with room for a boat); the engine looks round once first. A listed call waits on them.
  Ends done (data {:id :boat name :pos :boarded bool}), info boat.launched, or
  condition-failed: the place's own (:need, :not-allowed, :no-support, :occupied ...; its ending, nothing-possible :unreachable too, is the :because), or
  nothing-possible: :no-boat (the placed boat is not in sight), :board-failed (mount's ending is the :because).

  Recover: the boat is :id, else the nearest boat or raft in sight. When aboard, it gets off first (jobs.movement.leave-vehicle),
  walks into reach (jobs.movement.go-to), hits it until it breaks and collects the dropped item (jobs.forestry.collect-drops).
  Condition at the start: :no-boat (none in sight). Ends done (data {:id :item name :collected n}), info boat.recovered, or
  condition-failed :gone (the boat is no longer in sight), or
  nothing-possible: :leave-failed, :unreachable (the walk failed twice), :not-collected (the item was not picked up), or
  limit-reached :job-time (the engine ends it when the call's :spend :job-time ran out; data {:id :hits} of a recovery, {:placed} of a launch).

  Never :continue except when a child waits on the world.")

(a/defargs args
  {:action {:doc ":launch or :recover" :spec #{:launch :recover} :default :launch}
   :pos {:doc "launch: the water cell to put the boat on, [x y z] or {:x :y :z}; nil: the nearest suitable water in sight" :spec ::a/pos :default nil}
   :item {:doc "launch: the boat item to place; nil: the first carried boat or raft" :spec a/item? :default nil}
   :board {:doc "launch: get in once it is down" :spec boolean? :default true}
   :id {:doc "recover: the entity id of the boat; nil: the nearest in sight" :spec (a/int-in 1 nil) :default nil}
   :radius {:doc "how far to look for water or a boat" :spec (a/num-in 0 nil) :default 12}})

(def target [:action :pos :id])

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a launch or a recovery may take (a boat that does not break is hit until then)." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def default-item "oak_boat")
(def reach 3)
(def launch-s "Seconds a launch takes without a walk, for the quote." 4)
(def recover-s "Seconds a recovery takes without a walk, for the quote." 8)

(defn boat-name? [n] (boolean (and (string? n) (re-find #"_(boat|raft)$" n))))

(defn carried-boat [c]
  (some #(when (boat-name? (:name %)) (:name %)) (u/inventory (:primitives c))))

(defn boats-in-sight
  "The boats and rafts in sight within :radius, nearest first, as entity maps."
  [c]
  (let [p (:primitives c)
        me (u/self-pos c)]
    (->> (look/seen-entities p {:radius (:radius (:args c)) :max 64})
         (filter #(and (boat-name? (:name %)) (:pos %)))
         (sort-by #(u/dist me (:pos %))))))

(def ring
  "The 8 horizontal neighbours: a boat is 1.375 wide, so its box reaches into all of them."
  [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]])

(defn solid-name? [n] (boolean (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n)))))

(defn clear-water?
  "Whether a boat fits on the water cell: air above it and no solid block (seen) round it at the water's level or the one
  above, where the bank top would reach the boat's box and the server refuses the place."
  [p {:keys [x y z]}]
  (and (= "air" (u/seen-name p {:x x :y (inc y) :z z}))
       (not-any? (fn [[dx dz]]
                   (some #(solid-name? (u/block-name-or p {:x (+ x dx) :y % :z (+ z dz)} "air")) [y (inc y)]))
                 ring)))

(defn water-cells
  "The water cells in sight within :radius, nearest first."
  [c]
  (map :pos (look/seen-blocks (:primitives c) {:names ["water"] :radius (:radius (:args c)) :max 128 :live? true})))

(defn find-water
  "The nearest water cell in sight that has room for a boat, or nil. A bank cell not seen yet reads as air (noted as a guess)."
  [c]
  (let [p (:primitives c)]
    (first (filter #(clear-water? p %) (water-cells c)))))

(defn no-water-reason
  "Why find-water found nothing: :no-room when water is in sight, else :no-water."
  [c]
  (if (seq (water-cells c)) :no-room :no-water))

(defn no-water-text [reason]
  (if (= :no-room reason) "water in sight, but none with room round it for a boat" "no water to put a boat on in sight"))

(defn launch-pos [c]
  (let [{:keys [pos]} (:args c)]
    (if pos (:pos (b/parse (:args c))) (find-water c))))

(defn ^:async launch-pos!
  "launch-pos, the nearest-water search decided again after a look at the unseen cells it read."
  [c]
  (if (:pos (:args c))
    (launch-pos c)
    (await (sense/decide! c ::water #(find-water c) {:verify-guesses true}))))

(defn launching? [c] (= :launch (or (:action (:args c)) :launch)))

(def bad-args
  "Never :bad-args for a launch of an item that is no boat or onto a :pos that names no cell."
  (checks/args-fit "needs a boat :item and a cell :pos"
                   (fn [c] (let [{:keys [item pos]} (:args c)]
                             (not (and (launching? c)
                                       (or (and item (not (boat-name? item)))
                                           (and (some? pos) (nil? (:pos (b/parse (:args c))))))))))))

(defn aboard
  "Later :aboard at the start of a launch while the body is on a vehicle."
  [c]
  (if (and (launching? c) (vehicle/mounted? (:primitives c)))
    (contract/later :aboard "on a vehicle" :hint "get off the vehicle first")
    (contract/ok :on-foot "on foot")))

(defn no-water-cause [c]
  (let [r (no-water-reason c)]
    {:reason r :hint (no-water-text r)}))

(defn no-water
  "At the start of a launch: later :no-water or :no-room while no cell has room for a boat; a look first while the body has not looked round."
  [c]
  (if (and (launching? c) (nil? (launch-pos c)))
    (let [{:keys [reason hint]} (no-water-cause c)]
      (contract/look reason hint :hint hint))
    (contract/ok :water "room for a boat")))

(defn pick-boat [c]
  (let [id (:id (:args c))
        boats (boats-in-sight c)]
    (if id (first (filter #(= id (:id %)) boats)) (first boats))))

(defn no-boat
  "At the start of a recovery: later :no-boat while none is in sight."
  [c]
  (if (and (not (launching? c)) (nil? (pick-boat c)))
    (contract/later :no-boat "no boat in sight" :hint "no boat in sight")
    (contract/ok :boat "a boat in sight")))

(def can-start? (cond/and bad-args aboard no-water no-boat))

(defn done?
  "A launch is done once its boat is down; a recovery's end is its event."
  [c]
  (let [{:keys [placed hits]} (ctx/mem c)
        placed (and (launching? c) (some? placed))]
    (contract/later (if (launching? c) :launching :recovering) (if placed "the boat is down" "no boat down yet")
                    :units :boats :done (if placed 1 0) :left (if placed 0 1)
                    :result (if (launching? c) {:placed placed} {:id (:id (:args c)) :hits (or hits 0)}))))

(defn quote-of [c]
  (let [most (or (get-in (ctx/frozen-call c) [:spend :job-time]) (time-s))
        mean (min most (if (launching? c) launch-s recover-s))]
    {:uses {:job-time [(min 1 mean) mean most]}}))

(defn placed-boat
  "The boat just put at cell, as seen: the nearest boat entity within 2 blocks of its centre."
  [c cell]
  (let [centre {:x (+ (:x cell) 0.5) :y (:y cell) :z (+ (:z cell) 0.5)}]
    (->> (boats-in-sight c)
         (filter #(<= (u/dist centre (:pos %)) 2))
         first)))

(defn ^:async board! [c {:keys [id] :as boat} cell]
  (let [m (await (ctx/call c :mount 'jobs.movement.mount {:id id}))]
    (cond
      (= :continue m) :continue
      (not= :done (:kind m)) (ctx/end c :nothing-possible :board-failed {:id id :pos cell})
      :else
      (do (ctx/emit! c :boat.launched :info {:id id :boat (:name boat) :pos cell :text (str "launched " (:name boat))})
          (ctx/end c :done nil {:id id :boat (:name boat) :pos cell :boarded true})))))

(defn ^:async launch! [c]
  (let [p (:primitives c)
        {:keys [item board]} (:args c)
        item (or item (carried-boat c) default-item)
        remembered (:placed (ctx/mem c))
        cell (or (:pos remembered) (await (launch-pos! c)))]
    (cond
      (nil? cell) (ctx/end c :condition-failed (no-water-cause c))
      remembered
      (let [boat (first (filter #(= (:id remembered) (:id %)) (boats-in-sight c)))]
        (cond
          (nil? boat) (ctx/end c :nothing-possible :no-boat {:pos cell})
          (not board) (ctx/end c :done nil {:id (:id boat) :boat (:name boat) :pos cell :boarded false})
          :else (await (board! c boat cell))))
      (vehicle/mounted? p) (ctx/end c :condition-failed {:reason :aboard :hint "get off the vehicle first"})
      :else
      (let [e (await (ctx/call c :place 'jobs.blocks.place {:pos cell :item item :fetch true}))]
        (cond
          (= :continue e) :continue
          (not= :done (:kind e)) (ctx/end c (:kind e) (:cause e) {:pos cell})
          :else
          (let [boat (placed-boat c cell)]
            (cond
              (nil? boat) (ctx/end c :nothing-possible :no-boat {:pos cell})
              :else
              (do (ctx/update-mem! c assoc :placed {:id (:id boat) :pos cell})
                  (if board
                    (await (board! c boat cell))
                    (do (ctx/emit! c :boat.launched :info {:id (:id boat) :boat (:name boat) :pos cell :text (str "put " (:name boat) " on the water")})
                        (ctx/end c :done nil {:id (:id boat) :boat (:name boat) :pos cell :boarded false})))))))))))

(defn ^:async recover! [c]
  (let [p (:primitives c)
        id (:id (:args c))
        aboard (when (vehicle/mounted? p) (await (ctx/call c :leave 'jobs.movement.leave-vehicle {})))]
    (cond
      (= :continue aboard) :continue
      (and (some? aboard) (not= :done (:kind aboard))) (ctx/end c :nothing-possible :leave-failed {:id id})
      :else
      (loop [hits 0 failed 0]
        (let [boat (pick-boat c)]
          (cond
            (nil? boat) (ctx/end c :condition-failed :gone {:id id})
            (> (u/dist (u/self-pos c) (:pos boat)) reach)
            (let [w (await (child/call-walk* c :walk {:pos (:pos boat) :range 2 :ways #{:walk :clear-seal}}))]
              (cond
                (= :continue w) :continue
                (= :done (:kind w)) (recur hits failed)
                (>= (inc failed) 2) (ctx/end c :nothing-possible :unreachable {:id (:id boat) :why (contract/cause-key w)})
                :else (recur hits (inc failed))))
            :else
            (let [r (await (ctx/act c :attack #js {:id (:id boat)}))]
              (if (= "killed" (.-status r))
                (let [item (:name boat)
                      k (await (ctx/call c :collect 'jobs.forestry.collect-drops
                                         {:near (:pos boat) :radius 4 :filter [item]}))
                      n (:collected (:data k) 0)]
                  (cond
                    (= :continue k) :continue
                    (zero? n) (ctx/end c :nothing-possible :not-collected {:id (:id boat) :item item})
                    :else (do (ctx/emit! c :boat.recovered :info {:id (:id boat) :item item :collected n :text (str "took back " item)})
                              (ctx/end c :done nil {:id (:id boat) :item item :collected n}))))
                (do (ctx/update-mem! c assoc :hits (inc hits))
                    (recur (inc hits) failed))))))))))

(defn ^:async round [c]
  (if (launching? c)
    (await (launch! c))
    (await (recover! c))))
