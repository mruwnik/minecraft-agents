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
  "Put a boat on the water and get in it.

  The boat item is :item, else the first carried boat or raft; none carried is fetched (jobs.blocks.place :fetch, oak_boat).
  The water cell is :pos, else the nearest water in sight (::radius) that has air above it and room round it for the boat (no bank block within a cell).
  jobs.blocks.place puts the boat down (it walks into reach); then, with :board, jobs.movement.mount boards it. The placed boat is
  remembered, so a restart boards it rather than placing another; a remembered boat seen gone is replaced by a new one.
  Impossible: :bad-args (:item is no boat, :pos names no cell). Conditions at the start: :aboard (already on a vehicle), :no-water (none in sight),
  :no-room (water in sight, none with room for a boat); the engine looks round once first. A listed call waits on them.
  Done once the boat is down and, with :board, the body is aboard it (data {:id :boat name :pos :boarded bool}), or
  condition-failed: :place-failed with the place's ending as the :because (its :need, :not-allowed, :no-support, :occupied ...) when that was a condition, else
  nothing-possible :place-failed (the because says why, :unreachable too), or
  nothing-possible: :no-boat (the placed boat is not in sight and its place was not seen), :board-failed (mount's ending is the :because), or
  limit-reached :job-time (the engine ends it when the call's :spend :job-time ran out; data {:placed}).

  Never :continue except when a child waits on the world.")

(a/defargs args
  {:pos {:doc "the water cell to put the boat on, [x y z] or {:x :y :z}; nil: the nearest suitable water in sight" :spec ::a/pos :default nil}
   :item {:doc "the boat item to place; nil: the first carried boat or raft" :spec a/item? :default nil}
   :board {:doc "get in once it is down" :spec boolean? :default true}})

(def target [:pos])

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))
(def fit? ready/fit?)

(def defaults {:spend {:job-time ::job-time-s}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a launch may take." :spec (a/num-in 0 nil)}
   ::radius {:default 12 :doc "How far the boat jobs (launch, land, recover) look for water, a shore or a boat." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))
(defn radius [] (settings/get settings ::radius))

(def default-item "oak_boat")
(def launch-s "Seconds a launch takes without a walk, for the quote." 4)

(defn boat-name? [n] (boolean (and (string? n) (re-find #"_(boat|raft)$" n))))

(defn carried-boat [c]
  (some #(when (boat-name? (:name %)) (:name %)) (u/inventory (:primitives c))))

(defn boats-in-sight
  "The boats and rafts in sight within ::radius, nearest first, as entity maps."
  [c]
  (let [p (:primitives c)
        me (u/self-pos c)]
    (->> (look/seen-entities p {:radius (radius) :max 64})
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
  "The water cells in sight within ::radius, nearest first."
  [c]
  (map :pos (look/seen-blocks (:primitives c) {:names ["water"] :radius (radius) :max 128 :live? true})))

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

(def bad-args
  "Never :bad-args for an item that is no boat or onto a :pos that names no cell."
  (checks/args-fit "needs a boat :item and a cell :pos"
                   (fn [c] (let [{:keys [item pos]} (:args c)]
                             (not (or (and item (not (boat-name? item)))
                                    (and (some? pos) (nil? (:pos (b/parse (:args c)))))))))))

(defn aboard
  "Later :aboard at the start while the body is on a vehicle."
  [c]
  (if (vehicle/mounted? (:primitives c))
    (contract/later :aboard "on a vehicle" :hint "get off the vehicle first")
    (contract/ok :on-foot "on foot")))

(defn no-water-cause [c]
  (let [r (no-water-reason c)]
    {:reason r :hint (no-water-text r)}))

(defn no-water
  "At the start: later :no-water or :no-room while no cell has room for a boat; a look first while the body has not looked round."
  [c]
  (if (nil? (launch-pos c))
    (let [{:keys [reason hint]} (no-water-cause c)]
      (contract/look reason hint :hint hint))
    (contract/ok :water "room for a boat")))

(def can-start? (cond/and bad-args aboard no-water))

(defn done?
  "Ok once the placed boat is in sight and, with :board, the body sits in it."
  [c]
  (let [p (:primitives c)
        {:keys [id pos]} (:placed (ctx/mem c))
        boat (when id (first (filter #(= id (:id %)) (boats-in-sight c))))
        ride (vehicle/vehicle-of (.self p))
        aboard (and ride (= id (:id ride)))
        boarded (boolean (and boat aboard))]
    (if (and boat (or boarded (not (:board (:args c)))))
      (contract/ok :launched "the boat is down" :units :boats :done 1 :left 0 :result {:id id :boat (:name boat) :pos pos :boarded boarded})
      (contract/later :launching (if boat "the boat is down, not boarded" "no boat down yet")
                      :units :boats :done (if boat 1 0) :left (if boat 0 1) :result {:placed (some? boat)}))))

(defn quote-of [c]
  (let [most (or (get-in (ctx/frozen-call c) [:spend :job-time]) (time-s))
        mean (min most launch-s)]
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
      (ctx/end c :done nil {:id id :boat (:name boat) :pos cell :boarded true}))))

(defn ^:async walk-to-remembered!
  "The remembered boat is beyond ::radius, so its cell was only remembered, not seen: walk close enough to look (one go-to call), then a later round decides."
  [c cell]
  (let [r (await (child/run!* c :walk 'jobs.movement.go-to {:pos cell :range (max 1 (- (radius) 2)) :ways #{:walk :clear-seal}}))]
    (if (= :continue r)
      :continue
      (if (and (= :done (:kind r)) (:arrived (:data r)))
        :continue
        (ctx/end c :nothing-possible :no-boat {:pos cell :hint "the remembered boat is out of reach"})))))

(defn ^:async launch! [c]
  (let [p (:primitives c)
        {:keys [item board]} (:args c)
        item (or item (carried-boat c) default-item)
        saved (:placed (ctx/mem c))
        unsighted? (and saved (nil? (first (filter #(= (:id saved) (:id %)) (boats-in-sight c)))))
        near? (and saved (<= (u/dist (u/self-pos c) (:pos saved)) (radius)))
        gone? (and unsighted? near? (some? (u/seen-name p (:pos saved))))
        remembered (when-not gone? saved)
        cell (or (:pos remembered) (await (launch-pos! c)))]
    (cond
      (nil? cell) (ctx/end c :condition-failed (no-water-cause c))
      remembered
      (let [boat (first (filter #(= (:id remembered) (:id %)) (boats-in-sight c)))]
        (cond
          (and (nil? boat) (not near?)) (await (walk-to-remembered! c cell))
          (nil? boat) (ctx/end c :nothing-possible :no-boat {:pos cell})
          (not board) (ctx/end c :done nil {:id (:id boat) :boat (:name boat) :pos cell :boarded false})
          :else (await (board! c boat cell))))
      (vehicle/mounted? p) (ctx/end c :condition-failed {:reason :aboard :hint "get off the vehicle first"})
      :else
      (let [e (await (ctx/call c :place 'jobs.blocks.place {:pos cell :item item :fetch true}))]
        (cond
          (= :continue e) :continue
          (not= :done (:kind e)) (ctx/end c (if (= :condition-failed (:kind e)) :condition-failed :nothing-possible) :place-failed {:pos cell})
          :else
          (let [boat (placed-boat c cell)]
            (cond
              (nil? boat) (ctx/end c :nothing-possible :no-boat {:pos cell})
              :else
              (do (ctx/update-mem! c assoc :placed {:id (:id boat) :pos cell})
                  (if board
                    (await (board! c boat cell))
                    (ctx/end c :done nil {:id (:id boat) :boat (:name boat) :pos cell :boarded false}))))))))))

(defn ^:async round [c]
  (await (launch! c)))
