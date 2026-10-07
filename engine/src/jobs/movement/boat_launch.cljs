(ns jobs.movement.boat-launch
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.look :as look]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Put a boat on the water and get in it (:action :launch, the default), or take one back (:action :recover).

  Launch: the boat item is :item, else the first carried boat or raft; none carried is fetched (jobs.blocks.place :fetch, oak_boat).
  The water cell is :pos, else the nearest water in sight (:radius) that has air above it and a solid block beside it.
  jobs.blocks.place puts the boat down (it walks into reach); then, with :board, jobs.movement.mount boards it.
  The check waits (:no-water) while no such water is in sight. Ends {:status :done :id :boat name :pos :boarded bool}, info boat.launched, or
  {:status :stopped :reason r}: :bad-args, :aboard (already on a vehicle), :need (no boat and none to fetch), :refused (zone, claim),
  :no-support, :unreachable, :occupied, :no-boat (the placed boat is not in sight), :board-failed (with :cause), :failed (the place failed, with :primitive).

  Recover: the boat is :id, else the nearest boat or raft in sight. When aboard, it gets off first (jobs.movement.leave-vehicle),
  walks into reach (jobs.movement.go-to), hits it until it breaks and collects the dropped item (jobs.forestry.collect-drops).
  The check waits (:no-boat) while no boat is in sight. Ends {:status :done :id :item name :collected n}, info boat.recovered, or
  {:status :stopped :reason r}: :gone, :unreachable, :leave-failed, :not-broken (still there after :max-s), :not-collected (the item was not picked up).

  Never :continue except when a child waits on the world.")

(def args
  {:action {:doc ":launch or :recover" :default :launch}
   :pos {:doc "launch: the water cell to put the boat on, [x y z] or {:x :y :z}; nil: the nearest suitable water in sight" :type :pos :default nil}
   :item {:doc "launch: the boat item to place; nil: the first carried boat or raft" :default nil}
   :board {:doc "launch: get in once it is down" :default true}
   :id {:doc "recover: the entity id of the boat; nil: the nearest in sight" :default nil}
   :radius {:doc "how far to look for water or a boat" :default 12}
   :max-s {:doc "recover: seconds to keep hitting a boat that does not break (bare hands take 5 hits, a boat has no health to read)" :default 15}})

(def default-item "oak_boat")
(def reach 3)

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

(defn shore-water?
  "Whether the water cell has air above and a solid block beside it (a place against it has something to click)."
  [p {:keys [x y z]}]
  (and (= "air" (u/seen-name p {:x x :y (inc y) :z z}))
       (some (fn [[dx dz]]
               (let [n (u/seen-name p {:x (+ x dx) :y y :z (+ z dz)})]
                 (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n)))))
             [[1 0] [-1 0] [0 1] [0 -1]])))

(defn find-water
  "The nearest water cell in sight that a boat can be put on from the shore, or nil."
  [c]
  (let [p (:primitives c)]
    (->> (look/seen-blocks p {:names ["water"] :radius (:radius (:args c)) :max 128 :live? true})
         (map :pos)
         (filter #(shore-water? p %))
         first)))

(defn launch-pos [c]
  (let [{:keys [pos]} (:args c)]
    (if pos (:pos (b/parse (:args c))) (find-water c))))

(defn check [c]
  (case (or (:action (:args c)) :launch)
    :launch (or (some? (launch-pos c)) (ctx/wait c {:reason :no-water :why "no water to put a boat on in sight"}))
    :recover (or (if-let [id (:id (:args c))]
                   (some #(= id (:id %)) (boats-in-sight c))
                   (seq (boats-in-sight c)))
                 (ctx/wait c {:reason :no-boat :why "no boat in sight"}))
    true))

(defn placed-boat
  "The boat just put at cell, as seen: the nearest boat entity within 2 blocks of its centre."
  [c cell]
  (let [centre {:x (+ (:x cell) 0.5) :y (:y cell) :z (+ (:z cell) 0.5)}]
    (->> (boats-in-sight c)
         (filter #(<= (u/dist centre (:pos %)) 2))
         first)))

(def place-stops
  {:need "no boat carried and none could be fetched"
   :refused "a zone or claim refuses the place"
   :no-support "nothing beside the water to place against"
   :unreachable "could not walk within reach of the water"
   :occupied "the cell is taken"})

(defn ^:async launch! [c]
  (let [p (:primitives c)
        {:keys [item board]} (:args c)
        item (or item (carried-boat c) default-item)
        cell (launch-pos c)]
    (cond
      (not (boat-name? item)) (result/stop! c :bad-args (str item " is not a boat"))
      (nil? cell) (result/stop! c :no-water "no water to put a boat on in sight")
      (vehicle/mounted? p) (result/stop! c :aboard "already on a vehicle")
      :else
      (let [r (await (b/place-cell! c cell item {:fetch true}))]
        (cond
          (= :continue r) :continue
          (not= :placed r)
          (if-let [text (place-stops r)]
            (result/stop! c r text :pos cell)
            (result/stop! c :failed (str "the boat was not placed: " (name r)) :primitive r :pos cell))
          :else
          (let [boat (placed-boat c cell)]
            (cond
              (nil? boat) (result/stop! c :no-boat "the boat is not in sight after placing it" :pos cell)
              (not board)
              (do (ctx/emit! c :boat.launched :info {:id (:id boat) :boat (:name boat) :pos cell :text (str "put " (:name boat) " on the water")})
                  (result/finish! c {:id (:id boat) :boat (:name boat) :pos cell :boarded false}))
              :else
              (let [m (await (ctx/call-child c :mount 'jobs.movement.mount {:id (:id boat)}))
                    res (ctx/child-result c :mount)]
                (cond
                  (= :continue m) :continue
                  (not= :done (:status res))
                  (result/stop! c :board-failed (str "could not get in the boat: " (name (or (:reason res) :declined)))
                                :cause (result/cause-of :mount res) :id (:id boat) :pos cell)
                  :else
                  (do (ctx/emit! c :boat.launched :info {:id (:id boat) :boat (:name boat) :pos cell :text (str "launched " (:name boat))})
                      (result/finish! c {:id (:id boat) :boat (:name boat) :pos cell :boarded true})))))))))))

(defn pick-boat [c]
  (let [id (:id (:args c))
        boats (boats-in-sight c)]
    (if id (first (filter #(= id (:id %)) boats)) (first boats))))

(defn ^:async recover! [c]
  (let [p (:primitives c)
        id (:id (:args c))
        deadline (+ (js/Date.now) (* 1000 (:max-s (:args c))))]
    (let [aboard (when (vehicle/mounted? p)
                   (await (ctx/call-child c :leave 'jobs.movement.leave-vehicle {})))]
      (cond
        (= :continue aboard) :continue
        (and (some? aboard) (not= :done aboard)) (result/stop! c :leave-failed "could not get off the boat")
        (and (some? aboard) (= :stopped (:status (ctx/child-result c :leave))))
        (result/stop! c :leave-failed "could not get off the boat" :cause (result/cause-of :leave (ctx/child-result c :leave)))
        :else
        (loop [hits 0 failed 0]
          (let [boat (pick-boat c)]
            (cond
              (nil? boat) (result/stop! c :gone (if (pos? hits) "the boat broke but its item was not seen" "no boat in sight") :id id)
              (>= (js/Date.now) deadline) (result/stop! c :not-broken (str (:name boat) " is still there after " hits " hits") :id (:id boat))
              (> (u/dist (u/self-pos c) (:pos boat)) reach)
              (let [w (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (:pos boat) :range 2 :escalate false}))]
                (cond
                  (= :continue w) :continue
                  (= :done w) (recur hits failed)
                  (>= (inc failed) 2) (result/stop! c :unreachable (str "could not walk within reach of the " (:name boat) ", " (int (u/dist (u/self-pos c) (:pos boat))) " cells away") :id (:id boat))
                  :else (recur hits (inc failed))))
              :else
              (let [r (await (ctx/act c :attack #js {:id (:id boat)}))
                    status (.-status r)]
                (if (= "killed" status)
                  (let [item (:name boat)
                        k (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                                 {:near (:pos boat) :radius 4 :filter [item]}))
                        n (:collected (ctx/child-result c :collect) 0)]
                    (cond
                      (= :continue k) :continue
                      (zero? n) (result/stop! c :not-collected (str "the " item " was not picked up") :id (:id boat))
                      :else (do (ctx/emit! c :boat.recovered :info {:id (:id boat) :item item :collected n :text (str "took back " item)})
                                (result/finish! c {:id (:id boat) :item item :collected n}))))
                  (recur (inc hits) failed))))))))))

(defn ^:async round [c]
  (case (or (:action (:args c)) :launch)
    :launch (await (launch! c))
    :recover (await (recover! c))
    (result/stop! c :bad-args (str "unknown :action " (:action (:args c))))))
