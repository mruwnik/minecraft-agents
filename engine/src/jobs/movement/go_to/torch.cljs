(ns jobs.movement.go-to.torch
  "go-to :unknown's off-hand torch: held in the dark to see the floors ahead, put back before a child runs and when the
  call ends. The hold is written to body memory, so the next call returns a torch a cut call left in the hand.

  Rules of go-to:
  - :unknown true (the default) plans over what the body has seen or felt only: a cell it never saw is unknown (air to
    the body), a floor it never saw is stepped onto only after the walk stops to look at it (jobs.lib.walk.sight), and
    only where it can see it from the takeoff (a drop deeper than it sees in the dark is refused). An unseen y goal
    behind a seen solid, or one a found plan tours round (jobs.movement.go-to.walk/lookout-check), is first looked at
    from the nearest seen stand within 4 of it that sees it (info go-to.lookout, at most 3 stands a call). The prices are the settings of jobs.lib.cost.planner (::unknown-prices). When a dark drop was the better way it gives up :why :unseen-floor-dark; in the
    dark with a torch carried and the off hand empty it first holds the torch in the off hand (info go-to.off-hand-torch,
    once a call: it sees deeper) and plans again; it puts the torch back into the inventory before an escalation child runs and when the call ends (full: it
    stays, info go-to.off-hand-kept); one a cut call left in the hand goes back at the next call's start.
  - :torches :carried or :fetch (with :unknown; default :carried) may light such a dark floor instead: the planner prices a torch
    (jobs.lib.cost/light-cost) where it can stand on the takeoff, in no other owner's zone or
    claim; the walk stops there, places one (an off-hand stack goes back to the inventory first; info walk.torch-placed) and
    walks on once it sees the floor. With none carried, :fetch fetches torches once a call (a jobs.lib.need child);
    with none and :carried it gives up :unseen-floor-dark saying so. A torch that cannot go there is not tried there again."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.torch :as ltorch]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.result :as end]))

(def kind :go-to/off-hand-torch)

(def torches
  "The items that let the body see deeper in the dark from a hand (perception's near-torch), the first one carried is held."
  ["torch" "soul_torch"])

(defn off-hand-torch? [c]
  (boolean (some #{(some-> (.-equipment (.self (:primitives c))) .-offHand .-name)} torches)))

(defn carried-torch
  "The name of the first of torches the body carries, else nil."
  [p]
  (let [names (set (map :name (u/inventory p)))]
    (some names torches)))

(defn torch-aid?
  "Whether a masked round (walked) should hold a carried torch in the off hand to see deeper: its plan refused a dark floor or
  checks unseen ones, the body's feet are dark, it carries a torch, the off hand is empty, and this call has not tried."
  [c walked]
  (boolean (and (:unknown c) (or (:dark-best walked) (:unseen-floors walked))
                (not (:off-hand-tried (ctx/mem c)))
                (let [p (:primitives c)
                      {:keys [x y z]} (wworld/body-cell c)
                      lit? (look/see-lit-fn p)]
                  (and lit? (zero? (lit? x y z))
                       (carried-torch p)
                       (nil? (some-> (.-equipment (.self p)) .-offHand)))))))

(defn ^:async hold-torch!
  "Hold a carried torch (torches) in the off hand (once a call; info go-to.off-hand-torch; recorded in body memory): true when it is held there now."
  [c]
  (ctx/update-mem! c assoc :off-hand-tried true)
  (let [r (await (ctx/act c :equip #js {:item (or (carried-torch (:primitives c)) "torch") :dest "off-hand"}))
        held? (off-hand-torch? c)]
    (when held?
      (ctx/update-mem! c assoc :off-hand-held true)
      (ctx/remember! c kind {:held true} {:cap 1 :ttl :forever})
      (ctx/emit! c :go-to.off-hand-torch :info {:text "holding a torch in the off hand to see the floors ahead in the dark"}))
    (when-not held?
      (ctx/emit! c :go-to.off-hand-torch :info {:held false :equip (some-> r .-status) :text "could not hold a torch in the off hand"}))
    held?))

(defn ^:async return-torch!
  "The torch go-to holds in the off hand (this call's, or one recorded in body memory by a call that was cut) goes back
  into the inventory and the record goes; with no room it stays (info go-to.off-hand-kept)."
  [c]
  (when (or (:off-hand-held (ctx/mem c)) (ctx/latest c kind))
    (ctx/update-mem! c dissoc :off-hand-held)
    (when (off-hand-torch? c)
      (let [r (await (ctx/act c :unequip #js {:dest "off-hand"}))]
        (when-not (= "ok" (some-> r .-status))
          (ctx/emit! c :go-to.off-hand-kept :info {:status (some-> r .-status) :text "the torch stays in the off hand: no room to put it back"}))))
    (when-not (off-hand-torch? c)
      (ctx/forget-where! c kind (constantly true)))))

(def fetch-failed-kind "Body memory: the last time a torch fetch failed, so a later call does not try again at once." :go-to/torch-fetch-failed)
(def fetch-failed-ttl-ms 600000)
(def torch-count "How many torches a fetch asks for." 4)

(defn fetch-opts
  "The need options of the torch fetch: nil unless :torches is :fetch."
  [c]
  (when (= :fetch (:torches (:args c)))
    {:how need/sources :slot :need-torch}))

(defn torch-fetching? [c] (boolean (:torch-fetching (ctx/mem c))))

(defn torch-fetch-open?
  "Whether :torches :fetch may still fetch torches this call: none was fetched yet, none failed lately."
  [c]
  (boolean (and (fetch-opts c) (not (:torch-fetched (ctx/mem c))) (nil? (ctx/latest c fetch-failed-kind)))))

(defn light-ok
  "The planner's lightOk: (fn [x y z]) true where a torch may go at a takeoff (x y z its feet): the feet or the head cell
  in no other owner's zone or claim (none under :ignore-zones?); jobs.lib.walk.light/light! places it in the one allowed."
  [c]
  (let [in (access/zone-input c)
        ok? (fn [cell] (true? (:ok (zones/verdict (assoc in :action :place :cell cell)))))]
    (fn [x y z] (or (:ignore-zones? in) (ok? [x y z]) (ok? [x (inc y) z])))))

(defn round-unknown
  "The mask spec of this round (c's :unknown): with :torches not :never and a torch carried or a fetch still open, torches on
  (wworld/with-light), priced by jobs.lib.cost/light-cost. After a masked search ran out of nodes (mem :edge-mode) the mask
  reads unseen cells as the loaded edge instead (:edge): the walk tours to the edge of what the body has seen. After a
  search ran out of land a buried body's rock guess closed (mem :rock-edge), the guessed rock reads as the loaded edge
  (:rockEdge, wworld/path-world)."
  [c]
  (let [u (cond-> (:unknown c) (and (:unknown c) (:rock-edge (ctx/mem c))) (#(js/Object.assign #js {} % #js {:rockEdge true})))
        n (ltorch/torches-held (:primitives c))]
    (cond
      (and u (:edge-mode (ctx/mem c))) (js/Object.assign #js {} u #js {:edge true})
      (and u (not= :never (:torches (:args c) :never)) (or (pos? n) (torch-fetch-open? c)))
      (wworld/with-light u (:cost (cost/light-cost {:carried n})) (light-ok c))
      :else u)))

(defn ^:async fetch-torches!
  "A walk that reached a floor to light with no torch carried: fetch torches once a call (jobs.lib.need, a child),
  :continue while it waits, else :again (the next round plans without torches when none came; a failure is booked in
  body memory for fetch-failed-ttl-ms)."
  [c]
  (if (or (torch-fetching? c) (torch-fetch-open? c))
    (do (ctx/update-mem! c assoc :torch-fetched true :torch-fetching true)
        (let [e (await (need/item! c "torch" torch-count (fetch-opts c)))]
          (when-not (= :continue e)
            (ctx/update-mem! c dissoc :torch-fetching)
            (when-not (= :done (:kind e))
              (ctx/remember! c fetch-failed-kind {:why (some-> e :cause :why)} {:cap 1 :ttl fetch-failed-ttl-ms})))
          (if (= :continue e) :continue :again)))
    :again))

(defn none-held?
  "Whether the body carries no torch."
  [c]
  (zero? (ltorch/torches-held (:primitives c))))

(defn dark-end
  "A fruitless round's result as go-to gives it up when a dark way was the better way (:dark-best, jobs.lib.near/dark-best):
  :no-path :unseen-floor-dark with that cell (and :torches :none or :fetch-failed when :torches had no torch), unless the
  round failed for a reason of its own (stuck, health, air, ...)."
  [c result]
  (if (end/dark-end? result)
    (cond-> {:status :no-path :reason :unseen-floor-dark :dark-best (:dark-best result)}
      (and (not= :never (:torches (:args c) :never)) (zero? (ltorch/torches-held (:primitives c))))
      (assoc :torches (if (ctx/latest c fetch-failed-kind) :fetch-failed :none)))
    result))
