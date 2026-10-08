(ns jobs.movement.go-to.torch
  "go-to :unknown's off-hand torch: held in the dark to see the floors ahead, put back before a child runs and when the
  call ends. The hold is written to body memory, so the next call returns a torch a cut call left in the hand."
  (:require [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(def kind :go-to/off-hand-torch)

(defn off-hand-torch? [c]
  (= "torch" (some-> (.-equipment (.self (:primitives c))) .-offHand .-name)))

(defn torch-aid?
  "Whether a masked round (walked) should hold a carried torch in the off hand to see deeper: its plan refused a dark floor or
  checks unseen ones, the body's feet are dark, it carries a torch, the off hand is empty, and this call has not tried."
  [c walked]
  (let [p (:primitives c)
        {:keys [x y z]} (wworld/body-cell c)
        lit? (look/see-lit-fn p)]
    (boolean (and (:unknown c) (or (:dark-best walked) (:unseen-floors walked))
                  (not (:off-hand-tried (ctx/mem c)))
                  lit? (zero? (lit? x y z))
                  (some #(= "torch" (:name %)) (u/inventory p))
                  (nil? (some-> (.-equipment (.self p)) .-offHand))))))

(defn ^:async hold-torch!
  "Hold a carried torch in the off hand (once a call; info go-to.off-hand-torch; recorded in body memory): true when it is held there now."
  [c]
  (ctx/update-mem! c assoc :off-hand-tried true)
  (let [r (await (ctx/act c :equip #js {:item "torch" :dest "off-hand"}))
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
