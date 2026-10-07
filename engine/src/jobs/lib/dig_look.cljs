(ns jobs.lib.dig-look
  "Looking after a dig: a body that digs into rock sees what it laid open only by turning to it. The helpers the digging
  jobs (stair, tunnel, dig-in, retreat) share; memory keys :dug-at and :settled are the calling job's."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]))

(defn add [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])

(defn unknown?
  "Whether the body has not sensed cell [x y z] (loaded, never seen or too old to trust)."
  [p [x y z]]
  (true? (some-> (u/sensed p {:x x :y y :z z}) .-unknown)))

(defn ^:async look-at!
  "Turn the head to cell's centre, so perception glances it and its 6 neighbours; nothing for primitives that do not
  sense (they read blockAt)."
  [c [x y z]]
  (when (some? (.-sensedAt (:primitives c)))
    (await (ctx/act c :look (clj->js {:pos {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}})))))

(def flow-delay-ms "One overworld lava flow delay (30 ticks), with a tick or two over." 2000)

(defn ^:async settle!
  "Before the body steps into the open cut: when a cell beside it is unseen, what is behind it shows only once it flows
  in. :continue until a flow delay has passed since the last dig (memory :dug-at), then :again once after a look at each
  cut cell (memory :settled); nil when nothing is unseen beside the cut or it has been looked at."
  [c cut]
  (let [open? (set cut)
        unseen (some #(unknown? (:primitives c) %) (remove open? (for [o cut d rules/neighbour-deltas] (add o d))))]
    (cond
      (not unseen) nil
      (< (ctx/now c) (+ (:dug-at (ctx/mem c) 0) flow-delay-ms)) :continue
      (= cut (:settled (ctx/mem c))) nil
      :else (do (ctx/update-mem! c assoc :settled cut)
                (loop [cells cut]
                  (when-let [cell (first cells)] (await (look-at! c cell)) (recur (rest cells))))
                :again))))

(defn ^:async see-round!
  "After a dig of cell: look into it, then at each neighbour still unknown, the faces the dig laid open."
  [c cell]
  (await (look-at! c cell))
  (loop [ds rules/neighbour-deltas]
    (when-let [d (first ds)]
      (let [n (add cell d)]
        (when (unknown? (:primitives c) n) (await (look-at! c n))))
      (recur (rest ds)))))
