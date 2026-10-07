(ns jobs.lib.dig-look
  "Looking after a dig: a body that digs into rock sees what it laid open only by turning to it. The helpers the digging
  jobs (stair, tunnel, dig-in, retreat) share; memory keys :dug-at and :settled are the calling job's."
  (:require [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]))

(def settings
  {::flow-delay-ticks {:default 34 :type :int :min 1
                       :doc "One overworld lava flow delay (30 game ticks) with a few ticks over, in game ticks."}
   ::settle-wait-ms {:default 500 :type :int :min 0 :doc "One wait while a flow delay runs (wait-settled!), in ms."}
   ::settle-waits {:default 6 :type :int :min 0 :doc "Most waits of wait-settled!."}})

(defn flow-delay-ticks [] (settings/get settings ::flow-delay-ticks))
(defn settle-wait-ms [] (settings/get settings ::settle-wait-ms))
(defn settle-waits [] (settings/get settings ::settle-waits))

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

(defn flow-delay-ms
  "One overworld lava flow delay in wall ms: 30 game ticks at the live game rate, with a few ticks over."
  []
  (settings/ticks->ms (flow-delay-ticks)))

(defn ^:async settle!
  "Before the body steps into the open cut: when a cell beside it is unseen, what is behind it shows only once it flows
  in. :continue until a flow delay has passed since the last dig (memory :dug-at), then :again once after a look at each
  cut cell (memory :settled); nil when nothing is unseen beside the cut or it has been looked at."
  [c cut]
  (let [open? (set cut)
        unseen (some #(unknown? (:primitives c) %) (remove open? (for [o cut d rules/neighbour-deltas] (add o d))))]
    (cond
      (not unseen) nil
      (< (ctx/now c) (+ (:dug-at (ctx/mem c) 0) (flow-delay-ms))) :continue
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

(defn ^:async wait-settled!
  "settle! for a job that holds still while it runs: waits (act :wait) until the flow delay since the last dig has passed
  and the cut was looked at again, at most (settle-waits) times. Truthy while settle! still asks for more."
  [c cut]
  (loop [i 0]
    (when (< i (settle-waits))
      (when (await (settle! c cut))
        (await (ctx/act c :wait #js {:ms (settle-wait-ms) :why "letting what the dig opened settle"}))
        (recur (inc i))))))
