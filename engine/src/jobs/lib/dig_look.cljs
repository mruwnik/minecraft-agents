(ns jobs.lib.dig-look
  "Looking after a dig: a body that digs into rock sees what it laid open only by turning to it. The helpers the digging
  jobs (stair, tunnel, dig-in, retreat) share; memory keys :dug-at and :settled are the calling job's."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::flow-delay-ticks {:default 34 :spec (a/int-in 1 nil)
                       :doc "One overworld lava flow delay (30 game ticks) with a few ticks over, in game ticks."}
   ::settle-wait-ms {:default 500 :spec (a/int-in 0 nil) :doc "One wait while a flow delay runs (wait-settled!), in ms."}
   ::settle-waits {:default 6 :spec (a/int-in 0 nil) :doc "Most waits of wait-settled!."}
   ::rock-depth {:default 3 :spec (a/int-in 0 nil) :doc "How far behind a seen solid face a cell the body has not sensed is taken for rock."}})

(defn flow-delay-ticks [] (settings/get settings ::flow-delay-ticks))
(defn settle-wait-ms [] (settings/get settings ::settle-wait-ms))
(defn rock-depth [] (settings/get settings ::rock-depth))
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

(def neighbours rules/neighbour-deltas)

(defn shift [{:keys [x y z]} [dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})


(def loose-ground #{"sand" "red_sand" "gravel"})

(def soil #{"dirt" "grass_block" "coarse_dirt" "podzol" "mycelium" "rooted_dirt"})

(defn rock-name
  "The block name at cell as a player reads rock: what the body sees or remembers, else for a loaded cell it has
  not sensed the nearest seen solid block behind it within rock-depth cells along some axis (as loose-ground: sand and
  gravel are dug by hand and fall; soil reads \"dirt\") or \"stone\" (any other), when no neighbour is seen open (an open neighbour would have
  shown it; cells in except, the way in, are not counted). nil when it is not loaded or no seen face is near. The shelter
  plans (pit, pocket, niche) read the rock they dig through this way, then dig to see (jobs.lib.dig-look)."
  ([p cell] (rock-name p cell #{}))
  ([p cell except]
  (or (u/seen-name p cell)
      (when (some-> (u/sensed p cell) .-unknown)
        (let [name-at (fn [d k] (u/seen-name p (shift cell (mapv #(* k %) d))))
              behind (fn [d] (first (filter solid/solid? (take 1 (remove nil? (map #(name-at d %) (range 1 (inc (rock-depth)))))))))
              nearest (->> neighbours (keep (fn [d] (when-let [n (behind d)] [(count (take-while #(nil? (name-at d %)) (range 1 (inc (rock-depth))))) n]))) sort first second)]
          (when (and (every? #(or (nil? %) (solid/solid? %)) (map #(when-not (except (shift cell %)) (name-at % 1)) neighbours))
                     nearest)
            (cond (loose-ground nearest) nearest (soil nearest) "dirt" :else "stone")))))))

(defn rock-solid?
  "Whether cell is solid rock as the body reads it (rock-name)."
  [p cell]
  (solid/solid? (rock-name p cell)))

(def max-floor-looks "Most looks of look-unknown!." 5)

(defn ^:async look-unknown!
  "Look at each of cells ([x y z], nearest first) the body has not sensed, at most max-floor-looks looks; a look shows
  the cells round the one looked at too, so those are tested again before each look. For floors a judge of standable
  cells needs seen."
  [c cells]
  (let [p (:primitives c)]
    (loop [todo (seq cells) n 0]
      (when (and todo (< n max-floor-looks))
        (if (unknown? p (first todo))
          (do (await (look-at! c (first todo)))
              (recur (next todo) (inc n)))
          (recur (next todo) n))))))

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
