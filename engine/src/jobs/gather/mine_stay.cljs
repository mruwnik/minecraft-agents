(ns jobs.gather.mine-stay
  "The :stay-level option of jobs.gather.mine: ore only beside the body, and a cut that opens into a cave sealed again."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.bridge :as bridge]))

(def tunnel-way-policy {:cap 30 :ttl :forever})

(defn record-tunnel!
  "Remember the tunnel this run cut (memory :tunnel-way: dim, origin feet cell [x y z], delta [dx dz], steps), so a later stay-level run
  does not take its air for a cave."
  [c {:keys [origin steps]} delta]
  (when (pos? steps)
    (ctx/remember! c :tunnel-way {:dim (.-dimension (.self (:primitives c))) :origin [(:x origin) (:y origin) (:z origin)]
                                  :delta (vec delta) :steps steps}
                   tunnel-way-policy)))

(defn own-cells
  "The cells of the passages this body cut itself in its dimension: feet and head of each step of its stairs (memory :stair-way) and of its tunnels (:tunnel-way), as [x y z]."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))
        here? #(= dim (:dim (:data %)))
        stairs (for [e (ctx/entries c :stair-way) :when (here? e)
                     [x y z] (:floors (:data e)) dy [1 2]]
                 [x (+ y dy) z])
        tunnels (for [e (ctx/entries c :tunnel-way) :when (here? e)
                      :let [{[x y z] :origin [dx dz] :delta :keys [steps]} (:data e)]
                      k (range (inc steps)) dy [0 1]]
                  [(+ x (* k dx)) (+ y dy) (+ z (* k dz))])]
    (set (concat stairs tunnels))))

(defn beside-body?
  "Whether the target is one column or less from the body's feet cell and at its feet or head height."
  [body target]
  (and (<= (abs (- (:x target) (:x body))) 1)
       (<= (abs (- (:z target) (:z body))) 1)
       (<= 0 (- (:y target) (:y body)) 1)))

(def side-deltas {"north" [0 -1] "south" [0 1] "east" [1 0] "west" [-1 0]})

(defn side-cut
  "How to reach the ore at pos in the wall beside a tunnel running along heading [dx dz] from the body's feet cell: {:heading name :steps n}
  for a side cut of n blocks, straight out from the body, that ends beside it; nil when the ore does not lie in that line 2 to reach
  blocks to the side, or off the tunnel's two heights."
  [body [dx dz] reach pos]
  (let [x? (not (zero? dx))
        lat (if x? (- (:z pos) (:z body)) (- (:x pos) (:x body)))
        along (if x? (- (:x pos) (:x body)) (- (:z pos) (:z body)))]
    (when (and (zero? along) (<= 2 (abs lat) reach) (<= 0 (- (:y pos) (:y body)) 1))
      {:heading (cond (and x? (pos? lat)) "south" x? "north" (pos? lat) "east" :else "west")
       :steps (dec (abs lat))})))

(defn side-cells
  "The cells a side cut of the body at feet cell body digs: feet and head of each step."
  [body {:keys [heading steps]}]
  (let [[dx dz] (side-deltas heading)]
    (for [k (range 1 (inc steps)) dy [0 1]]
      {:x (+ (:x body) (* k dx)) :y (+ (:y body) dy) :z (+ (:z body) (* k dz))})))

(def around [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(defn breaches
  "The cells of cut (feet cell first) that touch seen air other than the cells of the cut, the cell the body stands in (from) and the one over it, and the own cells ([x y z] set)."
  [name-at from cut own]
  (let [open (into #{from (update from :y inc)} cut)]
    (filterv (fn [cell] (some #(let [n (mapv + [(:x cell) (:y cell) (:z cell)] %)
                                     q {:x (n 0) :y (n 1) :z (n 2)}]
                                 (and (not (open q)) (not (own n)) (rules/air (name-at q))))
                              around))
             cut)))

(defn ^:async seal!
  "Place a carried building block in each of cells: true when they all stand, false with none carried or a failed place. in: the rules input."
  [c in cells]
  (let [item (blocks/pick c blocks/building-blocks)
        block-at (:block-at in)]
    (loop [[cell & more] cells]
      (cond
        (nil? item) false
        (nil? cell) true
        :else (let [pos [(:x cell) (:y cell) (:z cell)]
                    plans (bridge/bridge-plans block-at pos (bridge/eye-y c))]
                (if (and (seq plans) (nil? (await (bridge/place-plans! c in item plans false))))
                  (recur more)
                  false))))))
