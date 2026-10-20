(ns jobs.gather.mine-stay
  "The :stay-level option of jobs.gather.dig-for: ore only beside the body, and a cut that opens into a cave sealed again."
  (:require [engine.args :as a]
            [jobs.lib.choose :as choose]
            [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.bridge :as bridge]
            [jobs.lib.ledger :as ledger]))

(a/defargs settings
  {::ore-reach {:default 2 :doc "With :stay-level: seen ore this many blocks into the wall beside the tunnel is taken by a short side cut (a child dig run), shut behind with a carried block; 0 or 1: only ore beside the body." :spec (a/int-in 0 nil)}})

(defn record-tunnel!
  "jobs.lib.ledger/record-tunnel! for the tunnel map of a strip run."
  [c {:keys [origin steps]} delta]
  (ledger/record-tunnel! c [(:x origin) (:y origin) (:z origin)] delta steps))

(defn beside-body?
  "Whether the target is one column or less from the body's feet cell, from one below it to one over the head (the cell under the feet itself never)."
  [body target]
  (let [dx (- (:x target) (:x body))
        dz (- (:z target) (:z body))
        dy (- (:y target) (:y body))]
    (and (<= (abs dx) 1) (<= (abs dz) 1)
         (or (<= 0 dy 2) (and (= -1 dy) (not (and (zero? dx) (zero? dz))))))))

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

(defn side-face
  "The cell of a side cut (cut: side-cut's result with :pos, the ore) that lies against the ore: the dig opens it, so it is the ore's face."
  [body {:keys [pos] :as cut}]
  (first (filter #(= (:y pos) (:y %)) (take-last 2 (side-cells body cut)))))

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
  (let [item (choose/filler c :build)
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

(def behind-policy {:cap 30 :ttl :forever})

(defn behind
  "What the breach cells (of cut, from the cell the body stands in) open onto: {:names sorted distinct, :cells [x y z] sorted} of the
  seen air and fluid cells beside them that are not the cut, the body's cells or the own cells; nil when none."
  [name-at from cut own breach]
  (let [open (into #{from (update from :y inc)} cut)
        cells (into (sorted-set)
                    (for [cell breach d around
                          :let [n (mapv + [(:x cell) (:y cell) (:z cell)] d)
                                q {:x (n 0) :y (n 1) :z (n 2)}]
                          :when (and (not (open q)) (not (own n)) (let [nm (name-at q)] (or (rules/air nm) (rules/fluids nm))))]
                      n))]
    (when (seq cells)
      {:names (vec (sort (distinct (map (fn [[x y z]] (name-at {:x x :y y :z z})) cells)))) :cells (vec cells)})))

(defn remember-behind!
  "Keep what a breach showed in body memory (:breach-behind: dim, the breach cell at, names, cells, the lava cells among them, the sealed breach
  cells), for jobs that look for caves or lava."
  [c at b name-at seals]
  (when b
    (ctx/remember! c :breach-behind
                   (assoc b :dim (.-dimension (.self (:primitives c))) :at at :seals (mapv (juxt :x :y :z) seals)
                          :lava (filterv (fn [[x y z]] (= "lava" (name-at {:x x :y y :z z}))) (:cells b)))
                   behind-policy)))
