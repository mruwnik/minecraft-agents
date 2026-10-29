(ns jobs.survival.respond.pillar
  "A cornered body's pillar (jobs.survival.respond-flight): where one fits, and building and raising it with jobs.access.pillar."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as lb]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.look :as lk]
            [jobs.lib.pillar :as pl]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.respond.state :as flight]))

(a/defargs settings
  {::pillar-height {:default 3 :doc "Height of the pillar built as a refuge, in blocks." :spec (a/int-in 0 nil)}
   ::pillar-spare {:default 3 :doc "Blocks a body must carry beyond the pillar's own for it to be the first refuge; with fewer the dig-down comes first (the night's dig-in cannot roof in on a pillar top without blocks)." :spec (a/int-in 0 nil)}
   ::pillar-extend {:default 2 :doc "Blocks a body hit on its pillar raises it by (up to ::pillar-raises times) before it gives the refuge up; 0: it gives up at once." :spec (a/int-in 0 nil)}
   ::pillar-raises {:default 3 :doc "Times a body hit on its pillar raises it before it gives the refuge up: a mob can stand on a block close under the top." :spec (a/int-in 0 nil)}
   ::climb-cells {:default 256 :doc "Cells climb-chance's search takes, likeliest first, before it takes the ground beyond for the world mobs roam: its search budget." :spec (a/int-in 0 nil)}})
(defn climb-cells [] (settings/get settings ::climb-cells))
(defn pillar-height [] (settings/get settings ::pillar-height))
(defn pillar-extend [] (settings/get settings ::pillar-extend))
(defn pillar-raises [] (settings/get settings ::pillar-raises))
(defn pillar-spare [] (settings/get settings ::pillar-spare))

(defn pillar-item
  "The carried block of the call's list with the most, when it is at least n (default pillar-height); else nil."
  ([c] (pillar-item c (pillar-height)))
  ([c n]
  (->> (lb/carried c (flight/blocks c))
       (filter #(>= (:count %) n))
       (sort-by :count >)
       first
       :name)))

(defn lean?
  "Whether a pillar could be built but would leave fewer than pillar-spare blocks."
  [c]
  (boolean (and (pillar-item c) (not (pillar-item c (+ (pillar-height) (pillar-spare)))))))

(defn climb-chance
  "The chance a mob climbs by steps of one block to stand beside the top of a pillar-height pillar at feet (levels 2 to 4
  above it: its hitbox then overlaps the body's): that of the likeliest way (the product of its cells' chances) down from
  there to a cell in mobs (where a mob stands) or to where the search runs out of ::climb-cells, likeliest first (the
  ground beyond joins the world mobs roam); 0 when the ground round the top is walled off from both. chance-of: cell ->
  [chance it is a floor, chance it is clear]."
  [chance-of feet mobs]
  (let [[x y z] feet
        standable (fn [cell] (if (mobs cell)
                               1
                               (* (first (chance-of (escape/up cell -1))) (second (chance-of cell)) (second (chance-of (escape/up cell 1))))))
        pillar? (fn [[cx _ cz]] (and (== cx x) (== cz z)))
        push (fn [[best todo] cell q] (if (> q (max 1e-6 (best cell 0))) [(assoc best cell q) (conj todo [(- q) cell])] [best todo]))
        tops (for [dx [-1 0 1] dz [-1 0 1] dy [2 3 4] :when (not= 0 dx dz)] [(+ x dx) (+ y dy) (+ z dz)])]
    (loop [[best todo] (reduce #(push %1 %2 (standable %2)) [{} (sorted-set)] tops) n 0]
      (if-let [[minus-q cell :as e] (first todo)]
        (let [q (- minus-q) todo (disj todo e)]
          (cond
            (< q (best cell)) (recur [best todo] n)
            (or (mobs cell) (>= n (climb-cells))) q
            :else (recur (reduce #(push %1 %2 (* q (standable %2)))
                                 [best todo]
                                 (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy [-1 0 1]
                                       :let [c (escape/add cell [dx dy dz])] :when (not (pillar? c))] c))
                         (inc n))))
        0))))

(defn seen-chance
  "climb-chance's chance-of over block-at, every cell read as it is."
  [block-at]
  (fn [cell] [(if (rules/solid-floor? block-at cell) 1 0) (if (pl/clear? (block-at cell)) 1 0)]))

(defn door-beside?
  "Whether a door, gate or trapdoor stands beside the feet or head cell: a column there would shut it."
  [block-at [x y z]]
  (boolean (some (fn [[dx dy dz]] (some-> (block-at [(+ x dx) (+ y dy) (+ z dz)]) reach/openable?))
                 (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy [0 1]] [dx dy dz]))))

(defn room?
  "Whether a pillar-height pillar has room here: a solid floor, the head cell and every cell the body rises into free, no
  door beside its foot (the column would shut it)."
  [block-at feet]
  (and (rules/solid-floor? block-at (escape/up feet -1))
       (not (door-beside? block-at feet))
       (every? #(pl/clear? (block-at (escape/up feet %))) (range 1 (+ 2 (pillar-height))))))

(defn pillar-ok?
  "Whether a pillar-height pillar fits here, every cell read as block-at gives it: room? and no way up for a mob from
  the open world (climb-chance)."
  [block-at feet]
  (and (room? block-at feet) (zero? (climb-chance (seen-chance block-at) feet #{}))))

(defn mob-cells
  "The cells the hostiles the body at primitives p knows of stand in."
  [p]
  (set (map #(let [{:keys [x y z]} (danger-q/mob-pos p %)] (mapv js/Math.floor [x y z]))
            (danger-q/known-hostiles p (danger-q/hostile-radius) {}))))

(defn pillar-climb
  "The chance a mob climbs to the top of a pillar where the body at primitives p stands at feet (climb-chance from the
  hostiles it knows, a cell it has not sensed solid by lk/unseen-solid-at-fn), nil when the pillar has no room? there
  (a cell it has not sensed read open: plan! looks at the rise first) or a mob surely climbs to its top."
  [p feet]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        seen (seen-chance block-at)
        solid-at (lk/unseen-solid-at-fn p)
        rise (set (map #(escape/up feet %) (range 1 (+ 2 (pillar-height)))))
        chance-of (memoize (fn [cell] (cond (not (unseen? cell)) (seen cell)
                                         (rise cell) [0 1]
                                         :else (let [s (solid-at cell)] [s (- 1 s)]))))]
    (when (room? (fn [cell] (if (unseen? cell) "air" (block-at cell))) feet)
      (let [q (climb-chance chance-of feet (mob-cells p))]
        (when (< q 1) q)))))

(defn pillar-fits?
  "Whether a pillar fits where the body at primitives p stands at feet (pillar-climb)."
  [p feet]
  (some? (pillar-climb p feet)))

(defn cell-map [[x y z]] {:x x :y y :z z})

(defn melee-hostiles
  "The hostiles seen within 4 that strike (not a shooter, a creeper or an enderman), as look/seen-entities maps."
  [p]
  (filter #(and (= "hostile" (:kind %)) (not (threat/ranged? %)) (not (#{"creeper" "enderman"} (:name %))))
          (lk/seen-entities p {:radius 4})))

(defn mob-class
  "The class of a pillar begun at feet now, for ::pillar-mob-fails: :beside when a melee hostile's box overlaps a cell the
  pillar fills (pl/mob-in: the server refuses a block there), else :clear."
  [p feet]
  (let [hs (melee-hostiles p)]
    (if (some #(pl/mob-in hs (escape/up feet %)) (range (pillar-height))) :beside :clear)))

(defn ^:async plan!
  "The pillar refuge ({:kind :pillar :item :anchor :cells :class}, :class mob-class as it begins) when a pillar-height pillar fits here, else nil. The cells it
  rises into that the body has not sensed are looked at first, so open sky is not taken for rock.
  A pillar through another's zone, claim or plan is planned only with trespass? (the last resort, after every way that keeps off)."
  [c trespass?]
  (let [p (:primitives c)
        feet (pl/feet-cell c)
        item (pillar-item c)]
    (when item
      (doseq [cell (map #(escape/up feet %) (range 1 (+ 2 (pillar-height))))]
        (when (look/unknown? p cell) (await (look/look-at! c cell)))))
    (when (and item (pillar-fits? p feet))
      (let [cells (mapv #(escape/up feet %) (range (pillar-height)))
            refusal (some #(access/trespass-refusal (access/rules-input c) :place (cell-map %)) cells)]
        (when (or trespass? (nil? refusal))
          (access/trespass! c "retreat" refusal)
          {:kind :pillar :item item :anchor (cell-map feet) :cells cells :class (mob-class p feet)})))))

(defn ^:async build!
  "A jobs.access.pillar child of height with item, from where the body stands: :continue, or {:built n :mob? m}, m
  whether it ended short for a mob (one in its cell, or a hostile near a refused block or a knockback: the give-up's
  :mob). A refused block is tried again while cost/try-again? :place says it pays; a knockback, or a refusal that names a mob, ends it at once as evidence for the next decision."
  [c height item]
  (let [r (await (ctx/call c :pillar 'jobs.access.pillar {:height height :use {:blocks [item]} :ways #{:carried} :ignore-zones? true
                                                          :settings {:jobs.access.pillar/mob-waits 0}}))]
    (if (contract/ending? r)
      {:built (:built (:data r) 0) :mob? (boolean (or (= :mob-in-cell (:reason (:data r))) (:mob (:data r))))}
      r)))

(defn ^:async step!
  "One round of the pillar: :again while it builds, :hidden once it ended at least 2 high, else :mob-failed when it
  ended short for a mob (build!), :failed for anything else."
  [c {:keys [item]}]
  (let [r (await (build! c (pillar-height) item))]
    (cond
      (= :continue r) :again
      (>= (:built r) 2) :hidden
      (:mob? r) :mob-failed
      :else :failed)))

(def max-knocks 6)

(defn ^:async strike-out!
  "A hostile stands in one of cells (the server refuses a block where a mob stands): strike it once, which knocks it out
  of the cell. True when it was struck, false when none is there or the knocks are used."
  [c cells]
  (let [p (:primitives c)
        hostiles (filter #(= "hostile" (:kind %)) (lk/seen-entities p {:radius 4}))
        mob (when (< (:knocks (ctx/mem c) 0) max-knocks)
              (some #(pl/mob-in hostiles %) cells))]
    (when mob
      (ctx/update-mem! c update :knocks (fnil inc 0))
      (let [{:keys [x y z]} (:pos mob)]
        (await (ctx/act c :look #js {:pos #js {:x x :y (+ y 1) :z z}}))
        (await (ctx/act c :attack #js {:id (:id mob)}))
        true))))

(defn ^:async knock-off!
  "strike-out! of the feet cell the pillar's next block goes into; the jump may still be in the air, so the cell under it counts too."
  [c]
  (let [feet (sh-sense/feet (:primitives c))]
    (await (strike-out! c (map (fn [y] [(:x feet) y (:z feet)]) [(:y feet) (dec (:y feet))])))))

(defn ^:async raise!
  "A pillared body that was hit: raise the pillar pillar-extend blocks from the top (its cells added to the refuge) when
  that many blocks are carried, up to pillar-raises times: :again when it rose, else :failed. A raise cut short for a
  mob (build!) is a seen failure of the pillar's class as it began (flight/seen-fail!); a hostile standing in the cell of the next block (a mob beside the top
  overlaps it) is struck out of it."
  [c]
  (let [{:keys [kind cells] :as refuge} (:refuge (ctx/mem c))
        n (pillar-extend)
        item (when (= :pillar kind) (pillar-item c n))
        top (peek cells)]
    (if-not (and item (pos? n) (< (:raised refuge 0) (pillar-raises)))
      :failed
      (let [r (await (build! c n item))
            built (:built r 0)]
        (when (:mob? r)
          (await (knock-off! c))
          (flight/seen-fail! c :pillar (:class refuge)))
        (ctx/update-mem! c update :refuge assoc :raised (inc (:raised refuge 0)) :cells (into cells (take built (mapv #(escape/up top %) (range 1 (inc n))))))
        (if (pos? built) :again :failed)))))
