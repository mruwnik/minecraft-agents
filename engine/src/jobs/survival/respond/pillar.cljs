(ns jobs.survival.respond.pillar
  "A cornered body's pillar (jobs.survival.respond-flight): where one fits, and building and raising it with jobs.access.pillar."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as lb]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.pillar :as pl]
            [jobs.survival.respond.state :as flight]))

(a/defargs settings
  {::pillar-height {:default 3 :doc "Height of the pillar built as a refuge, in blocks." :spec (a/int-in 0 nil)}
   ::pillar-spare {:default 3 :doc "Blocks a body must carry beyond the pillar's own for it to be the first refuge; with fewer the dig-down comes first (the night's dig-in cannot roof in on a pillar top without blocks)." :spec (a/int-in 0 nil)}
   ::pillar-extend {:default 2 :doc "Blocks a body hit on its pillar raises it by, once, before it gives the refuge up; 0: it gives up at once." :spec (a/int-in 0 nil)}
   ::climb-radius {:default 4 :doc "Blocks round the pillar within which a mob's climb to its top is looked for." :spec (a/int-in 0 nil)}})
(defn climb-radius [] (settings/get settings ::climb-radius))
(defn pillar-height [] (settings/get settings ::pillar-height))
(defn pillar-extend [] (settings/get settings ::pillar-extend))
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

(defn climbable-top?
  "Whether a mob could walk up a slope or ledge (steps of one block, over seen blocks) to stand beside the pillar's top
  (levels 2 to 4 above the start: its hitbox then overlaps the body's)."
  [block-at feet]
  (let [[x y z] feet
        standable? (fn [cell] (and (rules/solid-floor? block-at (escape/up cell -1))
                                   (pl/clear? (block-at cell))
                                   (pl/clear? (block-at (escape/up cell 1)))))
        near? (fn [[cx _ cz]] (and (<= (abs (- cx x)) (climb-radius)) (<= (abs (- cz z)) (climb-radius))))
        beside-top? (fn [[cx cy cz]] (and (<= (abs (- cx x)) 1) (<= (abs (- cz z)) 1) (not= [x z] [cx cz]) (<= (+ y 2) cy (+ y 4))))
        start (filter standable? (for [dx (range (- (climb-radius)) (inc (climb-radius))) dz (range (- (climb-radius)) (inc (climb-radius)))
                                       dy [0 1] :when (not= 0 dx dz)]
                                   [(+ x dx) (+ y dy) (+ z dz)]))]
    (loop [seen (set start) todo (vec start)]
      (let [cell (peek todo)]
        (cond
          (nil? cell) false
          (beside-top? cell) true
          :else
          (let [[cx cy cz] cell
                nxt (->> (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy [-1 0 1]] [(+ cx dx) (+ cy dy) (+ cz dz)])
                         (filter #(and (near? %) (not (seen %)) (not= [x z] [(first %) (nth % 2)]) (standable? %))))]
            (recur (into seen nxt) (into (pop todo) nxt))))))))

(defn pillar-ok?
  "Whether a pillar-height pillar fits here: a solid floor, the head cell and every cell the body rises into free, and
  no ledge beside its top a mob could climb to."
  [block-at feet]
  (and (rules/solid-floor? block-at (escape/up feet -1))
       (every? #(pl/clear? (block-at (escape/up feet %))) (range 1 (+ 2 (pillar-height))))
       (not (climbable-top? block-at feet))))

(defn cell-map [[x y z]] {:x x :y y :z z})

(defn unseen-zone
  "The cells the climb check reads (climb-radius round the pillar, from the floor to above its top) that the body has not sensed, nearest first."
  [p [x y z]]
  (let [r (climb-radius)]
    (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy (range -1 (+ 2 (pillar-height)))
               :let [cell [(+ x dx) (+ y dy) (+ z dz)]]
               :when (look/unknown? p cell)]
           [(+ (* dx dx) (* dz dz) (* dy dy)) cell])
         (sort-by first)
         (mapv second))))

(defn ^:async plan!
  "The pillar refuge ({:kind :pillar :item :anchor :cells}) when a pillar-height pillar fits here, else nil. The cells it
  rises into that the body has not sensed are looked at first, so open sky is not taken for rock; so are the unsensed cells
  round it when the pillar does not fit, so unseen ground behind the body is not taken for a ledge a mob climbs.
  A pillar through another's zone, claim or plan is planned only with trespass? (the last resort, after every way that keeps off)."
  [c trespass?]
  (let [p (:primitives c)
        feet (pl/feet-cell c)
        item (pillar-item c)]
    (when item
      (doseq [cell (map #(escape/up feet %) (range 1 (+ 2 (pillar-height))))]
        (when (look/unknown? p cell) (await (look/look-at! c cell)))))
    (when (and item (not (pillar-ok? (escape/block-at-of p) feet)))
      (when-let [zone (not-empty (unseen-zone p feet))]
        (await (look/look-unknown! c zone))))
    (when (and item (pillar-ok? (escape/block-at-of p) feet))
      (let [cells (mapv #(escape/up feet %) (range (pillar-height)))
            refusal (some #(access/trespass-refusal (access/rules-input c) :place (cell-map %)) cells)]
        (when (or trespass? (nil? refusal))
          (access/trespass! c "retreat" refusal)
          {:kind :pillar :item item :anchor (cell-map feet) :cells cells})))))

(defn ^:async build!
  "A jobs.access.pillar child of height with item, from where the body stands: :continue, or the blocks it built."
  [c height item]
  (let [r (await (ctx/call c :pillar 'jobs.access.pillar {:height height :use {:blocks [item]} :fetch false :ignore-zones? true :settings {:jobs.access.pillar/mob-waits 0}}))]
    (if (contract/ending? r) (:built (:data r) 0) r)))

(defn ^:async step!
  "One round of the pillar: :again while it builds, :hidden once it ended at least 2 high, else :failed."
  [c {:keys [item]}]
  (let [r (await (build! c (pillar-height) item))]
    (cond
      (= :continue r) :again
      (>= r 2) :hidden
      :else :failed)))

(defn ^:async raise!
  "A pillared body that was hit: once, raise the pillar pillar-extend blocks from the top (its cells added to the
  refuge) when that many blocks are carried: :again when it rose, else :failed."
  [c]
  (let [{:keys [kind cells] :as refuge} (:refuge (ctx/mem c))
        n (pillar-extend)
        item (when (= :pillar kind) (pillar-item c n))
        top (peek cells)]
    (if-not (and item (pos? n) (not (:raised refuge)))
      :failed
      (let [r (await (build! c n item))
            built (if (number? r) r 0)]
        (ctx/update-mem! c update :refuge assoc :raised true :cells (into cells (take built (mapv #(escape/up top %) (range 1 (inc n))))))
        (if (pos? built) :again :failed)))))
