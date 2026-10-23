(ns jobs.lib.bridge
  "Placing a carried block on the air floor of a step (a stair, a tunnel) against a neighbour's face."
  (:require [jobs.lib.block-sets :as bs]
            [engine.game :as game]
            [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.spent :as spent]
            [jobs.lib.util :as u]))

(def face-steps [[1 0 0] [-1 0 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn eye-y [c] (+ (:y (u/self-pos c)) game/eye-height))

(defn face-support?
  "Whether a block can be placed against a face of cell: some neighbour is solid (seen), or unseen when loose?. The block
  over the cell counts only from below its underside (eye-y under the cell's top): above it the face cannot be aimed at."
  [block-at cell loose? eye-y]
  (let [solid? (fn [d] (let [n (block-at (mapv + cell d))]
                         (if (nil? n) loose? (not (bs/replaceable n)))))]
    (boolean (or (some solid? face-steps)
                 (and (< eye-y (inc (second cell))) (solid? [0 1 0]))))))

(defn chain-plan
  "The cells to place, farthest first, to reach floor from a solid cell up to depth steps back along back ([dx dz]) at the
  floor's height: every cell between must be air, the support seen solid. nil when there is none."
  [block-at floor {:keys [back depth]}]
  (let [at (fn [k] (mapv + floor [(* k (first back)) 0 (* k (second back))]))
        solid? (fn [n] (and n (not (bs/replaceable n))))]
    (when back
      (some (fn [j]
              (when (and (solid? (block-at (at j))) (every? #(bs/open-floor? (block-at (at %))) (range j)))
                (mapv at (reverse (range j)))))
            (range 1 (inc depth))))))

(defn bridge-plans
  "The cell lists to place for a missing floor, to try in order: [floor] when it has a face to place against, [support
  floor] when only the cell under it has one (the previous floor of a stair up), the cells back along opts' :back (with
  :depth) from a solid one (a stair down whose cave undercuts its last floor). Cells with a seen face come before those
  whose face is an unseen neighbour, which may be air (place answers no-support). Only cells that are air are placed; a
  floor in a cave with nothing near has no plan. opts :seen-only drops the unseen-face plans. eye-y: the body's eye height."
  ([block-at floor eye-y] (bridge-plans block-at floor eye-y nil))
  ([block-at floor eye-y opts]
   (let [under (update floor 1 dec)
         plans (fn [loose?]
                 (for [[probe plan] [[floor [floor]] [under [under floor]]]
                       :when (face-support? block-at probe loose? eye-y)]
                   (filterv #(bs/open-floor? (block-at %)) plan)))]
     (vec (distinct (concat (plans false) (some-> (chain-plan block-at floor opts) vector) (when-not (:seen-only opts) (plans true))))))))

(defn flat-run
  "The length (1 to max) of a flat bridge along h ([dx dz]) from feet, after which a stair down's next floor is solid or has
  a seen face to place against (block-at nil: unseen): every floor of the run open or solid with room over it, the first
  floor open (the ledge ends there). nil when none comes within max or a cell is unsensed."
  [block-at feet h eye-y opts max]
  (let [at (fn [k dy] (mapv + feet [(* k (first h)) dy (* k (second h))]))
        clear? (fn [cell] (let [n (block-at cell)] (and n (bs/replaceable n) (not (bs/fluids n)))))
        floor-ok? (fn [k] (let [n (block-at (at k -1))] (and n (or (bs/open-floor? n) (rules/solid-floor? block-at (at k -1))))))]
    (when (bs/open-floor? (block-at (at 1 -1)))
      (loop [k 1]
        (when (and (<= k max) (floor-ok? k) (clear? (at k 0)) (clear? (at k 1)))
          (let [next-floor (at (inc k) -2)
                n (block-at next-floor)]
            (if (and n (or (not (bs/open-floor? n)) (seq (bridge-plans block-at next-floor eye-y (assoc opts :seen-only true)))))
              k
              (recur (inc k)))))))))

(defn ^:async place-floor!
  "Place item at cell (in the scaffold ledger when take-back?): nil when it stands, else a stop map. in: the rules input."
  [c in item cell take-back?]
  (let [verdict (rules/may-place? (assoc in :cell cell))
        [x y z] cell]
    (if-not (:ok verdict)
      (assoc (dissoc verdict :ok) :cell cell)
      (let [l (when take-back?
                (ledger/intend (ledger/open-entries (ctx/view c))
                               {:cell cell :item item :before ((:block-at in) cell) :job (:id c) :purpose :stair-floor}))
            _ (when l (ledger/remember! c l))
            r (await (ctx/act c :place #js {:pos #js {:x x :y y :z z} :item item}))]
        (when l (ledger/remember! c (ledger/reconcile l (escape/block-at-of (:primitives c)))))
        (if (bs/open-floor? ((:block-at in) cell))
          (if (= "no-support" (some-> r .-status))
            {:reason :no-support-floor :cell cell :item item :why (str "nothing to place " item " against at " (pr-str cell))}
            {:reason :place-failed :cell cell :item item})
          (do (ctx/update-mem! c update ::bridged (fnil conj #{}) cell)
              (spent/note! c item cell)
              nil))))))

(defn bridged?
  "Whether the job bridged the floor cell itself (placed it for a step), so what is under it is not judged."
  [c cell]
  (contains? (::bridged (ctx/mem c)) cell))

(defn ^:async place-all!
  "Place item at each of cells in order: nil when all stand, else the first stop map."
  [c in item cells take-back?]
  (loop [[cell & more] cells]
    (let [stop (await (place-floor! c in item cell take-back?))]
      (cond stop stop
            (seq more) (recur more)))))

(defn ^:async place-plans!
  "Try each of plans (bridge-plans) until one stands: nil then, else the last stop map (a plan answered no-support gives way to the next)."
  [c in item plans take-back?]
  (loop [[plan & more] plans]
    (let [stop (await (place-all! c in item plan take-back?))]
      (if (and (= :no-support-floor (:reason stop)) (seq more))
        (recur more)
        stop))))
